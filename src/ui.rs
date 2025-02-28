use super::riscv::*;
use super::*;
use std::mem::take;
use std::rc::Rc;

use crossterm::{
    cursor::MoveTo,
    event::{self, Event, KeyCode, KeyEvent},
    queue,
    style::{Color, Colors, Print, SetColors},
    tty::IsTty,
};

macro_rules! serr {
    ($expr:expr) => {
        $expr.map_err(|e| format!("{}", e))
    };
}

pub struct Tui {
    machine: Machine,
    instructions: Vec<Rc<Instruction>>,
    addresses: HashMap<i64, usize>,
    pseudo_addresses: HashMap<usize, usize>,
    sequence: Vec<Effects>,
    sequence_index: usize,
    cursor_index: usize,

    normal_color: Colors,
    inactive_stack_color: Colors,
    current_pc_color: Colors,
    cursor_color: Colors,
    data_colors: Vec<(i64, Colors)>,
    pastels: Vec<Colors>,

    hex_mode: bool,
    show_registers: bool,
    show_output: bool,
    show_stack: bool,
    show_data: bool,
    show_help: bool,
    verbose: bool,
    show_addresses: bool,
}

impl Tui {
    pub fn new(
        machine: Machine,
        instructions: Vec<Rc<Instruction>>,
        addresses: HashMap<i64, usize>,
        pseudo_addresses: HashMap<usize, usize>,
        sequence: Vec<Effects>,
    ) -> Result<Self, String> {
        // make sure stdout is connected to a tty
        if !io::stdout().is_tty() {
            return Err("debug mode only works in an interactive terminal/tty".to_string());
        }

        // make sure stdin is connected to the tty
        // if not, close it and re-open as /dev/tty
        if !io::stdin().is_tty() {
            let stdin_fd = io::stdin().as_raw_fd();
            drop(io::stdin());
            unsafe { libc::close(stdin_fd) };
            serr!(File::open("/dev/tty"))?;
        }

        // take over terminal
        serr!(crossterm::terminal::enable_raw_mode())?;
        serr!(queue!(io::stdout(), crossterm::terminal::EnterAlternateScreen, crossterm::cursor::Hide))?;

        // colors:
        let black = Color::AnsiValue(16);
        let white = Color::AnsiValue(231);

        // normal line (white text on black)
        let normal_color = Colors::new(white, black);

        // inactive stack space (grey text on black)
        let inactive_stack_color = Colors::new(Color::AnsiValue(240), black);

        // current pc (black text on DarkSeaGreen4)
        let current_pc_color = Colors::new(black, Color::AnsiValue(71));

        // cursor if != pc (black text on Grey53)
        let cursor_color = Colors::new(black, Color::AnsiValue(102));

        // memory chunks, with pastel foreground colors
        // the are all saturation 20%, lightness 60%, with various hues
        let pastels = vec![
            //Colors::new(Color::AnsiValue(102), black),
            Colors::new(Color::AnsiValue(138), black),
            Colors::new(Color::AnsiValue(144), black),
            Colors::new(Color::AnsiValue(108), black),
            Colors::new(Color::AnsiValue(109), black),
            Colors::new(Color::AnsiValue(103), black),
            Colors::new(Color::AnsiValue(139), black),
            //Colors::new(Color::Rgb{ r:144, g:173, b:133 }, black), // h:103
            //Colors::new(Color::Rgb{ r:133, g:156, b:173 }, black), // h:206
            //Colors::new(Color::Rgb{ r:173, g:133, b:167 }, black), // h:309
            //Colors::new(Color::Rgb{ r:173, g:167, b:133 }, black), // h:51
            //Colors::new(Color::Rgb{ r:133, g:173, b:156 }, black), // h:154
            //Colors::new(Color::Rgb{ r:144, g:133, b:173 }, black), // h:257
            //Colors::new(Color::Rgb{ r:173, g:133, b:133 }, black), // h:0
        ];

        // create cycling color pairs for address symbols
        let mut data_colors = Vec::new();
        let mut address_symbols: Vec<i64> = machine.address_symbols.keys().copied().collect();
        address_symbols.sort_unstable();
        for (i, &address) in address_symbols.iter().enumerate() {
            let color = pastels[i % pastels.len()];
            data_colors.push((address, color));
        }

        // if there is no data segments
        if data_colors.is_empty() {
            data_colors.push((0, normal_color));
        }

        Ok(Tui {
            machine,
            instructions,
            addresses,
            pseudo_addresses,
            sequence,
            sequence_index: 0,
            cursor_index: 0,

            normal_color,
            inactive_stack_color,
            current_pc_color,
            cursor_color,
            data_colors,
            pastels,

            hex_mode: false,
            show_registers: true,
            show_output: true,
            show_stack: true,
            show_data: true,
            show_help: false,
            verbose: false,
            show_addresses: false,
        })
    }

    pub fn main_loop(&mut self) -> Result<(), String> {
        loop {
            // Draw the current state
            let source_height = self.draw()?;
            if let Event::Key(key_event) = serr!(event::read())? {
                if self.handle_key(key_event, source_height)? {
                    break;
                }
            }
        }
        Ok(())
    }

    fn handle_key(&mut self, key_event: KeyEvent, source_height: u16) -> Result<bool, String> {
        match key_event.code {
            _ if self.show_help => {
                self.show_help = false;
            }

            // cursor motion
            KeyCode::Up => {
                self.cursor_index = self.cursor_index.saturating_sub(1);
            }
            KeyCode::Down => {
                self.cursor_index = min(self.cursor_index + 1, self.instructions.len() - 1);
            }
            KeyCode::PageUp => {
                self.cursor_index = self.cursor_index.saturating_sub(source_height as usize);
            }
            KeyCode::PageDown => {
                self.cursor_index = min(self.cursor_index + source_height as usize, self.instructions.len() - 1);
            }

            // stepping and jumping
            KeyCode::Left => {
                if self.sequence_index > 0 {
                    self.sequence_index -= 1;
                    self.machine.apply(&self.sequence[self.sequence_index], false);
                    self.machine.set_most_recent_memory(&self.sequence, self.sequence_index);
                    self.set_cursor_to_current();
                }
            }
            KeyCode::Right => {
                if self.sequence_index + 1 < self.sequence.len() {
                    self.machine.apply(&self.sequence[self.sequence_index], true);
                    self.sequence_index += 1;
                    self.machine.set_most_recent_memory(&self.sequence, self.sequence_index);
                    self.set_cursor_to_current();
                }
            }
            KeyCode::Home => {
                // jump back to where the current function was entered
                self.set_cursor_to_current();
                let (func_start_pc, func_end_pc) =
                    find_function_bounds(&self.machine.address_symbols, &self.instructions, self.cursor_index);
                let mut first = true;
                while self.sequence_index > 0 {
                    let effects = &self.sequence[self.sequence_index];
                    if first {
                        first = false;
                    } else {
                        self.machine.apply(effects, false);
                    }

                    let pc = effects.instruction.address;
                    if func_start_pc <= pc && pc < func_end_pc && self.sequence_index > 0 {
                        let prev_effects = &self.sequence[self.sequence_index - 1];
                        let (prev_pc, _) = prev_effects.pc;
                        if prev_pc < func_start_pc || prev_pc > func_end_pc {
                            match prev_effects.instruction.op {
                                Op::Jal { rd, .. } | Op::Jalr { rd, .. } if rd != ZERO => break,
                                _ => {}
                            }
                        }
                    }
                    self.sequence_index -= 1;
                }
                self.machine.set_most_recent_memory(&self.sequence, self.sequence_index);
                self.set_cursor_to_current();
            }
            KeyCode::End => {
                // jump forward to where the current function is about to exit
                self.set_cursor_to_current();
                let (func_start_pc, func_end_pc) =
                    find_function_bounds(&self.machine.address_symbols, &self.instructions, self.cursor_index);
                while self.sequence_index < self.sequence.len() - 1 {
                    let effects = &self.sequence[self.sequence_index];
                    let pc = effects.instruction.address;
                    if let Op::Jalr { rd: ZERO, rs1: RA, offset: 0 } = effects.instruction.op {
                        if func_start_pc <= pc && pc < func_end_pc {
                            break;
                        }
                    };
                    self.machine.apply(effects, true);
                    self.sequence_index += 1;
                }
                self.machine.set_most_recent_memory(&self.sequence, self.sequence_index);
                self.set_cursor_to_current();
            }
            KeyCode::Enter => {
                // jump forward to the next time this line is reached
                let target_pc = self.instructions[self.cursor_index].address;
                for peek in self.sequence_index + 1..self.sequence.len() {
                    if self.sequence[peek].instruction.address == target_pc {
                        // play the sequence forward to this point
                        while self.sequence_index < peek {
                            self.machine.apply(&self.sequence[self.sequence_index], true);
                            self.sequence_index += 1;
                        }
                        self.machine.set_most_recent_memory(&self.sequence, self.sequence_index);
                        self.set_cursor_to_current();
                        break;
                    }
                }
            }
            KeyCode::Backspace => {
                // find the previous time the pc was at this address
                let target_pc = self.instructions[self.cursor_index].address;
                for peek in (0..self.sequence_index).rev() {
                    if self.sequence[peek].instruction.address == target_pc {
                        // play the sequence backward to this point
                        while self.sequence_index > peek {
                            self.sequence_index -= 1;
                            self.machine.apply(&self.sequence[self.sequence_index], false);
                        }
                        self.machine.set_most_recent_memory(&self.sequence, self.sequence_index);
                        self.set_cursor_to_current();
                        break;
                    }
                }
            }

            KeyCode::Char('?') => {
                self.show_help = true;
            }

            KeyCode::Char('x') => {
                self.hex_mode = !self.hex_mode;
            }

            KeyCode::Char('r') => {
                self.show_registers = !self.show_registers;
            }

            KeyCode::Char('o') => {
                self.show_output = !self.show_output;
            }

            KeyCode::Char('s') => {
                self.show_stack = !self.show_stack;
            }

            KeyCode::Char('d') => {
                self.show_data = !self.show_data;
            }

            KeyCode::Char('v') => {
                self.verbose = !self.verbose;
            }

            KeyCode::Char('a') => {
                self.show_addresses = !self.show_addresses;
            }

            KeyCode::Char('q') => {
                return Ok(true);
            }

            // ignore other keys
            _ => {}
        }
        Ok(false)
    }

    fn set_cursor_to_current(&mut self) {
        self.cursor_index = self.addresses[&self.sequence[self.sequence_index].instruction.address];
    }

    fn draw(&mut self) -> Result<u16, String> {
        let (size_x, size_y) = serr!(crossterm::terminal::size())?;
        if size_y < 3 || size_x < 5 {
            return Ok(0);
        }

        // build the screen layout
        let mut out = Vec::new();
        for _ in 0..size_y {
            out.push(vec![(' ', self.normal_color); size_x as usize]);
        }
        let mut corners = Vec::new();
        let mut source = Pane::new(out, self.normal_color, 0, 0, size_x, size_y, true);

        // an 80-column terminal gets source and memory views, narrower does not
        let (stack, data, out) = if size_x >= 80 && (self.show_stack || self.show_data && self.machine.data_start > 0) {
            let mut mem = source.split_right(39, false, &mut corners);
            if mem.height >= 22 && self.show_stack && self.show_data && self.machine.data_start > 0 {
                let mut data = mem.split_bottom(mem.height * 2 / 3, false, &mut corners);
                let out = take(&mut data.out);
                (Some(mem), Some(data), out)
            } else if self.show_stack
                && (self.machine.most_recent_memory >= self.machine.stack_start || !self.show_data)
            {
                let out = take(&mut mem.out);
                (Some(mem), None, out)
            } else {
                let out = take(&mut mem.out);
                (None, Some(mem), out)
            }
        } else {
            (None, None, take(&mut source.out))
        };

        let output_lines = if self.show_output && !self.machine.stdout.is_empty() {
            // count the output lines (a single trailing newline is ignored)
            self.machine.stdout.iter().rev().skip(1).fold(1, |a, &elt| if elt == b'\n' { a + 1 } else { a })
        } else {
            0
        };

        // a 24-line terminal gets source, registers, and output, shorter does not
        source.out = out;
        let (registers, output, mut out) =
            if source.height >= 22 && self.show_registers && self.show_output && !self.machine.stdout.is_empty() {
                // output gets at least 4 lines and registers gets exactly 4
                // they sit at their minimums up to a 24-line terminal, then
                // additional lines are split 2/3 source and 1/3 output pane
                // with the output getting the first new line
                // so min source height is 12 before allowing other panes
                let surplus = source.height.saturating_sub(12).saturating_sub(10);
                let output_min = 4;
                let natural_size = output_min + (surplus + 2) / 3;

                // keep at least 4 lines but otherwise don't grow beyond the amount of output
                let lines = output_lines.max(output_min).min(natural_size);
                let mut registers = source.split_bottom(5 + lines, true, &mut corners);
                let mut output = registers.split_bottom(lines, true, &mut corners);
                let out = take(&mut output.out);
                (Some(registers), Some(output), out)
            } else if source.height >= 17 && !self.show_registers && output_lines > 0 {
                let surplus = source.height.saturating_sub(12).saturating_sub(5);

                // output claims any extra lines that would have gone to registers
                let output_min = 4 + surplus.min(5);
                let surplus = surplus.saturating_sub(5);
                let natural_size = output_min + (surplus + 2) / 3;
                let lines = output_lines.max(4).min(natural_size);
                let mut output = source.split_bottom(lines, true, &mut corners);
                let out = take(&mut output.out);
                (None, Some(output), out)
            } else if source.height >= 17 && self.show_registers {
                let mut registers = source.split_bottom(4, true, &mut corners);
                let out = take(&mut registers.out);
                (Some(registers), None, out)
            } else {
                (None, None, take(&mut source.out))
            };

        // now render each pane
        source.out = out;
        let status_line = self.render_source(&mut source);
        if !status_line.is_empty() {
            source.write_padded_str(&status_line, 2, size_y - 1, size_x.saturating_sub(4) as usize);
        }
        let help_msg = "? for help";
        if size_x >= status_line.len() as u16 + help_msg.len() as u16 + 8 {
            source.write_padded_str(help_msg, size_x - 3 - help_msg.len() as u16, size_y - 1, help_msg.len() + 2);
        }

        out = take(&mut source.out);
        if let Some(mut registers) = registers {
            registers.out = out;
            self.render_registers(&mut registers);
            out = take(&mut registers.out);
        }
        if let Some(mut stack) = stack {
            stack.out = out;
            self.render_memory(&mut stack, true);
            out = take(&mut stack.out);
        }
        if let Some(mut data) = data {
            data.out = out;
            self.render_memory(&mut data, false);
            out = take(&mut data.out);
        }
        if let Some(mut output) = output {
            output.out = out;
            self.render_output(&mut output);
            out = take(&mut output.out);
        }

        if self.show_help {
            let (help_x, help_y) = (63, 17);
            let (left, width) = if size_x >= help_x + 2 {
                let space = (size_x - (help_x + 2)) / 2;
                (space, help_x + 2)
            } else {
                (0, size_x)
            };
            let (top, height) = if size_y >= help_y + 2 {
                let space = (size_y - (help_y + 2)) / 2;
                (space, help_y + 2)
            } else {
                (0, size_y)
            };
            let mut help = Pane::new(out, self.normal_color, left, top, width, height, true);
            self.render_help(&mut help);
            out = take(&mut help.out);
        }

        let mut stdout = io::stdout();
        for (y, row) in out.iter().enumerate() {
            serr!(queue!(stdout, MoveTo(0, y as u16)))?;
            let mut buff = String::new();
            for (x, &(ch, color)) in row.iter().enumerate() {
                buff.push(ch);
                if x + 1 == row.len() || row[x + 1].1 != color {
                    serr!(queue!(stdout, SetColors(color), Print(&buff)))?;
                    buff.clear();
                }
            }
        }
        serr!(stdout.flush())?;

        Ok(source.height)
    }

    fn render_source(&self, pane: &mut Pane) -> String {
        // find the instruction
        let effects = &self.sequence[self.sequence_index];
        let pc = effects.instruction.address;
        let pc_i = self.addresses[&effects.instruction.address];

        // set the top label/status line
        let label = if self.hex_mode {
            format!("Step {}/{} PC:0x{:x}", self.sequence_index + 1, self.sequence.len(), pc)
        } else {
            format!("Step {}/{} PC:{}", self.sequence_index + 1, self.sequence.len(), pc)
        };

        pane.label(&label);

        // are we drawing a branch arrow?
        let (arrow_top_addr, arrow_bottom_addr) = if effects.instruction.op.branch_target(pc).is_some() {
            let (old, new) = effects.pc;
            if new != old && (pc_i + 1 >= self.instructions.len() || new != self.instructions[pc_i + 1].address) {
                (old.min(new), old.max(new))
            } else {
                (-1, -1)
            }
        } else {
            (-1, -1)
        };

        let (length, pc_index, cursor_index) = if self.verbose {
            (self.instructions.len(), pc_i, self.cursor_index)
        } else {
            (
                self.instructions.last().unwrap().pseudo_index + 1,
                effects.instruction.pseudo_index,
                self.instructions[self.cursor_index].pseudo_index,
            )
        };
        let (start, end) = calc_range(length, cursor_index, pane.height);

        for i in start..end {
            // handle out-of-range lines
            if i < 0 {
                writeln!(pane).unwrap();
                continue;
            } else if i >= length as i64 {
                break;
            }

            let index = if self.verbose { i as usize } else { self.pseudo_addresses[&(i as usize)] };

            // get the line as chars and pad it to required length
            let inst = &self.instructions[index];
            let addr = inst.address;
            let mut line: Vec<char> = fields_to_string(
                if self.verbose { &inst.verbose_fields } else { &inst.pseudo_fields },
                addr,
                self.machine.global_pointer,
                self.hex_mode,
                self.verbose,
                self.show_addresses,
                &self.machine.address_symbols,
            )
            .chars()
            .collect();
            while line.len() < pane.width as usize || line.len() < 15 {
                line.push(' ');
            }

            // arrows?
            // TODO: place arrows correctly when addresses are displayed
            if addr == arrow_top_addr {
                line[12] = '┌';
                line[13] = '─';
                line[14] = '─';
            } else if addr == arrow_bottom_addr {
                line[12] = '└';
                line[13] = '─';
                line[14] = '─';
            } else if addr > arrow_top_addr && addr < arrow_bottom_addr {
                line[12] = '│';
            }

            // draw the line in the correct color
            let line: String = line.iter().collect();
            if i as usize == pc_index {
                pane.color = self.current_pc_color;
            } else if i as usize == cursor_index {
                pane.color = self.cursor_color;
            } else {
                pane.color = self.normal_color;
            }
            writeln!(pane, "{}", line).unwrap();
            pane.color = self.normal_color;
        }

        // draw the side-effects label
        let mut side_effects = self.sequence[self.sequence_index].report(self.hex_mode);
        side_effects.truncate(2);
        if side_effects[0].is_empty() {
            side_effects.remove(0);
        }
        side_effects.join(", ")
    }

    fn render_registers(&mut self, pane: &mut Pane) {
        pane.label("Registers");

        let lines = vec![
            vec!["ra", "sp", "gp", "tp"],
            vec!["a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"],
            vec!["t0", "t1", "t2", "t3", "t4", "t5", "t6"],
            vec!["s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11"],
        ];
        for line in lines {
            for reg in line {
                let val = self.machine.get(R.iter().position(|&r_str| r_str == reg).unwrap());
                if self.hex_mode && !(0..=9).contains(&val) {
                    write!(pane, "{}:0x{:x} ", reg, val).unwrap();
                } else {
                    write!(pane, "{}:{:} ", reg, val).unwrap();
                }
            }
            writeln!(pane).unwrap();
        }
    }

    fn render_memory(&mut self, pane: &mut Pane, is_stack: bool) {
        let mut stack_colors = Vec::new();
        let (colors, start, mem_start, mem_end, most_recent_start, most_recent_end) = if is_stack {
            pane.label("Stack");

            // make a color list for stack frame boundaries
            // start at high-numbered addresses so colors are consistent
            // as the number of frames changes
            let sp = self.machine.get(SP);
            let mut found_sp = false;
            let mut i = 0;
            for &frame in &self.machine.stack_frames {
                if frame == sp {
                    found_sp = true;
                }
                stack_colors.push((frame, self.pastels[i % self.pastels.len()]));
                i += 1;
            }
            if !found_sp {
                stack_colors.push((sp, self.pastels[i % self.pastels.len()]));
            }
            stack_colors.push((0, self.inactive_stack_color));
            stack_colors = stack_colors.iter().rev().copied().collect();

            let (start, _end) = calc_range(
                ((self.machine.stack_end - self.machine.stack_start) / 8) as usize,
                ((self.machine.most_recent_stack.0 - self.machine.stack_start) / 8) as usize,
                pane.height,
            );

            let (mr_start, mr_size) = self.machine.most_recent_stack;
            (
                &stack_colors,
                start,
                self.machine.stack_start,
                self.machine.stack_end,
                mr_start,
                mr_start + mr_size as i64,
            )
        } else {
            pane.label("Data");

            let (start, _end) = calc_range(
                ((self.machine.data_end - self.machine.data_start) / 8) as usize,
                ((self.machine.most_recent_data.0 - self.machine.data_start) / 8) as usize,
                pane.height,
            );

            let (mr_start, mr_size) = self.machine.most_recent_data;
            (
                &self.data_colors,
                start,
                self.machine.data_start,
                self.machine.data_end,
                mr_start,
                mr_start + mr_size as i64,
            )
        };

        // render each memory line
        let mut current_region = 0;
        for i in 0..pane.height as i64 {
            let addr = mem_start + (start + i) * 8;

            // gather the bytes and colors to print
            let mut addr_to_print = None;
            let mut bytes = [(None as Option<u8>, self.normal_color); 8];
            for j in addr..addr + 8 {
                if j < mem_start || j >= mem_end {
                    continue;
                }

                // make sure the address for this line is printed
                addr_to_print = Some(addr);

                // did we just hit a region label/stack frame boundary?
                while current_region + 1 < colors.len() && colors[current_region + 1].0 <= j {
                    current_region += 1;
                }

                // get the color for this byte
                let mut next_color = colors[current_region].1;

                // does this need special highlighting as the most recent access?
                if most_recent_start <= j && j < most_recent_end {
                    // invert the color parts
                    let fg = next_color.foreground.unwrap();
                    let bg = next_color.background.unwrap();
                    next_color = Colors::new(bg, fg);
                }

                if let Ok(byte) = self.machine.load_u8(j) {
                    bytes[(j - addr) as usize] = (Some(byte as u8), next_color);
                }
            }

            // print the line
            pane.color = self.normal_color;
            if let Some(addr) = addr_to_print {
                write!(pane, "{:06x}:", addr).unwrap();
            } else {
                write!(pane, "       ").unwrap();
            }

            // print the bytes as hex
            for j in 0..8 {
                if let (Some(byte), color) = bytes[j] {
                    pane.color = color;
                    write!(pane, "{:02x}", byte).unwrap();

                    // trailing space?
                    if j + 1 < 8 {
                        match bytes[j + 1] {
                            (Some(_), next_color) if color == next_color => {}
                            _ => pane.color = self.normal_color,
                        }
                        write!(pane, " ").unwrap();
                    }
                } else {
                    pane.color = self.normal_color;
                    if j + 1 < 8 {
                        write!(pane, "   ").unwrap();
                    } else {
                        write!(pane, "  ").unwrap();
                    }
                }
            }

            pane.color = self.normal_color;
            write!(pane, " ").unwrap();

            // print the bytes as ascii
            for &maybe_byte in &bytes {
                if let (Some(byte), color) = maybe_byte {
                    pane.color = color;
                    if byte as char >= ' ' && byte as char <= '~' {
                        write!(pane, "{}", byte as char).unwrap();
                    } else {
                        write!(pane, "·").unwrap();
                    }
                } else {
                    pane.color = self.normal_color;
                    write!(pane, " ").unwrap();
                }
            }

            writeln!(pane).unwrap();
        }
    }

    fn render_output(&mut self, pane: &mut Pane) {
        pane.label("Output");

        for line in get_last_n_lines(&self.machine.stdout, pane.height as usize) {
            writeln!(pane, "{}", line).unwrap();
        }
    }

    fn render_help(&mut self, pane: &mut Pane) {
        pane.label("Help");

        writeln!(pane, " To move the cursor without stepping:                          ").unwrap();
        writeln!(pane, "   ↑/up arrow       : move cursor up one instruction           ").unwrap();
        writeln!(pane, "   ↓/down arrow     : move cursor down one instruction         ").unwrap();
        writeln!(pane, "   PgUp/Fn-↑        : move cursor up one page                  ").unwrap();
        writeln!(pane, "   PgDown/Fn-↓      : move cursor down one page                ").unwrap();
        writeln!(pane, "                                                               ").unwrap();
        writeln!(pane, " To step forward/rewind through program:                       ").unwrap();
        writeln!(pane, "   →/right arrow    : step forward one instruction             ").unwrap();
        writeln!(pane, "   ←/left arrow     : rewind one instruction                   ").unwrap();
        writeln!(pane, "   end key/Fn-→     : fast forward to end of current function  ").unwrap();
        writeln!(pane, "   home key/Fn-←    : rewind to start of current function      ").unwrap();
        writeln!(pane, "   enter/return     : fast forward to instruction under cursor ").unwrap();
        writeln!(pane, "   backspace/delete : rewind to instruction under cursor       ").unwrap();
        writeln!(pane, "                                                               ").unwrap();
        writeln!(pane, " To toggle what is displayed:                                  ").unwrap();
        writeln!(pane, "   (r)egister pane, (o)utput pane, (s)tack pane, (d)ata pane   ").unwrap();
        writeln!(pane, "   (v)erbose mode, show (a)ddresses, use he(x)adecimal         ").unwrap();
    }
}

impl Drop for Tui {
    fn drop(&mut self) {
        if let Err(e) = (|| {
            crossterm::terminal::disable_raw_mode()?;
            queue!(std::io::stdout(), crossterm::terminal::LeaveAlternateScreen, crossterm::cursor::Show)?;
            std::io::stdout().flush()
        })() {
            eprintln!("{}", e);
        }
    }
}

struct Pane {
    top: u16,
    left: u16,
    width: u16,
    height: u16,
    cursor_x: u16,
    cursor_y: u16,
    flow_down: bool,
    color: Colors,
    out: Vec<Vec<(char, Colors)>>,
}

impl Pane {
    fn new(
        out: Vec<Vec<(char, Colors)>>,
        color: Colors,
        origin_x: u16,
        origin_y: u16,
        outer_width: u16,
        outer_height: u16,
        flow_down: bool,
    ) -> Self {
        let mut pane = Pane {
            left: origin_x + 1,
            top: origin_y + 1,
            width: outer_width - 2,
            height: outer_height - 2,
            cursor_y: if flow_down { 0 } else { outer_height - 3 },
            cursor_x: 0,
            flow_down,
            color,
            out,
        };

        pane.out[origin_y as usize][origin_x as usize] = ('┌', color);
        pane.hline(origin_x + 1, origin_y, outer_width - 2);
        pane.out[origin_y as usize][(origin_x + outer_width - 1) as usize] = ('┐', pane.color);

        pane.out[(origin_y + outer_height - 1) as usize][origin_x as usize] = ('└', pane.color);
        pane.hline(origin_x + 1, origin_y + outer_height - 1, outer_width - 2);
        pane.out[(origin_y + outer_height - 1) as usize][(origin_x + outer_width - 1) as usize] = ('┘', pane.color);
        pane.vline(origin_x, origin_y + 1, outer_height - 2);
        pane.vline(origin_x + outer_width - 1, origin_y + 1, outer_height - 2);
        pane
    }

    fn split_right(&mut self, width: u16, flow_down: bool, corners: &mut Vec<(u16, u16)>) -> Self {
        self.width -= width + 1;
        let (top_y, bot_y, x) = (self.top - 1, self.top + self.height, self.left + self.width);

        // draw the split line
        self.vline(x, top_y + 1, self.height);

        // draw top and bottom corners
        let mut top_corner = '┼';
        if !corners.contains(&(top_y, x)) {
            corners.push((top_y, x));
            top_corner = '┬';
        }
        self.out[top_y as usize][x as usize] = (top_corner, self.color);

        let mut bot_corner = '┼';
        if !corners.contains(&(bot_y, x)) {
            corners.push((bot_y, x));
            bot_corner = '┴';
        }
        self.out[bot_y as usize][x as usize] = (bot_corner, self.color);

        Pane {
            top: self.top,
            left: x + 1,
            height: self.height,
            width,
            cursor_y: if flow_down { 0 } else { self.height - 1 },
            cursor_x: 0,
            flow_down,
            color: self.color,
            out: take(&mut self.out),
        }
    }

    fn split_bottom(&mut self, height: u16, flow_down: bool, corners: &mut Vec<(u16, u16)>) -> Self {
        self.height -= height + 1;
        let (y, left_x, right_x) = (self.top + self.height, self.left - 1, self.left + self.width);

        // draw the split line
        self.hline(left_x + 1, y, self.width);

        // draw left and righr corners
        let mut left_corner = '┼';
        if !corners.contains(&(y, left_x)) {
            corners.push((y, left_x));
            left_corner = '├';
        }
        self.out[y as usize][left_x as usize] = (left_corner, self.color);

        let mut right_corner = '┼';
        if !corners.contains(&(y, right_x)) {
            corners.push((y, right_x));
            right_corner = '┤';
        }
        self.out[y as usize][right_x as usize] = (right_corner, self.color);

        if !self.flow_down {
            self.cursor_y = self.height - 1;
        }

        Pane {
            top: self.top + self.height + 1,
            left: self.left,
            height,
            width: self.width,
            cursor_y: if flow_down { 0 } else { height - 1 },
            cursor_x: 0,
            flow_down,
            color: self.color,
            out: take(&mut self.out),
        }
    }

    fn hline(&mut self, x: u16, y: u16, length: u16) {
        for x in x..x + length {
            self.out[y as usize][x as usize] = ('─', self.color);
        }
    }

    fn vline(&mut self, x: u16, y: u16, length: u16) {
        for y in y..y + length {
            self.out[y as usize][x as usize] = ('│', self.color);
        }
    }

    fn label(&mut self, msg: &str) {
        self.write_padded_str(msg, self.left + 1, self.top - 1, self.width as usize - 4);
    }

    fn write_padded_str(&mut self, msg: &str, x: u16, y: u16, max_width: usize) {
        let mut msg: Vec<_> = msg.chars().collect();
        msg.truncate(max_width - 2);
        msg.insert(0, ' ');
        msg.push(' ');
        for (i, &ch) in msg.iter().enumerate() {
            self.out[y as usize][x as usize + i] = (ch, self.color);
        }
    }
}

impl fmt::Write for Pane {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for c in s.chars() {
            if c == '\n' {
                self.cursor_x = 0;
                if self.flow_down {
                    self.cursor_y += 1;
                } else if self.cursor_y > 0 {
                    self.cursor_y -= 1;
                } else {
                    self.cursor_y = u16::MAX;
                }
            } else if self.cursor_x < self.width && self.cursor_y < self.height {
                self.out[(self.top + self.cursor_y) as usize][(self.left + self.cursor_x) as usize] = (c, self.color);
                self.cursor_x += 1;
            }
        }
        Ok(())
    }
}

fn get_last_n_lines(data: &[u8], n: usize) -> Vec<String> {
    // split on newlines
    let chunks: Vec<_> = data.split(|&b| b == b'\n').collect();

    // take last n chunks, skipping the last one if it's empty
    let chunks =
        if chunks.last().is_some_and(|chunk| chunk.is_empty()) { &chunks[..chunks.len() - 1] } else { &chunks[..] };

    let start_idx = chunks.len().saturating_sub(n);

    // convert chunks to strings
    chunks[start_idx..].iter().map(|chunk| String::from_utf8_lossy(chunk).into_owned()).collect()
}

fn calc_range(length: usize, cursor: usize, window_size: u16) -> (i64, i64) {
    let half = (window_size - 1) / 2;
    let mut start = cursor as i64 - half as i64;
    let mut end = start + window_size as i64;

    // shift up or down if it will fit more info
    if start < 0 {
        let shift = std::cmp::min(-start, std::cmp::max(0, length as i64 - end));
        start += shift;
        end += shift;
    }

    if end > length as i64 {
        let shift = std::cmp::min(end - length as i64, std::cmp::max(0, start));
        start -= shift;
        end -= shift;
    }

    (start, end)
}

fn find_function_bounds(
    symbols: &HashMap<i64, String>,
    instructions: &[Rc<Instruction>],
    current: usize,
) -> (i64, i64) {
    let (mut start_pc, mut end_pc) = (instructions[0].address, instructions.last().unwrap().address);
    for instruction in instructions[0..=current].iter().rev() {
        if let Some(label) = symbols.get(&instruction.address) {
            if label.parse::<usize>().is_err() {
                start_pc = instruction.address;
                break;
            }
        }
    }
    for instruction in instructions[current + 1..].iter() {
        if let Some(label) = symbols.get(&instruction.address) {
            if label.parse::<usize>().is_err() {
                end_pc = instruction.address;
                break;
            }
        }
    }
    (start_pc, end_pc)
}
