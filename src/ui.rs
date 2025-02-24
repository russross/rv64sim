use super::*;

use crossterm::{
    cursor::MoveTo,
    event::{self, Event, KeyCode, KeyEvent},
    execute, queue,
    style::{Color, Colors, Print, SetColors},
    terminal::{Clear, ClearType},
    tty::IsTty,
};

macro_rules! serr {
    ($expr:expr) => {
        $expr.map_err(|e| format!("{}", e))
    };
}

macro_rules! ferr {
    ($expr:expr) => {
        $expr.map_err(|_| fmt::Error)
    };
}

pub struct Tui {
    machine: Machine,
    instructions: Vec<Rc<Instruction>>,
    addresses: HashMap<i64, usize>,
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

    output_buffer: Vec<u8>,
}

impl Pane {
    fn new(
        out: &mut Vec<u8>,
        color: Colors,
        origin_x: u16,
        origin_y: u16,
        outer_width: u16,
        outer_height: u16,
        flow_down: bool,
    ) -> Result<Self, String> {
        let pane = Pane {
            left: origin_x + 1,
            top: origin_y + 1,
            width: outer_width - 2,
            height: outer_height - 2,
            cursor_y: if flow_down { 0 } else { outer_height - 3 },
            cursor_x: 0,
            flow_down,
            color,
            output_buffer: Vec::new(),
        };

        serr!(queue!(out, SetColors(color)))?;
        serr!(queue!(out, MoveTo(origin_x, origin_y), Print("┌")))?;
        serr!(pane.hline(out, origin_x + 1, origin_y, outer_width - 2))?;
        serr!(queue!(
            out,
            MoveTo(origin_x + outer_width - 1, origin_y),
            Print("┐")
        ))?;
        serr!(queue!(
            out,
            MoveTo(origin_x, origin_y + outer_height - 1),
            Print("└")
        ))?;
        serr!(pane.hline(
            out,
            origin_x + 1,
            origin_y + outer_height - 1,
            outer_width - 2
        ))?;
        serr!(queue!(
            out,
            MoveTo(origin_x + outer_width - 1, origin_y + outer_height - 1),
            Print("┘")
        ))?;
        serr!(pane.vline(out, origin_x, origin_y + 1, outer_height - 2))?;
        serr!(pane.vline(
            out,
            origin_x + outer_width - 1,
            origin_y + 1,
            outer_height - 2
        ))?;
        Ok(pane)
    }

    fn split_right(
        &mut self,
        out: &mut Vec<u8>,
        width: u16,
        flow_down: bool,
        corners: &mut Vec<(u16, u16)>,
    ) -> Result<Self, String> {
        self.width -= width + 1;
        let (top_y, bot_y, x) = (self.top - 1, self.top + self.height, self.left + self.width);

        // draw the split line
        self.vline(out, x, top_y + 1, self.height)?;

        // draw top and bottom corners
        let mut top_corner = "┼";
        if !corners.contains(&(top_y, x)) {
            corners.push((top_y, x));
            top_corner = "┬";
        }
        serr!(queue!(out, MoveTo(x, top_y), Print(top_corner)))?;

        let mut bot_corner = "┼";
        if !corners.contains(&(bot_y, x)) {
            corners.push((bot_y, x));
            bot_corner = "┴";
        }
        serr!(queue!(out, MoveTo(x, bot_y), Print(bot_corner)))?;

        Ok(Pane {
            top: self.top,
            left: x + 1,
            height: self.height,
            width,
            cursor_y: if flow_down { 0 } else { self.height - 1 },
            cursor_x: 0,
            flow_down,
            color: self.color,
            output_buffer: Vec::new(),
        })
    }

    fn split_bottom(
        &mut self,
        out: &mut Vec<u8>,
        height: u16,
        flow_down: bool,
        corners: &mut Vec<(u16, u16)>,
    ) -> Result<Self, String> {
        self.height -= height + 1;
        let (y, left_x, right_x) = (
            self.top + self.height,
            self.left - 1,
            self.left + self.width,
        );

        // draw the split line
        self.hline(out, left_x + 1, y, self.width)?;

        // draw left and righr corners
        let mut left_corner = "┼";
        if !corners.contains(&(y, left_x)) {
            corners.push((y, left_x));
            left_corner = "├";
        }
        serr!(queue!(out, MoveTo(left_x, y), Print(left_corner)))?;

        let mut right_corner = "┼";
        if !corners.contains(&(y, right_x)) {
            corners.push((y, right_x));
            right_corner = "┤";
        }
        serr!(queue!(out, MoveTo(right_x, y), Print(right_corner)))?;

        if !self.flow_down {
            self.cursor_y = self.height - 1;
        }
        Ok(Pane {
            top: self.top + self.height + 1,
            left: self.left,
            height,
            width: self.width,
            cursor_y: if flow_down { 0 } else { height - 1 },
            cursor_x: 0,
            flow_down,
            color: self.color,
            output_buffer: Vec::new(),
        })
    }

    fn hline(&self, out: &mut Vec<u8>, x: u16, y: u16, length: u16) -> Result<(), String> {
        serr!(queue!(out, MoveTo(x, y)))?;
        for _ in 0..length {
            serr!(queue!(out, Print("─")))?;
        }
        Ok(())
    }

    fn vline(&self, out: &mut Vec<u8>, x: u16, y: u16, length: u16) -> Result<(), String> {
        for y in y..y + length {
            serr!(queue!(out, MoveTo(x, y), Print("│")))?;
        }
        Ok(())
    }

    fn label(&mut self, msg: &str) -> Result<(), String> {
        let msg = if msg.len() > self.width as usize - 4 {
            &msg[..self.width as usize - 4]
        } else {
            msg
        };
        serr!(queue!(
            &mut self.output_buffer,
            SetColors(self.color),
            MoveTo(self.left + 1, self.top - 1),
            Print(" "),
            Print(msg),
            Print(" ")
        ))
    }
}

impl fmt::Write for Pane {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        ferr!(queue!(&mut self.output_buffer, SetColors(self.color)))?;

        let mut buff = Vec::new();
        for c in s.chars() {
            if c == '\n' {
                if !buff.is_empty() {
                    if self.cursor_y < self.height && self.cursor_x < self.width {
                        buff.truncate((self.width - self.cursor_x) as usize);
                        ferr!(queue!(
                            &mut self.output_buffer,
                            MoveTo(self.left + self.cursor_x, self.top + self.cursor_y)
                        ))?;
                        ferr!(write!(
                            &mut self.output_buffer,
                            "{}",
                            buff.iter().collect::<String>()
                        ))?;
                    }
                    buff.clear();
                }
                self.cursor_x = 0;
                if self.flow_down {
                    self.cursor_y += 1;
                } else if self.cursor_y > 0 {
                    self.cursor_y -= 1;
                } else {
                    self.cursor_y = u16::MAX;
                }
            } else {
                buff.push(c);
            }
        }
        if !buff.is_empty() && self.cursor_y < self.height && self.cursor_x < self.width {
            buff.truncate((self.width - self.cursor_x) as usize);
            ferr!(queue!(
                &mut self.output_buffer,
                MoveTo(self.left + self.cursor_x, self.top + self.cursor_y)
            ))?;
            ferr!(write!(
                &mut self.output_buffer,
                "{}",
                buff.iter().collect::<String>()
            ))?;
            self.cursor_x += buff.len() as u16;
        }

        Ok(())
    }
}

impl Tui {
    pub fn new(
        machine: Machine,
        instructions: Vec<Rc<Instruction>>,
        addresses: HashMap<i64, usize>,
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
        serr!(execute!(
            io::stdout(),
            crossterm::terminal::EnterAlternateScreen,
            crossterm::cursor::Hide
        ))?;

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
        })
    }

    pub fn main_loop(&mut self) -> Result<(), String> {
        loop {
            // Draw the current state
            let source_height = self.draw()?;
            let event = serr!(event::read())?;
            match event {
                // watch for quit
                Event::Key(KeyEvent {
                    code: KeyCode::Char('q'),
                    ..
                }) => {
                    return Ok(());
                }

                // handle all the other keys
                Event::Key(key_event) => self.handle_key(key_event, source_height)?,

                // ignore other events
                _ => (),
            }
        }
    }

    fn handle_key(&mut self, key_event: KeyEvent, source_height: u16) -> Result<(), String> {
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
                self.cursor_index = min(
                    self.cursor_index + source_height as usize,
                    self.instructions.len() - 1,
                );
            }

            // stepping and jumping
            KeyCode::Left => {
                if self.sequence_index > 0 {
                    self.sequence_index -= 1;
                    self.machine
                        .apply(&self.sequence[self.sequence_index], false);
                    self.machine
                        .set_most_recent_memory(&self.sequence, self.sequence_index);
                    self.set_cursor_to_current();
                }
            }
            KeyCode::Right => {
                if self.sequence_index + 1 < self.sequence.len() {
                    self.machine
                        .apply(&self.sequence[self.sequence_index], true);
                    self.sequence_index += 1;
                    self.machine
                        .set_most_recent_memory(&self.sequence, self.sequence_index);
                    self.set_cursor_to_current();
                }
            }
            KeyCode::Home => {
                // jump back to where the current function was entered
                self.set_cursor_to_current();
                let (func_start_pc, func_end_pc) =
                    find_function_bounds(&self.instructions, self.cursor_index);
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
                        if let Field::Opcode("call" | "jal") =
                            self.sequence[self.sequence_index - 1].instruction.fields[0]
                        {
                            break;
                        }
                    }
                    self.sequence_index -= 1;
                }
                self.machine
                    .set_most_recent_memory(&self.sequence, self.sequence_index);
                self.set_cursor_to_current();
            }
            KeyCode::End => {
                // jump forward to where the current function is about to exit
                self.set_cursor_to_current();
                let (func_start_pc, func_end_pc) =
                    find_function_bounds(&self.instructions, self.cursor_index);
                while self.sequence_index < self.sequence.len() - 1 {
                    let effects = &self.sequence[self.sequence_index];
                    let pc = effects.instruction.address;
                    let Field::Opcode(op) =
                        self.sequence[self.sequence_index].instruction.fields[0]
                    else {
                        unreachable!()
                    };
                    if func_start_pc <= pc && pc < func_end_pc && op == "ret" {
                        break;
                    }
                    self.machine.apply(effects, true);
                    self.sequence_index += 1;
                }
                self.machine
                    .set_most_recent_memory(&self.sequence, self.sequence_index);
                self.set_cursor_to_current();
            }
            KeyCode::Enter => {
                // jump forward to the next time this line is reached
                let target_pc = self.instructions[self.cursor_index].address;
                for peek in self.sequence_index + 1..self.sequence.len() {
                    if self.sequence[peek].instruction.address == target_pc {
                        // play the sequence forward to this point
                        while self.sequence_index < peek {
                            self.machine
                                .apply(&self.sequence[self.sequence_index], true);
                            self.sequence_index += 1;
                        }
                        self.machine
                            .set_most_recent_memory(&self.sequence, self.sequence_index);
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
                            self.machine
                                .apply(&self.sequence[self.sequence_index], false);
                        }
                        self.machine
                            .set_most_recent_memory(&self.sequence, self.sequence_index);
                        self.set_cursor_to_current();
                        break;
                    }
                }
            }

            KeyCode::Char('?' | 'h') => {
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

            // ignore other keys
            _ => {}
        }
        Ok(())
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
        serr!(queue!(
            out,
            SetColors(self.normal_color),
            Clear(ClearType::All)
        ))?;
        let mut corners = Vec::new();
        let mut source = Pane::new(&mut out, self.normal_color, 0, 0, size_x, size_y, true)?;
        // an 80-column terminal gets source and memory views, narrower does not
        let (stack, data) = if size_x >= 80
            && (self.show_stack || self.show_data && self.machine.data_start > 0)
        {
            let mut mem = source.split_right(&mut out, 39, false, &mut corners)?;
            if mem.height >= 22 && self.show_stack && self.show_data && self.machine.data_start > 0
            {
                let data = mem.split_bottom(&mut out, mem.height * 2 / 3, false, &mut corners)?;
                (Some(mem), Some(data))
            } else if self.show_stack
                && (self.machine.most_recent_memory >= self.machine.stack_start || !self.show_data)
            {
                (Some(mem), None)
            } else {
                (None, Some(mem))
            }
        } else {
            (None, None)
        };

        let output_lines = if self.show_output && !self.machine.stdout.is_empty() {
            // count the output lines (a single trailing newline is ignored)
            self.machine
                .stdout
                .iter()
                .rev()
                .skip(1)
                .fold(1, |a, &elt| if elt == b'\n' { a + 1 } else { a })
        } else {
            0
        };

        // a 24-line terminal gets source, registers, and output, shorter does not
        let (registers, output) = if source.height >= 22
            && self.show_registers
            && self.show_output
            && !self.machine.stdout.is_empty()
        {
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
            let mut registers = source.split_bottom(&mut out, 5 + lines, true, &mut corners)?;
            let output = registers.split_bottom(&mut out, lines, true, &mut corners)?;
            (Some(registers), Some(output))
        } else if source.height >= 17 && !self.show_registers && output_lines > 0 {
            let surplus = source.height.saturating_sub(12).saturating_sub(5);

            // output claims any extra lines that would have gone to registers
            let output_min = 4 + surplus.min(5);
            let surplus = surplus.saturating_sub(5);
            let natural_size = output_min + (surplus + 2) / 3;
            let lines = output_lines.max(4).min(natural_size);
            let output = source.split_bottom(&mut out, lines, true, &mut corners)?;
            (None, Some(output))
        } else if source.height >= 17 && self.show_registers {
            let registers = source.split_bottom(&mut out, 4, true, &mut corners)?;
            (Some(registers), None)
        } else {
            (None, None)
        };

        // now render each pane
        let mut status_line = self.render_source(&mut source)?;
        out.append(&mut source.output_buffer);
        if !status_line.is_empty() {
            status_line.truncate(size_x.saturating_sub(6) as usize);
            serr!(queue!(
                out,
                MoveTo(2, size_y - 1),
                SetColors(self.normal_color),
                Print(" "),
                Print(&status_line),
                Print(" ")
            ))?;
        }
        let help_msg = " ? for help ";
        if size_x >= status_line.len() as u16 + help_msg.len() as u16 + 8 {
            serr!(queue!(
                out,
                MoveTo(size_x - 3 - help_msg.len() as u16, size_y - 1),
                SetColors(self.normal_color),
                Print(help_msg)
            ))?;
        }

        if let Some(mut registers) = registers {
            self.render_registers(&mut registers)?;
            out.append(&mut registers.output_buffer);
        }
        if let Some(mut stack) = stack {
            self.render_memory(&mut stack, true)?;
            out.append(&mut stack.output_buffer);
        }
        if let Some(mut data) = data {
            self.render_memory(&mut data, false)?;
            out.append(&mut data.output_buffer);
        }
        if let Some(mut output) = output {
            self.render_output(&mut output)?;
            out.append(&mut output.output_buffer);
        }

        if self.show_help {
            let (help_x, help_y) = (63, 22);
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
            let mut help = Pane::new(&mut out, self.normal_color, left, top, width, height, true)?;
            self.render_help(&mut help)?;
            out.append(&mut help.output_buffer);
        }

        let mut stdout = io::stdout();
        serr!(stdout.write_all(&out))?;
        serr!(stdout.flush())?;
        Ok(source.height)
    }

    fn render_source(&self, pane: &mut Pane) -> Result<String, String> {
        // find the instruction
        let effects = &self.sequence[self.sequence_index];
        let pc = effects.instruction.address;
        let pc_i = self.addresses[&effects.instruction.address];

        // set the top label/status line
        let label = if self.hex_mode {
            format!(
                "Step {}/{} PC:{:#x}",
                self.sequence_index + 1,
                self.sequence.len(),
                pc
            )
        } else {
            format!(
                "Step {}/{} PC:{}",
                self.sequence_index + 1,
                self.sequence.len(),
                pc
            )
        };

        pane.label(&label)?;

        // are we drawing a branch arrow?
        let (arrow_top_addr, arrow_bottom_addr) = if self.instructions[pc_i].branches() {
            let (old, new) = self.sequence[self.sequence_index].pc;
            if new != old
                && (pc_i + 1 >= self.instructions.len()
                    || new != self.instructions[pc_i + 1].address)
            {
                (old.min(new), old.max(new))
            } else {
                (-1, -1)
            }
        } else {
            (-1, -1)
        };

        let (start, end) = calc_range(self.instructions.len(), self.cursor_index, pane.height);
        for i in start..end {
            // handle out-of-range lines
            if i < 0 || i >= self.instructions.len() as i64 {
                pane.color = self.normal_color;
                serr!(writeln!(pane))?;
                continue;
            }

            // get the line as chars and pad it to required length
            let instruction = &self.instructions[i as usize];
            let addr = instruction.address;
            let mut line: Vec<char> = instruction.to_string(self.hex_mode).chars().collect();
            while line.len() < pane.width as usize || line.len() < 14 {
                line.push(' ');
            }

            // arrows?
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
            if i == pc_i as i64 {
                pane.color = self.current_pc_color;
            } else if i == self.cursor_index as i64 {
                pane.color = self.cursor_color;
            } else {
                pane.color = self.normal_color;
            }
            serr!(writeln!(pane, "{}", line))?;
        }

        // draw the side-effects label
        let mut side_effects = self.sequence[self.sequence_index].report(self.hex_mode);
        side_effects.truncate(2);
        if side_effects[0].is_empty() {
            side_effects.remove(0);
        }
        let status = side_effects.join(", ");

        Ok(status)
    }

    fn render_registers(&mut self, pane: &mut Pane) -> Result<(), String> {
        pane.label("Registers")?;

        let lines = vec![
            vec!["ra", "sp", "gp", "tp"],
            vec!["a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"],
            vec!["t0", "t1", "t2", "t3", "t4", "t5", "t6"],
            vec![
                "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11",
            ],
        ];
        for line in lines {
            for reg in line {
                let val = self
                    .machine
                    .get(R.iter().position(|&r_str| r_str == reg).unwrap());
                if self.hex_mode && !(0..=9).contains(&val) {
                    serr!(write!(pane, "{}:{:#x} ", reg, val))?;
                } else {
                    serr!(write!(pane, "{}:{:} ", reg, val))?;
                }
            }
            serr!(writeln!(pane))?;
        }
        Ok(())
    }

    fn render_memory(&mut self, pane: &mut Pane, is_stack: bool) -> Result<(), String> {
        let mut stack_colors = Vec::new();
        let (colors, start, mem_start, mem_end, most_recent_start, most_recent_end) = if is_stack {
            pane.label("Stack")?;

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
            pane.label("Data")?;

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
                serr!(write!(pane, "{:06x}:", addr))?;
            } else {
                serr!(write!(pane, "       "))?;
            }

            // print the bytes as hex
            for j in 0..8 {
                if let (Some(byte), color) = bytes[j] {
                    pane.color = color;
                    serr!(write!(pane, "{:02x}", byte))?;

                    // trailing space?
                    if j + 1 < 8 {
                        match bytes[j + 1] {
                            (Some(_), next_color) if color == next_color => {}
                            _ => pane.color = self.normal_color,
                        }
                        serr!(write!(pane, " "))?;
                    }
                } else {
                    pane.color = self.normal_color;
                    if j + 1 < 8 {
                        serr!(write!(pane, "   "))?;
                    } else {
                        serr!(write!(pane, "  "))?;
                    }
                }
            }

            pane.color = self.normal_color;
            serr!(write!(pane, " "))?;

            // print the bytes as ascii
            for &maybe_byte in &bytes {
                if let (Some(byte), color) = maybe_byte {
                    pane.color = color;
                    if byte as char >= ' ' && byte as char <= '~' {
                        serr!(write!(pane, "{}", byte as char))?;
                    } else {
                        serr!(write!(pane, "·"))?;
                    }
                } else {
                    pane.color = self.normal_color;
                    serr!(write!(pane, " "))?;
                }
            }

            serr!(write!(pane, "\n"))?;
        }

        Ok(())
    }

    fn render_output(&mut self, pane: &mut Pane) -> Result<(), String> {
        pane.label("Output")?;

        for line in get_last_n_lines(&self.machine.stdout, pane.height as usize) {
            serr!(writeln!(pane, "{}", line))?;
        }

        Ok(())
    }

    fn render_help(&mut self, pane: &mut Pane) -> Result<(), String> {
        pane.label("Help")?;

        serr!((|| {
            writeln!(
                pane,
                "                                                               "
            )?;
            writeln!(
                pane,
                " To move the cursor without stepping:                          "
            )?;
            writeln!(
                pane,
                "   ↑/up arrow       : move cursor up one instruction           "
            )?;
            writeln!(
                pane,
                "   ↓/down arrow     : move cursor down one instruction         "
            )?;
            writeln!(
                pane,
                "   PgUp/Fn-↑        : move cursor up one page                  "
            )?;
            writeln!(
                pane,
                "   PgDown/Fn-↓      : move cursor down one page                "
            )?;
            writeln!(
                pane,
                "                                                               "
            )?;
            writeln!(
                pane,
                " To step forward/rewind through program:                       "
            )?;
            writeln!(
                pane,
                "   →/right arrow    : step forward one instruction             "
            )?;
            writeln!(
                pane,
                "   ←/left arrow     : rewind one instruction                   "
            )?;
            writeln!(
                pane,
                "   end key/Fn-→     : fast forward to end of current function  "
            )?;
            writeln!(
                pane,
                "   home key/Fn-←    : rewind to start of current function      "
            )?;
            writeln!(
                pane,
                "   enter/return     : fast forward to instruction under cursor "
            )?;
            writeln!(
                pane,
                "   backspace/delete : rewind to instruction under cursor       "
            )?;
            writeln!(
                pane,
                "                                                               "
            )?;
            writeln!(
                pane,
                " Toggles:                                                      "
            )?;
            writeln!(
                pane,
                "   x                : switch between hexadecimal/decimal mode  "
            )?;
            writeln!(
                pane,
                "   r                : hide/show register window pane           "
            )?;
            writeln!(
                pane,
                "   o                : hide/show output window pane             "
            )?;
            writeln!(
                pane,
                "   s                : hide/show stack segment window pane      "
            )?;
            writeln!(
                pane,
                "   d                : hide/show data segment window pane       "
            )?;
            writeln!(
                pane,
                "                                                               "
            )
        })())?;

        Ok(())
    }
}

impl Drop for Tui {
    fn drop(&mut self) {
        if let Err(e) = (|| {
            crossterm::terminal::disable_raw_mode()?;
            execute!(
                std::io::stdout(),
                crossterm::terminal::LeaveAlternateScreen,
                crossterm::cursor::Show
            )?;
            std::io::stdout().flush()
        })() {
            eprintln!("{}", e);
        }
    }
}

fn get_last_n_lines(data: &[u8], n: usize) -> Vec<String> {
    // Split on newlines
    let chunks: Vec<_> = data.split(|&b| b == b'\n').collect();

    // Take last n chunks, skipping the last one if it's empty
    let valid_chunks = if chunks.last().is_some_and(|chunk| chunk.is_empty()) {
        &chunks[..chunks.len() - 1]
    } else {
        &chunks[..]
    };

    let start_idx = valid_chunks.len().saturating_sub(n);

    // Convert chunks to strings, replacing invalid UTF-8 sequences
    valid_chunks[start_idx..]
        .iter()
        .map(|chunk| String::from_utf8_lossy(chunk).into_owned())
        .collect()
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

fn find_function_bounds(instructions: &[Rc<Instruction>], current: usize) -> (i64, i64) {
    let (mut start_pc, mut end_pc) = (
        instructions[0].address,
        instructions.last().unwrap().address,
    );
    for i in (0..=current).rev() {
        if let Some(label) = &instructions[i].label {
            if !is_digit(label) {
                start_pc = instructions[i].address;
                break;
            }
        }
    }
    for instruction in instructions.iter().skip(current + 1) {
        if let Some(label) = &instruction.label {
            if !is_digit(label) {
                end_pc = instruction.address;
                break;
            }
        }
    }
    (start_pc, end_pc)
}
