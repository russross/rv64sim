                .global _start
                .global return_addr
                .equ    stdout, 1
                .equ    sys_write, 64
                .equ    sys_exit, 93
                .text

_start:
				# test addi
				la      a0, test_addi_msg
				call	print_string
				call	test_addi

				# test addiw
				la      a0, test_addiw_msg
				call	print_string
				call	test_addiw

				# test add
				la      a0, test_add_msg
				call	print_string
				call	test_add

				# test addw
				la      a0, test_addw_msg
				call	print_string
				call	test_addw

				# test andi
				la      a0, test_andi_msg
				call	print_string
				call	test_andi

				# test and
				la      a0, test_and_msg
				call	print_string
				call	test_and

				# test auipc
				la      a0, test_auipc_msg
				call	print_string
				call	test_auipc

				# test beq
				la      a0, test_beq_msg
				call	print_string
				call	test_beq

				# test bge
				la      a0, test_bge_msg
				call	print_string
				call	test_bge

				# test bgeu
				la      a0, test_bgeu_msg
				call	print_string
				call	test_bgeu

				# test blt
				la      a0, test_blt_msg
				call	print_string
				call	test_blt

				# test bltu
				la      a0, test_bltu_msg
				call	print_string
				call	test_bltu

				# test bne
				la      a0, test_bne_msg
				call	print_string
				call	test_bne

				# test div
				la      a0, test_div_msg
				call	print_string
				call	test_div

				# test divu
				la      a0, test_divu_msg
				call	print_string
				call	test_divu

				# test divuw
				la      a0, test_divuw_msg
				call	print_string
				call	test_divuw

				# test divw
				la      a0, test_divw_msg
				call	print_string
				call	test_divw

				# test jalr
				la      a0, test_jalr_msg
				call	print_string
				call	test_jalr

				# test jal
				la      a0, test_jal_msg
				call	print_string
				call	test_jal

				# test lb
				la      a0, test_lb_msg
				call	print_string
				call	test_lb

				# test lbu
				la      a0, test_lbu_msg
				call	print_string
				call	test_lbu

				# test ld
				la      a0, test_ld_msg
				call	print_string
				call	test_ld

				# test lh
				la      a0, test_lh_msg
				call	print_string
				call	test_lh

				# test lhu
				la      a0, test_lhu_msg
				call	print_string
				call	test_lhu

				# test lui
				la      a0, test_lui_msg
				call	print_string
				call	test_lui

				# test lw
				la      a0, test_lw_msg
				call	print_string
				call	test_lw

				# test lwu
				la      a0, test_lwu_msg
				call	print_string
				call	test_lwu

				# test mulh
				la      a0, test_mulh_msg
				call	print_string
				call	test_mulh

				# test mulhsu
				la      a0, test_mulhsu_msg
				call	print_string
				call	test_mulhsu

				# test mulhu
				la      a0, test_mulhu_msg
				call	print_string
				call	test_mulhu

				# test mul
				la      a0, test_mul_msg
				call	print_string
				call	test_mul

				# test mulw
				la      a0, test_mulw_msg
				call	print_string
				call	test_mulw

				# test ori
				la      a0, test_ori_msg
				call	print_string
				call	test_ori

				# test or
				la      a0, test_or_msg
				call	print_string
				call	test_or

				# test rem
				la      a0, test_rem_msg
				call	print_string
				call	test_rem

				# test remu
				la      a0, test_remu_msg
				call	print_string
				call	test_remu

				# test remuw
				la      a0, test_remuw_msg
				call	print_string
				call	test_remuw

				# test remw
				la      a0, test_remw_msg
				call	print_string
				call	test_remw

				# test sb
				la      a0, test_sb_msg
				call	print_string
				call	test_sb

				# test sd
				la      a0, test_sd_msg
				call	print_string
				call	test_sd

				# test sh
				la      a0, test_sh_msg
				call	print_string
				call	test_sh

				# test slli
				la      a0, test_slli_msg
				call	print_string
				call	test_slli

				# test slliw
				la      a0, test_slliw_msg
				call	print_string
				call	test_slliw

				# test sll
				la      a0, test_sll_msg
				call	print_string
				call	test_sll

				# test sllw
				la      a0, test_sllw_msg
				call	print_string
				call	test_sllw

				# test slti
				la      a0, test_slti_msg
				call	print_string
				call	test_slti

				# test sltiu
				la      a0, test_sltiu_msg
				call	print_string
				call	test_sltiu

				# test slt
				la      a0, test_slt_msg
				call	print_string
				call	test_slt

				# test sltu
				la      a0, test_sltu_msg
				call	print_string
				call	test_sltu

				# test srai
				la      a0, test_srai_msg
				call	print_string
				call	test_srai

				# test sraiw
				la      a0, test_sraiw_msg
				call	print_string
				call	test_sraiw

				# test sra
				la      a0, test_sra_msg
				call	print_string
				call	test_sra

				# test sraw
				la      a0, test_sraw_msg
				call	print_string
				call	test_sraw

				# test srli
				la      a0, test_srli_msg
				call	print_string
				call	test_srli

				# test srliw
				la      a0, test_srliw_msg
				call	print_string
				call	test_srliw

				# test srl
				la      a0, test_srl_msg
				call	print_string
				call	test_srl

				# test srlw
				la      a0, test_srlw_msg
				call	print_string
				call	test_srlw

				# test sub
				la      a0, test_sub_msg
				call	print_string
				call	test_sub

				# test subw
				la      a0, test_subw_msg
				call	print_string
				call	test_subw

				# test sw
				la      a0, test_sw_msg
				call	print_string
				call	test_sw

				# test xori
				la      a0, test_xori_msg
				call	print_string
				call	test_xori

				# test xor
				la      a0, test_xor_msg
				call	print_string
				call	test_xor

				# test ma_data
				la      a0, test_ma_data_msg
				call	print_string
				call	test_ma_data

                # finished all tests
                la      a0, finished_msg
                call    print_string

                # exit
                li      a0, 0
                li      a7, sys_exit
                ecall

# print_string(s)
print_string:
                # a1: ptr
                # a2: len
                mv      a1, a0
                li      a2, 0
1:
                add     t0, a1, a2
                lb      t1, (t0)
                beqz    t1, 2f
                addi    a2, a2, 1
                j       1b
2:
                li      a0, stdout
                li      a7, sys_write
                ecall
                bgez    a0, 3f
                neg     a0, a0
                li      a7, sys_exit
                ecall
3:
                ret

                .data
test_addi_msg:  .asciz  "testing addi...\n"
test_addiw_msg: .asciz  "testing addiw...\n"
test_add_msg:   .asciz  "testing add...\n"
test_addw_msg:  .asciz  "testing addw...\n"
test_andi_msg:  .asciz  "testing andi...\n"
test_and_msg:   .asciz  "testing and...\n"
test_auipc_msg: .asciz  "testing auipc...\n"
test_beq_msg:   .asciz  "testing beq...\n"
test_bge_msg:   .asciz  "testing bge...\n"
test_bgeu_msg:  .asciz  "testing bgeu...\n"
test_blt_msg:   .asciz  "testing blt...\n"
test_bltu_msg:  .asciz  "testing bltu...\n"
test_bne_msg:   .asciz  "testing bne...\n"
test_div_msg:   .asciz  "testing div...\n"
test_divu_msg:  .asciz  "testing divu...\n"
test_divuw_msg: .asciz  "testing divuw...\n"
test_divw_msg:  .asciz  "testing divw...\n"
test_jalr_msg:  .asciz  "testing jalr...\n"
test_jal_msg:   .asciz  "testing jal...\n"
test_lb_msg:    .asciz  "testing lb...\n"
test_lbu_msg:   .asciz  "testing lbu...\n"
test_ld_msg:    .asciz  "testing ld...\n"
test_lh_msg:    .asciz  "testing lh...\n"
test_lhu_msg:   .asciz  "testing lhu...\n"
test_lui_msg:   .asciz  "testing lui...\n"
test_lw_msg:    .asciz  "testing lw...\n"
test_lwu_msg:   .asciz  "testing lwu...\n"
test_mulh_msg:  .asciz  "testing mulh...\n"
test_mulhsu_msg: .asciz  "testing mulhsu...\n"
test_mulhu_msg: .asciz  "testing mulhu...\n"
test_mul_msg:   .asciz  "testing mul...\n"
test_mulw_msg:  .asciz  "testing mulw...\n"
test_ori_msg:   .asciz  "testing ori...\n"
test_or_msg:    .asciz  "testing or...\n"
test_rem_msg:   .asciz  "testing rem...\n"
test_remu_msg:  .asciz  "testing remu...\n"
test_remuw_msg: .asciz  "testing remuw...\n"
test_remw_msg:  .asciz  "testing remw...\n"
test_sb_msg:    .asciz  "testing sb...\n"
test_sd_msg:    .asciz  "testing sd...\n"
test_sh_msg:    .asciz  "testing sh...\n"
test_slli_msg:  .asciz  "testing slli...\n"
test_slliw_msg: .asciz  "testing slliw...\n"
test_sll_msg:   .asciz  "testing sll...\n"
test_sllw_msg:  .asciz  "testing sllw...\n"
test_slti_msg:  .asciz  "testing slti...\n"
test_sltiu_msg: .asciz  "testing sltiu...\n"
test_slt_msg:   .asciz  "testing slt...\n"
test_sltu_msg:  .asciz  "testing sltu...\n"
test_srai_msg:  .asciz  "testing srai...\n"
test_sraiw_msg: .asciz  "testing sraiw...\n"
test_sra_msg:   .asciz  "testing sra...\n"
test_sraw_msg:  .asciz  "testing sraw...\n"
test_srli_msg:  .asciz  "testing srli...\n"
test_srliw_msg: .asciz  "testing srliw...\n"
test_srl_msg:   .asciz  "testing srl...\n"
test_srlw_msg:  .asciz  "testing srlw...\n"
test_sub_msg:   .asciz  "testing sub...\n"
test_subw_msg:  .asciz  "testing subw...\n"
test_sw_msg:    .asciz  "testing sw...\n"
test_xori_msg:  .asciz  "testing xori...\n"
test_xor_msg:   .asciz  "testing xor...\n"
test_ma_data_msg: .asciz  "testing ma_data...\n"
finished_msg:   .asciz  "completed all tests\n"
                .bss
                .balign 16
return_addr:    .quad   0
