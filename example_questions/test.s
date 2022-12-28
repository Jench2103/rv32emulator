.data
MAT_A:
.byte   1 4 5 3
.byte   1 4 2 2
.byte   5 5 3 3
.byte   4 3 1 5
MAT_B:
.byte   1 4 3 4
.byte   5 5 5 2
.byte   4 4 5 4
.byte   3 4 3 5
MAT_C:
.byte   0 0 0 0
.byte   0 0 0 0
.byte   0 0 0 0
.byte   0 0 0 0

# MAT_ANS:
# .byte   0x32 0x38 0x39 0x2f
# .byte   0x23 0x28 0x27 0x1e
# .byte   0x33 0x45 0x40 0x39
# .byte   0x26 0x37 0x2f 0x33

ACCEL_REG_BASE_ADDR:
.word   0x100000

MAT_SIZE:
.word   0x00030003

MAT_MEM_STRIDE:
.word   0x040404

.text
la      s0, ACCEL_REG_BASE_ADDR
lw      s0, 0(s0)

la      s1, MAT_A
la      s2, MAT_B
la      s3, MAT_C

la      s4, MAT_SIZE
lw      s4, 0(s4)

la      s5, MAT_MEM_STRIDE
lw      s5, 0(s5)

sw      s4, 8(s0)
sw      s4, 12(s0)
sw      s4, 16(s0)
sw      s1, 20(s0)
sw      s2, 24(s0)
sw      s3, 28(s0)
sw      s5, 32(s0)

li      t0, 1
sw      t0, 0(s0)

check_finish:
lw      t6, 0(s1)
lw      t1, 4(s0)
bne     t1, t0, check_finish

lw      s8, 0(s3)
lw      s9, 4(s3)
lw      s10, 8(s3)
lw      s11, 12(s3)

end:
hcf
