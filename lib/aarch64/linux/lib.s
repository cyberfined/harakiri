.section .rodata
input_error_message: .string "error: non-integer input\n"

.text
.globl _start
.globl echo_string
.globl echo_int
.globl input
.extern main

strlen:
    mov x2, x0
strlen_cycle:
    ldrb w3, [x2]
    cmp w3, #0
    beq strlen_exit
    add x2, x2, #1
    b strlen_cycle
strlen_exit:
    sub x2, x2, x0
    ret x30

echo_string:
    sub sp, sp, #16
    str x30, [sp]
    str x8, [sp, #8]
    mov x1, x0
    bl strlen
    mov x0, #1
    mov x8, #64
    svc #0
    ldr x8, [sp, #8]
    ldr x30, [sp]
    add sp, sp, #16
    ret x30

echo_int: 
    sub sp, sp, #136
    str x8, [sp]
    mov x2, #10
    add x5, sp, #8

    cmp x0, #0
    mov x1, #0
    bge echo_int_itoa
    neg x0, x0
    mov x1, #1

echo_int_itoa:
    sdiv x3, x0, x2
    mov x4, x3
    msub x3, x3, x2, x0
    add x3, x3, #48
    strb w3, [x5]
    add x5, x5, #1
    mov x0, x4
    cmp x0, #0
    bne echo_int_itoa

    cmp x1, #0
    beq echo_int_reverse
    mov w3, #45
    strb w3, [x5]
    add x5, x5, #1

echo_int_reverse:
    mov w3, #0
    strb w3, [x5]
    add x4, sp, #8
    sub x8, x5, x4
    sub x5, x5, #1

echo_int_reverse_cycle:
    cmp x4, x5
    bge echo_int_print
    ldrb w1, [x4]
    ldrb w2, [x5]
    mov w3, w2
    strb w1, [x5]
    strb w3, [x4]
    sub x5, x5, #1
    add x4, x4, #1
    b echo_int_reverse_cycle

echo_int_print:
    mov x2, x8
    mov x8, #64
    mov x0, #1
    add x1, sp, #8
    svc #0

    ldr x8, [sp]
    add sp, sp, #136
    ret x30

input:
    sub sp, sp, #136
    str x8, [sp, #128]

    mov x8, #63
    mov x0, #1
    mov x1, sp
    mov x2, #128
    svc #0

    sub x0, x0, #1
    ldrb w1, [sp, x0]
    cmp w1, #0xa
    bne input_sign
    sub x0, x0, #1

input_sign:
    mov x4, #0
    mov x8, #1
    mov x2, sp
    ldrb w1, [sp]
    cmp w1, #45
    mov x1, #0
    bne input_atoi
    mov x1, #1
    sub x0, x0, #1
    add x2, x2, #1

input_atoi:
    cmp x0, #0
    blt input_return
    ldrb w3, [x2, x0]
    subs x3, x3, #48
    blt input_print_error
    cmp x3, #9
    bgt input_print_error

    mul x3, x3, x8
    add x4, x4, x3

    mov x3, #10
    mul x8, x8, x3
    sub x0, x0, #1
    b input_atoi

input_return:
    cmp x1, #0
    beq input_pos_return
    neg x4, x4

input_pos_return:
    mov x0, x4
    ldr x8, [sp, #128]
    add sp, sp, #136
    ret x30

input_print_error:
    mov x8, #64
    mov x0, #1
    ldr x1, =input_error_message
    mov x2, 25
    svc #0
    mov x8, #93
    mov x0, #2
    svc #0

_start:
    bl main
    mov x8, #93
    mov x0, #0
    svc #0
