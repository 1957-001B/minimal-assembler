.text
.global _start

_start:
    // Load the syscall number for write (4) into X8
    movz x8, #0x4

    // Load the file descriptor for stdout (1) into X0
    movz x0, #0x1

    // Load the address of the message into X1
    movz x1, #:abs16:message
    movk x1, #:abs16:message, lsl #16

    // Load the length of the message into X2
    movz x2, #0xC  
    // Length of "Hello, World!\n" is 12 bytes

    // Make the syscall (write)
    svc #0

    // Load the syscall number for exit (1) into X8
    movz x8, #0x1

    // Load the exit status (0) into X0
    movz x0, #0x0

    // Make the syscall (exit)
    svc #0

.data
message:
    .asciz "Hello, World!\n"
