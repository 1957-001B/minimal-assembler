.section __TEXT,__text
.globl _start
_start:
    b _code_start

.section __TEXT,__code
_code_start:
    .incbin "output.bin"
