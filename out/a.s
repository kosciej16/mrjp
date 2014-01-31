.file "a"
.text
.data
.globl _start
_main:
pushl %ebp
movl %esp, %ebp
subl $0, %ebp
movl $1, %eax
movl $1, %eax
pushl %eax
call printInt
popl %ebx
.L0:
movl $0, %eax
jmp _return_main
_return_main:
popl %ebp
ret
_start:
call _main
movl $1,%eax
int  $0x80

