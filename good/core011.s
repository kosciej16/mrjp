.file "a"
.text
.data
.globl _start
main:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
negl %eax
pushl %eax
call printInt
popl %ebx
movl $0, %eax
jmp _return_main
_return_main:
addl $0, %esp
popl %ebp
ret
_start:
call main
movl $1,%eax
int  $0x80

