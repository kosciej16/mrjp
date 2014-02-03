.file "a"
.text
.data
.globl _start
main:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
movl $1, %eax
pushl %eax
movl $1, %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
je T0
movl $0, %eax
T0:
cmpl $0, %eax
je .endIf0
movl $42, %eax
pushl %eax
call printInt
popl %ebx
.endIf0:
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

