.file "a"
.text
.data
.globl _start
main:
pushl %ebp
movl %esp, %ebp
subl $4, %esp
movl $7, %eax
movl %eax, -4(%ebp)
movl -4(%ebp), %eax
pushl %eax
call printInt
popl %ebx
movl $0, %eax
jmp _return_main
_return_main:
addl $4, %esp
popl %ebp
ret
_start:
call main
movl $1,%eax
int  $0x80

