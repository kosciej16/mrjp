.file "a"
.text
.data
.globl _start
main:
pushl %ebp
movl %esp, %ebp
subl $8, %esp
movl $56, %eax
movl %eax, -8(%ebp)
movl -8(%ebp), %eax
pushl %eax
movl $45, %eax
popl %ebx
addl %ebx, %eax
pushl %eax
movl $2, %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
jle T0
movl $0, %eax
T0:
cmpl $0, %eax
je .else0
movl $1, %eax
movl %eax, -4(%ebp)
jmp .endElse1
.else0:
movl $2, %eax
movl %eax, -4(%ebp)
.endElse1:
movl -4(%ebp), %eax
pushl %eax
call printInt
popl %ebx
movl $0, %eax
jmp _return_main
_return_main:
addl $8, %esp
popl %ebp
ret
_start:
call main
movl $1,%eax
int  $0x80

