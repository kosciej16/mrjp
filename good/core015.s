.file "a"
.text
.data
.globl _start
main:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
movl $17, %eax
pushl %eax
call ev
popl %ebx
pushl %eax
call printInt
popl %ebx
movl $0, %eax
jmp _return_main
_return_main:
addl $0, %esp
popl %ebp
ret
ev:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
movl 8(%ebp), %eax
pushl %eax
movl $0, %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
jg T0
movl $0, %eax
T0:
cmpl $0, %eax
je .else0
movl $2, %eax
pushl %eax
movl 8(%ebp), %eax
popl %ebx
subl %ebx, %eax
pushl %eax
call ev
popl %ebx
jmp _return_ev
jmp .endElse1
.else0:
movl 8(%ebp), %eax
pushl %eax
movl $0, %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
jl T1
movl $0, %eax
T1:
cmpl $0, %eax
je .else2
movl $0, %eax
jmp _return_ev
jmp .endElse3
.else2:
movl $1, %eax
jmp _return_ev
.endElse3:
.endElse1:
_return_ev:
addl $0, %esp
popl %ebp
ret
_start:
call main
movl $1,%eax
int  $0x80

