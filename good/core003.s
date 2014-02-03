.file "a"
.text
.data
.globl _start
f:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
movl $1, %eax
cmpl $0, %eax
je .else0
movl $0, %eax
jmp _return_f
jmp .endElse1
.else0:
.endElse1:
_return_f:
addl $0, %esp
popl %ebp
ret
g:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
movl $0, %eax
cmpl $0, %eax
je .else2
jmp .endElse3
.else2:
movl $0, %eax
jmp _return_g
.endElse3:
_return_g:
addl $0, %esp
popl %ebp
ret
p:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
_return_p:
addl $0, %esp
popl %ebp
ret
main:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
call p
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

