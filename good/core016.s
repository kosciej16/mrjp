.file "a"
.text
.data
.globl _start
main:
pushl %ebp
movl %esp, %ebp
subl $4, %esp
movl $17, %eax
movl %eax, -4(%ebp)
while_0:
movl -4(%ebp), %eax
pushl %eax
movl $0, %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
jg T0
movl $0, %eax
T0:
cmpl $0, %eax
je end0
movl $2, %eax
pushl %eax
movl -4(%ebp), %eax
popl %ebx
subl %ebx, %eax
movl %eax, -4(%ebp)
jmp while_0
end0:
movl -4(%ebp), %eax
pushl %eax
movl $0, %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
jl T1
movl $0, %eax
T1:
cmpl $0, %eax
je .else1
movl $0, %eax
pushl %eax
call printInt
popl %ebx
movl $0, %eax
jmp _return_main
jmp .endElse2
.else1:
movl $1, %eax
pushl %eax
call printInt
popl %ebx
movl $0, %eax
jmp _return_main
.endElse2:
_return_main:
addl $4, %esp
popl %ebp
ret
_start:
call main
movl $1,%eax
int  $0x80

