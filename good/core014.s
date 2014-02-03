.file "a"
.text
.data
.globl _start
main:
pushl %ebp
movl %esp, %ebp
subl $12, %esp
movl $1, %eax
movl %eax, -4(%ebp)
movl -4(%ebp), %eax
movl %eax, -8(%ebp)
movl $5000000, %eax
movl %eax, -12(%ebp)
movl -4(%ebp), %eax
pushl %eax
call printInt
popl %ebx
while_0:
movl -8(%ebp), %eax
pushl %eax
movl -12(%ebp), %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
jl T0
movl $0, %eax
T0:
cmpl $0, %eax
je end0
movl -8(%ebp), %eax
pushl %eax
call printInt
popl %ebx
movl -4(%ebp), %eax
pushl %eax
movl -8(%ebp), %eax
popl %ebx
addl %ebx, %eax
movl %eax, -8(%ebp)
movl -4(%ebp), %eax
pushl %eax
movl -8(%ebp), %eax
popl %ebx
subl %ebx, %eax
movl %eax, -4(%ebp)
jmp while_0
end0:
movl $0, %eax
jmp _return_main
_return_main:
addl $12, %esp
popl %ebp
ret
_start:
call main
movl $1,%eax
int  $0x80

