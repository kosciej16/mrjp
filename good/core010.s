.file "a"
.text
.data
.globl _start
main:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
movl $5, %eax
pushl %eax
call fac
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
fac:
pushl %ebp
movl %esp, %ebp
subl $8, %esp
movl $1, %eax
movl %eax, -4(%ebp)
movl 8(%ebp), %eax
movl %eax, -8(%ebp)
while_0:
movl -8(%ebp), %eax
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
movl -4(%ebp), %eax
pushl %eax
movl -8(%ebp), %eax
popl %ebx
imull %ebx
movl %eax, -4(%ebp)
movl $1, %eax
pushl %eax
movl -8(%ebp), %eax
popl %ebx
subl %ebx, %eax
movl %eax, -8(%ebp)
jmp while_0
end0:
movl -4(%ebp), %eax
jmp _return_fac
_return_fac:
addl $8, %esp
popl %ebp
ret
_start:
call main
movl $1,%eax
int  $0x80

