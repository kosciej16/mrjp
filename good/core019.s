.file "a"
.text
.data
.globl _start
.Str0:
.string "foo"
main:
pushl %ebp
movl %esp, %ebp
subl $16, %esp
movl $78, %eax
movl %eax, -4(%ebp)
movl $1, %eax
movl %eax, -8(%ebp)
movl -8(%ebp), %eax
pushl %eax
call printInt
popl %ebx
movl -4(%ebp), %eax
pushl %eax
call printInt
popl %ebx
while_0:
movl -4(%ebp), %eax
pushl %eax
movl $76, %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
jg T0
movl $0, %eax
T0:
cmpl $0, %eax
je end0
movl -8(%ebp), %eax
subl $1, %eax
movl %eax, -8(%ebp)
movl -8(%ebp), %eax
pushl %eax
call printInt
popl %ebx
movl -12(%ebp), %eax
pushl %eax
movl $7, %eax
popl %ebx
addl %ebx, %eax
movl %eax, -12(%ebp)
movl -12(%ebp), %eax
pushl %eax
call printInt
popl %ebx
jmp while_0
end0:
movl -4(%ebp), %eax
pushl %eax
call printInt
popl %ebx
movl -4(%ebp), %eax
pushl %eax
movl $4, %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
jg T2
movl $0, %eax
T2:
cmpl $0, %eax
je .else2
movl $4, %eax
movl %eax, -16(%ebp)
movl -16(%ebp), %eax
pushl %eax
call printInt
popl %ebx
jmp .endElse3
.else2:
movl $(.Str0), %eax
pushl %eax
call printString
popl %ebx
.endElse3:
movl -4(%ebp), %eax
pushl %eax
call printInt
popl %ebx
movl $0, %eax
jmp _return_main
_return_main:
addl $16, %esp
popl %ebp
ret
_start:
call main
movl $1,%eax
int  $0x80

