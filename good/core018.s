.file "a"
.text
.data
.globl _start
main:
pushl %ebp
movl %esp, %ebp
subl $12, %esp
call readInt
movl %eax, -4(%ebp)
call readString
movl %eax, -8(%ebp)
call readString
movl %eax, -12(%ebp)
movl $5, %eax
pushl %eax
movl -4(%ebp), %eax
popl %ebx
subl %ebx, %eax
pushl %eax
call printInt
popl %ebx
movl -8(%ebp), %eax
pushl %eax
movl -12(%ebp), %eax
popl %ebx
pushl %eax
pushl %ebx
call concat
popl %ebx
popl %ebx
pushl %eax
call printString
popl %ebx
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

