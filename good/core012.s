.file "a"
.text
.data
.globl _start
.Str0:
.string "string"
.Str1:
.string " "
.Str2:
.string "concatenation"
.Str3:
.string "true"
.Str4:
.string "false"
main:
pushl %ebp
movl %esp, %ebp
subl $8, %esp
movl $56, %eax
movl %eax, -4(%ebp)
negl %eax
movl %eax, -8(%ebp)
movl -4(%ebp), %eax
pushl %eax
movl -8(%ebp), %eax
popl %ebx
addl %ebx, %eax
pushl %eax
call printInt
popl %ebx
movl -8(%ebp), %eax
pushl %eax
movl -4(%ebp), %eax
popl %ebx
subl %ebx, %eax
pushl %eax
call printInt
popl %ebx
movl -4(%ebp), %eax
pushl %eax
movl -8(%ebp), %eax
popl %ebx
imull %ebx
pushl %eax
call printInt
popl %ebx
movl $2, %eax
pushl %eax
movl $45, %eax
popl %ebx
movl $0, %edx
idivl %ebx
pushl %eax
call printInt
popl %ebx
pushl %eax
call printInt
popl %ebx
movl -8(%ebp), %eax
pushl %eax
movl -4(%ebp), %eax
popl %ebx
subl %ebx, %eax
pushl %eax
movl -4(%ebp), %eax
pushl %eax
movl -8(%ebp), %eax
popl %ebx
addl %ebx, %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
jg T0
movl $0, %eax
T0:
pushl %eax
call printBool
popl %ebx
movl -8(%ebp), %eax
pushl %eax
movl -4(%ebp), %eax
popl %ebx
movl $0, %edx
idivl %ebx
pushl %eax
movl -4(%ebp), %eax
pushl %eax
movl -8(%ebp), %eax
popl %ebx
imull %ebx
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
jle T1
movl $0, %eax
T1:
pushl %eax
call printBool
popl %ebx
movl $(.Str0), %eax
pushl %eax
movl $(.Str1), %eax
popl %ebx
pushl %eax
pushl %ebx
call concat
popl %ebx
popl %ebx
pushl %eax
movl $(.Str2), %eax
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
addl $8, %esp
popl %ebp
ret
printBool:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
movl 8(%ebp), %eax
cmpl $0, %eax
je .else2
movl $(.Str3), %eax
pushl %eax
call printString
popl %ebx
jmp _return_printBool
jmp .endElse3
.else2:
movl $(.Str4), %eax
pushl %eax
call printString
popl %ebx
jmp _return_printBool
.endElse3:
_return_printBool:
addl $0, %esp
popl %ebp
ret
_start:
call main
movl $1,%eax
int  $0x80

