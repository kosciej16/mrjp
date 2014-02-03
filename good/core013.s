.file "a"
.text
.data
.globl _start
.Str0:
.string "&&"
.Str1:
.string "||"
.Str2:
.string "!"
.Str3:
.string "false"
.Str4:
.string "true"
main:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
movl $(.Str0), %eax
pushl %eax
call printString
popl %ebx
negl %eax
pushl %eax
call test
popl %ebx
cmpl $0, %eax
je end0
movl $0, %eax
pushl %eax
call test
popl %ebx
end0:
pushl %eax
call printBool
popl %ebx
negl %eax
pushl %eax
call test
popl %ebx
cmpl $0, %eax
je end1
movl $1, %eax
pushl %eax
call test
popl %ebx
end1:
pushl %eax
call printBool
popl %ebx
movl $3, %eax
pushl %eax
call test
popl %ebx
cmpl $0, %eax
je end2
negl %eax
pushl %eax
call test
popl %ebx
end2:
pushl %eax
call printBool
popl %ebx
movl $234234, %eax
pushl %eax
call test
popl %ebx
cmpl $0, %eax
je end3
movl $21321, %eax
pushl %eax
call test
popl %ebx
end3:
pushl %eax
call printBool
popl %ebx
movl $(.Str1), %eax
pushl %eax
call printString
popl %ebx
negl %eax
pushl %eax
call test
popl %ebx
cmpl $1, %eax
je end4
movl $0, %eax
pushl %eax
call test
popl %ebx
end4:
pushl %eax
call printBool
popl %ebx
negl %eax
pushl %eax
call test
popl %ebx
cmpl $1, %eax
je end5
movl $1, %eax
pushl %eax
call test
popl %ebx
end5:
pushl %eax
call printBool
popl %ebx
movl $3, %eax
pushl %eax
call test
popl %ebx
cmpl $1, %eax
je end6
negl %eax
pushl %eax
call test
popl %ebx
end6:
pushl %eax
call printBool
popl %ebx
movl $234234, %eax
pushl %eax
call test
popl %ebx
cmpl $1, %eax
je end7
movl $21321, %eax
pushl %eax
call test
popl %ebx
end7:
pushl %eax
call printBool
popl %ebx
movl $(.Str2), %eax
pushl %eax
call printString
popl %ebx
movl $1, %eax
pushl %eax
call printBool
popl %ebx
movl $0, %eax
pushl %eax
call printBool
popl %ebx
movl $0, %eax
jmp _return_main
_return_main:
addl $0, %esp
popl %ebp
ret
printBool:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
cmpl $0, %eax
je .else0
movl $(.Str3), %eax
pushl %eax
call printString
popl %ebx
jmp .endElse1
.else0:
movl $(.Str4), %eax
pushl %eax
call printString
popl %ebx
.endElse1:
jmp _return_printBool
_return_printBool:
addl $0, %esp
popl %ebp
ret
test:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
movl 8(%ebp), %eax
pushl %eax
call printInt
popl %ebx
movl 8(%ebp), %eax
pushl %eax
movl $0, %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
jg T2
movl $0, %eax
T2:
jmp _return_test
_return_test:
addl $0, %esp
popl %ebp
ret
_start:
call main
movl $1,%eax
int  $0x80

