.file "a"
.text
.data
.globl _start
.Str0:
.string "apa"
.Str1:
.string "true"
.Str2:
.string "false"
main:
pushl %ebp
movl %esp, %ebp
subl $4, %esp
movl $4, %eax
movl %eax, -4(%ebp)
movl $3, %eax
pushl %eax
movl -4(%ebp), %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
jle T0
movl $0, %eax
T0:
cmpl $0, %eax
je end0
movl $4, %eax
pushl %eax
movl $2, %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
jne T1
movl $0, %eax
T1:
end0:
cmpl $0, %eax
je end1
movl $1, %eax
end1:
cmpl $0, %eax
je .else0
movl $1, %eax
pushl %eax
call printBool
popl %ebx
jmp .endElse1
.else0:
movl $(.Str0), %eax
pushl %eax
call printString
popl %ebx
.endElse1:
movl $1, %eax
pushl %eax
movl $1, %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
je T2
movl $0, %eax
T2:
cmpl $1, %eax
je end2
movl $1, %eax
pushl %eax
call dontCallMe
popl %ebx
end2:
pushl %eax
call printBool
popl %ebx
movl $4, %eax
pushl %eax
negl %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
jl T3
movl $0, %eax
T3:
cmpl $0, %eax
je end3
movl $2, %eax
pushl %eax
call dontCallMe
popl %ebx
end3:
pushl %eax
call printBool
popl %ebx
movl $4, %eax
pushl %eax
movl -4(%ebp), %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
je T4
movl $0, %eax
T4:
cmpl $0, %eax
je end4
movl $1, %eax
pushl %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
je T5
movl $0, %eax
T5:
end4:
cmpl $0, %eax
je end5
movl $1, %eax
end5:
pushl %eax
call printBool
popl %ebx
movl $0, %eax
pushl %eax
movl $0, %eax
pushl %eax
call implies
popl %ebx
popl %ebx
pushl %eax
call printBool
popl %ebx
movl $0, %eax
pushl %eax
movl $1, %eax
pushl %eax
call implies
popl %ebx
popl %ebx
pushl %eax
call printBool
popl %ebx
movl $1, %eax
pushl %eax
movl $0, %eax
pushl %eax
call implies
popl %ebx
popl %ebx
pushl %eax
call printBool
popl %ebx
movl $1, %eax
pushl %eax
movl $1, %eax
pushl %eax
call implies
popl %ebx
popl %ebx
pushl %eax
call printBool
popl %ebx
movl $0, %eax
jmp _return_main
_return_main:
addl $4, %esp
popl %ebp
ret
dontCallMe:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
movl 8(%ebp), %eax
pushl %eax
call printInt
popl %ebx
movl $1, %eax
jmp _return_dontCallMe
_return_dontCallMe:
addl $0, %esp
popl %ebp
ret
printBool:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
movl 8(%ebp), %eax
cmpl $0, %eax
je .else6
movl $(.Str1), %eax
pushl %eax
call printString
popl %ebx
jmp .endElse7
.else6:
movl $(.Str2), %eax
pushl %eax
call printString
popl %ebx
.endElse7:
jmp _return_printBool
_return_printBool:
addl $0, %esp
popl %ebp
ret
implies:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
cmpl $1, %eax
je end6
movl 12(%ebp), %eax
pushl %eax
movl 8(%ebp), %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
je T8
movl $0, %eax
T8:
end6:
jmp _return_implies
_return_implies:
addl $0, %esp
popl %ebp
ret
_start:
call main
movl $1,%eax
int  $0x80

