.file "a"
.text
.data
.globl _start
.Str0:
.string "="
.Str1:
.string "hello */"
.Str2:
.string "/* world"
.Str3:
.string ""
main:
pushl %ebp
movl %esp, %ebp
subl $12, %esp
movl $10, %eax
pushl %eax
call fac
popl %ebx
pushl %eax
call printInt
popl %ebx
movl $10, %eax
pushl %eax
call rfac
popl %ebx
pushl %eax
call printInt
popl %ebx
movl $10, %eax
pushl %eax
call mfac
popl %ebx
pushl %eax
call printInt
popl %ebx
movl $10, %eax
pushl %eax
call ifac
popl %ebx
pushl %eax
call printInt
popl %ebx
movl $10, %eax
movl %eax, -8(%ebp)
movl $1, %eax
movl %eax, -12(%ebp)
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
movl -12(%ebp), %eax
pushl %eax
movl -8(%ebp), %eax
popl %ebx
imull %ebx
movl %eax, -12(%ebp)
movl -8(%ebp), %eax
subl $1, %eax
movl %eax, -8(%ebp)
jmp while_0
end0:
movl -12(%ebp), %eax
pushl %eax
call printInt
popl %ebx
movl $(.Str0), %eax
pushl %eax
movl $60, %eax
pushl %eax
call repStr
popl %ebx
popl %ebx
pushl %eax
call printString
popl %ebx
movl $(.Str1), %eax
pushl %eax
call printString
popl %ebx
movl $(.Str2), %eax
pushl %eax
call printString
popl %ebx
movl $0, %eax
jmp _return_main
_return_main:
addl $12, %esp
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
while_2:
movl -8(%ebp), %eax
pushl %eax
movl $0, %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
jg T2
movl $0, %eax
T2:
cmpl $0, %eax
je end2
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
jmp while_2
end2:
movl -4(%ebp), %eax
jmp _return_fac
_return_fac:
addl $8, %esp
popl %ebp
ret
rfac:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
movl 8(%ebp), %eax
pushl %eax
movl $0, %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
je T4
movl $0, %eax
T4:
cmpl $0, %eax
je .else4
movl $1, %eax
jmp _return_rfac
jmp .endElse5
.else4:
movl 8(%ebp), %eax
pushl %eax
movl $1, %eax
pushl %eax
movl 8(%ebp), %eax
popl %ebx
subl %ebx, %eax
pushl %eax
call rfac
popl %ebx
popl %ebx
imull %ebx
jmp _return_rfac
.endElse5:
_return_rfac:
addl $0, %esp
popl %ebp
ret
mfac:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
movl 8(%ebp), %eax
pushl %eax
movl $0, %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
je T5
movl $0, %eax
T5:
cmpl $0, %eax
je .else5
movl $1, %eax
jmp _return_mfac
jmp .endElse6
.else5:
movl 8(%ebp), %eax
pushl %eax
movl $1, %eax
pushl %eax
movl 8(%ebp), %eax
popl %ebx
subl %ebx, %eax
pushl %eax
call nfac
popl %ebx
popl %ebx
imull %ebx
jmp _return_mfac
.endElse6:
_return_mfac:
addl $0, %esp
popl %ebp
ret
nfac:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
movl 8(%ebp), %eax
pushl %eax
movl $0, %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
jne T6
movl $0, %eax
T6:
cmpl $0, %eax
je .else6
movl $1, %eax
pushl %eax
movl 8(%ebp), %eax
popl %ebx
subl %ebx, %eax
pushl %eax
call mfac
popl %ebx
pushl %eax
movl 8(%ebp), %eax
popl %ebx
imull %ebx
jmp _return_nfac
jmp .endElse7
.else6:
movl $1, %eax
jmp _return_nfac
.endElse7:
_return_nfac:
addl $0, %esp
popl %ebp
ret
ifac:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
movl $1, %eax
pushl %eax
movl 8(%ebp), %eax
pushl %eax
call ifac2f
popl %ebx
popl %ebx
jmp _return_ifac
_return_ifac:
addl $0, %esp
popl %ebp
ret
ifac2f:
pushl %ebp
movl %esp, %ebp
subl $4, %esp
movl 12(%ebp), %eax
pushl %eax
movl 8(%ebp), %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
je T7
movl $0, %eax
T7:
cmpl $0, %eax
je .endIf7
movl 12(%ebp), %eax
jmp _return_ifac2f
.endIf7:
movl 12(%ebp), %eax
pushl %eax
movl 8(%ebp), %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
jg T8
movl $0, %eax
T8:
cmpl $0, %eax
je .endIf8
movl $1, %eax
jmp _return_ifac2f
.endIf8:
movl $2, %eax
pushl %eax
movl 12(%ebp), %eax
pushl %eax
movl 8(%ebp), %eax
popl %ebx
addl %ebx, %eax
popl %ebx
movl $0, %edx
idivl %ebx
movl %eax, -4(%ebp)
movl 12(%ebp), %eax
pushl %eax
movl -4(%ebp), %eax
pushl %eax
call ifac2f
popl %ebx
popl %ebx
pushl %eax
movl -4(%ebp), %eax
pushl %eax
movl $1, %eax
popl %ebx
addl %ebx, %eax
pushl %eax
movl 8(%ebp), %eax
pushl %eax
call ifac2f
popl %ebx
popl %ebx
popl %ebx
imull %ebx
jmp _return_ifac2f
_return_ifac2f:
addl $4, %esp
popl %ebp
ret
repStr:
pushl %ebp
movl %esp, %ebp
subl $8, %esp
movl $(.Str3), %eax
movl %eax, -4(%ebp)
movl $0, %eax
movl %eax, -8(%ebp)
while_9:
movl -8(%ebp), %eax
pushl %eax
movl 8(%ebp), %eax
popl %ebx
cmpl %eax, %ebx
movl $1, %eax
jl T9
movl $0, %eax
T9:
cmpl $0, %eax
je end9
movl -4(%ebp), %eax
pushl %eax
movl 12(%ebp), %eax
popl %ebx
pushl %eax
pushl %ebx
call concat
popl %ebx
popl %ebx
movl %eax, -4(%ebp)
movl -8(%ebp), %eax
addl $1, %eax
movl %eax, -8(%ebp)
jmp while_9
end9:
movl -4(%ebp), %eax
jmp _return_repStr
_return_repStr:
addl $8, %esp
popl %ebp
ret
_start:
call main
movl $1,%eax
int  $0x80

