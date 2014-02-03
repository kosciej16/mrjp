.file "a"
.text
.data
.globl _start
.Str0:
.string "foo"
main:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
call foo
movl $0, %eax
jmp _return_main
_return_main:
addl $0, %esp
popl %ebp
ret
foo:
pushl %ebp
movl %esp, %ebp
subl $0, %esp
movl $(.Str0), %eax
pushl %eax
call printString
popl %ebx
jmp _return_foo
_return_foo:
addl $0, %esp
popl %ebp
ret
_start:
call main
movl $1,%eax
int  $0x80

