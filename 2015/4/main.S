/* compile with
 *	gcc -DPART1 -o main main.S -lcrypto
 * for the answer to part 1 and 
 *	gcc -o main main.S -lcrypto
 * for part 2
 */

.data
input: /* the given input string */
	.ascii "iwrupvqb"
salt: /* the endings, with 512 bytes of space */
	.ascii "1"
	.zero 503
hex: /* help with printing */
	.string "0123456789abcdef"
abcd: /* old, but eventually I want to use md5-fast instead of libcrypto */
	.long 0x67452301
	.long 0xEFCDAB89
	.long 0x98BADCFE
	.long 0x10325476
output: /* where hash is stored */
	.long 0
	.long 0
	.long 0
	.long 0
i: /* counter */
	.long 0
l: /* another counter */
	.long 0
len: /* total length of input + salt */
	.long 9
slen: /* length of salt - 1, makes it easier */
	.long 0
ctx: /* MD5_CTX struct */
	.zero 92

.text
.globl main

main:
	pushq %rbp
	movq %rsp, %rbp

.md5sum:
	movl $ctx, %edi
	call MD5_Init

	movl len, %eax
	cltq
	movq %rax, %rdx
	movl $input, %esi
	movl $ctx, %edi
	call MD5_Update

	movl $ctx, %esi
	movl $output, %edi
	call MD5_Final

	movl $0, l

.check:
	/* output[0] == 0x00 */
	movzbl output, %eax
	cmpl $0, %eax
	jne .next
	/* output[1] == 0x00 */
	movl $1, %eax
	movzbl output(%eax), %eax
	cmpl $0, %eax
	jne .next
	/* output[3] == 0x0? */
	movl $2, %eax
	movzbl output(%eax), %eax
#ifdef PART1
	andl $0xf0, %eax
#endif
	cmpl $0, %eax
	jne .next

	jmp .print

.next:
/*	l = slen */
	movl slen, %eax
	movl %eax, l

.add:
/*
 *	if(salt[l] == '9')
 *		salt[l] = '0'
 *		if(l==0)
 *			goto .add_shift
 *		l-=1;
 *		goto .add
 */
	movl l, %ebx
	movb salt(%ebx), %al
	cmp $57, %al
	jne .add_end

	movb $48, salt(%ebx)
	cmpl $0, %ebx
	je .add_shift

	subl $1, %ebx
	movl %ebx, l
	jmp .add
.add_end:
/*
 *	salt[l] = salt[l] + 1
 *	goto .md5sum
 */
	add $1, %al
	movb %al, salt(%ebx)
	jmp .md5sum

.add_shift:
/*
 *	l = slen
 *	._add_shift:
 *	bl = salt[l]
 *	l++
 *	salt[l] = bl
 *	l--
 *	if(l == 0)
 *		salt[l] = '1'
 *		slen++
 *		len++
 *		goto .md5sum
 *	l--;
 *	goto ._add_shift
 */
	movl slen, %eax
._add_shift:
	movb salt(%eax), %bl
	addl $1, %eax
	movb %bl, salt(%eax)
	cmpl $1, %eax
	je ._add_shift_end
	subl $2, %eax
	jmp ._add_shift
._add_shift_end:
	movb $49, salt
	movl slen, %eax
	addl $1, %eax
	movl %eax, slen
	movl len, %eax
	addl $1, %eax
	movl %eax, len
	jmp .md5sum

.print:
	movl $0, i
.printnum:
	movl i, %eax
	movb salt(%eax), %al
	movzbl %al, %edi
	call putchar
	movl i, %eax
	addl $1, %eax
	movl %eax, i
	movl slen, %ebx
	addl $1, %ebx
	cmpl %eax, %ebx
	jg .printnum

	movl $32, %edi
	call putchar

	movl $0, i
.printmd5:
	movl i, %eax
	movzbl output(%eax), %eax
	andl $0xf0, %eax
	sarl $4, %eax
	movzbl hex(%eax), %edi
	call putchar

	movl i, %eax
	movzbl output(%eax), %eax
	andl $0xf, %eax
	movzbl hex(%eax), %edi
	call putchar

	movl i, %eax
	addl $1, %eax
	movl %eax, i
	cmpl $16, %eax
	jl .printmd5

	movl $10, %edi
	call putchar

.end:
	movl $0, %eax
	popq %rbp
	ret
