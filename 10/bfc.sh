#!/bin/sh

MEM=100000000
OPTIMIZE=0

while test $# -gt 0; do
	case "$1" in
		-o)
			shift
			if test $# -gt 0; then
				exec > "$1"
			else
				echo "no outfile specified" >&2
				exit 1
			fi
			shift
			;;
		-O)
			OPTIMIZE=1
			shift
			;;
		-*)
			echo "$1: option not recognized" >&2
			shift
			;;
		*)
			if test ! -r "$1"; then
				echo "file $1 does not exist or is not readable" >&2
				exit 1
			fi
			exec < "$1"
			shift
			;;
	esac
done

if test "$OPTIMIZE" = "0" ; then
awk "$(cat - <<ENDAWK
BEGIN {
	FS=""
	cmt=0
	sti=0
	lbi=0
	printf "\t.section .bss\n"
	printf "\t.lcomm buffer, $MEM\n"
	printf "\t.section .text\n"
	printf "\t.globl _start\n"
	printf "_start:\n"
	printf "\tmovl \$buffer, %%edi"
}
{
	if(cmt == 0)
		printf "\n"
	cmt=0
	for(i=1;i<=NF;i++)
	switch(\$i) {
	case "+": 
		printf "\n\tincb (%%edi)"
		break
	case "-": 
		printf "\n\tdecb (%%edi)"
		break
	case ">": 
		printf "\n\tinc %%edi"
		break
	case "<": 
		printf "\n\tdec %%edi"
		break
	case "[": 
		lbi++
		st[sti]=lbi
		printf "\n\tcmpb \$0, (%%edi)\n\tjz .LE%d", lbi
		printf "\n.LB%d:", lbi
		sti++
		break
	case "]": 
		cmt=0
		--sti
		printf "\n\tcmpb \$0, (%%edi)\n\tjnz .LB%d", st[sti]
		printf "\n.LE%d:", st[sti]
		break
	case ",": 
		cmt=0
		printf "\n\tmovl \$3, %%eax"
		printf "\n\tmovl \$0, %%ebx"
		printf "\n\tmovl %%edi, %%ecx"
		printf "\n\tmovl \$1, %%edx"
		printf "\n\tint \$0x80"
		break
	case ".": 
		cmt=0
		printf "\n\tmovl \$4, %%eax"
		printf "\n\tmovl \$1, %%ebx"
		printf "\n\tmovl %%edi, %%ecx"
		printf "\n\tmovl \$1, %%edx"
		printf "\n\tint \$0x80"
		break
	case "\t": 
	case " ":
		if(cmt == 0)
			break
	default:
		if(cmt == 0) {
			cmt=1
			printf "\t#"
		}
		printf \$i
	}
}
END {
	printf "\n\n"
	printf "\t%s\n", "movl \$1, %eax"
	printf "\t%s\n", "movl \$0, %ebx"
	printf "\tint \$0x80\n"
}
ENDAWK
)"
exit

else

awk "$(cat - <<ENDAWK
function perror(lvl, msg, pos) {
	printf "%s: %s %d:%d\n", lvl, msg, NR, pos > "/dev/stderr"
	if(pos>0) {
		print \$0 >"/dev/stderr"
		pos--
		while(pos) {
			printf " " >"/dev/stderr"
			pos--
		}
		printf "^\n" >"/dev/stderr"
	}
}
function asm() {
	switch(prev) {
	case "+": 
		if(pren == 1) {
			printf "\n\tincb (%%edi)"
		} else {
			printf "\n\taddb \$%d, (%%edi)",(pren % 256)
		}
		break
	case "-": 
		if(pren == 1) {
			printf "\n\tdecb (%%edi)"
		} else {
			printf "\n\tsubb \$%d, (%%edi)",(pren % 256)
		}
		break
	case ">": 
		if(pren == 1) {
			printf "\n\tinc %%edi"
		} else {
			printf "\n\tadd \$%d, %%edi",pren
		}
		break
	case "<": 
		if(pren == 1) {
			printf "\n\tdec %%edi"
		} else {
			printf "\n\tsub \$%d, %%edi",pren
		}
		break
	case "[": 
		printf "\n\tcmpb \$0, (%%edi)"
		while(pren) {
			pren--
			lbi++
			st[sti]=lbi
			sti++
			printf "\n\tjz .LE%d", lbi
			printf "\n.LB%d:", lbi
		}
		break
	case "]": 
		printf "\n\tcmpb \$0, (%%edi)"
		while(pren) {
			if(sti == 0) {
				perror("ERROR", "] missing matching [", i-pren)
				break
			}
			pren--
			sti--
			printf "\n\tjnz .LB%d", st[sti]
			printf "\n.LE%d:", st[sti]
		}
		break
	case ",":
		while(pren) {
			pren--
			printf "\n\tmovl \$3, %%eax"
			printf "\n\tmovl \$0, %%ebx"
			printf "\n\tmovl %%edi, %%ecx"
			printf "\n\tmovl \$1, %%edx"
			printf "\n\tint \$0x80"
		}
		break
	case ".": 
		while(pren) {
			pren--
			printf "\n\tmovl \$4, %%eax"
			printf "\n\tmovl \$1, %%ebx"
			printf "\n\tmovl %%edi, %%ecx"
			printf "\n\tmovl \$1, %%edx"
			printf "\n\tint \$0x80"
		}
		break
	default:
		break
	}
}
BEGIN{
	FS=""
	sti=0
	lbi=0
	prev="#"
	pren=0
	i=0
	printf "\t.section .bss\n"
	printf "\t.lcomm buffer, $MEM\n"
	printf "\t.section .text\n"
	printf "\t.globl _start\n"
	printf "_start:\n"
	printf "\tmovl \$buffer, %%edi"
}
{
	for(i=1;i<=NF;i++) {
		if(\$i !~ /[+-><[\],.]/)
			continue
		if(\$i == prev) {
			pren++
		} else {
			asm()
			prev=\$i
			pren=1
		}
	}
}
END{
	asm()
	if(sti != 0) {
		perror("ERROR", "reached EOF while looking for ]") > "/dev/stderr"
	}
	printf "\n\n"
	printf "\t%s\n", "movl \$1, %eax"
	printf "\t%s\n", "movl \$0, %ebx"
	printf "\tint \$0x80\n"
}
ENDAWK
)"
exit
fi
