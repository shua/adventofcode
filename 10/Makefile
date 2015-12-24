BFC=./bfc.sh
AS=as
LD=ld

printlen: printlen.o
	$(LD) printlen.o -o printlen

main: main.o
	$(LD) main.o -o main

main2: main.bf
	$(BFC) -O main.bf -o main2.s
	$(AS) main2.s -o main2.o
	$(LD) main2.o -o main2

%.s : %.bf
	$(BFC) $< -o $@

.PRECIOUS: %.s

%.o : %.s
	$(AS) $< -o $@

% : %.o
	$(LD) $< -o $@

