
all:	camel.hex

camel.hex:	src/camel.asm src/camel18.asm src/camel18d.asm src/camel18r.asm src/camel18h.asm src/camel18x.asm 
	(cd src && a18 camel.asm -l ../camel.lst -o ../camel.hex)

clean:
	rm -f *.hex *.lst
