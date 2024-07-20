
all:	CF1802.OBJ

CF1802.OBJ:	build.bat src/camel18.asm src/camel18d.asm src/camel18r.asm src/camel18h.asm src/camel18x.asm
	dosbox build.bat

clean:
	rm -f CF1802.*
