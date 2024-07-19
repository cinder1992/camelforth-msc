
all:	CF1802.OBJ

CF1802.OBJ:	build.bat
	dosbox build.bat

clean:
	rm -f CF1802.*
