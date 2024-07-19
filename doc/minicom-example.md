# Example using minicom with a membership card with MCSMP20:

1. Open minicom using `minicom -b 4800 -c on -d <your-serial-device>`, have the Membership card running the monitor.

![minicom-example1.png](pic/minicom-example1.png)

2. Hit `CTRL+A` followed by `Shift+O` to open the configurator, then select and open `File transfer protocols`
3. if not present, create a new xfer protocol with the following options:
	Name: `ascii-slw`
	Command: `ascii-xfr -dsv -l 100 -c 10`
	Everything else the same as `ascii`
	The window should now look like this:

![minicom-example2.png](pic/minicom-example2.png)

4. hit the `ESC` key twice to return to the terminal
5. Type `L` to prime the Monitor to accept the Intel HEX file
6. Hit `CTRL+A` followed by `SHIFT+S` to open the file send dialog.
7. Select the entry `ascii-slw` and navigate to the root directory of the repository (double-space like double-click to enter a directory)
8. Tag the file `CF1802.OBJ` with `Space` and hit `Return` to begin sending the file.
9. The lights on the Membership Card will begin flashing with the contents of the file, once it is complete the window will look like this

![minicom-example3.png](pic/minicom-example3.png)

10. Type `R` and hit `Return`, The monitor will hand over control to the Forth interpreter
11. Hit `Return` again and CamelForth will sign on. Code away!

![minicom-example4.png](pic/minicom-example4.png)
