# CamelForth for the Membership Card

This is a port of [CamelForth](http://www.camelforth.com) for use with Lee Hart's 1802 Membership card, found here: https://www.sunrise-ev.com/1802.htm

## Changes from Vanilla CamelForth
### Additions:

 - New words: `INP` and `OUTP` for reading and writing directly to the
 1802's I/O ports
 - New words: `EF1?` `EF2?` `EF3?` `EF4?` query the status of the 1802's EF* lines
 - New word: `0<>` Returns `FALSE` if TOS = 0
 - New word: `DFREE` Returns the number of free dictionary bytes
 - New word: `GENBAUD` sets the baudrate register depending on the next received byte, in the same way as MCSMP20 does. uses code derived from [The serial binary loader](https://www.retrotechnology.com/memship/mship_binloader.html)

### Removals:

-	`KEY?` is not implemented, as it's not possible to query the bit-banging code to detect pending serial transfers.
-	`TRACE` has been removed, as it serves no function on real hardware

### Changes: 

- `ACCEPT` has been reverted to its reference implementation, and some minor enhancements have been made for modern terminals.
- `COLD` now both 
	1. Calls `GENBAUD` to get the baud rate
	2. Prints the free amount of Dictionary Space
- the Parameter Stack, Return Stack, and Leave Space have been reduced from 2048 cells to 256 cells, to make more room for Dictionary space
- `BYE` now jumps to 0x8B5E, the reentry point of MCSMP20
- `KEY` and `EMIT` are both implemented using Q-as-TX and EF3-as-RX bit-banging, using code derived from [The serial binary loader](https://www.retrote)

##  Examples

Example programs can be found in the `forth` directory.

## Building

### All platforms:

You will require `A18.COM` from the PseudoSam 1802 assembler to be present in the root directory of the repository. A copy can be found [here](http://www.camelforth.com/download.php?view.13) on the CamelForth website.

### Linux:

Building on Linux requires DOSBOX. Assuming DOSBOX is in your `$PATH`, navigate to the root of the repository and run `make`

### Windows:

On Windows that support 16-bit DOS executables, simply run `build.bat`

On Windows that do **not** support 16-bit DOS executables, DOSBOX is required. assuming the DOSBOX executable is in your `%PATH%`, open the Command Prompt, navigate to the root of the repository and run `dosbox build.bat`

### Macintosh:

Building on Mac requires DOSBOX. Open DOSBOX and mount the repository as `A:`, navigate to `A:` in DOSBOX and run `build`

## Known Issues

- `ascii-xfr` transfers from `minicom` do not play well with the reference `ACCEPT` routine, you'll have to key in your programs by hand. sorry.
- There is **ABSOLUTELY NO BOUNDS CHECKING** on the stacks, it's up to you to not clobber memory!
- The program has only been tested on a REV. L CPU board with a REV. L Front panel. `SEQ` and `REQ` logic as well as `B3` and `BN3` logic in the `KEY` and `EMIT` routines may be reversed for other revisions!
- There is no way to reenter the Forth interpreter after exiting with `BYE`, leaving programs potentially clobbered by the monitor
- The interpreter takes up around 100-200 bytes more than it should, thanks to page-alignment issues.
- `ACCEPT`, `WORD`, and `FIND` are implemented in high-level Forth, making the interpreter slower than it really should be when compiling words, sometimes taking up to several seconds to compile for larger words.

## TODO

 - [ ] Port to Lee Hart's A18 assembler, PseudoSam is ancient, doesn't support macros, and has a few bugs of its own.
 - [ ] Implement a SAVE and LOAD routine
 - [ ] Implement a way to build for use in ROM
 - [ ] Save the structures/registers needed by MCSMP20 so that invocation doesn't clobber the Forth stack
 - [ ] Update `GENBAUD`, `EMIT`, and `KEY` to use a USER variable rather than requiring a whole register

