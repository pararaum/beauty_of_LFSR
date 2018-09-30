#! /usr/bin/make -f

LFSR = lfsr_are_beautiful
PRG = $(LFSR).prg

all: $(PRG)
.PHONY: all

clean:
	rm -f $(PRG) *.lst *.prg
distclean: clean
	rm -f *~
.PHONY: clean distclean

run: $(PRG)
	x64 $(PRG) & 
.PHONY: run

$(PRG): $(LFSR).asm *.inc
	asm6502 -l $(basename $@).lst -o $@ -e -b 0x07ff $<
	sed -n '/Total Error/,$$p' lfsr_are_beautiful.lst
