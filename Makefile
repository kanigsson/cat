cat.exe: cat.adb const_h.ads const.c
	gprbuild -p -P cat

const_h.ads: const.h
	gcc -fdump-ada-spec const.h

.PHONY: proof

proof:
	gnatprove -P cat --no-counterexample --no-inlining -j 16 --level=4
