cat.exe: cat.adb errors.ads
	gprbuild -p -P cat

errors.ads: errors.ads_templ
	cpp -P $^ | tail -n +3 > $@
	

.PHONY: proof

proof:
	gnatprove -P cat --no-counterexample --no-inlining -j 16 --level=4
