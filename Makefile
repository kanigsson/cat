cat.exe: cat.adb errors.ads
	gprbuild -p -P cat

errors.ads: errors.ads_templ
	cpp -P $^ | tail -n +3 > $@

const_h.ads: const_h.ads_templ
	cpp -P $^ | tail -n +102 > $@

.PHONY: proof

proof:
	gnatprove -P cat --no-counterexample --no-inlining -j 16 --level=4
