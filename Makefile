source_dir ?= ./src
template_dir ?= ./src/types/templates
types_dir ?= ./src/types

cat: $(source_dir)/cat.adb
	gprbuild -p -P cat

errors.ads: $(template_dir)/errors.ads_templ
	cpp -P $^ | tail -n +3 > $(types_dir)/$@

const_h.ads: $(template_dir)/const_h.ads_templ
	cpp -P $^ | tail -n +102 > $(types_dir)/$@

.PHONY: proof

proof:
	gnatprove -P cat --no-counterexample --no-inlining -j 16 --replay
