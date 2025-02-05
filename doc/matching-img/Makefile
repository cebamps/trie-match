SMCAT := npx state-machine-cat@12.0.21

%.svg: %.smcat
	$(SMCAT) -d left-right $< -o $@

%.dot: %.smcat
	$(SMCAT) -d left-right -T dot $< -o $@

all: svg dot
svg: $(patsubst %.smcat, %.svg, $(wildcard *.smcat))
dot: $(patsubst %.smcat, %.dot, $(wildcard *.smcat))

.PHONY: all svg dot
