%.svg: %.dot defs.m4
	m4 $< | dot -Tsvg -o $@

all: $(patsubst %.dot, %.svg, $(wildcard *.dot))

.PHONY: all
