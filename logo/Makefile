# Makes the two trees used in the logo.

all: tree-1.svg tree-2.svg

%.svg: %.dot
	./$< > $@

.NOTINTERMEDIATE: tree-%.dot
tree-1.dot: gen-tree.hs
	./gen-tree.hs 1 > $@
	chmod u+x $@

tree-2.dot: gen-tree.hs
	./gen-tree.hs 5 0.3 > $@
	chmod u+x $@

.PHONY: all
