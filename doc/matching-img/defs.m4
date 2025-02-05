divert(-1)
Styles borrowed from state-machine-cat: https://github.com/sverweij/state-machine-cat

define(S_GRAPH, `
fontname=Helvetica
fontsize=12
penwidth=2
rankdir=LR
bgcolor=white

node [fontname=Helvetica fontsize=12 shape=rectangle style=rounded penwidth=2]
edge [fontname=Helvetica fontsize=10]
')
define(S_INITIAL, `
shape=circle
style=filled
color=black
fillcolor=black
fixedsize=true
height=0.15
label=""
')
define(S_FINAL, `
S_INITIAL
peripheries=2
')
define(S_ACCEPT, `
penwidth=5
')
define(S_CLUSTER, `
cluster=true
style=rounded
penwidth=2
')

define(NAMESPACE)
define(NAMESPACED, `"NAMESPACE()$1"')
define(NAMESPACED_NODES,
  `patsubst($1, \S+, "NAMESPACE()\&" [label="\&"])dnl'
)
define(ENTER_NAMESPACE, `pushdef(`NAMESPACE', $1_)')
define(EXIT_NAMESPACE, `popdef(`NAMESPACE')')

define(TR_LIT, `TR_LIT2($1, $2, $2)')
define(TR_LIT2, `NAMESPACED($1) -> NAMESPACED($2) [label="\"$3\""]')
define(TR_STAR, `NAMESPACED($1) -> NAMESPACED($2) NAMESPACED($2) -> NAMESPACED($2) [label="any"]')
define(TR_PLUS, `NAMESPACED($1) -> NAMESPACED($2) [label="any"] NAMESPACED($2) -> NAMESPACED($2) [label="any"]')
divert(0)dnl
