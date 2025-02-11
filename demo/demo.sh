#!/usr/bin/env bash

: ${TRIE_MATCH:=trie-match}

function trie-match() {
  command $TRIE_MATCH "$@"
}

if ! trie-match --help > /dev/null 2>&1; then
  echo "Please run in a shell where '$TRIE_MATCH' is available" >&2
  exit 1
elif ! which jq > /dev/null 2>&1; then
  echo "Please run in a shell where jq is available" >&2
  exit 1
elif ! which mlr > /dev/null 2>&1; then
  echo "Please run in a shell where mlr is available" >&2
  exit 1
fi

function pattern_input() {
  jq -r '.[] | [.pattern, (del(.pattern)|tojson)] | join("\t")' pattern-source.json
}

function query_input() {
  jq -r 'path(.. | strings) as $p | [($p | join(".")), (getpath($p) | tojson)] | join("\t")' query-source.json
}

trie-match -p <(pattern_input) -q <(query_input) |
  mlr --ifs=tab --idkvp --ojson \
    label 'query,pattern,data_query,data_pattern' then \
    json-parse -f data_query,data_pattern
