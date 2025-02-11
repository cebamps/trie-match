# Pipeline demo

trie-match is intended to be used in a data pipeline, together with tools like
jq and Miller.

This demo shows two data sources for pattern and query inputs, in a scenario
where the queries represent the data from a nested dictionary of strings, and
the patterns represent extracted locations in a set of files where paths in that
dictionary are referenced.

Concretely, the patterns could be statically extracted translation keys from a
codebase, and the queries could be the translation data.

trie-match can join the two sources of data to pair up each location with the
string values matching the reference's pattern.

To this end, the data associated with the patterns and queries is serialized to
JSON to be used as annotations for trie-match, then parsed out of the output
with Miller.

Inputs: [pattern-source.json](./pattern-source.json) and
[query-source.json](./query-source.json).

Expected output:

```json
[
{
  "query": "foo.bar.x.subtitle",
  "pattern": "foo.bar.*.subtitle",
  "data_query": "Subtitle x",
  "data_pattern": {
    "path": "src/foo/bar.txt",
    "line_no": "124"
  }
},
{
  "query": "foo.bar.x.title",
  "pattern": "foo.bar.*.title",
  "data_query": "Title x",
  "data_pattern": {
    "path": "src/foo/bar.txt",
    "line_no": "123"
  }
},
{
  "query": "foo.bar.y.subtitle",
  "pattern": "foo.bar.*.subtitle",
  "data_query": "Subtitle y",
  "data_pattern": {
    "path": "src/foo/bar.txt",
    "line_no": "124"
  }
},
{
  "query": "foo.bar.y.title",
  "pattern": "foo.bar.*.title",
  "data_query": "Title y",
  "data_pattern": {
    "path": "src/foo/bar.txt",
    "line_no": "123"
  }
},
{
  "query": "foo.qux.something",
  "pattern": "foo.qux.something",
  "data_query": "Something",
  "data_pattern": {
    "path": "src/foo/qux.txt",
    "line_no": "42"
  }
}
]
```
