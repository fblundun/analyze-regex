# analyze-regex

### Overview

This program finds an upper bound on the length of strings a regex in the "pattern" field of a JSON schema can validate.

This program currently works for all the regex components listed [here](http://spacetelescope.github.io/understanding-json-schema/reference/regular_expressions.html).

The upper bound may be greater than the actual maximum length if:

* the regex uses special zero-width escape characters (such as `\b` for a word boundary)
* the regex involves a choice of options, one of which can never match (for example, `a|($bc)`).

Note that if a regex doesn't contain `^` and `$` anchors, there may be no upper bound on the length of strings it can validate.

### Example usage

```scala
println(RegexAnalyzer.getMaxLength("""^[0-9a-f]{8}-([0-9a-f]{4}-){3}[0-9a-f]{12}$|^.{20}$""")
// prints "Some(36)"
```
