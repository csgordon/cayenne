/^interface/,/^in$/d
/^-- Generated .*/d
s/System$String.String/String/g
s/System$Int.Int/Int/g
s/System$List.List/List/g
s/System$Unit.Unit/Unit/g
s/System$Bool.Bool/Bool/g
s/System$Char.Char/Char/g
s/System$Tuples.Pair/Char/g
/^let type Pair.*/d
s/^in  sig/sig/
s/^      /  /
s/^    };/};/
s/</\&lt;/g
s/native \(.*\) = ".*/\1;/
