# **[WIP]** POSet

Depends on [`gvs`](https://github.com/siiky/gvs)

## Problems

For simplicity, all of the following examples will use a single POSet operated
with itself, but the problem is not excluseive to same-set operations.

Let's call our working POSet `S = (S, <=)`, with `S = { 0, 1, 2 }` and
`<= = { (0,0), (1,1), (2,2), (0,1), (1,2), (0,2) }`. Visually, this is the
lattice:

![S](S.svg)

This `<=` relation **is correct**, and it's easy to calculate a similar one
from any two POSets. However, depending on use cases, it may be optimised.

I don't see anything that can be done to improve calculating or representing
`S*S`.

### Memory optimised

The representation above is really inappropriate for big sets. Worst case
scenario happens when for all `a` and `b` in `S`, `a <= b`, which means
`|<=| = |S|^2`; and the best happens when `<=` is the identity relation, which
means `|<=| = |S|`. Neither case is useful -- they don't really represent a
POSet anymore, but merely a set. All other (useful) cases fall in the middle.

Since we already know that `<=` is reflexive and transitive (because we're
dealing with POSets), using that representation on a computer is a waste of
memory. We can "shorten" it to `<= = { (0,1), (1,2) }`. But _how_? I feel like
this is some sort of graph problem... It's kind of the oppositve of the
transitive closure -- given a relation `<=`, I want the relation `<='` such
that the transitive closure of `<='` is `<=`.

### Relation speed optimised

This is kinda tricky... At first glance, it may seem that removing the elements
`(a, a)`, for all `a` in `S`, from the relation `<=` is the best possible
implementation, but for a big enough set I think it actually ends up being
worse than the memory optimised version. Let's not promise anything instead,
just say "at least it's better than nothing", and give the programmer the
responsibility.

### POSet creation speed optimised

The relation for `S*S` is defined like this:
`(a1, a2) <=_S*S (b1, b2) <=> a1 <=_S b1 && a2 <=_S b2`

To create the reation quickly, instead of creating a set representing the
relation, we can create a function. In Scheme:

```scm
(define ((<=* <=1 <=2) a b)
  (match-let (((a1 a2) a)
              ((b1 b2) b))
    (and (<=1 a1 b1)
         (<=2 a2 b2))))
```

You can use it like this:

```scm
(let ((<=1 ...)
      (<=2 ...))
  (let ((<= (<=* <=1 <=2))
        (a ...)
        (b ...))
    (<= a b)))
```
