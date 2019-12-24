(import chicken.pretty-print)

(import
  (only gvs
        gvs-write))

(import (prefix poset poset:))

;
; P := { 1, 2, 3 }
; < := { (1, 2), (2, 3), (1, 3) }
;
;     3
;     |
;     2
;     |
;     1
;
(define poset1
  (poset:new '((1 2) (2 3))
             #:type-cmp poset:default-comparator
             #:set '(1 2 3)))

;
; P := { a, b, c, d }
; < := { (a, b), (a, c), (b, d), (c, d), (c, d) }
;
;       d
;      / \
;     b   c
;      \ /
;       a
;
(define poset2
  (poset:new '((a b c) (b d) (c d))
             #:type-cmp poset:default-comparator
             #:set '(a b c d)))

(gvs-write (poset:gvs (poset:* poset1 poset2)))
