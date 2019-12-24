(module
  poset
  (
   *
   brel
   brel->gvs
   default-comparator
   gvs
   lt-table
   lt-table->brel
   new
   null
   prod
   set
   uniq
   )

  (import
    ; Import only what is strictly necessary and avoid redefining
    ; the base language
    (only scheme
          and
          car
          cdar
          cdr
          cons
          define
          delay
          eq?
          force
          if
          lambda
          let
          let*
          modulo
          not
          null?
          or
          quasiquote
          quote)

    (only chicken.base
          cute
          delay-force
          gensym
          identity
          when)

    (only chicken.string
          ->string))

  (import
    (only srfi-1
          append-map
          concatenate
          filter
          map
          member)

    (only srfi-42
          list-ec
          :list
          )

    (only srfi-69
          alist->hash-table
          hash-table->alist
          hash-table-ref/default
          hash-table-update!/default
          make-hash-table)

    (rename
      (only srfi-113
            list->set
            set
            set->list
            set-any?
            set-contains?)
      (set mkset))

    (rename
      (only srfi-128
            <?
            =?
            comparator-equality-predicate
            comparator-hash-function
            comparator-ordering-predicate
            comparator-type-test-predicate
            make-comparator
            make-default-comparator
            make-pair-comparator)
      (<? cmp<?)
      (=? cmp=?))

    (only typed-records
          defstruct))

  (define ((flip f) a b) (f b a))

  (define default-comparator (make-default-comparator))

  (define ((srfi-128-hf->srfi-69-hf hf) obj size)
    (modulo (hf obj) size))

  (define (srfi-128-cmp->srfi-69-hf cmp)
    (srfi-128-hf->srfi-69-hf (comparator-hash-function cmp)))

  (define (swap pair)
    (cons (cdr pair) (car pair)))

  (define (make-swapped-pair-comparator pair-comparator)
    (define type?     (comparator-type-test-predicate pair-comparator))
    (define type=?    (comparator-equality-predicate  pair-comparator))
    (define type-ord  (comparator-ordering-predicate  pair-comparator))
    (define type-hash (comparator-hash-function       pair-comparator))

    (define (swapped-type?     b.a)     (type?     (swap b.a)))
    (define (swapped-type=?    b.a d.c) (type=?    (swap b.a) (swap d.c)))
    (define (swapped-type-ord  b.a d.c) (type-ord  (swap b.a) (swap d.c)))
    (define (swapped-type-hash b.a)     (type-hash (swap b.a)))

    (make-comparator swapped-type? swapped-type=? swapped-type-ord swapped-type-hash))

  ;; TODO: use sets and tables

  ;; We'll need a type comparator, to be used for internal operations, like
  ;; operations on sets.
  ;;
  ;; How to compare elements of a POSet according to the POSet's <-relation?
  ;; What's the best/most general abstraction? It should be possible to use
  ;; this abstraction to calculate the <-relation of a new POSet based on
  ;; other POSets (e.g., direct product). How to calculate the <-relation of
  ;; the direct product of two POSets?

  ;; type-cmp is a SRFI-128 comparator to be applied to two elements of the set
  ;;
  ;; <-entries :: Elem -> [Elem] is the binary less-than relation; represented
  ;;     as a map from elements to a list of elements greater
  ;;
  ;; set The set of the poset's elements
  ;; set-list The set of the poset's elements (in a list)
  (defstruct poset-int type-cmp <-entries set set-list)

  (define (uniq elems #!optional (type-cmp default-comparator))
    (set->list (list->set type-cmp elems)))

  ; <-entries is an alist
  ; <-entries is then converted into a SRFI-113 set
  (define (new <-entries #!key (type-cmp default-comparator) (set #f))
    (let ((set (if set
                   (delay (list->set type-cmp set))
                   (delay (list->set type-cmp (concatenate <-entries)))))
          (<-entries
            (delay
              (alist->hash-table
                (map (lambda (a.bs)
                       `(,(car a.bs) . ,(list->set type-cmp (cdr a.bs))))
                     <-entries)
                #:test (comparator-equality-predicate type-cmp)
                #:hash (srfi-128-cmp->srfi-69-hf type-cmp)))))
      (make-poset-int
        #:type-cmp type-cmp
        #:<-entries <-entries
        #:set set
        #:set-list (delay (set->list (force set)))
        )))

  (define null
    (new
      '((()))
      #:type-cmp default-comparator
      #:set '(())))

  ;;;
  ;;; Higher level accessors
  ;;;

  (define (type-comparator poset)
    (poset-int-type-cmp poset))

  (define (set poset)
    (force (poset-int-set poset)))

  (define (set-list poset)
    (force (poset-int-set-list poset)))

  (define (lt-table poset)
    (force (poset-int-<-entries poset)))

  ;;;
  ;;; Utitlities
  ;;;

  (define (elem? a as)
    (set-contains? as a))

  (define (table-lts table type-cmp a)
    (hash-table-ref/default table a (mkset type-cmp)))

  (define (lts poset a)
    (table-lts (lt-table poset) (type-comparator poset) a))

  ;; @brief Given a relation table @a table and the type comparator @a
  ;;        type-cmp of a POSet, check if @a a is less-than @a b
  ;; @param table The POSet's relation table
  ;; @param type-cmp The POSet's type comparator
  ;; @param a An element of the POSet
  ;; @param b An element of the POSet
  ;; @returns #t if @a a is less than @a b in the POSet
  ;;
  ;; Internally memoizes results to avoid rechecking arguments. Does not
  ;;     memoize across table<? calls!
  (define (table<? table type-cmp a b)

    ;; @see table<?
    ;; @param memo The memoization table on @a a
    ;; @param ref A unique symbol to be used as the default return value on
    ;;        hash-table lookups
    (define (table<?/memo memo ref table type-cmp a b)
      (let* ((ret (hash-table-ref/default memo a ref))
             (dnf? (eq? ret ref))
             (ret
               (if dnf?
                   (and (not (cmp=? type-cmp a b)) ; non-reflexivity
                        (let ((bs (table-lts table type-cmp a))) ; elements covering a
                          (or
                            ; does b cover a ?
                            (elem? b bs)
                            ; transitivity
                            (set-any? (cute table<?/memo memo ref table type-cmp <> b) bs))))
                   ret)))
        (when dnf? (hash-table-update!/default memo a identity ret))
        ret))

    (let ((ref (gensym))
          (ht (make-hash-table
                #:test (comparator-equality-predicate type-cmp)
                #:hash (srfi-128-cmp->srfi-69-hf type-cmp))))
      (table<?/memo ht ref table type-cmp a b)))

  (define (<? poset a b)
    (table<? (lt-table poset) (type-comparator poset) a b))

  (define (set* s1 s2)
    (list-ec
      (:list a s1)
      (:list b s2)
      (cons a b)))

  ;;;
  ;;; POSets operations
  ;;;

  (define (invert poset)
    (let ((<-entries (lt-table poset))
          (swapped-type-cmp (make-swapped-pair-comparator (type-comparator poset))))
      ; TODO:
      (let ((inverted-<-entries <-entries))
        (new inverted-<-entries #:type-cmp swapped-type-cmp))))

  (define (prod ps1 ps2)
    (define ((*<? a1.a2) b1.b2)
      (and (<? ps1 (car a1.a2) (car b1.b2))
           (<? ps2 (cdr a1.a2) (cdr b1.b2))))

    (let ((type-cmp (make-pair-comparator (type-comparator ps1) (type-comparator ps2)))
          (s1*s2 (set* (set-list ps1) (set-list ps2))))
      ; TODO: how to improve this?
      ; FIXME: This could be improved! Because of transitivity, we need only
      ;        direct greater-thans. Finding the smallest of the greater-thans
      ;        isn't the right answer if the POSet is not a chain.
      ;
      ; Ou entao poe todos os >a num set e depois seja b um elem
      ; desse set se existir um c nesse set tal que c<b, eliminas o
      ; b do set. No final tens um set dos maiores que a
      (let ((<-entries
              ; get all the elements greater than a.b
              (map (lambda (a.b) (cons a.b (filter (*<? a.b) s1*s2)))
                   s1*s2)))
        (new <-entries #:type-cmp type-cmp))))

  ;; TODO: How to improve this? `prod` is already pretty bad as it is... Is
  ;;       there a way to make this an iterative process?
  ;; (* A B C D) => (A x (B x (C x D)))
  (define (* poset . posets)
    ; (*) [ poset ] = poset
    ; (*) [ poset : posets ] = prod poset ((*) posets)
    (let loop ((poset poset)
               (posets posets))
      (if (null? posets)
          poset
          (prod poset (loop (car posets) (cdr posets))))))

  (define (lt-table->brel lt-table)
    (append-map
      (lambda (a.bs)
        (let ((a  (car a.bs))
              (bs (set->list (cdr a.bs))))
          (map (cute cons a <>) bs)))
      (hash-table->alist lt-table)))

  (define (brel poset)
    (lt-table->brel (lt-table poset)))

  (define (brel-nodes brel type-cmp)
    (map ->string
         (uniq (append-map
                 (lambda (a.b)
                   `(,(car a.b) ,(cdr a.b)))
                 brel)
               type-cmp)))

  (define (poset-nodes poset)
    (brel-nodes (brel poset) (type-comparator poset)))

  (define (brel->gvs
            brel
            #!key
            (type-cmp default-comparator)
            (name 'poset)
            (settings '())
            (nodes #f)
            )
    ; What kind of graph/edges to use? either digraph/-> or graph/--
    (let ((edge-type '->)
          (graph-type 'digraph))
      (let ((edges
              (map
                (lambda (a.b)
                  `(,edge-type ,(->string (car a.b))
                               ,(->string (cdr a.b))))
                brel))
            (nodes (map ->string (or nodes (brel-nodes brel type-cmp)))))
        `(,graph-type
           ,name
           (settings
             (graph (layout dot) (rankdir BT))
             . ,settings)
           (nodes . ,nodes)
           .
           ,edges))))

  (define (gvs
            poset
            #!key
            (name 'poset)
            (settings '())
            )
    (brel->gvs (brel poset)
               #:name name
               #:settings settings
               #:type-cmp (type-comparator poset)
               ))
  )
