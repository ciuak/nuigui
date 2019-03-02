(define-module (nuigui)
               #:export (perform))

;             list ops      let-values     cut
(use-modules (srfi srfi-1) (srfi srfi-11) (srfi srfi-26))

;;; Class definitions and suchlikes.
;; `gui` -- the serial connection keyword
(define (gui? sym) (eq? sym 'gui))
;; `unary` -- unary operator application keyword
(define (unary? sym) (eq? sym 'unary))
;; `sa` identification
(define (sa? sym) (eq? sym 'sa))
;; TO -- afterthought connective particle
(define (to? sym) (eq? sym 'to))
;; RU -- binary connective
(define (ru? sym) (member sym '(ra ru ri ro re roi)))
;; MU -- unary operator
(define (mu? sym) (eq? sym 'mu))
;; FI -- place number selector.
;; This predicate also returns the ordinal of the place.
(define (fi? sym) (list-index (lambda (_) (eq? _ sym))
                              '(fi go cu ke)))
;; PI -- branching operator
(define (pi? sym) (eq? sym 'pi))
;; The things that can appear after `to`.
(define (after-to? sym) (or (ru? sym) (pi? sym)))

;; `e`[xpression] with `s`[ubstition] for `v`[ariable]. (No name
;; collision checking.) `s` must be a symbol.
(define (substitute e s v)
 (cond
  ((null? e) e)
  ((pair? e)
   (cons (substitute (car e) s v)
         (substitute (cdr e) s v)))
  (#t (if (eqv? e v) s e))))

;; `huai` converts a list of tokens input by the user into a binary
;; syntax tree (dotted lists of length 3). The keyword `unary` is used
;; in order to keep this property.
(define (huai atoms)
 (cond
  ((null? atoms) '())
  ((null? (cdr atoms)) (car atoms))
  (#t
   (let ((first  (car  atoms))
         (second (cadr atoms)))
    (cond
     ((and (to? first)
           (after-to? second))
      ; Find the matching `to`.
      (let loop ((level 0) (left '()) (right (cddr atoms)))
       (if (not (to? (car right)))
           (loop level (cons (car right) left) (cdr right))
           (if (after-to? (cadr right))
               (loop (1+ level)
                     (cons* (cadr right) (car right) left)
                     (drop right 2))
               (if (positive? level)
                   (loop (1- level)
                         (cons (car right) left)
                         (cdr right))
                   `(,second ,(huai (reverse left)) .
                             ,(huai (cdr right))))))))
     ((or (to? first) (pi? first))
      ; `to`'s structure is an exception, as its main purpose is
      ; to be captured by the parent call to `huai` that had
      ; registered a `to RU` construct.
      `(,first ,(huai (cdr atoms))))
     ((or (ru? second) (fi? second))
      `(,second ,first . ,(huai (cddr atoms))))
     ((and (mu? first))
      (let ((huaise (huai (cdr atoms))))
       (if (symbol? huaise) `(unary ,first . ,huaise)
           `(,(car huaise) (unary ,first . ,(cadr huaise))
                           . ,(cddr huaise)))))
     (#t (let ((huaise (huai (cdr atoms))))
          (cond
           ; Change a `pi` tail into a `pi` serial.
           ((and (pair? huaise) (pi? (car huaise)))
            `(gui pi . (gui ,first . ,(cadr huaise))))
           ; Propagate the `pi` serial.
           ((and (pair? huaise) (pi? (cadr huaise)))
            `(gui pi . (gui (gui ,first . ,(cadddr huaise)) .
                            ,(cddddr huaise))))
           (#t `(gui ,first . ,huaise))))))))))

;; `Haqbai` constructs the serial predicate from the syntax tree. The
;; lookup argument is the lookup function `haqbai` will use to check
;; argument structures of predicates. The lookup function shall return
;; a list of numbers-or-#f, where numbers represent arities, and #f is
;; a (non-subordinating) concrete slot, or #f on failure (which will
;; stop the procedure).
;; The output of haqbai is the triplet (slot-types vars . definition).
(define (haqbai lookup st)
 (cond
  ((null? st) (error 'implementation-oopsie))
  ((symbol? st)
   (let ((juqmu (lookup st)))
    (if (not juqmu) (throw 'lookup-error st)
        (let ((unders (list-tabulate (length juqmu)
                                    (lambda (_) (gensym "_")))))
             ; Creates impromptu variables starting with underscores.
             `(,juqmu ,unders ,st ,@unders)))))
  (#t
   (let ((op (car st)))
    (cond
     ((gui? op)
      (if (pi? (cadr st)) (haqbai lookup (cddr st))
       (let* ((l (haqbai lookup (cadr st)))
              (r (haqbai lookup (cddr st))))
        (let ((; subordinating arg place for `l`
               coemoa 
               (let ; try to find it among the non-first values
                    ((ind (list-index values (cdar l))))
                    (cond (ind (1+ ind))
                          ((car (car l)) 0) ; or the first place?
                          (#t #f)))))       ; or fail miserably
         (if (not coemoa) ; remap to ru on failure
             (haqbai lookup `(ru ,(cadr st) . ,(cddr st)))
          (let* ((arity (list-ref (car l) coemoa)))
                `(; Seems scary, but all it does is jam one serial into
                  ; another, field-wise.
                  ,@(map (lambda (n)
                          (append (take (list-ref l n) coemoa)
                                  (drop (list-ref r n) arity)
                                  (drop (list-ref l n) (1+ coemoa))))
                         '(0 1))
                  ; The definition field gets jammed in in a bit
                  ; different way.
                  ,@(take (cddr l) (1+ coemoa))
                  (,(take (car  r) arity)
                   ,(take (cadr r) arity)
                   ,@(cddr r))
                  ,@(drop (cddr l) (+ 2 coemoa)))))))))

  ; This part is not official. The current behaviour is to always
  ; unify the first places, and then stop unifying as soon as the
  ; argument slot types don't match.
  ((ru? op)
   (let* ((l (haqbai lookup (cadr st)))
          (r (haqbai lookup (cddr st)))
          ; Merge as long as the places' arities match, although the
          ; first place gets merged no matter what.
          (merge-extent (1+ (length
                             (take-while
                              (lambda (x) (eq? (car x) (cadr x)))
                              ; (Not taking the first places.)
                              (zip (cdar l) (cdar r))))))
          (slot-types (append       (car  l)
                              (drop (car  r) merge-extent)))
          (vars       (append       (cadr l)
                              (drop (cadr r) merge-extent)))
          (new-rexp ; Unify the respective variables of the right
                    ; serial with those of the left serial.
           (let loop ((vars   (take (cadr r) merge-extent))
                      (substs (take (cadr l) merge-extent))
                      (rexp (cddr r)))
                     (if (null? vars) rexp
                         (loop (cdr vars) (cdr substs)
                               (substitute rexp
                                           (car substs)
                                           (car vars)))))))
    `(,slot-types ,vars . (,op ,(cddr l) ,new-rexp))))

  ((unary? op)
   (let ((unop (cadr st)))
    (cond
     ((mu? unop)
      (let* ((haq (haqbai lookup (cddr st)))
             (slots (car  haq))
             (vars  (cadr haq))
             (defn  (cddr haq)))
       `((,(cadr slots) ,(car slots) . ,(cddr slots))
         (,(cadr vars)  ,(car vars)  . ,(cddr vars))
         . ,defn))))))

  (else
   (let ((f (fi? op)))
    (if (not f) (throw 'bu-deq-haqbai f)
     (let* ((l (haqbai lookup (cadr st)))
            (r (haqbai lookup (cddr st)))
            (u (gensym "_")))
      `(,(append (take (car  l) f) (drop (car l) (1+ f)))
        ,(append (take (cadr l) f) (drop (cadr l) (1+ f)))
        ,(caddr l) ,@(take (cadr l) f) (sa ,r)
                   ,@(drop (cadr l) (1+ f))))))))))))

;; Utility function for `leache`: make a Toaq number.
(define (count n)
 (cond
  ((zero? n) "")
  ((>= n 1000)
   (let-values (((div mod) (floor/ n 1000)))
               (string-append (count div) "biq" (count mod))))
  (#t
   (let*-values (((huns rest) (floor/ n 100))
                 ((tens ones) (floor/ rest 10)))
    (apply string-append
     (map (lambda (num)
           (cdr (assq num
                      '((1 . "shi")  (2 . "gu")   (3 . "saq")
                        (4 . "jo")   (5 . "fe")   (6 . "ci")
                        (7 . "diai") (8 . "roai") (9 . "nei")
                        (10 . "hei") (100 . "fue")
                        (0 . "")))))
          (list huns (if (zero? huns) 0 100)
                tens (if (zero? tens) 0 10)
                ones)))))))

;; Make variable names.
(define (don n)
 (stick 2 (string-append "do" (count n))))
(define (jadon n) (string-append "ja " (don n)))

;; Stick tones to arbitrary names.
(define (stick n w)
 (letrec
  ((tones  '(#f      #\x0304 #\x0301 #\x030c
             #\x0309 #\x0302 #\x0300 #\x0303))
   (vowel? (cut member <> '(#\a #\u #\i #\o #\e)))
   (stick (lambda (n l)
           (if (null? l) l
            (if (vowel? (car l))
                (let-values (((head tail)
                              (span vowel? (cdr l))))
                            (append
                             (list (car l))
                             (if (zero? n)
                                 '() (list (list-ref tones n)))
                             head
                             (stick 1 tail)))
                           (cons (car l) (stick n (cdr l))))))))
  (string-map (lambda (c)
                      (if (eq? c #\i) #\ı c))
              (string-normalize-nfc
               (list->string (stick n (string->list w)))))))

(define (strip-na s)
 (if (string-suffix? " na" s)
     (strip-na (string-drop-right s 3))
     s))

;; `leache` converts `haqbai`'s output into readable Toaq.
(define (leache haq)
 (letrec
  ((lea
    ; This anonymous function returns a pair (count . string). The
    ; left element is for keeping track of the slot names' indices.
    (lambda (haq n)
     (cond
      ((symbol? haq)
       ; This should never happen -- by the time the leaf would be
       ; reached, the symbol should've been substituted by a string.
       (error 'oops-reached-symbol))
      ((string? haq) (cons n haq))
      ((and (pair? haq) (sa? (car haq)))
       (let-values (((count expr)
                     (car+cdr (lea (cadr haq) n))))
        (cons count (string-append "sa lí " (string-drop expr 3)))))
      ((pair? haq)
       ; Substitute `(cadr haq)` (variables) with fresh "do" slot
       ; names.
       (let loop
        ((count n)
         (vars (cadr haq))
         ; `cdddr` -- not counting the content word
         (expr (cdddr haq)))
        (if (pair? vars)
            (loop (1+ count) (cdr vars)
                  (substitute expr (don count) (car vars)))
            (let* ((is-ru? (ru? (caddr haq)))
                   (new-expr
                    ; Expand each argument place (element of `expr`)
                    ; recursively.
                    (let loop ((count count) (old expr)
                               (new-expr '()))
                     (if (null? old) new-expr
                      (let-values (((count expr)
                                    (car+cdr
                                     (lea (if is-ru?
                                              `(() () . ,(car old))
                                              (car old))
                                          count))))
                                  (loop count
                                        (cdr old)
                                        (cons expr new-expr)))))))
                 (cons count
                  (string-join
                   (append
                    (if ; The prenex might be nullary. We must exclude
                        ; "bı" then.
                        (zero? (length (cadr haq))) '("lû")
                        (append '("lî")
                                ; The prenex.
                                (map (lambda (i)
                                             (jadon (+ n i)))
                                     (iota (length (cadr haq))))
                                '("bı")))
                    ; The predicate/connector and the
                    ; arguments/connectands.
                    (if is-ru?
                     (list "to"
                      (stick 0 (symbol->string (caddr haq)))
                      "lủ"
                      (string-drop (strip-na (cadr new-expr)) 3)
                      "to lủ"
                      (string-drop (car new-expr) 3)
                      "na")
                     (append
                      (list (stick 4 (symbol->string (caddr haq))))
                      (reverse new-expr)))
                    (list "na"))
                   " "))))))))))
  (strip-na (cdr (lea haq 1)))))

(define (perform lookup list-of-tokens)
 (let ((haq (haqbai lookup (huai list-of-tokens))))
      (cons (car haq)
            (leache haq))))
