(define-module (common)
               #:export (process-line))
(use-modules (nuigui)
             (srfi srfi-1)
             (ice-9 regex)
             (ice-9 format))

(define frames
 (read (open-input-file "common/frame-types.scm")))

(define frame-lookup
 (read (open-input-file "common/frame-lookup.scm")))

(define (lookup w)
 (let ((l (assv w frame-lookup)))
  (and l
   (let ((f (assv (cdr l) frames)))
    (and f (cdr f))))))

(define (process-line l)
 (let* ((m (map string->symbol (map match:substring
                                      (list-matches "[a-z]+" l)))))
  (catch #t (lambda ()
             (let ((p (perform lookup m)))
                  (format #f "~a~%" p)))
            (lambda arg
             (format #f "There was an error ~s.~%" arg)))))
