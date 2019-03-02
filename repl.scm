(add-to-load-path ".")
(use-modules (common) (ice-9 textual-ports))

(let loop ()
 (let ((line (get-line (current-input-port))))
  (if (and (not (eof-object? line)) (positive? (string-length line)))
   (begin (put-string (current-output-port) (process-line line))
          (loop)))))
