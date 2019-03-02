(add-to-load-path ".")
(use-modules (web server) (rnrs bytevectors) (common))
(run-server
 (lambda (request request-body)
  (values '((content-type . (text/plain)))
   (process-line (utf8->string request-body))))
 'http '(#:port 7183))
