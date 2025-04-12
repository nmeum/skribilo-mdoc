(define-module (mdoc utils markup)
  #:use-module (skribilo ast)
  #:use-module (skribilo writer)
  #:use-module (skribilo utils syntax)
  #:use-module (skribilo evaluator)

  #:use-module (mdoc utils output)

  #:export (make-block
            make-listing
            make-ornament
            make-macro
            make-parsed-macro))

(skribilo-module-syntax)

(define* (make-block markup start end #:optional opts)
  (markup-writer markup
    :options (or opts '())
    :before (lambda (n e)
              (apply output-macro e start))
    :after (lambda (n e)
             (apply output-macro e end))))

(define (make-listing markup . list-opts)
  (make-block markup
              (cons* 'Bl list-opts)
              '(El)
              '(:symbol)))

(define (make-ornament markup macro)
  (markup-writer markup
    :before (string-append "\n." (symbol->string macro) " ")
    :after  "\n"))

(define* (make-macro markup macro #:optional opts)
  (markup-writer markup
    :options (or opts '())
    :action (lambda (n e)
              (output-macro e macro (markup-body n)))))

(define* (make-parsed-macro markup macro #:optional opts)
  (markup-writer markup
    :options (or opts '())
    :action (lambda (n e)
              (with-parsed-macro (e macro)
                (evaluate-document (markup-body n) e)))))
