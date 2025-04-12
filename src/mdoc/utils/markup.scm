(define-module (mdoc utils markup)
  #:use-module (skribilo ast)
  #:use-module (skribilo writer)
  #:use-module (skribilo utils syntax)

  #:use-module (mdoc utils output)

  #:export (make-listing
            make-ornament
            make-macro))

(skribilo-module-syntax)

(define (make-listing markup . list-opts)
  (markup-writer markup
     :options '(:symbol)
     :before (lambda (n e)
               (apply output-macro e 'Bl list-opts))
     :after (lambda (n e)
              (output-macro e 'El))))

(define (make-ornament markup macro)
  (markup-writer markup
    :before (string-append "\n." (symbol->string macro) " ")
    :after  "\n"))

(define (make-macro markup macro . opts)
  (markup-writer markup
    :options opts
    :action (lambda (n e)
              (output-macro e macro (markup-body n)))))
