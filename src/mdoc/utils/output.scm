(define-module (mdoc utils output)
  #:use-module (skribilo output)
  #:use-module (skribilo ast)

  #:export (in-parsed-macro?
            with-parsed-macro

            output-macro
            output-newline
            output-section
            output-preamble))

(define in-parsed-macro?
  (make-parameter #f))

(define-syntax with-parsed-macro
  (syntax-rules ()
    ((with-parsed-macro (E M) BODY ...)
     (begin
       (%output-macro E M "")
       (parameterize ((in-parsed-macro? #t))
         BODY ...)
       (output-newline E)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (%output-macro e name . value)
  (define (->string obj)
    (if (string? obj)
      obj
      (ast->string obj)))

  (unless (in-parsed-macro?)
    (output-newline e)
    (output "." e))

  (output
    (if (null? value)
      (symbol->string name)
      (format #f "~a ~a"
              (symbol->string name)
              (string-join (map ->string value) " ")))
    e))

(define (output-macro e name . value)
  (%output-macro e name value)
  (unless (in-parsed-macro?)
    (output-newline e)))

(define (output-newline e)
  (output "\n" e))

(define (output-section e title)
  (output-macro e 'Sh (string-upcase title)))

(define (output-preamble e name date section system)
  (output-macro e 'Dd date)
  (output-macro e 'Dt (string-upcase name) section)
  (if system
    (output-macro e 'Os system)
    (output-macro e 'Os)))
