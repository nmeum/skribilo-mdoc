;; This file implements an engine for creating mdoc(7) manual
;; pages using Skribilo. Semantic mdoc(7) markup is supported
;; through a custom Skribilo markup package.
;;
;; See https://man.openbsd.org/mdoc.7

(define-module (skribilo engine mdoc)
  #:use-module (skribilo evaluator)
  #:use-module (skribilo lib)
  #:use-module (skribilo ast)
  #:use-module (skribilo engine)
  #:use-module (skribilo writer)
  #:use-module (skribilo utils syntax)
  #:use-module (skribilo package base)
  #:autoload   (skribilo parameters)    (*destination-file*)
  #:use-module (skribilo output)

  #:export (mdoc-engine))

(skribilo-module-syntax)

;; State to detect empty lines in filter function.
(define filter-prev-empty? #t)

(define mdoc-engine
  (default-engine-set!
    (make-engine 'mdoc
      :version 0.1
      :format "mdoc"
      :delegate (find-engine 'base)
      :filter (lambda (str)
                (let* ((is-empty? (equal? str "\n"))
                       (ret-value (if (and is-empty? filter-prev-empty?)
                                    ""
                                    (string-trim str char-set:blank))))
                  (set! filter-prev-empty? is-empty?)
                  ret-value))
      :custom '())))

(define in-parsed-macro?
  (make-parameter #f))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-ornament markup macro)
  (markup-writer markup
    :before (string-append "\n." (symbol->string macro) " ")
    :after  "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-ornament 'bold 'Sy)
;; TODO: 'code
(make-ornament 'emph 'Em)
(make-ornament 'it 'Em)
;; TODO: 'kbd
(make-ornament 'roman 'No)
;; TODO: 'sc
;; TODO: 'underline
;; TODO: 'sf
;; TODO: 'sub
;; TODO: 'sup
;; TODO: 'tt
;; TODO: 'underline
(make-ornament 'var 'Va)

(markup-writer 'blockquote
  :options '(:ident :class)
  :before (lambda (doc e)
            (output-macro e 'Bd "-literal" "-offset indent"))
  :after  (lambda (doc e)
            (output-newline e) ;; TODO
            (output-macro e 'Ed)))

(markup-writer 'document
  :options '(:title :author :ending :mdoc-desc :mdoc-date :mdoc-section :mdoc-system)
  :action (lambda (doc e)
            (set-port-encoding! (current-output-port) "UTF-8")
            (let ((title   (markup-option doc :title))
                  (desc    (markup-option doc :mdoc-desc))
                  (section (markup-option doc :mdoc-section))
                  (date    (markup-option doc :mdoc-date))
                  (system  (markup-option doc :mdoc-system))
                  (body    (markup-body doc)))
              (output-preamble e
                (if (string? title)
                  title
                  (ast->string title))
                (or date "$Mdocdate$")
                (or section
                    (begin
                      (skribe-warning 1 "mdoc section not defined, defaulting to '1'")
                      1))
                system)

              (output-section e "name")
              (output-macro e 'Nm title)
              (if desc
                (output-macro e 'Nd desc)
                (skribe-warning 1 "mdoc one-line description is missing"))

              (output body e)
              (output-newline e))))

(markup-writer 'paragraph
  :options '(:ident :class)
  :before (lambda (n e)
            (output-macro e 'Pp))
  :after  (lambda (n e)
            (output-newline e)))

(markup-writer 'section
  :options '(:title :number :file :toc)
  :action (lambda (n e)
            (let ((body  (markup-body n))
                  (title (markup-option n :title)))
              (output-section e (ast->string title))
              (output body e))))

(markup-writer 'itemize
   :options '(:symbol)
   :before (lambda (n e)
             (output-macro e 'Bl "-tag" "-width Ds"))
   :after (lambda (n e)
            (output-macro e 'El)))

(markup-writer 'item
   :options '(:key)
   :action (lambda (n e)
             (%output-macro e 'It "")
             (parameterize ((in-parsed-macro? #t))
               (evaluate-document (markup-body n) e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(markup-writer 'man-name
  :action (lambda (n e)
            (output-macro e 'Nm)))

(markup-writer 'man-arg
  :options '(:ident :class)
  :action (lambda (n e)
            (output-macro e 'Ar (markup-body n))))

(markup-writer 'man-flags
  :action (lambda (n e)
            (%output-macro e 'Fl "")
            (parameterize ((in-parsed-macro? #t))
              (evaluate-document (markup-body n) e))
            (output-newline e)))
