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
  #:use-module (skribilo utils strings)
  #:use-module (skribilo package base)
  #:use-module (skribilo output)
  #:autoload   (skribilo parameters)    (*destination-file*)

  #:use-module (mdoc utils markup)
  #:use-module (mdoc utils output)

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
      ;; TODO: Remove the newline filtering code at some point.
      :filter (lambda (str)
                (let* ((is-empty? (equal? str "\n"))
                       (ret-value (if (and is-empty? filter-prev-empty?)
                                    ""
                                    (string-trim str char-set:blank))))
                  (set! filter-prev-empty? is-empty?)
                  ret-value))
      :custom '())))

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

(make-block 'blockquote '(Bd "-literal" "-offset indent") '(Ed))
(make-block 'center '(Bd "-centered") '(Ed))
(make-block 'pre '(Bd "-literal") '(Ed))
;; TODO: flush

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

(make-listing 'itemize "-tag" "-width Ds")
(make-listing 'enumerate "-enum")
;; TODO: description

(markup-writer 'item
   :options '(:key)
   :action (lambda (n e)
             (let ((k (markup-option n :key)))
               (if k
                 (with-parsed-macro (e 'It)
                   (evaluate-document k e))
                 (output-macro e 'It)))
             (evaluate-document (markup-body n) e)))

(make-macro 'mark 'Tg)
(make-macro 'mailto 'Mt '(:text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(markup-writer 'man-name
  :action (lambda (n e)
            (output-macro e 'Nm)))

(make-parsed-macro 'man-arg 'Ar)
(make-parsed-macro 'man-flags 'Fl)
(make-parsed-macro 'man-opt 'Op)
