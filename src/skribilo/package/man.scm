;; This file provides a package for describing man pages using
;; Skribilo. While currently, markup writers for the defined
;; markup are only implemented for the mdoc engine, these
;; markups could also be used with different engines. For example,
;; with an engine implementing the legacy man(7) markup.
;;
;; See https://man.openbsd.org/man.7

(define-module (skribilo package man)
  #:use-module (skribilo utils syntax)
  #:use-module (skribilo lib)
  #:use-module (skribilo ast)
  #:use-module (skribilo engine))

(skribilo-module-syntax)

;; Markup for inserting the name of the current man page.
(define-markup (man-name :rest ignored)
  (new markup
       (loc &invocation-location)
       (markup 'man-name)))
