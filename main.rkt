#lang racket/base

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [module-begin #%module-begin]))
(require syntax/wrap-modbeg
         syntax/parse/define
         syntax/srcloc
         images/compile-time
         racket/gui
         framework
         string-constants
         "external.rkt"
         (for-syntax racket/base
                     images/icons/symbol images/icons/style))

(define bitmap (compiled-bitmap (check-icon #:color run-icon-color)))

(define note%
  (class clickable-image-snip%
    (init-field callback srcloc)
    (define frame #f)
    (define edition #f)
    (define/override (copy)
      (define new-note (new note% [callback callback] [srcloc srcloc]))
      (send new-note set-frame frame)
      new-note)
    (define/public (set-frame the-frame)
      (set! frame the-frame)
      (set! edition (if frame
                        (send (send frame get-definitions-text) get-edition-number)
                        -1)))
    (super-make-object bitmap)
    (inherit set-callback)
    (set-callback (λ (_) (callback frame edition)))))

(define (open-and-highlight-in-file raw-srcloc frame edition)
  (define srclocs (if (srcloc? raw-srcloc) (list raw-srcloc) raw-srcloc))
  (define sources (filter values (map srcloc-source srclocs)))
  (unless (or (null? sources) (not frame))
    (define debug-source (car sources))
    (define same-src-srclocs
      (filter (λ (x) (eq? debug-source (srcloc-source x)))
              srclocs))
    (define editor (send frame get-definitions-text))
    (define rep (send frame get-interactions-text))
    (when frame (send frame show #t))
    (define out-of-date? (not (= edition (send editor get-edition-number))))

    (when out-of-date?
      (message-box (string-constant drscheme)
                   (string-constant editor-changed-since-srcloc-recorded)
                   frame
                   '(ok caution)
                   #:dialog-mixin frame:focus-table-mixin))
    (send rep highlight-errors same-src-srclocs '())))

(define (make-print x srcloc)
  (unless (or (void? x) (not (port-writes-special? (current-output-port))))
    (write-special
     (new note%
          [callback (λ (frame edition)
                      (open-and-highlight-in-file srcloc frame edition))]
          [srcloc srcloc]))
    (display " "))
  x)

(define-simple-macro (wrap x)
  #:with srcloc #'(build-source-location (quote-syntax x))
  (make-print x srcloc))

(define-syntax module-begin (make-wrapping-module-begin #'wrap #'#%module-begin))

(module reader syntax/module-reader
  drracket-find-expr)
