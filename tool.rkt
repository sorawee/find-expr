#lang racket/base

(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit)

(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define frame #f)

    (define find-expr-mixin
      (mixin ((class->interface text%)) ()
        (inherit find-first-snip)
        (define/augment (after-insert _start _len)
          (when frame
            (let loop ([snip (find-first-snip)])
              (when snip
                (with-handlers ([exn:fail? void])
                  ;; NOTE: ideally we should be able to use is-a? note%
                  ;; but it looks like note% is in a different namespace
                  ;; so it doesn't work
                  (send snip set-frame frame))
                (loop (send snip next))))))
        (super-new)))

    (define dummy-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (set! frame this)))

    (define phase1 void)
    (define phase2 void)

    (drracket:get/extend:extend-unit-frame dummy-mixin)
    (drracket:get/extend:extend-interactions-text find-expr-mixin)))
