#lang racket/base

(provide clickable-image-snip%)
(require racket/class
         racket/gui/base)

(define arrow-cursor (make-object cursor% 'arrow))

(define (clickable-snip-mixin snip%)
  (class snip%
    (init-rest args)
    (inherit get-flags set-flags get-admin get-extent)

    (define callback void)
    (define/public (set-callback cb) (set! callback cb))
    (define/public (get-callback) callback)

    (define grabbed? #f)
    (define in-bounds? #f)

    (define/private (set-clicked new-grabbed? new-in-bounds? dc)
      (define needs-invalidate?
        (or (not (equal? grabbed? new-grabbed?))
            (not (equal? new-in-bounds? in-bounds?))))
      (set! grabbed? new-grabbed?)
      (set! in-bounds? new-in-bounds?)
      (when needs-invalidate?
        (invalidate dc)))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (super draw dc x y left top right bottom dx dy draw-caret)
      (when (and in-bounds? grabbed?)
        (let ([brush (send dc get-brush)]
              [pen (send dc get-pen)])
          (let-values ([(w h) (get-w/h dc)])
            (send dc set-brush (send the-brush-list find-or-create-brush "black" 'hilite))
            (send dc set-pen (send the-pen-list find-or-create-pen "white" 1 'transparent))
            (send dc draw-rectangle x y w h)
            (send dc set-pen pen)
            (send dc set-brush brush)))))

    (define/override (on-event dc x y editorx editory evt)
      (define-values (w h) (get-w/h dc))
      (define in-bounds? (and (<= (- (send evt get-x) x) w)
                              (<= (- (send evt get-y) y) h)))
      (cond
        [(send evt button-down? 'left)
         (set-clicked #t in-bounds? dc)]
        [(send evt button-up? 'left)
         (let ([admin (send this get-admin)])
           (when admin
             (send (send admin get-editor) set-caret-owner #f 'global)))
         (when (and grabbed? in-bounds?)
           (callback this))
         (set-clicked #f in-bounds? dc)]
        [else
         (set-clicked grabbed? in-bounds? dc)]))

    (define/private (invalidate dc)
      (let ([admin (get-admin)])
        (when admin
          (let-values ([(w h) (get-w/h dc)])
            (send admin needs-update this 0 0 w h)))))

    (define/private (get-w/h dc)
      (let ([wb (box 0)]
            [hb (box 0)])
        ;; know that the snip is the same size everywhere,
        ;; so just use (0,0) for its position
        (get-extent dc 0 0 wb hb #f #f #f #f)
        (values (unbox wb)
                (unbox hb))))

    (define/override (adjust-cursor dc x y editorx editory event)
      arrow-cursor)

    (apply super-make-object args)
    (set-flags (cons 'handles-events (get-flags)))))

(define clickable-image-snip% (clickable-snip-mixin image-snip%))
