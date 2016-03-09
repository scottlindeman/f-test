#lang racket

(require 2htdp/universe 2htdp/image)

(define f-img (bitmap/file "f-img.png"))
(define width 400)
(define height 500)
(define center (/ width 2))
(define forward 'forward)
(define backward 'backward)

;; World
(struct world (state history stop) #:transparent)

;; State
(struct state (img degree dir start-time))

;; History Item
(struct hist-item (time key dir correct) #:transparent)

(define initial-world (world (state f-img 0 forward (current-inexact-milliseconds)) empty #f))

(define (run)
  (big-bang initial-world
            (on-key submit)
            (to-draw draw)
            (stop-when pls-stop)))

(define (was-correct answer dir)
                     (if (forward? dir)
                         (key=? answer "f")
                         (key=? answer "j")))

(define (direction dir) (if (forward? dir) forward backward))

(define (forward? dir)
  (cond [(number? dir) (> dir 0)]
        [(symbol? dir) (symbol=? dir forward)]
        [else #f]))

(define (new-f dir deg img)
  (let ([new-img (rotate deg img)])
    (if (forward? dir) new-img (flip-horizontal new-img))))

(define (submit w key)
  (let* ([new-dir (direction (random 2))]
        [new-deg (* 45 (random 8))]
        [s (world-state w)]
        [old-dir (state-dir s)]
        [current-time (current-inexact-milliseconds)])
    (cond [(key=? key "escape") (world s (world-history w) #t)]
          [(or (key=? key "f") (key=? key "j"))
           (world (state (new-f new-dir new-deg (state-img s)) new-deg new-dir current-time)
                  (cons (hist-item (- current-time (state-start-time s)) key old-dir (was-correct key old-dir))
                        (world-history w)) #f)]
          [else w])))

(define (draw w)
  (place-image (state-img (world-state w)) center center
               (place-image (text "Type f for forwards and j for backwards" 16 'black) center width
                            (empty-scene width height))))


(define (pls-stop w) (world-stop w))