#lang racket
(require data/heap)
;------------------------------------------------------------------------------------------------------
(struct wire ([value #:mutable] [actions #:mutable] [sim #:mutable]))
(struct sim ([stack #:mutable][time #:mutable]))

(define (call-actions xs)
  (if (null? xs)
      (void)
      (begin
        ((car xs))
        (call-actions (cdr xs)))))

(define (wire-set! w v)
  (if (eq? v (wire-value w))
      (void)
      (begin
        (set-wire-value! w v)
        (call-actions (wire-actions w)))))

(define (add-action! w f)
  (set-wire-actions! w (cons f (wire-actions w))))

(define (nand-gate a b c)
  (define (and-action)
    (wire-set! c (not (and (wire-value a) (wire-value b)))))
  (add-action! a and-action)
  (add-action! b and-action))

(define (make-wire sim)
  (wire #f null sim))

(define (xor-gate a b c)
  (define d (make-wire (wire-sim a)))
  (define e (make-wire (wire-sim a)))
  (define f (make-wire (wire-sim a)))
  (nand-gate a b d)
  (nand-gate a d e)
  (nand-gate b d f)
  (nand-gate e f c))

(define (probe name w)
  (add-action! w (lambda ()
                   (display name)
                   (display " = ")
                   (display (wire-value w))
                   (display "\n"))))
;---------------------------------------------------------------------------------------------------------

(provide sim? wire?
         (contract-out
          [make-sim        (-> sim?)]
          [sim-wait!       (-> sim? positive? void?)]
          [sim-time        (-> sim? real?)]
          [sim-add-action! (-> sim? positive? (-> any/c) void?)]

          [make-wire       (-> sim? wire?)]
          [wire-on-change! (-> wire? (-> any/c) void?)]
          [wire-value      (-> wire? boolean?)]
          [wire-set!       (-> wire? boolean? void?)]

          [bus-value (-> (listof wire?) natural?)]
          [bus-set!  (-> (listof wire?) natural? void?)]

          [gate-not  (-> wire? wire? void?)]
          [gate-and  (-> wire? wire? wire? void?)]
          [gate-nand (-> wire? wire? wire? void?)]
          [gate-or   (-> wire? wire? wire? void?)]
          [gate-nor  (-> wire? wire? wire? void?)]
          [gate-xor  (-> wire? wire? wire? void?)]; to ma miec czas 2

          [wire-not  (-> wire? wire?)]
          [wire-and  (-> wire? wire? wire?)]
          [wire-nand (-> wire? wire? wire?)]
          [wire-or   (-> wire? wire? wire?)]
          [wire-nor  (-> wire? wire? wire?)]
          [wire-xor  (-> wire? wire? wire?)]

          [flip-flop (-> wire? wire? wire? void?)]))

(define (bus-set! wires value)
  (match wires
    ['() (void)]
    [(cons w wires)
     (begin
       (wire-set! w (= (modulo value 2) 1))
       (bus-set! wires (quotient value 2)))]))

(define (bus-value ws)
  (foldr (lambda (w value) (+ (if (wire-value w) 1 0) (* 2 value)))
         0
         ws))

(define (flip-flop out clk data)
  (define sim (wire-sim data))
  (define w1  (make-wire sim))
  (define w2  (make-wire sim))
  (define w3  (wire-nand (wire-and w1 clk) w2))
  (gate-nand w1 clk (wire-nand w2 w1))
  (gate-nand w2 w3 data)
  (gate-nand out w1 (wire-nand out w3)))
;---------------------------------------------------------------------------------
