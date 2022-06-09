#lang racket
(require data/heap)
(require compatibility/mlist)
;------------------------------------------------------------------------------------------------------
(struct wire ([value #:mutable] [actions #:mutable] [sim]))
(struct sim ([stack #:mutable][time #:mutable]))

(define (my-fold f lst);nie dziala
  (cond[(empty? lst) ]
       [#t (begin
             (f (car lst))
             (my-fold f (cdr lst)))]
  ))

(define (call-actions xs);na pierwszych miejscach w parach będą akcje
  (if (null? xs)
      (void)
      (begin
        ((car (car xs)))
        (call-actions (cdr xs)))))

(define (wire-set! w v)
  (if (eq? v (wire-value w))
      (void)
      (begin
        (set-wire-value! w v)
        (call-actions (wire-actions w)))))

(define (add-action! w f)
  (set-wire-actions! w (mcons f (wire-actions w))))

(define (make-wire sim)
  (wire #f null sim))

(define (make-sim)
  (sim (mlist ) 0))

;(define (sim-time source)
;  (sim-time source)
; )

(define (sim-add-action! simu extime action)
  (mappend! (sim-stack simu) '(extime action) );;czemu???
    )

(define (sim-wait! simu break)
  (begin
    (set-sim-time! (+ (sim-time simu) break))
    (sim-stack simu);dopracować
    )

  
  ;zdejmujemy ze stacka i wykonujemy te akcje które miały czas się wykonać
  )

(define (wire-on-change! w action)
  (set-wire-actions! w (cons (wire-actions w) action) )
  )

(define (xor-gate a b c)
  (define d (make-wire (wire-sim a)))
  (define e (make-wire (wire-sim a)))
  (define f (make-wire (wire-sim a)))
  (nand-gate a b d)
  (nand-gate a d e)
  (nand-gate b d f)
  (nand-gate e f c))
  
(define (nand-gate a b c)
  (define (and-action)
    (wire-set! c (not (and (wire-value a) (wire-value b)))))
  (add-action! a and-action)
  (add-action! b and-action))

(define (compile w)
  (begin (car w) (compile (cdr w)) )
  )
;---------------------------------------------------------------------------------------------------------

(provide sim? wire?
         (contract-out
          [make-sim        (-> sim?)];
          [sim-wait!       (-> sim? positive? void?)];*
          [sim-time        (-> sim? real?)];
          [sim-add-action! (-> sim? positive? (-> any/c) void?)];*

          [make-wire       (-> sim? wire?)];
          [wire-on-change! (-> wire? (-> any/c) void?)];dodawanie do kabla akcji (good?)
          [wire-value      (-> wire? boolean?)];
          [wire-set!       (-> wire? boolean? void?)];

          [bus-value (-> (listof wire?) natural?)];
          [bus-set!  (-> (listof wire?) natural? void?)];

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

          [flip-flop (-> wire? wire? wire? void?)]));

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
;;gates
(define (gate-not wi wo)
  (sim-add-action! (wire-sim wi)
                   (+ 1 (sim-time (wire-sim wi)))
                   (wire-set! wo (not (wire-value wi)))
    ));we wszystkich trzeba dodać chodzenie po dzieciach

(define (gate-and wi1 wi2 wo)
  (sim-add-action! (wire-sim wi1)
                   (+ 1 (sim-time (wire-sim wi1)))
                   (wire-set! wo (and (wire-value wi1) (wire-value wi2)))
  ))

(define (gate-or wi1 wi2 wo)
  (sim-add-action! (wire-sim wi1)
                   (+ 1 (sim-time (wire-sim wi1)))
                   (wire-set! wo (or (wire-value wi1) (wire-value wi2)))
  ))

(define (gate-nand wi1 wi2 wo)
  (sim-add-action! (wire-sim wi1)
                   (+ 1 (sim-time (wire-sim wi1)))
                   (wire-set! wo (not (and (wire-value wi1) (wire-value wi2))))
  ))

(define (gate-nor wi1 wi2 wo)
  (sim-add-action! (wire-sim wi1)
                   (+ 1 (sim-time (wire-sim wi1)))
                   (wire-set! wo (not (or (wire-value wi1) (wire-value wi2))))
  ))

(define (gate-xor wi1 wi2 wo)
  (sim-add-action! (wire-sim wi1)
                   (+ 2 (sim-time (wire-sim wi1)))
                   (wire-set! wo (xor (wire-value wi1) (wire-value wi2)))
  ))
;;wires---------------------------------------------------------------------
(define ( wire-not wi )
    (wire-on-change! wi gate-not)
    (make-wire (wire-sim wi))
    )

(define (wire-and w1 w2)
    (wire-on-change! w1 gate-and)
    (wire-on-change! w2 gate-and)
    (make-wire (wire-sim w1))
    )

(define (wire-nand w1 w2)
    (wire-on-change! w1 gate-nand)
    (wire-on-change! w2 gate-nand)
    (make-wire (wire-sim w1))
    )

(define (wire-or w1 w2)
    (wire-on-change! w1 gate-or)
    (wire-on-change! w2 gate-or)
    (make-wire (wire-sim w1))
    )

(define (wire-nor w1 w2)
    (wire-on-change! w1 gate-nor)
    (wire-on-change! w2 gate-nor)
    (make-wire (wire-sim w1))
    )

(define (wire-xor w1 w2)
    (wire-on-change! w1 gate-xor)
    (wire-on-change! w2 gate-xor)
    (make-wire (wire-sim w1))
    )
;;actions--------------------------------------------------------------------
(define (not-action w o)
  (wire-set! o (not (wire-value w)))
  )

(define (and-action w1 w2 o)
  (wire-set! o (and (wire-value w1) (wire-value w2)))
  )

(define (nand-action w1 w2 o)
  (wire-set! o (not (and (wire-value w1) (wire-value w2))))
  )

(define (or-action w1 w2 o)
  (wire-set! o (or (wire-value w1) (wire-value w2)))
  )

(define (nor-action w1 w2 o)
  (wire-set! o (not (or (wire-value w1) (wire-value w2))))
  )

(define (xor-action w1 w2 o)
  (wire-set! o (xor (wire-value w1) (wire-value w2)))
  )