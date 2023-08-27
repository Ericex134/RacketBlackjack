;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname BlackJack) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

;; BLACKJACK

(@htdw Blackjack) ;(give WS a better name)

;; =================
;; Constants:

(define CARDS (list "A" "A" "A" "A"
                    2   2   2   2
                    3   3   3   3
                    4   4   4   4
                    5   5   5   5
                    6   6   6   6
                    7   7   7   7
                    8   8   8   8
                    9   9   9   9
                    10  10  10  10
                    "J" "J" "J" "J"
                    "Q" "Q" "Q" "Q"
                    "K" "K" "K" "K"))

(define HEIGHT 600)
(define WIDTH 500)
(define TEXT-SIZE 24)
(define TEXT-COLOR "black")
(define CARD-HEIGHT 90)
(define CARD-WIDTH 64)
(define CARD-COLOR "white")
(define GAP-WIDTH 10)
(define GAP-COLOR "transparent")
(define INSTRUCTIONS-HIT (text "H to HIT" 32 "light gray"))
(define INSTRUCTIONS-STAND (text "S to STAND" 32 "light gray"))

(define BGS (empty-scene WIDTH HEIGHT "dark seagreen"))

(define MTS (overlay/align "middle" "middle"
                           (above INSTRUCTIONS-HIT INSTRUCTIONS-STAND)
                           (empty-scene WIDTH HEIGHT "seagreen")))

;; END SCREENS
(define WIN-SCREEN
  (text "YOU WIN" 70 "mint cream"))
(define LOSE-SCREEN 
  (text "YOU LOSE" 70 "crimson"))

(define BLACKJACK-SCREEN-WIN
  (overlay/align "middle" "middle"
                 (above (text "BLACKJACK" 50 "crimson")
                        (text "YOU WIN" 50 "mint cream"))
                 (empty-scene WIDTH HEIGHT "seagreen"))) 

(define BLACKJACK-SCREEN-LOSE
  (overlay/align "middle" "middle"
                 (above (text "DEALER BLACKJACK" 40 "crimson")
                        (text "YOU LOSE" 40 "mint cream"))
                 (empty-scene WIDTH HEIGHT "seagreen")))

(define BUST-SCREEN-LOSE
  (above (text "BUST" 70 "crimson")
         (text "YOU LOSE" 70 "mint cream")))
                
 
(define BUST-SCREEN-WIN
  (above (text "DEALER BUST" 60 "crimson")
         (text "YOU WIN" 70 "mint cream")))
(define PUSH-SCREEN
  (text "PUSH" 70 "mint cream")) 
                 


;; =================
;; Data definitions:

(@htdd Blackjack)
(define-struct bj (p-cards d-cards state))
;; Blackjack is (make-bj (listof Card) (listof Card) State)
;; Boolean is whether or not it is on dealer turn

(@htdd State)

;; State is one of:
;; - Boolean (true=player turn)
;; - "p-blackjack"
;; - "d-blackjack"
;; - "p-win"
;; - "d-win"
;; - "p-bust"
;; - "d-bust"
;; - "push"

(@htdd Card)
;; Card is one of:
;; - Natural [2, 10]
;; - "J" "Q" "K" "A"





;; =================
;; Functions: 

(@htdf main)
(@signature Blackjack -> Blackjack)
;; start the world with 
;; (main (make-bj (list (gen-card (random 52)) (gen-card (random 52))) (list (gen-card (random 52)) (gen-card (random 52))) true))

(@template-origin htdw-main)


(define (main ws) 
  (big-bang ws                   ; Blackjack
    (on-tick   tock 1.2)     ; Blackjack -> Blackjack 
    (to-draw   render)   ; Blackjack -> Image
    ;;    (stop-when ...)      ; Blackjack -> Boolean
    ;;  (on-mouse  ...)      ; Blackjack Integer Integer MouseEvent ->
    ;;                             Blackjack
    (on-key    handle-key)))   ; Blackjack KeyEvent -> Blackjack 


(@htdf handle-key) 
(@signature Blackjack KeyEvent -> Blackjack)

(check-expect (handle-key (make-bj (list 3 4) (list 5 6) false) "h")
              (make-bj (list 3 4) (list 5 6) false))
(check-expect (handle-key (make-bj (list 6 4) (list 5 8) false) "s")
              (make-bj (list 6 4) (list 5 8) false))
(check-expect (handle-key (make-bj (list "A" 4) (list "K" 6) false) " ")
              (make-bj (list "A" 4) (list "K" 6) false))
(check-expect (handle-key (make-bj (list "A" 4) (list "K" 6) true) " ")
              (make-bj (list "A" 4) (list "K" 6) true))

(check-expect (handle-key (make-bj (list 3 2) (list 5 6) true) "s")
              (make-bj (list 3 2) (list 5 6) false))

(check-random (handle-key (make-bj (list 4 2) (list 5 7) true) "h")
              (make-bj (append (list 4 2) (list (gen-card (random 52))))
                       (list 5 7) true))

(check-random (handle-key (make-bj (list 4 2) (list 5 7 7) "d-win") " ")
              (make-bj (list (gen-card (random 52)) (gen-card (random 52)))
                       (list (gen-card (random 52)) (gen-card (random 52)))
                       true))
  
(define (handle-key bj ke)
  (cond [(and (key=? ke "h") (bj-state bj))
         (make-bj (append (bj-p-cards bj) (list (gen-card (random 52))))
                  (bj-d-cards bj) true)] 
        [(and (key=? ke "s") (bj-state bj)) 
         (make-bj (bj-p-cards bj) (bj-d-cards bj) false)]
        [(and (string? (bj-state bj)) (key=? ke " "))  
         (make-bj (list (gen-card (random 52)) (gen-card (random 52)))
                  (list (gen-card (random 52)) (gen-card (random 52)))
                  true)]
        [else  
         bj])) 


(@htdf tock)
(@signature Blackjack -> Blackjack)

(check-expect (tock (make-bj (list 3 4 5) (list 9 8) true)) 
              (make-bj (list 3 4 5) (list 9 8) true))
(check-expect (tock (make-bj (list 3 "K") (list "A" 8) true))
              (make-bj (list 3 "K") (list "A" 8) true))

;(check-expect (tock (make-bj (list 8 5 7) (list 2 5) false))
;              (make-bj (list 8 5 7) (list 2 5 6) false))
;(check-expect (tock (make-bj (list 8 5 7) (list 2 5 6) false))
;              (make-bj (list 8 5 7) (list 2 5 6 8) false))
(check-expect (tock (make-bj (list 8 5 7) (list 2 5 6 8) false))
              (make-bj (list 8 5 7) (list 2 5 6 8) "d-win"))

;(check-expect (tock (make-bj (list 8 5 7) (list "J" 5) false)) 
;              (make-bj (list 8 5 7) (list "J" 5 5) false))
(check-expect (tock (make-bj (list 8 5 7) (list "J" 5 5) false))
              (make-bj (list 8 5 7) (list "J" 5 5) "push"))


(check-expect (tock (make-bj (list 3 "K") (list "A" 8) false))
              (make-bj (list 3 "K") (list "A" 8) "d-win"))
(check-expect (tock (make-bj (list 8 5 7) (list "J" 5 6) false))
              (make-bj (list 8 5 7) (list "J" 5 6) "d-win"))

(check-expect (tock (make-bj (list "Q" "K") (list "A" 8) false))
              (make-bj (list "Q" "K") (list "A" 8) "p-win"))
(check-expect (tock (make-bj (list 8 5 7) (list "A" 8) false))
              (make-bj (list 8 5 7) (list "A" 8) "p-win"))

(check-expect (tock (make-bj (list 8 5 9) (list "A" 8) true))
              (make-bj (list 8 5 9) (list "A" 8) "p-bust"))

(check-expect (tock (make-bj (list 8 5 7) (list "J" 8) false)) 
              (make-bj (list 8 5 7) (list "J" 8) "p-win"))
(check-expect (tock (make-bj (list 8 5 7) (list "J" 5 10) false))
              (make-bj (list 8 5 7) (list "J" 5 10) "d-bust")) 

 
(define (tock bj)
  (cond [(> (calculate (bj-p-cards bj)) 21)
         (make-bj (bj-p-cards bj) (bj-d-cards bj) "p-bust")]
        [(false? (bj-state bj))
         (cond [(> (calculate (bj-d-cards bj)) 21)
                (make-bj (bj-p-cards bj) (bj-d-cards bj) "d-bust")]
               [(<= 17 (calculate (bj-d-cards bj)) 21)
                (determine-winner bj)]
               [else
                (make-bj (bj-p-cards bj)
                         (append (bj-d-cards bj) (list (gen-card (random 52))))
                         false)])]
        [else bj]))

(@htdf determine-winner)
(@signature Blackjack -> Blackjack)


(check-expect (determine-winner (make-bj (list "J" "K") (list 5 5 7) false))
              (make-bj (list "J" "K") (list 5 5 7) "p-win"))

(check-expect (determine-winner (make-bj (list 6 "K") (list 5 6 7) false))
              (make-bj (list 6 "K") (list 5 6 7) "d-win"))

(check-expect (determine-winner (make-bj (list 6 4 9) (list "J" 9) false)) 
              (make-bj (list 6 4 9) (list "J" 9) "push"))

(define (determine-winner bj)
  (cond [(> (calculate (bj-p-cards bj)) (calculate (bj-d-cards bj)))
         (make-bj (bj-p-cards bj) (bj-d-cards bj) "p-win")]
        [(< (calculate (bj-p-cards bj)) (calculate (bj-d-cards bj)))
         (make-bj (bj-p-cards bj) (bj-d-cards bj) "d-win")]
        [else
         (make-bj (bj-p-cards bj) (bj-d-cards bj) "push")]))

(@htdf render)
(@signature Blackjack -> Image) 


;(check-expect (render (make-bj (list "J" 10) (list 3 9) false))
;              (overlay/align "middle" "middle" 
;                             (above (render-cards (list "3" 9))
;                                    (rectangle WIDTH
;                                               (- HEIGHT (* 2 CARD-HEIGHT) 20)
;                                               "solid" "transparent")
;                                    (render-cards (list "J" 10))) 
;                             MTS))
;
;(check-expect (render (make-bj (list "J" 10) (list 3 9 8) false)) 
;              (overlay/align "middle" "middle"
;                             (above (render-cards (list 3 9 8))
;                                    (rectangle WIDTH
;                                               (- HEIGHT (* 2 CARD-HEIGHT) 20)
;                                               "solid" "transparent")
;                                    (render-cards (list "J" 10))) 
;                             MTS)) 
;
;(check-expect (render (make-bj (list "J" 10 8) (list 3 9) true))
;              (overlay/align "middle" "middle"
;                             (above (render-cards (list "3" "?"))
;                                    (rectangle WIDTH
;                                               (- HEIGHT (* 2 CARD-HEIGHT) 20)
;                                               "solid" "transparent")
;                                    (render-cards (list "J" 10 8))) 
;                             MTS))
;
;(check-expect (render (make-bj (list "J" 10) (list 3 9 6) "p-win"))
;              (overlay/align "middle" "middle"
;                             WIN-SCREEN
;                             (above (render-cards (list 3 9 6))
;                                    (rectangle WIDTH
;                                               (- HEIGHT (* 2 CARD-HEIGHT) 20)
;                                               "solid" "transparent")
;                                    (render-cards (list "J" 10))) 
;                             BGS))
;(check-expect (render (make-bj (list "J" 6) (list 3 9 6) "d-win"))
;              (overlay/align "middle" "middle"
;                             LOSE-SCREEN
;                             (above (render-cards (list 3 9 6))
;                                    (rectangle WIDTH
;                                               (- HEIGHT (* 2 CARD-HEIGHT) 20)
;                                               "solid" "transparent")
;                                    (render-cards (list "J" 6))) 
;                             BGS))
;
;(check-expect (render (make-bj (list "J" 6) (list 3 9 "J") "d-bust"))
;              (overlay/align "middle" "middle"
;                             BUST-SCREEN-WIN
;                             (above (render-cards (list 3 9 "J"))
;                                    (rectangle WIDTH
;                                               (- HEIGHT (* 2 CARD-HEIGHT) 20)
;                                               "solid" "transparent")
;                                    (render-cards (list "J" 6))) 
;                             BGS))
;
;(check-expect (render (make-bj (list "J" 6 8) (list 3 9) "p-bust"))
;              (overlay/align "middle" "middle"
;                             BUST-SCREEN-LOSE
;                             (above (render-cards (list 3 9))
;                                    (rectangle WIDTH
;                                               (- HEIGHT (* 2 CARD-HEIGHT) 20)
;                                               "solid" "transparent")
;                                    (render-cards (list "J" 6 8))) 
;                             BGS))
;
;(check-expect (render (make-bj (list "J" 8) (list 3 9 6) "push"))
;              (overlay/align "middle" "middle"
;                             PUSH-SCREEN
;                             (above (render-cards (list 3 9 6))
;                                    (rectangle WIDTH
;                                               (- HEIGHT (* 2 CARD-HEIGHT) 20)
;                                               "solid" "transparent")
;                                    (render-cards (list "J" 8))) 
;                             BGS))


 
;(define (render bj) empty-image)  

(define (render bj) 
  (cond [(false? (bj-state bj))
         (overlay/align "middle" "middle"
                        (above (render-cards (bj-d-cards bj))
                               (rectangle WIDTH GAP-WIDTH "solid" GAP-COLOR)
                               (text (number->string (calculate (bj-d-cards bj)))
                                     36 "light gray")
                               (rectangle WIDTH
                                          (- HEIGHT (* 2 CARD-HEIGHT) 100)
                                          "solid" "transparent")
                               (text (number->string (calculate (bj-p-cards bj)))
                                     36 "light gray")
                               (rectangle WIDTH GAP-WIDTH "solid" GAP-COLOR)
                               (render-cards (bj-p-cards bj)))
                        MTS)]
        [(and (string? (bj-state bj)) (string=? "p-win" (bj-state bj)))   
         (overlay/align "middle" "middle"
                        WIN-SCREEN
                        (above (render-cards (bj-d-cards bj))
                               (rectangle WIDTH GAP-WIDTH "solid" GAP-COLOR)
                               (text (number->string (calculate (bj-d-cards bj)))
                                     36 "light gray")
                               (rectangle WIDTH
                                          (- HEIGHT (* 2 CARD-HEIGHT) 100)
                                          "solid" "transparent")
                               (text (number->string (calculate (bj-p-cards bj)))
                                     36 "light gray")
                               (rectangle WIDTH GAP-WIDTH "solid" GAP-COLOR)
                               (render-cards (bj-p-cards bj)))
                        BGS)]
        [(and (string? (bj-state bj)) (string=? "d-win" (bj-state bj)))
         (overlay/align "middle" "middle"
                        LOSE-SCREEN
                        (above (render-cards (bj-d-cards bj))
                               (rectangle WIDTH GAP-WIDTH "solid" GAP-COLOR)
                               (text (number->string (calculate (bj-d-cards bj)))
                                     36 "light gray")
                               (rectangle WIDTH
                                          (- HEIGHT (* 2 CARD-HEIGHT) 100)
                                          "solid" "transparent")
                               (text (number->string (calculate (bj-p-cards bj)))
                                     36 "light gray")
                               (rectangle WIDTH GAP-WIDTH "solid" GAP-COLOR)
                               (render-cards (bj-p-cards bj)))
                        BGS)]
        [(and (string? (bj-state bj)) (string=? "p-bust" (bj-state bj)))
         (overlay/align "middle" "middle"
                        BUST-SCREEN-LOSE
                        (above (render-cards (bj-d-cards bj))
                               (rectangle WIDTH GAP-WIDTH "solid" GAP-COLOR)
                               (text (number->string (calculate (bj-d-cards bj)))
                                     36 "light gray")
                               (rectangle WIDTH
                                          (- HEIGHT (* 2 CARD-HEIGHT) 100)
                                          "solid" "transparent")
                               (text (number->string (calculate (bj-p-cards bj)))
                                     36 "light gray")
                               (rectangle WIDTH GAP-WIDTH "solid" GAP-COLOR)
                               (render-cards (bj-p-cards bj)))
                        BGS)]
        [(and (string? (bj-state bj)) (string=? "d-bust" (bj-state bj)))
         (overlay/align "middle" "middle"
                        BUST-SCREEN-WIN
                        (above (render-cards (bj-d-cards bj))
                               (rectangle WIDTH GAP-WIDTH "solid" GAP-COLOR)
                               (text (number->string (calculate (bj-d-cards bj)))
                                     36 "light gray")
                               (rectangle WIDTH
                                          (- HEIGHT (* 2 CARD-HEIGHT) 100)
                                          "solid" "transparent")
                               (text (number->string (calculate (bj-p-cards bj)))
                                     36 "light gray")
                               (rectangle WIDTH GAP-WIDTH "solid" GAP-COLOR)
                               (render-cards (bj-p-cards bj)))
                        BGS)]
        [(and (string? (bj-state bj)) (string=? "push" (bj-state bj)))
         (overlay/align "middle" "middle"
                        PUSH-SCREEN
                        (above (render-cards (bj-d-cards bj))
                               (rectangle WIDTH GAP-WIDTH "solid" GAP-COLOR)
                               (text (number->string (calculate (bj-d-cards bj)))
                                     36 "light gray")
                               (rectangle WIDTH
                                          (- HEIGHT (* 2 CARD-HEIGHT) 100)
                                          "solid" "transparent")
                               (text (number->string (calculate (bj-p-cards bj)))
                                     36 "light gray")
                               (rectangle WIDTH GAP-WIDTH "solid" GAP-COLOR)
                               (render-cards (bj-p-cards bj)))
                        BGS)]
         
        [else
         (overlay/align "middle" "middle"
                        (above (render-cards (list (first (bj-d-cards bj)) "?"))
                               (rectangle WIDTH GAP-WIDTH "solid" GAP-COLOR) 
                               (text "?"
                                     36 "light gray")
                               (rectangle WIDTH
                                          (- HEIGHT (* 2 CARD-HEIGHT) 100)
                                          "solid" "transparent")
                               (text (number->string (calculate (bj-p-cards bj)))
                                     36 "light gray")
                               (rectangle WIDTH GAP-WIDTH "solid" GAP-COLOR)
                               (render-cards (bj-p-cards bj)))
                        MTS)])) 


(@htdf render-cards)
(@signature (listof Card) -> Image)

(check-expect (render-cards (list 10 "K"))
              (beside (overlay (text "10" TEXT-SIZE TEXT-COLOR) 
                               (rectangle CARD-WIDTH CARD-HEIGHT
                                          "solid" CARD-COLOR))
                      (rectangle GAP-WIDTH CARD-HEIGHT "solid" GAP-COLOR)
                      (overlay (text "K" TEXT-SIZE TEXT-COLOR)
                               (rectangle CARD-WIDTH CARD-HEIGHT
                                          "solid" CARD-COLOR))
                      (rectangle GAP-WIDTH CARD-HEIGHT "solid" GAP-COLOR))) 
(check-expect (render-cards (list 9 "J" 7))
              (beside (overlay (text "9" TEXT-SIZE TEXT-COLOR)
                               (rectangle CARD-WIDTH CARD-HEIGHT
                                          "solid" CARD-COLOR))
                      (rectangle GAP-WIDTH CARD-HEIGHT "solid" GAP-COLOR)
                      (overlay (text "J" TEXT-SIZE TEXT-COLOR)
                               (rectangle CARD-WIDTH CARD-HEIGHT
                                          "solid" CARD-COLOR))
                      (rectangle GAP-WIDTH CARD-HEIGHT "solid" GAP-COLOR)
                      (overlay (text "7" TEXT-SIZE TEXT-COLOR)
                               (rectangle CARD-WIDTH CARD-HEIGHT
                                          "solid" CARD-COLOR))
                      (rectangle GAP-WIDTH CARD-HEIGHT "solid" GAP-COLOR)))  

;(define (render-cards loc) empty-image)

(define (render-cards loc)
  (local [(define (cd c)
            (beside
             (overlay (text c TEXT-SIZE TEXT-COLOR)
                      (rectangle CARD-WIDTH CARD-HEIGHT "solid" CARD-COLOR))
             (rectangle GAP-WIDTH CARD-HEIGHT "solid" GAP-COLOR)))]
    (foldr beside empty-image (map cd (cards-string loc))))) 



(@htdf cards-string) 
(@signature (listof Card) -> (listof String))

(check-expect (cards-string (list "J" 10 9))
              (list "J" "10" "9"))
(check-expect (cards-string (list "J" "K" "A"))
              (list "J" "K" "A"))
(check-expect (cards-string (list "2" 10 9))
              (list "2" "10" "9"))

(define (cards-string loc)
  (cond [(empty? loc) empty]
        [else
         (cons (card-string (first loc))
               (cards-string (rest loc)))]))

(define (card-string c)
  (cond [(string? c) c] 
        [else
         (number->string c)]))

(@htdf calculate)
(@signature (listof Card) -> Natural)

(check-expect (calculate (list "J" "J" 4)) 24)
(check-expect (calculate (list "A" "K")) 21) 
(check-expect (calculate (list "A" 4)) 15)
(check-expect (calculate (list 3 4)) 7) 
(check-expect (calculate (list "A" "A" 9)) 21)
(check-expect (calculate (list 9 "A" "A")) 21)   

(define (calculate loc)
  (local [(define (calc loc rsf)
            (cond [(empty? loc) rsf]
                  [(number? (first loc))
                   (calc (rest loc) (+ rsf (first loc)))]
                  [(string=? (first loc) "A")
                   (if (<= rsf 10)
                       (calc (rest loc) (+ rsf 11))
                       (calc (rest loc) (+ rsf 1)))]
                  [else
                   (calc (rest loc) (+ rsf 10))]))]
    (calc loc 0)))

(define (gen-card n)
  (cond [(<= 0 n 3) "A"]
        [(<= 4 n 7) 2]
        [(<= 8 n 11) 3]
        [(<= 12 n 15) 4]
        [(<= 16 n 19) 5]
        [(<= 20 n 23) 6]
        [(<= 24 n 27) 7]
        [(<= 28 n 31) 8]
        [(<= 32 n 35) 9]
        [(<= 36 n 39) 10]
        [(<= 40 n 43) "J"]
        [(<= 44 n 47) "Q"]
        [(<= 48 n 51) "K"]))
          
  
