;;;; poker.lisp

(require 'cl)

(defconst *ranks*                '(2 3 4 5 6 7 8 9 10 J Q K A))
(defconst *suits*                '(♣ ♦ ♥ ♠))
(defconst *best-hands*
  '(
    royal-flush
    straight-flush
    four-of-a-kind
    full-house
    flush
    straight
    three-of-a-kind
    two-pair
    one-pair
    ))


(defconst *flush-hand*           '((2 h) (5 h) (q h) (k h) (8 h)))
(defconst *straight-hand-1*      '((5 c) (6 h) (4 d) (2 h) (3 h)))
(defconst *straight-hand-2*      '((10 c) (9 h) (k d) (j h) (q h)))
(defconst *straight-flush-hand*  '((j h) (k h) (10 h) (9 h) (q h)))
(defconst *royal-flush-hand*     '((j h) (k h) (10 h) (a h) (q h)))
(defconst *two-pair-hand*        '((j h) (k h) (j c) (k c) (2 h)))
(defconst *three-of-a-kind-hand* '((j h) (k h) (9 c) (k c) (k h)))
(defconst *four-of-a-kind-hand*  '((j h) (k h) (k d) (k c) (k s)))
(defconst *full-house-hand*      '((j h) (k h) (j c) (k c) (k s)))
(defconst *debug*                nil)

(defvar   *deck*                 '())
(defvar   *hand*                 '()) ;; community card
(defvar   *hand1*                '()) ;; my card
(defvar   *hand2*                '()) ;; com card
(defvar   *discard-list*         '())
(defvar   *state*                0)
(defvar   *win*                  0)  ;; 0: lose 1: win
 
;; Set chips nums
(defconst   *com-chips*               100)
(defconst   *My-chips*                100)


(defun rank-of-card (card)
  (first card))

(defun rank-value (rank)
  (position rank *ranks*))

(defun rank-value-of-card (card)
  (rank-value (rank-of-card card)))

(defun max-rank (rank1 rank2)
  (elt *ranks* (max (rank-value rank1)(rank-value rank2))))

(defun card-< (card1 card2)
    (<= (rank-value-of-card card1) (rank-value-of-card card2)))

(defun max-card-2 (card1 card2)
  (cond
    ((>= (rank-value-of-card card1) (rank-value-of-card card2)) card1)
    (t card2)))

(defun max-card-in-hand (hand)
  (cond
    ((= 1 (length hand)) (car hand))
    (t (max-card-2 (car hand) (max-card-in-hand (cdr hand))))))

(defun is-straight (hand)
  (cond
    ((null hand) (error 'null-hand))
    ((= 1 (length hand)) t)
    (t
     (let* ((hi-card-1 (max-card-in-hand hand))
	    (hand-without-hi-card-1 (remove hi-card-1 hand))
	    (hi-card-2 (max-card-in-hand hand-without-hi-card-1)))
       (and (= 1 (- (rank-value-of-card hi-card-1) (rank-value-of-card hi-card-2)))
	    (is-straight hand-without-hi-card-1))))))
     
(defun suit (card) 
  (second card))

(defun eq-suit (card1 card2)
  (let ((ret (eq (suit card1) (suit card2))))
    (when *debug* (format "(eq-suit %s %s) => %s\n" card1 card2 ret))
    ret))

(defun is-flush (hand)
  (cond
    ((null hand) (error 'null-hand))
    ((= 1 (length hand)) t)
    (t (and (eq-suit (cl-first hand) (second hand))
	    (is-flush (cdr hand))))))

(defun is-straight-flush (hand)
  (and (is-straight hand) (is-flush hand)))

(defun is-royal-flush (hand)
  (and (is-straight-flush hand) (equal 'A (rank-of-card (max-card-in-hand hand)))))

(defun get-best-hand (hand)
  (dolist (x *best-hands*)
    (let* ((pred-func    (intern-soft (concat "is-" (symbol-name x)))))
      (when (funcall pred-func hand) (cl-return x)))))

(defun count-ranks (hand)
  (let* ((ranks (mapcar 'rank-of-card hand))
	 (ranks-no-dups (remove-duplicates ranks)))
    (mapcar
     (lambda (rank) (list rank (count rank ranks)))
     ranks-no-dups)))

(defun num-pairs (hand)
  (count 2 (count-ranks hand) :key 'second))

(defun is-two-pair (hand)
  (= 2 (num-pairs hand)))

(defun is-one-pair (hand)
  (= 1 (num-pairs hand)))

(defun is-three-of-a-kind (hand)
  (when (find 3 (count-ranks hand) :key 'second) t))
  
(defun is-four-of-a-kind (hand)
  (when (find 4 (count-ranks hand) :key 'second) t))

(defun is-full-house (hand)
  (and (is-three-of-a-kind hand) (is-one-pair hand)))

(defun card (rank suit)
  (list rank suit))

(defun random-rank ()
  (elt *ranks* (random (length *ranks*))))

(defun random-suit ()
  (elt *suits* (random (length *suits*))))

(defun random-card ()
  (card (random-rank) (random-suit)))
 
(defun full-deck ()
  (let ((ret '()))
    (dolist (suit *suits*)
      (dolist (rank *ranks*)
	(setq ret (cons (card rank suit) ret))))
    ret))

(length *deck*)

(defun random-deck ()
  (interactive)
  (let* ((ret             '())
 	 (random-card     '())
	 (deck            (full-deck))
	 (deck-len        (length deck)))
    (dotimes (i deck-len)
      (setf deck-len (length deck))
      (setf random-card-num (random deck-len))
      (setf random-card (elt deck (random deck-len)))
      (setf deck (remove random-card deck))
      (setf ret (cons random-card ret)))
    ret))

(defun new-deck! ()
  (setf *deck* (random-deck)))

(defun num-cards-left-in-deck (deck)
  (length deck))

(defun take-from-deck! (num)
  (let ((ret '()))
    (dotimes (i num)
      (setf ret (push (pop *deck*) ret)))
    (setf ret (nreverse ret))))

(defun deal-me! (num)
  (when (< (num-cards-left-in-deck *deck*) 9)
    (new-deck!))
  (setf *hand* (append *hand* (take-from-deck! num)))
  (setf *hand1* (append *hand1* (take-from-deck! (/ num 2.5))))
  (setf *hand2* (append *hand2* (take-from-deck! (/ num 2.5))))
  )

(defun discard! (list-of-nums)
  (when *debug* (insert (format "list-of-nums = %s\n" list-of-nums)))
  (let* ((list-of-cards  (mapcar (lambda (x) (elt *hand* x)) list-of-nums)))
    (when *debug* (insert (format "list-of-cards = %s\n" list-of-cards)))
    (dolist (card list-of-cards)
      (when *debug* (insert (format "Discarding: %s\n" card)))
      (setf *hand* (remove card *hand*))))
  *hand*)

(defun goto-pos (x y)
  ; (insert (format "goto %d %d\n" x y))
  (goto-line y)
  (beginning-of-line)
  (forward-char x))

;; ------------------
;; display card
(defun display-card-old (card)
  (interactive)
  (save-excursion
    (insert "+---------+")
    (forward-line)
    (backward-char 11)
    (insert (format "| %-2s    %1s |" (rank-of-card card) (suit card)))
    (forward-line)
    (backward-char 11)
    (dotimes (i 3)
      (insert "|         |")
      (forward-line)
      (backward-char 11))
    (insert (format "| %1s    %-2s |" (suit card) (rank-of-card card)))
    (forward-line)
    (backward-char 11)
    (insert "+---------+")))

(defun display-card (i hand)       ; i is 0-based index
  (let ((card (elt hand i)))
    (goto-pos (* i 13) *card-index*) (delete-char 11) (insert "+---------+")
    (goto-pos (* i 13) (+ 1 *card-index*)) (delete-char 11) (insert (format "| %-2s    %1s |" (rank-of-card card) (suit card)))
    (dotimes (y 3)
      (goto-pos (* i 13) (+ y (+ 2 *card-index*))) (delete-char 11) (insert "|         |"))
    (goto-pos (* i 13) (+ 5 *card-index*)) (delete-char 11)  (insert (format "| %1s    %2s |" (suit card) (rank-of-card card)))
    (goto-pos (* i 13) (+ 6 *card-index*)) (delete-char 11) (insert "+---------+"))
  )

 (defun display-card1 (i hand)       ; i is 0-based index
   (let ((card (elt hand i)))
    (goto-pos (* i 13) 21) (delete-char 11) (insert "+---------+")
    (goto-pos (* i 13) 22) (delete-char 11)  (insert "|xxxxxxxxx|")
    (dotimes (y 3)
      (goto-pos (* i 13) (+ y 23)) (delete-char 11) (insert "|xxxxxxxxx|"))
    (goto-pos (* i 13) 26) (delete-char 11) (insert "|xxxxxxxxx|")
    (goto-pos (* i 13) 27) (delete-char 11) (insert "+---------+")))



(defun mark-for-discard (i)   ; i is 0-based index
  (goto-pos (* i 13) 4) (delete-char 11) (insert "+---------+")
  (dotimes (y 5)
    (goto-pos (* i 13) (+ y 5)) (delete-char 11) (insert "|xxxxxxxxx|")))

(defun toggle-card-display (i)  ; i is 0-based index
  (interactive)
  (let ((discarded (find i *discard-list*)))
    (cond
     (discarded     (display-card-old i))
     (t             (mark-for-discard i)))
    (setf *discard-list* (if discarded (remove i *discard-list*) (cons i *discard-list*)))))

(defconst poker-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "1"           '(lambda () (interactive) (toggle-card-display 0)))
    (define-key map "2"           '(lambda () (interactive) (toggle-card-display 1)))
    (define-key map "3"           '(lambda () (interactive) (toggle-card-display 2)))
    (define-key map "4"           '(lambda () (interactive) (toggle-card-display 3)))
    (define-key map "5"           '(lambda () (interactive) (toggle-card-display 4)))
    (define-key map (kbd "RET")   '(lambda () (interactive) (discard-and-deal-new)))
    map))

(defconst poker-mode-map-new
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "c"           '(lambda () (interactive)  (when (= *state* 1)(call)))) ;; call
    (define-key map "f"           '(lambda () (interactive)  (when (= *state* 1)(fold)))) ;; fold
    (define-key map (kbd "RET")   '(lambda () (interactive)  (when (= *state* 2)(discard-and-deal-new))))
    map))

(defun fold()
  (when (= *state* 1)
    (setq *state* 2))
  (discard-and-deal-new)
  )

(defun call ()
  (interactive)
  (when (= *state* 1)
    (setq *My-chips* (- *My-chips* 1))
    (setq *com-chips* (- *com-chips* 1))
    )
  (discard-and-deal-new)
  )

    
(defun poker-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map poker-mode-map-new) ;; change map
  (setq major-mode 'poker-mode)
  (setq mode-name "Poker"))

(defun display-hand (hand)
  (interactive)
  (dotimes (r 8)
    (dotimes (c 68)
      (insert " "))
    (insert "\n"))
  ;(goto-pos (* i 13) 4) (delete-char 11) (insert "Community Cards:")
  (dolist (card hand)
    (display-card (cl-position card hand) hand))
  )

(defun display-hand2 (hand)
  (interactive)
  (dotimes (r 8)
    (dotimes (c 68)
      (insert " "))
    (insert "\n"))
  ;(goto-pos (* i 13) 20) (delete-char 11) (insert "Enemy hand : ")
  (dolist (card hand)
     (when (= *state* 1)
       (display-card1 (cl-position card hand) hand))
     (when (= *state* 2)
       (display-card (cl-position card hand) hand))
     )
  )
;; display end 
;; ----------------------------

(defun update-screen()
  (erase-buffer)
  (setq *card-index* 5)
  (setq hand-t (transform-card *hand*))
  (setq hand-value (poker-hand-value hand-t))
  (insert "\n\n\n")
  
  (display-hand *hand*)
  (goto-pos (* i 13) 4) (delete-char 11) (insert "Community Cards:")
  (goto-char (point-max))
  
  (setq *card-index* (+ 8 *card-index*))
  (display-hand *hand1*)
  (goto-pos (* i 13) 12) (delete-char 11) (insert "Your hands: ")
  (goto-char (point-max))

  (setq *card-index* (+ 8 *card-index*))
  (display-hand2 *hand2*)
  (goto-pos (* i 13) 20) (delete-char 11) (insert "Enemy hand : ")
  (goto-char (point-max))
  
  (when (= *state* 1)
    (insert "You have: " (symbol-name (get-best-hand (get-best-7hand *hand* *hand1*))) "\n")
    (insert "Press <c> to call.Press <f> to call.\n\n")
    (insert "You have " (format "%d" *My-chips*) " chips.\n")
    (insert "Enemy have " (format "%d" *com-chips*) " chips.\n"))
    
  (when (= *state* 2)
    (setq *card-index* (+ 8 *card-index*))
    (display-hand2 (get-best-7hand *hand* *hand1*))
    (goto-pos (* i 13) 28) (delete-char 11) (insert "My best hands : ")
    (goto-char (point-max))
    (insert "You have: " (symbol-name (get-best-hand (get-best-7hand *hand* *hand1*))) "\n")
    (insert "Enemy have: " (symbol-name (get-best-hand (get-best-7hand *hand* *hand2*))) "\n")
    (insert "\nPress <Enter> to deal a new hand.\n")
    (who-win)
    (insert "You have " (format "%d" *My-chips*) " chips.   ")
    (insert "Enemy have " (format "%d" *com-chips*) " chips.")
    (if (= *win* 1)

	(insert "
 __| |____________________________________________| |__
(__   ____________________________________________   __)
   | |                                            | |
   | |                                            | |
   | |                   WIN!!                    | |
   | |                                            | |
 __| |  __________________________________________| |__
(__   ____________________________________________   __)
   | |                                            | |\n")
	 
    
      (insert "
 __| |____________________________________________| |__
(__   ____________________________________________   __)
   | |                                            | |
   | |                                            | |
   | |                   LOSE!!                   | |
   | |                                            | |
 __| |  __________________________________________| |__
(__   ____________________________________________   __)
   | |                                            | |\n")
	) ;; if end
    (goto-char (point-min))
    ) ;; when end
  )

(defun who-win ()
  (setq tmp-hand1 (append *hand* *hand1*))
  (setq tmp-hand2 (append *hand* *hand2*))
  
  (setq tmp-hand1 (transform-card tmp-hand1))
  (setq tmp-hand2 (transform-card tmp-hand2))
 
  (setq tmp-hand1 (poker-best-hand tmp-hand1))
  (setq tmp-hand2 (poker-best-hand tmp-hand2))
  (if 
      (poker-hand-> tmp-hand1 tmp-hand2)
      (progn
	(setq *win* 1)
	(if (= *com-chips* 0) (insert "Victory\n"))
	(setq *My-chips* (+ *My-chips* 2))
	)
    (progn
      (setq *win* 0)
      (if (= *My-chips* 0) (insert "Defeat\n"))
      (setq *com-chips* (+ *com-chips* 2))
      )
  )
)

(defun discard-and-deal-new ()
  (when (= *state* 2)
    (setf *discard-list* '(0 1 2 3 4))
    (setf *hand1* '())
    (setf *hand2* '()))
  (new-deck!)
  (when *debug* (insert (format "Discard: %s\n" *discard-list*)))
  (discard! *discard-list*)
  (when *debug* (insert (format "Hand: %s\n" *hand*)))
  (deal-me! (length *discard-list*))  ;;change x cards 
  (when *debug* (insert (format "Hand: %s\n" *hand*)))
  (setf *discard-list* '())
  (setf *state* (if (= *state* 1) 2 1))
  (update-screen)
  )

;; Game start 
(defun poker-play ()
  (interactive)
  (switch-to-buffer "*poker*")
  (poker-mode) ;;  set mode && use-local-map
  (setf *deck* (full-deck))  ;; new ordered deck 
  (new-deck!)  ;; shuffle the deck 
  (setf *hand* '())
  (setf *hand1* '())
  (setf *hand2* '())
  (deal-me! 5 )  ;; deal 5 commmunity card and two players 2 cards
  (setq *state* 1)
  (update-screen)
  )

;; -----------------------------------------------------------------
;; Eval hand1 vs hand2 end

(defsubst poker-card-suit (card)
  "The suit (an integer from 0 to 3) of a poker CARD."
  (cl-check-type card (integer 0 51))
  (/ card 13))

(defsubst poker-card-rank (card)
  "The rank (a integer from 0 to 12) of a poker CARD."
  (cl-check-type card (integer 0 51))
  (% card 13))

(defun poker-best-hand (cards)
  "Find the best hand for a number of CARDS (usually a list of 6 or 7 elements)."
  ;;(print cards) 
  (let ((max 0) (best-hand nil))
    (dolist (hand (poker-possible-hands cards) best-hand)
      (let ((value (poker-hand-value hand)))
	(when (> value max) (setq max value best-hand hand))))))

(defun poker-hand-value (hand)
  "Calculate the value of a given 5 card poker HAND.
The result is a 24 bit integer where the leftmost 4 bits (0-8) indicate the type
of hand, and the remaining nibbles are rank values of decisive cards.
The highest possible value is therefore #x8CBA98 and the lowest is #x053210."
  (let* ((rank-counts (sort (let ((cards hand) result)
			      (while cards
				(let ((rank (poker-card-rank (car cards))))
				  (unless (rassq rank result)
				    (push (cons (let ((count 1))
						  (dolist (card (cdr cards) count)
						    (when (eq (poker-card-rank card)
							      rank)
						      (setq count (1+ count)))))
						rank)
					  result)))
				(setq cards (cdr cards)))
			      result)
			    (lambda (lhs rhs) (or (> (car lhs) (car rhs))
						  (and (= (car lhs) (car rhs))
						       (> (cdr lhs) (cdr rhs)))))))
	 (ranks-length (length rank-counts))
	 (ranks (mapcar #'cdr rank-counts)))
    (setq rank-counts (mapcar #'car rank-counts))
    (logior (cond
	     ((eq ranks-length 4) #x100000)
	     ((eq ranks-length 5)
	      (let ((straight (or (when (and (eq (nth 0 ranks) 12)
					     (eq (nth 1 ranks) 3))
				    (setq ranks '(3 2 1 0 0)))
				  (eq (- (nth 0 ranks) (nth 4 ranks)) 4)))
		    (flush (let ((suit (poker-card-suit (car hand)))
				 (tail (cdr hand)))
			     (while (and tail
					 (eq suit (poker-card-suit (car tail))))
			       (setq tail (cdr tail)))
			     (not tail))))
		(cond ((and straight flush) #x800000)
		      (straight             #x400000)
		      (flush                #x500000)
		      (t                      0))))
	     ((equal rank-counts '(2 2 1)) #x200000)
	     ((equal rank-counts '(3 1 1)) #x300000)
	     ((equal rank-counts '(3 2)) #x600000)
	     ((equal rank-counts '(4 1)) #x700000))
	    (ash (nth 0 ranks) 16)
	    (ash (nth 1 ranks) 12)
	    (if (> ranks-length 2) (ash (nth 2 ranks) 8) 0)
	    (if (> ranks-length 3) (ash (nth 3 ranks) 4) 0)
	    (if (> ranks-length 4) (nth 4 ranks) 0))))

(defun poker-possible-hands (cards)
  "Generate a list of possible 5 card poker hands from CARDS.
CARDS is a list of 5 to 7 poker cards."
  (cl-check-type (length cards) (integer 5 7))
  (cond
   ;; While this could certainly be made generic,
   ;; the performance of this hand-crafted implementation is unmatched.
   ((eq 7 (length cards))
    (let ((car (car cards))
	  (cdr (cdr cards)))
      (let ((cadr (car cdr))
	    (cddr (cdr cdr)))
	(let ((caddr (car cddr))
	      (cdddr (cdr cddr)))
	  (let ((cadddr (car cdddr))
		(cddddr (cdr cdddr)))
	    (let ((caddddr (car cddddr))
		  (cdddddr (cdr cddddr)))
	      (let ((cadddddr (car cdddddr))
		    (cddddddr (cdr cdddddr)))
		(list (list car cadr caddr cadddr caddddr)
		      (list car cadr caddr cadddr cadddddr)
		      (cons car (cons cadr (cons caddr (cons cadddr cddddddr))))
		      (list car cadr caddr caddddr cadddddr)
		      (cons car (cons cadr (cons caddr (cons caddddr cddddddr))))
		      (cons car (cons cadr (cons caddr cdddddr)))
		      (cons car (cons cadr (butlast cdddr)))
		      (cons car (cons cadr (cons cadddr (cons caddddr cddddddr))))
		      (cons car (cons cadr (cons cadddr cdddddr)))
		      (cons car (cons cadr cddddr))
		      (cons car (butlast cddr))
		      (cons car (cons caddr (cons cadddr (cons caddddr cddddddr))))
		      (cons car (cons caddr (cons cadddr cdddddr)))
		      (cons car (cons caddr cddddr))
		      (cons car cdddr)
		      (butlast cdr)
		      (cons cadr (cons caddr (cons cadddr (cons caddddr cddddddr))))
		      (cons cadr (cons caddr (cons cadddr cdddddr)))
		      (cons cadr (cons caddr cddddr))
		      (cons cadr cdddr)
		      cddr))))))))
   (t (poker-combinations 5 cards))))

(defun poker-combinations (n list)
  "A list of all unique ways of taking N different elements from LIST."
  (when list
    (let ((length (length list)))
      (nconc (if (eq n 1)
		 (list (if (cdr list) (list (car list)) list))
	       (if (eq n length)
		   (list list)
		 (mapcar (lambda (rest) (cons (car list) rest))
			 (poker-combinations (1- n) (cdr list)))))
	     (when (> length n) (poker-combinations n (cdr list)))))))

(defun poker-hand-> (hand1 hand2)
  "Return non-nil if HAND1 is better than HAND2."
  (> (poker-hand-value hand1) (poker-hand-value hand2)))

;; Eval hand1 vs hand2 end 

;; Test eval hand1 hand2
;; (setq hand1 '(1 2 3 4 5 6 7))

;; (setq hand2 '(8 9 10 11 12 6 7))

;;(poker-hand-> (poker-best-hand hand1) (poker-best-hand hand2))

;;(poker-hand-value '(1 2 3 4 5))

;;(poker-possible-hands '(1 2 3 4 5 6 7))

;;(poker-best-hand '(1 2 3 4 5 6 7))

;;(poker-card-suit 3)
;; Test end

(defsubst poker-make-card (rank suit)
  "Make a poker card from RANK and SUIT.
RANK is one of `*ranks*' and SUIT is one of `*suits*'."
  (cl-assert (memq rank *ranks*))
  (cl-assert (memq suit *suits*))
  (+ (* (cl-position suit  *suits*) 13) (cl-position rank *ranks*)))

;;(poker-make-card 2 '♣) ;; 0
;;(poker-make-card 2 '♦) ;; 13
;; '(♣ ♦ ♥ ♠)
;; community card ((8 ♣) (10 ♠) (9 ♠) (A ♠) (5 ♦))
;; hand1 card ((7 ♠) (Q ♣))
;; hand2 card ((A ♣) (J ♣))


(defun get-best-7hand (cards1 hands1)
  ;;(setq cards1  '((8 ♣) (10 ♠) (9 ♠) (A ♠) (5 ♦)))
  ;;(setq hands1  ' ((7 ♠) (Q ♣)))
  (setq hands2 (append cards1 hands1))
  ;(print hands2)
  (setq get_list (transform-card hands2))
  ;(print get_list)
  (setq get_best (poker-best-hand get_list))
  (setq i 0)
  (setq inverse_list '())
  (cl-loop do
           (setq tmp '())
	   (push (elt hands2 (cl-position (pop get_best) get_list)) tmp)
           (setq inverse_list (append inverse_list tmp))
	   until(= i (length get_best))
	   )
  (setq output inverse_list)
  output
)

(defun transform-card (cards1)
  "transform 7 cards to assigned number
  cards1 = community card
  cards2 = hand card
"
  (setq tmp-card '())
  (setq tmp-list '())
  (setq i 0)
  (cl-loop do
	   (setq tmp-list (pop cards1))
	   (setq r (pop tmp-list))
	   (setq s (pop tmp-list))
	   (setq tmp '())
	   (push (poker-make-card r s) tmp)
	   (setq tmp-card (append tmp-card tmp))
	   ;(push (poker-make-card r s) tmp-card)
           ;(print tmp-card)
	   until (= i (length cards1))
	   )
  (setq output tmp-card)
  output
  )

