; Library used 

(require racket/stream)

; Supporting code 

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (combine-two-streams stream1 stream2)
  (stream-cons (list (stream-first stream1) (stream-first stream2))
               (combine-two-streams (stream-rest stream1) (stream-rest stream2)))

(define (combine-three-streams stream1 stream2 stream3)
  (stream-cons (list (stream-first stream1) (stream-first stream2) (stream-first stream3))
               (combine-three-streams (stream-rest stream1) (stream-rest stream2) (stream-rest stream3)))

(define (position-of-maximum lst)
  (let loop ((lst lst) (max -inf.0) (position 0) (max-position -1))
    (cond
      ((null? lst) max-position)
      ((> (car lst) max) (loop (cdr lst) (car lst) (+ position 1) position))
      (else (loop (cdr lst) max (+ position 1) max-position))))

(define (replace-at-position lst n value)
  (if (or (null? lst) (< n 0))
      lst
      (if (= n 0)
          (cons value (cdr lst))
(cons (car lst) (replace-at-position (cdr lst) (- n 1) value))))

(define (find-position value lst)
  (define (iter lst position)
    (cond
      ((null? lst) #f)
      ((equal? (car lst) value) position)
      (else (iter (cdr lst) (+ position 1)))))
  (iter lst 0))

; Code for creating the environment of maze

(define maze
  (list 'w 'w 'w 'w 'w 'w 'w 'w 'w 'w 'w 
       'w 0 1 2 'w 3 4 5 6 7 'w 
       'w 8 9 10 'w 11 12 13 14 15 'w 
       'w 16 'w 17 18 19 20 21 22 23 'w
        'w 24 'w 25 26 27 'w 28 29 30 'w 
        'w 31 32 33 34 35 'w 36 37 38 'w 
        'w 'w 'w 'w 'w 'w 'w 'w 'w 'w))

(define Q
  (list (list 0 0 0 0) (list 0 0 0 0) (list 0 0 0 0)
        (list 0 0 0 0) (list 0 0 0 0) (list 0 0 0 0)
        (list 0 0 0 0) (list 0 0 0 0) (list 0 0 0 0)
        (list 0 0 0 0) (list 0 0 0 0) (list 0 0 0 0)
        (list 0 0 0 0) (list 0 0 0 0) (list 0 0 0 0)
        (list 0 0 0 0) (list 0 0 0 0) (list 0 0 0 0)
        (list 0 0 0 0) (list 0 0 0 0) (list 0 0 0 0)
        (list 0 0 0 0) (list 0 0 0 0) (list 0 0 0 0)
        (list 0 0 0 0) (list 0 0 0 0) (list 0 0 0 0)
        (list 0 0 0 0) (list 0 0 0 0) (list 0 0 0 0)
        (list 0 0 0 0) (list 0 0 0 0) (list 0 0 0 0) 0))

(define (bounce-or-not old new)
  (if (eq? new 'w) old new))

(define (newstate oldstate action)
  (let ((position (find-position oldstate maze)))
    (cond
      ((= action 0) (bounce-or-not (list-ref maze position) (list-ref maze (- position 1))))
   
((= action 1) (bounce-or-not (list-ref maze position) (list-ref maze (+ position 1))))
      ((= action 2) (bounce-or-not (list-ref maze position) (list-ref maze (- position 11)))
      (else (bounce-or-not (list-ref maze position) (list-ref maze (+ position 11)))))))

(define (reward state action)
  (if (or (and (= state 32) (= action 0)) (and (= state 24) (= action 3)))
      100000
      0))

(define (act state Q epsilon)
  (cond
    ((> (random) epsilon)
     (position-of-maximum (list-ref Q state))
     )
    (else (random 4))))

; Code for the Q learning

(define eps_taper 0.0025)

(define (epsilon-update epsilon)
  (/ epsilon (+ 1 eps_taper)))

(define epsilons (stream-cons 1 (stream-map epsilon-update epsilons)))

(define alpha_taper 0.003)

(define (alpha-update alpha)
  (/ alpha (+ 1 alpha_taper)))

(define alphas (stream-cons 1 (stream-map alpha-update alphas)))

(define gamma 0.9)

(define (update-value state Qgame action epsilon alpha)
  (define nextaction (act (newstate state action) Qgame epsilon))
  (values
   (+ (list-ref (list-ref Qgame state) action)
      (* alpha
         (- (+ (reward state action)
               (* gamma (list-ref (list-ref Qgame (newstate state action)) nextaction)))
            (list-ref (list-ref Qgame state) action)))))

(define (newQfinder state Qgame action epsilon alpha)
  (replace-at-position
   (replace-at-position Qgame state
                       (replace-at-position (list-ref Qgame state) action
                                            (update-value state Qgame action epsilon alpha)))
   39 (reward state action)))

(define (game Q epsilon alpha)
  (define randstate (random 39))
  (define states (stream-cons randstate
                    (stream-map
                     (lambda (s_a)
                       (newstate (list-ref s_a 0) (list-ref s_a 1))
                       (combine-two-streams states actions))))
  (define Qgames (stream-cons (replace-at-position Q 39 0)
                       (stream-map
                        (lambda (s_q_a)
                          (newQfinder (list-ref s_q_a 0) (list-ref s_q_a 1) (list-ref s_q_a 2) epsilon alpha))
                        (combine-three-streams states Qgames actions))))
  (define actions (stream-map
                   (lambda (s_q)
                     (act (list-ref s_q 0) (list-ref s_q 1) epsilon))
                   (combine-two-streams states Qgames)))
  (define (cdring Qgames i)
    (if (or (> (list-ref (stream-first Qgames) 39) 0) (= i 0))
        (stream-first Qgames)
        (cdring (stream-rest Qgames) (- i 1))))
  (cdring Qgames 100))

(define Qs (stream-cons Q
            (stream-map
             (lambda (q_e_a)
               (game (list-ref q_e_a 0) (list-ref q_e_a 1) (list-ref q_e_a 2)))
             (combine-three-streams Qs epsilons alphas)))

(define (pathfinder Q state)
  (if (= state 31)
      (display "end")
      (let ((new-state (newstate state (act state Q 0)))
           (display (act state Q 0))
           (pathfinder Q new-state))))

(stream-ref Qs 500)
(pathfinder (stream-ref Qs 500) 36)


