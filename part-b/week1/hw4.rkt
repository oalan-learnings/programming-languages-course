
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride) 
	(if (<= low high)
            (cons low (sequence (+ low stride) high stride))
            empty))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))


(define (list-nth-mod xs n)
  (define (get-element-n xs n) (if (zero? n) (car xs) (get-element-n (cdr xs) (- n 1))))
  (cond
    [(negative? n) (error "list-nth-mod : negative number")]
    [(empty? xs) (error "list-nth-mod : empty list")]
    [(get-element-n xs (remainder n (length xs)))]))

(define (stream-for-n-steps s n)
  (cond
    [(zero? n) '()]
    [(= n 1) (cons (car (s)) '())]
    [(cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))]))
	
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons (if (zero? (remainder x 5)) (- x) x)
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x)
                (cons x
                      (lambda () (f (if (equal? x "dan.jpg") "dog.jpg" "dan.jpg")))))])
  (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (lambda ()
    (let ([thunk (s)])
                (cons (cons 0 (car thunk))(stream-add-zero (cdr thunk))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x y)
                (cons (cons (car x) (car y))
                      (lambda () (f (if (empty? (cdr x)) xs (cdr x)) (if (empty? (cdr y)) ys (cdr y))))))])
    (lambda () (f xs ys))))

(define (vector-assoc v vec)
  (letrec ([check-element (lambda (i)
                          (if (= i (vector-length vec))
                              #f
                            (let ([fetched (vector-ref vec i)])
                              (if (and (pair? fetched) (equal? v (car fetched)))
                                  fetched
                            (check-element (add1 i))))))])
  (check-element 0)))

(define (cached-assoc xs n)
  (let ([cache-index 0]
        [cache (make-vector n #f)])
           (lambda (v)
                (let ([c-ans (vector-assoc v cache)])
                  (if c-ans
                      c-ans
                      (let ([ans (assoc v xs)])
                        (if ans
                            (begin
                              (vector-set! cache cache-index ans)
                              (if (= (add1 cache-index) n)
                                  (set! cache-index 0)
                                  (set! cache-index (add1 cache-index)))
                              ans)
                            ans)))))))
  
