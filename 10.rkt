; Environment structure
(define env-empty '())

(define (env-extend env var val)
  (cons (cons var val) env))

(define (env-lookup env var)
  (cdr (first (filter (lambda (entry) (eq? (car entry) var)) env))))

; Lambda abstractions
(define (make-lambda var body)
  (list 'lambda var body))

(define (lambda-var lam)
  (cadr lam))

(define (lambda-body lam)
  (caddr lam))

(define (lambda? lam)
  (and (list? lam) (eq? (first lam) 'lambda)))

; Applications
(define (make-app lam term)
  (list 'app lam term))

(define (app-lambda app)
  (cadr app))

(define (app-argument app)
  (caddr app))

(define (app? app)
  (and (list? app) (not (empty? app)) (eq? (first app) 'app)))

; Closure
(define (make-closure env lam)
  (cons env lam))

(define (closure? c)
  (and (pair? c) (lambda? (cdr c))))

(define (closure-env c) (car c))
(define (closure-lambda c) (cdr c))

; Evaluator

(define (computation-end? S E C D)
  (and (eq? (length S) 1) (empty? C) (empty? D)))

(define (function-return? S E C D)
  (and (eq? (length S) 1) (empty? C) (not (empty? D))))

(define (integer-push? S E C D)
  (and (not (empty? C)) (integer? (first C))))

(define (var-lookup? S E C D)
  (and (not (empty? C)) (string? (first C))))

(define (closure-push? S E C D)
  (and (not (empty? C)) (lambda? (first C))))

(define (apply-push? S E C D)
  (and (not (empty? C)) (app? (first C))))

(define (succ-apply? S E C D)
  (and
    (>= (length S) 2)
    (eq? (first S) 'succ)
    (integer? (first (rest S)))
    (not (empty? C))
    (eq? (first C) 'apply)))

(define (closure-apply? S E C D)
  (and
    (not (empty? C))
    (>= (length S) 2)
    (closure? (first S))
    (eq? (first C) 'apply)))

(define (run S E C D)
  (cond
    [(computation-end? S E C D) (first S)]
    [(function-return? S E C D)
     (let ([s (car (first D))]
           [e (cadr (first D))]
           [c (caddr (first D))])
       (run (append S s) e c (rest D)))]
    [(integer-push? S E C D)
     (run (cons (first C) S) E (rest C) D)]
    [(var-lookup? S E C D)
     (run (cons (env-lookup E (first C)) S) E (rest C) D)]
    [(closure-push? S E C D)
     (run (cons (make-closure E (first C)) S) E (rest C) D)]
    [(apply-push? S E C D)
     (run S E (append (list (app-argument (first C)) (app-lambda (first C)) 'apply) (rest C)) D)]
    [(succ-apply? S E C D)
     (let ([int (first (rest S))]
           [Sp (rest (rest S))])
       (run (cons (+ int 1) Sp) E (rest C) D))]
    [(closure-apply? S E C D)
     (let* ([c (first S)]
            [v (first (rest S))]
            [Ep (closure-env c)]
            [x (lambda-var (closure-lambda c))]
            [t (lambda-body (closure-lambda c))])
       (run '() (env-extend Ep x v) (list t) (cons (list (rest (rest S)) E (rest C)) D)))]
    [#t (cons 'malformed-state (list S E C D))]))

(define (evaluate t)
  (run '() (env-extend env-empty "succ" 'succ) (list t) '()))

(define (const n)
  (make-lambda "f" (make-lambda "x" n)))

(define fixpoint
  (make-lambda "f" (make-lambda "x" (make-app (make-app "f" "f") "x"))))

