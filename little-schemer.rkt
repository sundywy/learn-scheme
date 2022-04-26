#lang racket

(require rackunit)
(require rackunit/text-ui)


(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define atom-test
  (test-suite
   "test atom"
   (test-case "atom true"
     (check-true (atom? 'a)))

   (test-case "atom false"
     (check-false (atom? '())))))

(run-tests atom-test)

;; chapter 2

;; lat? return true for list of atoms
(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))

;; member? check if a is member of lat
(define member?
  (lambda (a lat)
    (cond [(null? lat) #f]
          [else (or (eq? (car lat) a)
                    (member? a (cdr lat)))])))

;; chapter 3

;; rember remove first occurence of 'a in lat
(define (rember a lat)
  (cond [(null? lat) lat]
        [(eq? a (car lat)) (cdr lat)]
        [else (cons (car lat)
                    (rember a (cdr lat)))]))

;; firsts return the car of each list in lat
(define firsts
  (lambda (l)
    (cond [(null? l) l]
          [else (cons (car (car l)) (firsts (cdr l)))])))

;; insertR will insert new in the right of first old
(define insertR
  (lambda (new old lat)
    (cond [(null? lat) lat]
          [(eq? old (car lat)) (cons (car lat) (cons new (cdr lat)))]
          [else (cons (car lat) (insertR new old (cdr lat)))])))

;; insertL will insert new in the left of first old
(define insertL
  (lambda (new old lat)
    (cond [(null? lat) lat]
          [(eq? old (car lat)) (cons new lat)]
          [else (cons (car lat) (insertL new old (cdr lat)))])))

;; subst will replace the first old with new
(define subst
  (lambda (new old lat)
    (cond [(null? lat) lat]
          [(eq? old (car lat)) (cons new (cdr lat))]
          [else (cons (car lat) (subst new old (cdr lat)))])))

;; subst will replace the first o1 or o2 with new
(define subst2
  (lambda (new o1 o2 lat)
    (cond [(null? lat) lat]
          [(or (eq? o1 (car lat))(eq? o2 (car lat))) (cons new (cdr lat))]
          [else (cons (car lat) (subst2 new o1 o2 (cdr lat)))])))

;; multirember will remove all the occurence of a in lat
(define (multirember a lat)
  (cond [(null? lat) lat]
        [(eq? a (car lat)) (multirember a (cdr lat))]
        [else (cons (car lat)
                    (multirember a (cdr lat)))]))

;; multiinsertR will insert new in the right of old
(define multiinsertR
  (lambda (new old lat)
    (cond [(null? lat) lat]
          [(eq? old (car lat)) (cons (car lat) (cons new (multiinsertR new old (cdr lat))))]
          [else (cons (car lat) (multiinsertR new old (cdr lat)))])))

;; multiinsertL will insert new in the left of old
(define multiinsertL
  (lambda (new old lat)
    (cond [(null? lat) lat]
          [(eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat))))]
          [else (cons (car lat) (multiinsertL new old (cdr lat)))])))

;; multisubst will replace all old with new
(define multisubst
  (lambda (new old lat)
    (cond [(null? lat) lat]
          [(eq? old (car lat)) (cons new (multisubst new old (cdr lat)))]
          [else (cons (car lat) (multisubst new old (cdr lat)))])))

;; chapter 4

(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))

(define (o+ n m)
  (if (zero? m) n (o+ (add1 n) (sub1 m))))

(define (o- n m)
  (if (zero? m) n (o- (sub1 n) (sub1 m))))

(define addtup
  (lambda (tup)
    (if (null? tup) 0 (o+ (car tup) (addtup (cdr tup))))))

(define ox
  (lambda (n m)
    (cond [(zero? m) 0]
          [else (o+ n (ox n (sub1 m)))])))

(define tup+
  (lambda (tup1 tup2)
    (cond [(null? tup1) tup2]
          [(null? tup2) tup1]
          [else (cons (o+ (car tup1)(car tup2))
                      (tup+ (cdr tup1)(cdr tup2)))])))

(define o>
  (lambda (n m)
    (cond [(zero? n) #f]
          [(zero? m) #t]
          [else (o> (sub1 n)(sub1 m))])))

(define o<
  (lambda (n m)
    (cond [(zero? m) #f]
          [(zero? n) #t]
          [else (o< (sub1 n)(sub1 m))])))


(define o=
  (lambda (n m)
    (cond [(o> n m) #f]
          [(o< n m) #f]
          [else #t])))

(define expt
  (lambda (n m)
    (cond [(zero? m) 1]
          [else (ox n (expt n (sub1 m)))])))

(define quotient
  (lambda (n m)
    (cond [(o< n m) 0]
          [else (add1 (quotient (o- n m) m))])))

(define length
  (lambda (lat)
    (cond [(null? lat) 0]
          [else (add1 (length (cdr lat)))])))

(define pick
  (lambda (n lat)
    (cond [(zero? (sub1 n))(car lat)]
          [else (pick (sub1 n)(cdr lat))])))

(define rempick
  (lambda (n lat)
    (cond [(one? n)(cdr lat)]
          [else (cons (car lat)
                      (rempick (sub1 n)(cdr lat)))])))

(define (no-nums lat)
  (cond [(null? lat) lat]
        [(number? (car lat))(no-nums (cdr lat))]
        [else (cons (car lat)(no-nums (cdr lat)))]))

(define (all-nums lat)
  (cond [(null? lat) lat]
        [(number? (car lat))(cons (car lat)
                                  (all-nums (cdr lat)))]
        [else (all-nums (cdr lat))]))

(define (eqan? a1 a2)
  (cond [(number? a1)(and (number? a2) (o= a1 a2))]
        [(atom? a1) (and (atom? a2)(eq? a1 a2))]
        [else #f]))

(define (occur a lat)
  (cond [(null? lat) 0]
        [(eqan? (car lat) a) (add1 (occur a (cdr lat)))]
        [else (occur a (cdr lat))]))

(define one?
  (lambda (a)
    (= a 1)))

;; chapter 5

(define (rember* a l)
  (cond [(null? l) '()]
        [(atom? (car l)) (if (eq? (car l) a)
                             (rember* a (cdr l))
                             (cons (car l) (rember* a (cdr l))))]
        [else (cons (rember* a (car l))(rember* a (cdr l)))]))

(define test-rember*
  (test-suite
   "rember*"
   (test-case "remove 'cup"
     (check-equal?
      (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
      '((coffee) ((tea)) (and (hick)))))
   (test-case "remove 'sauce"
     (check-equal?
      (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
      '(((tomato)) ((bean))(and ((flying))))))))

(run-tests test-rember*)

(define insertR*
  (lambda (new old lat)
    (cond [(null? lat) lat]
          [(atom? (car lat))
           (cond [(eq? old (car lat)) (cons (car lat) (cons new (insertR* new old (cdr lat))))]
                 [else (cons (car lat) (insertR* new old (cdr lat)))])]
          [else (cons (insertR* new old (car lat))
                      (insertR* new old (cdr lat)))])))

(define test-insertR*
  (test-suite
   "insert*"
   (test-case "insert 'roast to the right of 'chuck"
     (check-equal?
      (insertR* 'roast 'chuck
                '((how much (wood)) could ((a (wood) chuck)) (((chuck)))(if (a) ((wood chuck))) could chuck wood))
     '((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast)))
                         (if (a) ((wood chuck roast))) could chuck roast wood)))))

(run-tests test-insertR* 'verbose)

(define (occur* a l)
  (cond [(null? l) 0]
        [(atom? (car l)) (if (eq? (car l) a)
                             (add1 (occur* a (cdr l)))
                             (occur* a (cdr l)))]
        [else (+ (occur* a (car l)) (occur* a (cdr l)))]))

(define test-occur*
  (test-suite
   "occur*"
   (test-case "check if 'banana occur"
     (check-equal? (occur* 'banana
             '((banana) (split (((((banana ice))) (cream (banana)) sherber)) (banana) (bread) (banana brandy))))
     5))))

(run-tests test-occur* 'verbose)

(define subst*
  (lambda (new old l)
    (cond [(null? l) '()]
          [(atom? (car l)) (if (eq? (car l) old)
                               (cons new (subst* new old (cdr l)))
                               (cons (car l) (subst* new old (cdr l))))]
          [else (cons (subst* new old (car l))
                      (subst* new old (cdr l)))])))

(define test-subst*
  (test-suite
   "subst*"
   (test-case "substitute banana with orange"
     (check-equal?
      (subst* 'orange 'banana
              '((banana) (split ((((banana ice)))
                                 (cream (banana))
                                 sherbet))
                         (banana)
                         (bread)
                         (banana brandy)))
      '((orange) (split ((((orange ice)))
                         (cream (orange))
                         sherbet))
                 (orange)
                 (bread)
                 (orange brandy))))))

(run-tests test-subst* 'verbose)

(define insertL*
  (lambda (new old lat)
    (let ([x (car lat)]
          [xs (cdr lat)])
      (cond [(null? lat) lat]
            [(atom? x)
             (cond [(eq? old x) (cons new (cons old (insertL* new old xs)))]
                   [else (cons x (insertL* new old xs))])]
            [else (cons (insertL* new old x)
                        (insertL* new old xs))]))))

(define test-insertL*
  (test-suite
   "insertL*"
   (test-case "insert 'pecker to the left of 'chuck"
     (check-equal?
      (insertL* 'pecker 'chuck
                '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
     '((how much (wood)) could ((a (wood) pecker chuck)) (((pecker chuck)))
                         (if (a) ((wood pecker chuck))) could pecker chuck wood)))))

(run-tests test-insertL* 'verbose)

(define member*
  (lambda (a l)
    (cond [(null? l) #f]
          [(atom? (car l))(or (eq? (car l) a) (member* a (cdr l)))]
          [else (or (member* a (car l)) (member* a (cdr l)))])))

(define leftmost
  (lambda (l)
    ))

(define test-member*
  (test-suite
   "member*"
   (check-true (member* 'chips '((potato)(chips ((with) fish)(chips)))))))

(run-tests test-member*)

(define leftmost
  (lambda (l)
    (cond [(null? l) '()]
          [(atom? (car l)) (car l)]
          [else (leftmost (car l))])))

(define test-leftmost
  (test-suite
   "leftmost"
   (check-equal? (leftmost '((potato) (chips ((with) fish) (chips)))) 'potato)
   (check-equal? (leftmost '(((hot)(tuna (and))) cheese)) 'hot)
   (check-equal? (leftmost '(((() four)) 17 (seventeen))) '())
   (check-equal? (leftmost '()) '())))

(run-tests test-leftmost)

(define eqlist
  (lambda (l1 l2)
    (cond [(null? l1) (null? l2)]
          [(atom? (car l1)) (and (atom? (car l2)) (eq? (car l1) (car l2)) (eqlist (cdr l1) (cdr l2)))]
          [else (and (eqlist (car l1) (car l2)) (eqlist (cdr l1) (cdr l2)))])))

(define test-eqlist
  (test-suite
   "eqlist"
   (check-true (eqlist '(strawberry ice cream) '(strawberry ice cream)))
   (check-false (eqlist '(strawberry ice cream) '(strawberry cream ice)))
   (check-false (eqlist '(banana ((split))) '((banana) (split))))
   (check-false (eqlist '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda)))))
   (check-true (eqlist '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))))))

(run-tests test-eqlist)

;; chapter 6

(define numbered?
  (lambda (l)
    (cond [(atom? l) (number? l)]
          [(or (number? (car l)) (eq? '+ (car l)) (eq? 'x (car l)) (eq? '^ (car l))) (numbered? (cdr l))]
          [else (or (null? l) (and (numbered? (car l)) (numbered? (cdr l))))])))

(define test-numbered?
  (test-suite
   "numbered?"
   (check-true (numbered? 1))
   (check-true (numbered? '(3 + (4 ^ 5))))
   (check-false (numbered? '(2 x sausage)))))

(run-tests test-numbered?)
