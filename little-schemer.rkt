#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))

(define lat2?
  (lambda (l)
    (cond ((null? l) #t)
          (else (and (atom? (car l)) (lat? (cdr l)))))))

(define member?
  (lambda (a lat)
    (cond [(null? lat) #f]
          [else (or (eq? (car lat) a)
                    (member? a (cdr lat)))])))

(define (rember a lat)
  (cond [(null? lat) lat]
        [(eq? a (car lat)) (cdr lat)]
        [else (cons (car lat)
                    (rember a (cdr lat)))]))

(define firsts
  (lambda (l)
    (cond [(null? l) l]
          [else (cons (car (car l)) (firsts (cdr l)))])))
