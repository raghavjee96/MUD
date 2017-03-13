#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)

;; advanced command-line processor

(define responses
  '((1 "What type of cars do you like?")
    (2 "Do you enjoy driving fast?")
    (3 "Are you a safe driver?")
    (4 "Have you ever driven on a track?")
    (8 "Are you not a confident driver?")))


(define decisiontable
  '((1 ((suv) 3) ((coupe) 2) ((crossover) 6) ((sedans) 5) ((sports) 2) ((performance) 4))
    (2 ((yes) 3) ((a lot) 3) ((no) 8) ((not really) 8))
    (8 ((yes) non-gory) ((ok) gory) ((no) 0))))

;;functions to support main function

;;functions to get info from the associated lists 
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

(define (get-response id)
  (car (assq-ref responses id)))

(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (lambda (key) (car key)) keys)))


;; outputs a list in the form: (0 0 0 2 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))

(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
      #f
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))


(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index 
      (cadr (list-ref record index))
      #f)))


(define (recommend initial-id)
  (let loop ((id initial-id))
    (format #t "~a\n> " (get-response id))
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((eq? #f response)
	       (format #t "huh? I didn't understand that! ")
	       (loop id)) 
	      ((eq? 'gory response)
	       (format #t "Searching for gory horror films ....\n")
	       (exit))
 	      ((eq? 'non-gory response)
	       (format #t "Searching for non-gory scarey films ....\n")
	       (exit))             
	      ((zero? response)
	       (format #t "So Long, and Thanks for All the Fish...\n")
	       (exit))
	      (else
	       (loop response)))))))
