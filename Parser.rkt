;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Parser) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define no-parser
  (lambda (no-code)
    (cond
      ((symbol? no-code) (list 'var-exp no-code)) ;if you are looking at the identifier, output the parsed identifier
      ((eq? (car no-code) 'function) ; this is what you do when you see a function 
       (list 'func-exp
             (append (list 'params) (car (cdr no-code)))
             (list 'body
                   (no-parser (caddr no-code)))))
      (else (list 'call-exp
                  (no-parser (car (cdr no-code)))
                  (no-parser (car (cdr (cdr no-code)))))))))

(define sample-no-code '(call (function (x) x) a))

;(display (no-parser sample-no-code))

(define undoNoParser
  (lambda (parsedNoCode) 
    (cond
      ((eq? (car parsedNoCode) 'var-exp) (car (cdr parsedNoCode)))
      ((eq? (car parsedNoCode) 'func-exp) (list 'function (cdr (car (cdr parsedNoCode))) (car (cdr (car (cdr (car (cdr (cdr parsedNoCode)))))))))
      (else
       (list 'call (undoNoParser (car (cdr parsedNoCode))) (undoNoParser (car (cdr (cdr parsedNoCode)))))))))


(display (undoNoParser (no-parser sample-no-code)))