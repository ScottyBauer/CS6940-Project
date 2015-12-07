#lang s-exp "../permission-lang.rkt"

(define args (vector->list (current-command-line-arguments)))

(define (print-listing dir)
  ;; I'm sure there is a better way to catch all exceptions...
  (with-handlers ([(λ _ #t) (λ (exn) (displayln exn (current-error-port)))])
    (for ([f (directory-list dir)])
      (displayln f))))

(if (null? args)
    (print-listing ".")
    (for ([dir args])
      (print-listing dir)))

