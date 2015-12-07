#lang s-exp "../permission-lang.rkt"

(require racket/port) ; for port->string

;(printf "current permissions: ~a~n" permissions)
(define (do-cat file)
  (with-handlers ([(λ _ #t) (λ (exn) (displayln exn (current-error-port)))])
    (if (equal? file "-")
        (display (port->string (current-input-port)))
        (display (file->string file)))))

(if (equal? 0 (vector-length (current-command-line-arguments)))
    (do-cat "-")
    (for ([a (current-command-line-arguments)])
      (do-cat a)))


