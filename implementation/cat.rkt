#lang s-exp "permission-lang.rkt"

;(printf "current permissions: ~a~n" permissions)
(for ([a (current-command-line-arguments)])
  (display (file->string a)))


