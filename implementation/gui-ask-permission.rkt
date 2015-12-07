#lang racket/base

(require racket/gui/base)
(require racket/class)
(require racket/port)
(require "permission-lang.rkt")

(provide gui-ask-permission)

(define (gui-ask-permission perm)
  (define answer 'no-once)
  (define dialog (new dialog%
                      [label "Give permission?"]))
  (define perm-message (new message%
                            [label (format "Allow app the following permission?~n~a"
                                           perm)]
                            [parent dialog]))
  (define panel (new horizontal-panel% [parent dialog]
                     [alignment '(center center)]))
  (define-syntax-rule (mk-button labeltext response)
    (new button%
         [parent panel]
         [label labeltext]
         [callback (λ _
                     (set! answer response)
                     (send dialog show #f))]))
  (mk-button "no once" 'no-once)
  (mk-button "no this run" 'no-this-run)
  (mk-button "do not ask for any more permissions this run" 'no-all-this-run)
  ;(mk-button "no forever" 'no-permanent)
  ;(mk-button "yes once" 'yes-once)
  (mk-button "yes this run" 'yes-this-run)
  (mk-button "yes forever" 'yes-permanent)

  (define expanded-perms (expand-composite-perm perm))

  (define perm-expand-button
    (new button%
         [parent dialog]
         [label "Show expanded permission"]
         [callback
          (λ _ (new message%
                    [parent dialog]
                    [label (with-output-to-string
                             (λ _
                               (let ((old-ep expanded-perms))
                                 (set! expanded-perms '())
                                 (for ((p old-ep))
                                   (printf "~a~n" p)
                                   (set! expanded-perms (append expanded-perms
                                                                (expand-composite-perm p)))))
                               ))]))]))
  (send dialog show #t)
  answer)
