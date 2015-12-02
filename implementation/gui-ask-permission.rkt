#lang racket/base

(require racket/gui/base)
(require racket/class)

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
  (mk-button "yes once" 'yes-once)
  (mk-button "yes this run" 'yes-this-run)
  (mk-button "yes forever" 'yes-permanent)
  (send dialog show #t)
  answer)
