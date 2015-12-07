#lang s-exp "../permission-lang.rkt"

((λ _
   (ask-user-for-permission! '(calendar "awesome"))
   (void)))

(with-handlers ([(λ _ #t) (λ (x) (void))])
  (display-to-file (list-ref '("Oct 31 - Tubular"
                               "Dec 25 - Radical"
                               "Jan 1 - Gnarly"
                               "Jul 4 - Liberatious"
                               )
                             (random 4))
                   "~/app-data/calendar/awesome"
                   #:exists 'truncate))

(with-handlers ([(λ _ #t) (λ _ (void))])
  (print (file->string "~/app-data/calendar/awesome")))
