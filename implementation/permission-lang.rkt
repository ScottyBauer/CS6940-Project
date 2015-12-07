#lang racket/base
(require racket/file)
(require racket/list)
(require racket/path)
(require racket/string)
(require racket/match)
(require "file-permission-tree.rkt")
(require "gui-ask-permission.rkt")


(define global-perms '())
(define app-perms '())
(define permissions #f)
(define fs-perm-tree (new-file-perm-tree))
(define full-user-permissions? #f)
(define do-not-ask-user #f)
(define do-not-ask-user-map (make-hash))

(define (perms-of-type t)
  ;; gets all permissions of a given type
  (filter (lambda (p) (equal? (first p) t))
          permissions))

(define (normalize-path p)
  (resolve-path (expand-user-path p)))

(define (dirname p)
  ;; like the unix command, only with racket paths
  (apply build-path (reverse (cdr (reverse (explode-path p))))))

(define (get-app-permission-file-name)
  (reroot-path (path->complete-path (find-system-path 'run-file))
               (normalize-path "~/app-permissions")))

(define perm-constant-table
  (hash "@APP_DATA" "~/app-data"
        "@HOME" "~"
        "@APP_PERMISSIONS" "~/app-permissions"
        "@PERMISSION_DEFINITIONS" "~/permission-definitions"
        ))

(define (load-composite-permission-def perm-name)
  (file->perms (normalize-path (string-append "~/permission-definitions/"
                                              (symbol->string perm-name)))))

(define (expand-composite-perm-template template args)
  (define (mk-perm-at-expander args)
    (lambda (path-part)
      (cond [(regexp-match #px"^@\\d+$" path-part)
             (with-handlers ([(λ _ #t) (λ _ path-part)])
               (list-ref args (sub1 (string->number
                                     (substring path-part 1)))))]
            [(regexp-match "^@.*$" path-part)
             (with-handlers ([(λ _ #t) (λ _ path-part)])
               (hash-ref perm-constant-table path-part))]
            [else path-part])))

  (define (expand-composite-part template-part args)
    ;; TODO - this should accept multiple paths (IE multiple perm arguments)
    (let* ((type (car template-part))
           (path (cadr template-part))
           (path-parts (string-split path "/" #:trim? #f)))
      (list type (string-join (map (mk-perm-at-expander args) path-parts) "/"))))

  (for/list ((perm template))
    (expand-composite-part perm args)))

(define (expand-composite-perm p)
  (let* ((name (car p))
         (args (cdr p))
         (template (load-composite-permission-def name))
         )
    (expand-composite-perm-template template args)
    ))

(define (file->perms f)
  (with-handlers ([(λ _ #t) (λ _ '())])
    (let ((perms (file->value (normalize-path f))))
      (if (list? perms) perms '()))))

(define (update-fs-perm-tree-with-perm! p)
  (cond [(equal? (car p) 'fs-read)
         (add-to-file-perm-tree! fs-perm-tree (normalize-path (second p)) #t #f #f)]
        [(equal? (car p) 'fs-write)
         (add-to-file-perm-tree! fs-perm-tree (normalize-path (second p)) #f #t #f)]
        [(equal? (car p) 'fs-protect)
         (add-to-file-perm-tree! fs-perm-tree (normalize-path (second p)) #f #f #t)]
        [else (for ((sub-p (expand-composite-perm p)))
                (update-fs-perm-tree-with-perm! sub-p))]))

(define (add-runtime-perm! perm)
  (set! permissions (cons perm permissions))
  (update-fs-perm-tree-with-perm! perm))

(define (add-perm-to-app-perms-file! perm)
  (let ((app-perms (file->perms (get-app-permission-file-name))))
    (begin
      (make-directory* (dirname (get-app-permission-file-name)))
      (write-to-file (cons perm app-perms)
                     (get-app-permission-file-name)
                     #:mode 'text
                     #:exists 'replace))))

(define (load-permissions!)
  ;; TODO - there should be some post processing to be able to have some sort of
  ;;        require form within the permissions
  (set! global-perms (file->perms "~/permissions"))
  (set! app-perms (file->perms (get-app-permission-file-name)))
  (set! permissions (append global-perms app-perms))
  (for ([p permissions])
    (update-fs-perm-tree-with-perm! p))
  (when (not (empty? (perms-of-type 'do-not-ask-user)))
    (set! do-not-ask-user #t))
  (when (not (empty? (perms-of-type 'full-user-permissions)))
    (set! full-user-permissions? #t)))
(load-permissions!)

(define (ask-user-for-permission! perm)
  (cond [do-not-ask-user #f]
        [(hash-has-key? do-not-ask-user-map perm) #f]
        [else
         (match (gui-ask-permission perm)
             ['yes-once #t]
             ['yes-this-run (begin (add-runtime-perm! perm)
                                   #t)]
             ['yes-permanent (begin (add-perm-to-app-perms-file! perm)
                                    (add-runtime-perm! perm)
                                    #t)]
             ['no-once #f]
             ['no-this-run (begin (hash-set! do-not-ask-user-map perm #t)
                                  #f)]
             ['no-all-this-run (begin (set! do-not-ask-user #t)
                                      #f)]
             ;['no-permanent #f]
             [else #f]
             )]))

(define (has-permission? perm)
  (cond [full-user-permissions? #t]
        [(not permissions) #f]
        [else (let* ((type (first perm))
                     (check-func (hash-ref has-permission-table type #f)))
                (if check-func
                    (or (check-func perm)
                        (ask-user-for-permission! perm))
                    #f))]))

(define has-permission-table (make-hash))

(hash-set! has-permission-table 'fs-read
           (lambda (perm) (has-read? fs-perm-tree (normalize-path (second perm)))))
(hash-set! has-permission-table 'fs-write
           (lambda (perm) (has-write? fs-perm-tree (normalize-path (second perm)))))

(define (wrap-read func)
  (lambda (filename)
    (if (has-permission? `(fs-read ,filename))
        (func filename)
        (raise (string-append "Read access denied: " filename)))))
(define (wrap-write func)
  (lambda (val filename #:mode [mode 'binary] #:exists [exists 'error])
    (if (has-permission? `(fs-write ,filename))
        (func val filename #:mode mode #:exists exists)
        (raise (string-append "Write access denied: " filename)))))

(define p-open-input-file (wrap-read open-input-file))
(define p-file->string (wrap-read file->string))
(define p-file->bytes (wrap-read file->bytes))
(define p-file->value (wrap-read file->value))
(define p-file->lines (wrap-read file->lines))
(define p-file->bytes-lines (wrap-read file->bytes-lines))
(define p-directory-list (wrap-read directory-list))
(define p-display-to-file (wrap-write display-to-file))
(define p-write-to-file (wrap-write write-to-file))
(define p-display-lines-to-file (wrap-write display-lines-to-file))



(provide
 (rename-out
  [p-open-input-file open-input-file]
  [p-file->string file->string]
  [p-file->bytes file->bytes]
  [p-file->value file->value]
  [p-file->lines file->lines]
  [p-file->bytes-lines file->bytes-lines]
  [p-directory-list directory-list]
  [p-display-to-file display-to-file]
  [p-write-to-file write-to-file]
  [p-display-lines-to-file display-lines-to-file]
  )
 (except-out
  (all-from-out racket/base)
  open-input-file
  directory-list
  )
 (except-out
  (all-from-out racket/file)
  file->string
  file->bytes
  file->value
  file->lines
  file->bytes-lines
  display-to-file
  write-to-file
  display-lines-to-file
  )

 permissions
 ask-user-for-permission!
 )
