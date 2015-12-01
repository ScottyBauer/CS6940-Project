#lang racket/base
(require racket/file)
(require racket/list)
(require racket/path)
(require "file-permission-tree.rkt")


(define global-perms '())
(define app-perms '())
(define permissions #f)
(define fs-perm-tree (new-file-perm-tree))
(define full-user-permissions? #f)

(define (perms-of-type t)
  ;; gets all permissions of a given type
  (filter (lambda (p) (equal? (first p) t))
          permissions))

(define (get-app-permission-file-name)
  (reroot-path (path->complete-path (find-system-path 'run-file)) (expand-user-path "~/app-permissions")))

(define (load-permissions)
  ;; TODO - this needs to be a per-app permission file
  ;; TODO - there should be some post processing to be able to have some sort of
  ;;        require form within the permissions
  (set! global-perms
        (with-handlers ([(λ _ #t) (λ _ '())])
          (file->value (expand-user-path "~/permissions"))))
  (set! app-perms (with-handlers ([(λ _ #t) (λ _ '())])
                    (file->value (get-app-permission-file-name))))
  (set! permissions (append global-perms app-perms))
  (for ([p (perms-of-type 'fs-read)])
    (add-to-file-perm-tree fs-perm-tree (second p) #t #f #f))
  (for ([p (perms-of-type 'fs-write)])
    (add-to-file-perm-tree fs-perm-tree (second p) #f #t #f))
  (for ([p (perms-of-type 'fs-protect)])
    (add-to-file-perm-tree fs-perm-tree (second p) #f #f #t))
  (when (not (empty? (filter (lambda (p) (equal? (first p) 'full-user-permissions))
                             permissions)))
    (set! full-user-permissions? #t)))
(load-permissions)

(define (has-permission? perm)
  (cond [full-user-permissions? #t]
        [(not permissions) #f]
        [else (let* ((type (first perm))
                     (check-func (hash-ref has-permission-table type #f)))
                (if check-func
                    (check-func perm)
                    #f))]))

(define has-permission-table (make-hash))

(define (sub-path? path-a path-b)
  ;; tell if b is a child of (or the same as) a.
  (define (recur a b)
    (cond [(empty? a) #t]
          [(empty? b) #f]
          [(equal? (first a) (first b)) (recur (rest a) (rest b))]
          [else #f]))
  (let ((a (explode-path (path->complete-path (expand-user-path path-a))))
        (b (explode-path (path->complete-path (expand-user-path path-b)))))
    (recur a b)))

(define (mk-path-perm-checker type)
  ;; for making fs-read/write permission checkers
  (lambda (perm)
             (let ((dir (second perm))
                   (dirs (map second (perms-of-type type))))
               (for/or ([d dirs])
                 (sub-path? d dir)))))

;(hash-set! has-permission-table 'fs-read (mk-path-perm-checker 'fs-read))
;(hash-set! has-permission-table 'fs-write (mk-path-perm-checker 'fs-write))
(hash-set! has-permission-table 'fs-read (lambda (perm) (has-read? fs-perm-tree (second perm))))
(hash-set! has-permission-table 'fs-write (lambda (perm) (has-write? fs-perm-tree (second perm))))

(define (wrap-read func)
  (lambda (filename)
    (if (has-permission? `(fs-read ,filename))
        (func filename)
        (raise (string-append "Access denied: " filename)))))

(define p-open-input-file (wrap-read open-input-file))
(define p-file->string (wrap-read file->string))
(define p-file->bytes (wrap-read file->bytes))
(define p-file->value (wrap-read file->value))
(define p-file->lines (wrap-read file->lines))
(define p-file->bytes-lines (wrap-read file->bytes-lines))
(define p-directory-list (wrap-read directory-list))



(provide
 (rename-out
  [p-open-input-file open-input-file]
  [p-file->string file->string]
  [p-file->bytes file->bytes]
  [p-file->value file->value]
  [p-file->lines file->lines]
  [p-file->bytes-lines file->bytes-lines]
  [p-directory-list directory-list]
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
  )

 permissions
 )
