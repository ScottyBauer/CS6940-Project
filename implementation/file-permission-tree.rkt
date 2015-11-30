#lang racket/base

;;; Structure and functions for file permissions.
;;; This incorporates the protected path model, where
;;; protected paths have no access unless permission to that
;;; path has been given explicitly.

(provide
 (rename-out [mk-file-perm-tree-node new-file-perm-tree])
 add-to-file-perm-tree
 has-read?
 has-write?
 )

(define-struct file-perm-tree
  (read write protect children)
  #:mutable
  #:transparent)

(define (mk-file-perm-tree-node)
  (file-perm-tree #f #f #f (make-hash)))

(define (add-to-file-perm-tree-relative tree path-parts read write protect)
  ;; recurse down the tree until you get to the end of the path
  (if (null? path-parts)
      (begin
        (set-file-perm-tree-read! tree (or read (file-perm-tree-read tree)))
        (set-file-perm-tree-write! tree (or write (file-perm-tree-write tree)))
        (set-file-perm-tree-protect! tree (or protect (file-perm-tree-protect tree)))
        )
      (let ((children (file-perm-tree-children tree)))
        (if (hash-has-key? children (car path-parts))
            (add-to-file-perm-tree-relative (hash-ref children (car path-parts))
                                            (cdr path-parts)
                                            read
                                            write
                                            protect)
            (let ((new-node (mk-file-perm-tree-node)))
              (begin
                (hash-set! children
                           (car path-parts)
                           new-node)
                (add-to-file-perm-tree-relative new-node
                                                (cdr path-parts)
                                                read
                                                write
                                                protect)))))))

(define (add-to-file-perm-tree tree path read write protect)
  (add-to-file-perm-tree-relative tree
                                  (explode-path (expand-user-path path))
                                  read
                                  write
                                  protect))

(define (get-rw tree path-parts r w)
  (let* ((p (file-perm-tree-protect tree))
         (new-r (or (and (not p) r)
                    (file-perm-tree-read tree)))
         (new-w (or (and (not p) w)
                    (file-perm-tree-write tree))))
    (cond [(null? path-parts)
           (values new-r new-w)]
          [(not (hash-has-key? (file-perm-tree-children tree)
                               (car path-parts)))
           (values new-r new-w)]
          [else (get-rw (hash-ref (file-perm-tree-children tree)
                                  (car path-parts))
                        (cdr path-parts)
                        new-r
                        new-w)])))

(define (has-read? tree path)
  (let-values ([(r w) (get-rw tree (explode-path (expand-user-path path)) #f #f)])
    r))
(define (has-write? tree path)
  (let-values ([(r w) (get-rw tree (explode-path (expand-user-path path)) #f #f)])
    w))

(module+ test
  (require rackunit)
  (define test-perm-tree (mk-file-perm-tree-node))
  (add-to-file-perm-tree test-perm-tree "/home/foo/secrit" #f #f #t)
  (add-to-file-perm-tree test-perm-tree "/home/foo" #t #f #f)
  (add-to-file-perm-tree test-perm-tree "/home/foo/writable" #f #t #f)
  (add-to-file-perm-tree test-perm-tree "/home/foo/secrit/bar" #t #f #f)

  (check-equal? #t (has-read? test-perm-tree "/home/foo"))
  (check-equal? #f (has-read? test-perm-tree "/home/foo/secrit"))
  (check-equal? #t (has-read? test-perm-tree "/home/foo/secrit/bar"))
  (check-equal? #t (has-read? test-perm-tree "/home/foo/secrit/bar/aoeu"))
  (check-equal? #f (has-read? test-perm-tree "/home/foo/secrit/still-secrit"))
  (check-equal? #t (has-write? test-perm-tree "/home/foo/writable/foo"))
  (check-equal? #f (has-write? test-perm-tree "/home/foo"))

  )
