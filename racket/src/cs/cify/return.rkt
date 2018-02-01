#lang racket/base
(require "out.rkt")

(provide (struct-out tail-return)
         (struct-out multiple-return)
         (struct-out multiple-return/suffix)
         return
         return-can-omit?)

(struct tail-return (function-id lam self-args))
(struct multiple-return (prefix))
(struct multiple-return/suffix multiple-return (generate-suffix))

(define (return ret runstack s
                #:can-omit? [can-omit? #f]
                #:can-pre-pop? [can-pre-pop? #f])
  (unless (and can-omit? (return-can-omit? ret))
    (let loop ([ret ret])
      (cond
        [(tail-return? ret)
         (cond
           [can-pre-pop?
            (out "*__runstack_ptr = __orig_runstack;")
            (out "return ~a;" s)]
           [else
            (out-open "{")
            (out "Scheme_Object *__retval = ~a;" s)
            (out "*__runstack_ptr = __orig_runstack;")
            (out "return __retval;")
            (out-close "}")])]
        [(multiple-return? ret) (loop (multiple-return-prefix ret))]
        [(procedure? ret) (ret s)]
        [else (out "~a ~a;" ret s)]))
    (when (multiple-return/suffix? ret)
      ((multiple-return/suffix-generate-suffix ret)))))

(define (return-can-omit? ret)
  (or (equal? ret "")
      (and (multiple-return? ret)
           (equal? (multiple-return-prefix ret) ""))))
