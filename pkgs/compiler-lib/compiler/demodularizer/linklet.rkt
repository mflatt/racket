#lang racket/base
(require racket/match
         racket/linklet
         compiler/zo-structs
         compiler/private/deserialize)

(provide linklet*-exports
         linklet*-internals
         linklet*-importss
         linklet*-import-shapess
         linklet*-lifts
         linklet*-body)

(define (linklet*-exports linkl)
  (cond
    [(linkl? linkl) (linkl-exports linkl)]
    [(faslable-correlated-linklet? linkl)
     (match (faslable-correlated-linklet-expr linkl)
       [`(linklet ,imports ,exports . ,_) (for/list ([ex (in-list exports)])
                                            (if (pair? ex)
                                                (cadr ex)
                                                ex))])]
    [else (unsupported linkl)]))

(define (linklet*-internals linkl)
  (cond
    [(linkl? linkl) (linkl-internals linkl)]
    [(faslable-correlated-linklet? linkl)
     (match (faslable-correlated-linklet-expr linkl)
       [`(linklet ,imports ,exports . ,body)
        (for/fold ([l '()]) ([form (in-list body)])
          (match form
            [`(define-values ,ids ,_)
             (append ids l)]
            [else l]))])]
    [else (unsupported linkl)]))

(define (linklet*-importss linkl)
  (cond
    [(linkl? linkl) (linkl-importss linkl)]
    [(faslable-correlated-linklet? linkl)
     (match (faslable-correlated-linklet-expr linkl)
       [`(linklet ,importss ,exports . ,_) (for/list ([imports (in-list importss)])
                                             (for/list ([im (in-list imports)])
                                               (if (pair? im)
                                                   (car im)
                                                   im)))])]
    [else (unsupported linkl)]))

(define (linklet*-import-shapess linkl)
  (cond
    [(linkl? linkl) (linkl-import-shapess linkl)]
    [(faslable-correlated-linklet? linkl)
     (match (faslable-correlated-linklet-expr linkl)
       [`(linklet ,importss ,exports . ,_) (for/list ([imports (in-list importss)])
                                             (for/list ([im (in-list imports)])
                                               #f))])]
    [else (unsupported linkl)]))

(define (linklet*-lifts linkl)
  (cond
    [(linkl? linkl) (linkl-lifts linkl)]
    [(faslable-correlated-linklet? linkl) '()]
    [else (unsupported linkl)]))

(define (linklet*-body linkl)
  (cond
    [(faslable-correlated-linklet? linkl)
     (match (faslable-correlated-linklet-expr linkl)
       [`(linklet ,imports ,exports . ,body)
        (strip-correlated body)])]
    [else #f]))


(define (unsupported linkl)
  (error 'demodularize "unsupported linklet format"))
