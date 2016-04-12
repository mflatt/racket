#lang scheme/base
(require launcher
         setup/dirs)

;; Builds different kinds of executables for different platforms.
;; The `plt-help' executable is for backward compatibity.
;; The `Racket Documentation' executable is to help Windows and
;;  Mac users who are completely lost and need something to click.

(provide installer
         addon-installer)

(define (installer path coll user?)
  (do-installer path coll user? #f))

(define (addon-installer path coll user?)
  (do-installer path coll #t #t))

(define (do-installer path collection user? addon?)
  (for ([mr? (case (system-type)
               [(macosx)  '(#t #f)]
               [(windows) '(#t)]
               [else      '(#f)])]
        #:when (or (not addon?)
                   (if mr?
                       (find-addon-gui-bin-dir)
                       (find-addon-console-bin-dir))))
    (define-values (variants mk-launcher mk-path extras)
      (if mr?
        (values available-mred-variants
                make-mred-launcher
                mred-program-launcher-path
                (build-aux-from-path
                 (path-replace-suffix (collection-file-path "help.ico" "help") #"")))
        (values available-mzscheme-variants
                make-mzscheme-launcher
                mzscheme-program-launcher-path
                '())))
    (for ([variant (remove* '(script-3m script-cgc) (variants))])
      (parameterize ([current-launcher-variant variant])
        (mk-launcher (append
                      (if addon? (addon-flags) null)
                      '("-l-" "help/help"))
                     (mk-path (if mr? "Racket Documentation" "plt-help") #:user? user? #:addon? addon?)
                     `([exe-name . ,(if mr? "Racket Documentation" "plt-help")]
                       [relative? . ,(not user?)]
                       [install-mode . ,(if user? 'user 'main)]
                       [start-menu? . ,(not user?)]
                       ,@extras))))))

(define (addon-flags)
  (list "-A" (path->string (find-system-path 'addon-dir))))
