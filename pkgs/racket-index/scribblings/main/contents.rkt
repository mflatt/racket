#lang at-exp racket/base
(require scribble/manual
         scribble/core
         scribble/html-properties
         setup/dirs
         "private/utils.rkt"
         "private/manuals.rkt")

(provide build-contents)

(define path-info-style (style "RootPathInfo" (list (attributes '((id . "rootPathInfo"))))))
(define go-style (style "RootPathAction" (list (attributes '((onclick . "return GoToRootPath();"))))))
(define disable-style (style "RootPathAction" (list (attributes '((onclick . "return DisableRootPath();"))))))

(define (build-contents #:user? [user? #f]
                        #:main-language-family [main-language-family (get-main-language-family)]
                        #:title-content [title-content (list main-language-family
                                                             (element (style #f '(aux)) " Documentation"))])
  (list
   @main-page['start (not user?) #:show-root-info? (not user?) #:title-content title-content]

   (if user?
       @margin-note{This is an installation- and user-specific listing,
             including documentation for installed
             packages.}
      @margin-note{
        @not-on-the-web{This is an installation-specific listing.}
        Running @exec{raco docs}
        (or @exec{Racket Documentation} on Windows or Mac OS)
        may open a different page with local and user-specific
        documentation, including documentation for installed packages.
        @elem[#:style path-info-style]{Searching or following a
         ``top'' link will go to a different starting point that
         includes user-specific information.
         @hyperlink["#"]{@elem[#:style go-style]{[Go to user-specific start]}}
         @hyperlink["#"]{@elem[#:style disable-style]{[Forget user-specific start]}}}})

   @(make-start-page user? #:main-language-family main-language-family)))
