;; The client half of this interaction is in "cs/linklet/cross-compile.ss".

;; Communication uses the Chez Scheme printer and reader so make the
;; server independent from Racket, although it is run by the Racket
;; executable.

;; Suppress printout on startup:
(define original-output-port (current-output-port))
(let-values ([(o get) (open-bytevector-output-port (current-transcoder))])
  (current-output-port o))

;; Server function to run after cross-compiler is loaded:
(define (serve-cross-compile)
  ;; Don't exit due to Ctl-C:
  (keyboard-interrupt-handler void)
  ;; Restore output
  (current-output-port original-output-port)
  ;; Serve requests to compile or to fasl data:
  (let loop ()
    (let ([cmd (read)])
      (unless (eof-object? cmd)
        (let-values ([(o get) (open-bytevector-output-port)])
          (case cmd
            [(fasl)
             (fasl-write (unmarshal-annotation (read)) o)]
            [(compile)
             (compile-to-port (list `(lambda () ,(unmarshal-annotation (read)))) o)]
            [else
             (error 'serve-cross-compile (format "unrecognized command: ~s" cmd))])
          (let ([result (get)])
            (write result)
            (newline)
            (flush-output-port)))
        (loop)))))

;; ----------------------------------------

;; Copied in "../linklet/annotation.ss"
(define-record-type marshaled-annotation
  (fields expression source-object)
  (nongenerative #{marshaled-annotation gd3r4cl07w9emgzjvdmpf3qpq-0}))

(define (unmarshal-annotation v)
  (let ([ht (make-hashtable equal-hash equal?)])
    (let-values ([(a stripped)
                  (let loop ([v v])
                    (cond
                     [(marshaled-annotation? v)
                      (let-values ([(e s-e) (loop (marshaled-annotation-expression v))])
                        (values (make-annotation e
                                                 (unmarshal-source-object
                                                  (marshaled-annotation-source-object v)
                                                  ht)
                                                 s-e)
                                s-e))]
                     [(pair? v)
                      (let-values ([(a s-a) (loop (car v))]
                                   [(d s-d) (loop (cdr v))])
                        (if (and (eq? a (car v))
                                 (eq? d (cdr v)))
                            (values v v)
                            (values (cons a d) (cons s-a s-d))))]
                     [else (values v v)]))])
      a)))

(define (unmarshal-source-object s ht)
  (let ([p (#%vector-ref s 0)]
        [bfp (#%vector-ref s 1)]
        [efp (#%vector-ref s 2)]
        [line (#%vector-ref s 3)]
        [column (#%vector-ref s 4)])
    (let ([sfd (or (hashtable-ref ht p #f)
                   (let ([sfd (source-file-descriptor p 0)])
                     (hashtable-set! ht p sfd)
                     sfd))])
      (cond
       [line (make-source-object sfd bfp efp line column)]
       [else (make-source-object sfd bfp efp)]))))
