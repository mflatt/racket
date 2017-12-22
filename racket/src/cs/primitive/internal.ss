
;; Exports that are not exposed to Racket, but
;; can be used in a linklet:

(define internal-table
  (make-primitive-table
   call/cm
   extract-procedure
   set-ctl-c-handler!
   register-linklet-instantiate-continuation!
   impersonator-val
   impersonate-ref
   impersonate-set!
   struct-type-install-properties!
   structure-type-lookup-prefab-uid
   struct-type-constructor-add-guards
   register-struct-constructor!
   register-struct-predicate!
   register-struct-field-accessor!
   register-struct-field-mutator!
   struct-property-set!   
   |#%call-with-values|

   make-record-type-descriptor
   make-record-constructor-descriptor
   record-constructor
   record-predicate
   record-accessor
   record-mutator))
