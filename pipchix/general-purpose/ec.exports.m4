do-ec
list-ec
append-ec
string-ec
string-append-ec
vector-ec
vector-of-length-ec
sum-ec
product-ec
min-ec
max-ec
any?-ec
every?-ec
first-ec
last-ec
fold-ec
fold3-ec

:list
:string
:vector
:integers
:range
:real-range
:char-range
:port
:dispatched
:do
:let
:parallel
:while
:until

;;;
;;; We do not support dispatching. It does not work in R⁶RS.
;;;
;;; :generator-proc
;;; dispatch-union
;;; :-dispatch-ref
;;; :-dispatch-set!
;;; make-initial-:-dispatch
;;; :
