;;;
;;; FIXME: NOT YET WRITTEN
;;;

(define ast100
  (nix-set
   ('emojis ==> "🐶🐱🐭🐹🐰🦊🐻🐼🐻‍❄️🐨🐯🦁🐮🐷🐽🐸")))
(output-nix-abstract-syntax-tree ast100)
;;;(output-nix-abstract-syntax-tree ast100 fill-result)
;;;(unless (string=? result xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx)
;;;  (exit FAIL))

(exit PASS)
