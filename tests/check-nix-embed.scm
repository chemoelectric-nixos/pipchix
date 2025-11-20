(define ast100 (nix-embed "let a = 4; b = a + 5; in b"))
(set! result "")
(output-nix-abstract-syntax-tree ast100 fill-result)
(unless (string=? result "let a = 4; b = a + 5; in b")
  (exit FAILURE))

(exit SUCCESS)
