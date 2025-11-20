(define ast100 (nix-list 1 2 3 4 5))
(set! result "")
(output-nix-abstract-syntax-tree ast100 fill-result)
(unless (string=? result "[\n(1)\n(2)\n(3)\n(4)\n(5)\n]\n")
  (exit FAILURE))

(define ast200 (list->nix-list (list "1" "2" "3" "4" "5")))
(set! result "")
(output-nix-abstract-syntax-tree ast200 fill-result)
(unless (string=?
         result "[\n(\"1\")\n(\"2\")\n(\"3\")\n(\"4\")\n(\"5\")\n]\n")
  (exit FAILURE))
