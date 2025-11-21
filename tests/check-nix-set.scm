(define ast100
  (nix-set
   ('emojis ==> "🐶🐱🐭🐹🐰🦊🐻🐼🐻‍❄️🐨🐯🦁🐮🐷🐽🐸")))
(output-nix-abstract-syntax-tree ast100)
;;;(output-nix-abstract-syntax-tree ast100 fill-result)
;;;(unless (string=? result "({\nemojis\n=\n(builtins.fromJSON ''"\\uDB3D\\uDC36\\uDB3D\\uDC31\\uDB3D\\uDC2D\\uDB3D\\uDC39\\uDB3D\\uDC30\\uDB3E\\uDD8A\\uDB3D\\uDC3B\\uDB3D\\uDC3C\\uDB3D\\uDC3B\\u200D\\u2744\\uFE0F\\uDB3D\\uDC28\\uDB3D\\uDC2F\\uDB3E\\uDD81\\uDB3D\\uDC2E\\uDB3D\\uDC37\\uDB3D\\uDC3D\\uDB3D\\uDC38"'')\n;\n})\n")
;;;  (exit FAILURE))

(exit SUCCESS)
