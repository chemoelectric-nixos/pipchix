thrush λ~> lambda~>
thrush+ ~>
thrush* ~>*
thrush-and λand~> lambda-and~>
thrush+-and and~>
thrush*-and and~>*

thrush-syntax λσ~>
thrush+-syntax σ~>
thrush*-syntax σ~>*

join wind-pre wind-post wind
join* wind-pre* wind-post* wind*

cps uncps
λcps~> lambda-cps~>
cps~>*

define-cps-syntax define-cpsσ
define-uncps-syntax define-uncpsσ
cps-syntax cpsσ
uncps-syntax uncpsσ
lambda-cps-syntax~> λcpsσ~>
cps-syntax~>* cpsσ~>*

expand-thrush*-syntax expandσ~>*
expand-cps-syntax~>* expand-cpsσ~>*
bind*

lambda-if λif

alternatives cps-alternatives

letσ letrecσ defineσ σrules

if-identifier
if-free-identifier=
if-bound-identifier=
if-...
syntax-identity
syntax-values1 syntax-values2 syntax-values3
if-identifier-in-list
if-unbound-or-equiv-variable
default-initialization
has-default-initialization?
if-default-initialization-or-equiv-object
if-default-initialization-or-equiv-variable
delete-duplicate-identifiers
extract-identifiers-from-proper-list
make-identifiers-environment
syntax-proper-list-length
match-proper-list
match-proper-list-syntax
match-vector-syntax
match-literal-syntax
split-syntax-at-last-pair
split-syntax
