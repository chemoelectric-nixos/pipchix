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

expand-cps-syntax~>* expand-cpsσ~>*
expand-lambda-cps-syntax~> expand-λcpsσ~>

if-free-identifier=
if-bound-identifier=

lambda-if λif

letσ letrecσ defineσ σrules
