* Leslie Huang
* MA paper

* set up the workspace
clear all
set more off
cd "/Users/lesliehuang/Dropbox/MA-thesis-analysis/"
capture log close
log using validate_log.log, replace

set seed 1234

* import data from R: already cleaned and transformed
use validate_data.dta

* convert dates to stata format
gen statadate = date - td(01jan1960)
format statadate %td

* run logit Joint_issued ~ est state w/ linear interaction term
logit joint_dummy i.F_est_state##i.g_est_state , or
outreg2 using validate_logit, tex title("Joint statement issuance") label parenthesis(se) eform coefastr symb(***,**,*) rdec(3) bdec(3) ctitle("Coefficient") nocons r2 addstat(Pseudo R2, e(r2_p), chi2, e(N)) nonotes replace

* run reg Emo ~ est state w/ linear interaction term
reg EmoNeg i.F_est_state##i.g_est_state
outreg2 using reg_results, tex title("Joint statement sentiment") label parenthesis(se) coefastr symb(***,**,*) rdec(3) bdec(3) ctitle("Coefficient") nocons r2 addstat(Pseudo R2, e(N)) nonotes replace

reg EmoPos i.F_est_state##i.g_est_state
outreg2 using reg_results, tex title("Joint statement sentiment") label parenthesis(se) coefastr symb(***,**,*) rdec(3) bdec(3) ctitle("Coefficient") nocons r2 addstat(Pseudo R2,e(N)) nonotes append
