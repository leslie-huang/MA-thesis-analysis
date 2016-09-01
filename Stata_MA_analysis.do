* Leslie Huang
* MA paper

quietly {
* set up the workspace
clear all
set more off
cd "/Users/lesliehuang/Dropbox/MA-thesis-analysis/"
capture log close
log using MA_mnl.log, replace

set seed 1234

* import data from R: already cleaned and transformed
use mnl_data.dta

* convert dates to stata format
gen statadate = date - td(01jan1960)
format statadate %td

* tsset statadate, daily

* create an ID var
gen ID = _n

* need to make some factors
describe year
destring year, replace
}


* MNL Model #1
quietly {
mlogit state_y i.state_x , baseoutcome(3)
}
* outreg2 using mnl_table, se bfmt(fc) bdec(3) tdec(3) bracket drop(year*) label title("Multinomial Logit Model Results") eform tex replace
estimates store ll1
* get AIC and BIC
fitstat

* get the conditional odds ratios' exponentiated coefficients (e^b) to calculate the Markov transition probabilities
listcoef

* Model # 2: ADD YEAR

mlogit state_y i.state_x i.year , baseoutcome(3)
* outreg2 using mnl_table, se bfmt(fc) bdec(3) tdec(3) bracket drop(year*) label title("Multinomial Logit Model Results") eform tex append

estimates store ll2

fitstat

mlogit, rrr


* Model #3: VIOL/OPINION COVARS
mlogit state_y i.state_x FARC_actions pres_approve , baseoutcome(3)
fitstat
estimates store ll3

* Model #4: VIOL/OPINION COVARS PLUS YEAR
mlogit state_y i.state_x FARC_actions pres_approve i.year , baseoutcome(3)
estimates store ll4

**************************************************************

* tests


* Compare nested models
* di "chi2(2) = " 2*(ll2-ll1)
* di "Prob > chi2 = "chi2tail(2, 2*(ll2-ll1))

lrtest ll1 ll2

lrtest ll1 ll3 , force

lrtest ll1 ll4 , force

lrtest ll2 ll4, force

lrtest ll3 ll4, force

* get rid of factor vars because mlogtest doesn't like them
xi i.state_x i.year
mlogit state_y _Is* army_casualties pres_approve _Iy*
mlogtest, lr
mlogtest , hausman smhsiao base

***********************************************************************
* graphs graphs graphs
* We're going with Model # 4

quietly {
mlogit state_y i.state_x


margins state_x, atmeans predict(outcome(1))
marginsplot, name(state_x) 
margins state_x, atmeans predict(outcome(2))
marginsplot, name(state_x2) 
margins state_x, atmeans predict(outcome(3))
marginsplot, name(state_x3) 
margins state_x, atmeans predict(outcome(4))
marginsplot, name(state_x4) 
}
graph combine state_x state_x2 state_x3 state_x4, ycommon

* get transition probabilities (each line is a column vector)
margins, at (state_x = (1(1) 4)) predict(outcome(1))
margins, at (state_x = (1(1) 4)) predict(outcome(2))
margins, at (state_x = (1(1) 4)) predict(outcome(3))
margins, at (state_x = (1(1) 4)) predict(outcome(4))
