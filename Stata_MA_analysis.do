* Leslie Huang
* MA paper

* set up the workspace
clear
set more off
cd "/Users/lesliehuang/Dropbox/MA-thesis-analysis/"
capture log close
log using MA_mnl.log, replace

* import data from R: already cleaned and transformed
use mnl_data.dta

* convert dates to stata format
gen statadate = date - td(01jan1960)
format statadate %td

* tsset statadate, daily

* need to make some factors
describe year
destring year, replace

* MNL Model #1
mlogit state_y i.state_x FARC_actions pres_approve i.year

mlogit, rrr

test 2.state_x 3.state_x

margins state_x, atmeans predict(outcome(1))
marginsplot, name(state_x) 
margins state_x, atmeans predict(outcome(2))
marginsplot, name(state_x2) 
margins state_x, atmeans predict(outcome(3))
marginsplot, name(state_x3) 
margins state_x, atmeans predict(outcome(4))
marginsplot, name(state_x4) 
graph combine state_x state_x2 state_x3 state_x4, ycommon

* Model #2
mlogit state_y i.state_x army_casualties pres_approve i.year

* Model #3
mlogit state_y i.state_x FARC_actions peace_approve i.year
