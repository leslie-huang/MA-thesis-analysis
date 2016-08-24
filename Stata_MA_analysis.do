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
replace statadate = date - td(01jan1960)
format statadate %td

* need to make some factors
describe year
destring year, replace

* MNL Model #1
mlogit state_y state_x FARC_actions pres_approve
