clear
log using code, text replace
use "/Users/yongyilin/Desktop/Duke MAE/2021 Spring/ECON 613/Final Project/df.dta"
gen logsavings = log(savings)
gen share_child = childrensize / familysize

*** Table & Graph ***
hist saving_ratio if saving_ratio < 30 & cyear == 2014
hist saving_ratio if saving_ratio < 30 & cyear == 2016
hist log_consumption_ratio if cyear == 2014
hist log_consumption_ratio if cyear == 2016
asdoc sum

*** Analysis ***
* OLS
asdoc reg saving_ratio if_nrspi_1 if_nrspi_2 logsavings familysize share_child if_health_child if_health if_hospital_child if_hospital, r nest replace
* fixed effects
xtset cyear fid12
asdoc xi:xtreg saving_ratio if_nrspi_1 if_nrspi_2 logsavings familysize share_child if_health_child if_health if_hospital_child if_hospital i.cyear, fe r nest append
* 2SLS
asdoc ivregress 2sls saving_ratio (if_nrspi_1 = if_nrspi_by_community) if_nrspi_2 logsavings familysize share_child if_health_child if_health if_hospital_child if_hospital i.cyear i.fid12, r nest append

log close
