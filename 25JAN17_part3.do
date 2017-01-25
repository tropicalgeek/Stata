*=====================================================
*=====================================================
sysuse census.dta
describe 
label variable region "Census Region"

label define cenreg 1 "N. East", modify
label define cenreg 2 "N. Central", modify
label define cenreg 3 "South", modify
label define cenreg 4 "West", modify
label values region cenreg

tab region, m 
tab region, gen(region_)

encode state2, gen(statenum)

tab statenum
tab statenum, nol
*-------------------- character plots
plot marriage divorce

*--------------------graphical plots
graph twoway (scatter marriage divorce) (lfit marriage divorce)

*-------------------- order of plots matter.. hidding of points
graph twoway (scatter marriage pop18p) (lfitci marriage pop18p)
graph twoway (lfitci marriage pop18p) (scatter marriage pop18p) 

graph twoway (lfitci marriage pop18p, ciplot(rline)) (scatter marriage pop18p) blabel(bar, size(vsmall)) 

graph bar (mean) pop (p25) pop  (p50) pop (p75) pop  (p90) pop (p99) pop, by(region, total row(1) legend(off)) blabel(bar, size(vsmall))
*--------------------
graph box medage, over(region) yscale(range(20 35)) ylabel(20(5)35)

*=====================http://www.ats.ucla.edu/stat/stata/examples/ara/arastata3.htm================================
use http://www.ats.ucla.edu/stat/stata/examples/ara/quartet, clear
plot x1 y1

graph twoway (scatter y1 x1) (lfit y1 x1), xlabel(0(5)20) ylabel(0(5)15)
graph twoway (scatter y2 x2) (lfit y2 x2), xlabel(0(5)20) ylabel(0(5)15)
graph twoway (scatter y3 x3) (lfit y3 x3), xlabel(0(5)20) ylabel(0(5)15)
graph twoway (scatter y4 x4) (lfit y4 x4), xlabel(0(5)20) ylabel(0(5)15)


set autotabgraphs on
forvalues ival=1(1)4 {
	graph twoway (scatter y`ival' x`ival') (lfitci y`ival' x`ival'), xlabel(0(5)20) ylabel(0(5)15) name(graf_`ival')
	graph save Graph "C:\1PURPLET50\DROPBOX\Dropbox\MAYRA\RAMON\BIO2\Graph_`ival'.gph"
}

set autotabgraphs on
forvalues ival=1(1)4 {
	capture graph drop graf_`ival'
	graph twoway (lfitci y`ival' x`ival') (scatter y`ival' x`ival') , xlabel(0(5)20) ylabel(0(5)15) name(graf_`ival')
	*graph save Graph "C:\1PURPLET50\DROPBOX\Dropbox\MAYRA\RAMON\BIO2\Graph_`ival'.gph"
}


*=====================================================General Social Survey, 1991-- religion
* http://www.thearda.com/Archive/Files/Downloads/GSS1991_DL2.asp
* General Social Survey, 1991.dta

cd C:\Users\vcarrion\Desktop\vc_IMHR\Dropbox\MAYRA\RAMON\BIO2\25JAN17



clonevar weight=formwt 

gen iattend_c2= i_attend >=4
tab i_attend iattend

gen happy_c2= 1 if happy==1 |happy==2
replace happy_c2=. if happy==9
replace happy_c2=0 if happy==3
*-------------------

rename *, lower
tab sex
tab1 sex age, plot

* *-------------------UNIVARIATE ANALYSES.
tab sex, gen(sex_)
tab1 sex age, plot


stem age
stem age, lines(10)

table happy
table happy, row

**------------------- Descriptive Statistics.

sum age educ
sum age educ, detail
by sex: su age educ, detail

**-------------------means
means age educ

*T-tests – ttest command -- testing the hypothesis that a variable has a specified mean.
ttest age=45
*Confidence Intervals – ci command.
ci mean educ age sei
ci mean educ age sei, level(99)

**-------------------BIVARIATE ANALYSES.
*Cross Tabulations -- tabulate command.
tab sex happy, row col cell
tab sex happy, chi2
tab sex happy, all exact
* Correlation Coefficients and Covariances
corr age educ
corr age educ prestg80, covariance
*  Pwcorr command – calculations of correlations are done on a pairwise basis.
pwcorr age educ prestg80, obs sig
* T-tests – ttest command
*Independent samples -- testing the hypothesis that two groups have the same mean on one variable.
ttest educ, by (sex)
ttest educ, by (sex) unequal
*Paired samples – testing the hypothesis that one group has the same mean on two variables.
ttest maeduc=paeduc
*One-Way ANOVA – oneway command.
oneway educ race, scheffe tabulate

MULTIVARIATE ANALYSES.
*OLS Regression – regress command.
reg educ age paeduc sex
*The following commands can be executed after the regress command to perform regression diagnostics.
rvfplot, yline(0) /* to produce a fitted residual versus fitted value plot */
avplots  /*  to produce an added-variable plot (a.k.a. partial-regression leverage plot, partial regression plot, or
    adjusted partial residual plots*/
hettest  /* – to test for heteroscedasticity*/
vif  /* – to test for multicollinearity*/




*-----------------------------
logistic happy_c2 age i.sex##i.educ [pweight=weight],  cformat(%9.2f)

*ologit command for ordered logistic regression
*mlogit command for multinomial logistic regression.

*ANOVA – anova command.
anova educ sex race sex#race

*Generalized Linear Models – glm command.


*-------------------
logistic iattend_c2 i.i_age##i_edu i.i_gender i.i_race sei [pweight=weight],  cformat(%9.2f)


*use http://www.ats.ucla.edu/stat/data/hsbanova, clear


