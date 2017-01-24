* vcarrion
* Biostatistics II
* 25JAN17
*=====================================================
*-------------------- entering text data in the editor
clear
input str12 (name team) byte points int minutes float test1
Capel UNC-CH 8 42 123.24
"Lang" "UNC-CH" 4 18 .666
Haywood UNC-CH 8 32 5.98765
Cota UNC-CH 5 40 .000008
Forte UNC-CH 28 39 666666666.2
Owens UNC-CH 0 1 454.454
Peppers UNC-CH 6 30 676767676
Coley Tulsa 6 24 .
Hill "Tulsa" 11 30 .
Kurtz Tulsa 11 26 0
Harrington Tulsa 2 33 9
Heard Tulsa 8 33 35.292929292
Davis Tulsa 0 6 .
"Swanson" Tulsa 15 18 .
Shelton Tulsa 2 17 0
Johnson Tulsa 0 8 1.111111
McDaniel Tulsa 0 5 .
end

list 


*--------------------importing excel file via windowing system
*https://stat.utexas.edu/images/SSC/Site/documents/stata12_dataanalysis_tutorial.pdf

* FILE --> IMPORT....

import excel "C:\Users\vcarrion\Desktop\vc_IMHR\Dropbox\MAYRA\RAMON\BIO2\purplework_\cars_1993.xls", sheet("CARS_1993") firstrow case(lower) clear


save "C:\Users\vcarrion\Desktop\vc_IMHR\Dropbox\MAYRA\RAMON\BIO2\purplework_\cars1993.dta"

des

table manu type , by(origin) c(mean citympg mean highwaympg) format(%9.3f) sc col row
graph box Price, over(Origin)

twoway (scatter Horsepower EngineSize), ytitle(Horsepower) xtitle(Engine Size)
corr citympg highwaympg engine size fueltalk


*Confidence Intervals – ci command.
ci mean price enginesize weight fueltank
by origin, sort: ci mean price enginesize weight fueltank

by origin: stem weight

*a one-sample t-test
ttest price == 15 if origin == "US”

ttest price == 15 if origin == "US"



*test of equality of variances
sdtest citympg, by(origin)
*white green yellow red
display in red "Since the two-tailed p-value is less than 0.05, we must reject the null hypothesis, which in this case is that the variances are equal"

*oneway anova: weight over types of cars
oneway weight type
*marginal means
margin

tab origin,gen(origin_)

*two way anova=differences between 2 categorical variables
anova weight type##origin_1
*test interaction term
testparm type#origin_1

*ancova=anova with a continous x variable being controlled for
*if not stated Stata assumes categoricals are entered in anova model
anova weight type##fueltank

anova weight type##c.fueltank
testparm type#c.fueltank

*final model
anova weight type c.fueltank
*------------

*simple linear regression
regress weight type c.fueltank

regress weight type##c.fueltank
testparm type#c.fueltank
*------------
regress weight c.type##c.fueltank
*what happened here?? :)
testparm type#c.fueltank
*------------

regress weight c.type##c.fueltank, coeflegend
testparm c.type#c.fueltank
*------------


glm  weight c.type##c.fueltank
testparm c.type#c.fueltank
*------------

glm  weight i.type##c.fueltank, family(gaussian) link(identity)
testparm i.type#c.fueltank

*------------
*------------Multiple linear regession

glm  weight i.type##c.fueltank i.origin, family(gaussian) link(identity)
*what happened???
encode origin, gen(originnum)
tab originnum,nol
 
tab type originnum, chi2
 
glm  weight i.type##c.fueltank i.originnum, family(gaussian) link(identity)

glm  weight i.type##i.originnum  c.fueltank , family(gaussian) link(identity)
testparm i.type#i.originnum


fvset base 2 originnum

glm  weight i.type##i.originnum  c.fueltank , family(gaussian) link(identity) coeflegend
testparm i.type#i.originnum
*------------

glm  weight i.type##i.originnum  c.fueltank , family(gaussian) link(identity) 
testparm i.type#i.originnum

regress weight i.type##i.originnum  c.fueltank 
testparm i.type#i.originnum

*The following commands can be executed after the regress command to perform regression diagnostics.
rvfplot, yline(0) /* to produce a fitted residual versus fitted value plot */
avplots  /*  to produce added-variable plot (a.k.a. partial-regression leverage plot, partial regression plot, or
    adjusted partial residual plot)  */
hettest  /* – to test for heteroscedasticity*/
vif  /* – to test for multicollinearity*/

*------------


*------------
glm  weight i.type##i.originnum  c.fueltank , family(gaussian) link(identity) 
glm  weight i.type##i.originnum  c.fueltank , family(poisson) link(log) 
poisson weight i.type##i.originnum  c.fueltank 
glm  weight i.type##i.originnum  c.fueltank , family(poisson) link(log) eform
*------------
tab type, gen(type_)
corr  weight type originnum type_1 type_2 type_3 type_4 type_5 type_6 origin_1 origin_2  fueltank 

regress weight i.type i.originnum  c.fueltank 

regress weight type_1 type_2 type_3 type_4 type_5 type_6 origin_1 origin_2  c.fueltank 

regress weight i.type i.originnum  c.fueltank , beta



*=====================================================http://www.ats.ucla.edu/stat/stata/library/anova_comp.htm


*******************
* IMPORT TEXT FILE Document1.txt
*******************

import delimited C:\Users\vcarrion\Desktop\vc_IMHR\Dropbox\MAYRA\RAMON\BIO2\purplework_\Document1.txt, delimiter(space, collapse) clear
save "C:\Users\vcarrion\Desktop\vc_IMHR\Dropbox\MAYRA\RAMON\BIO2\purplework_\document1.dta"

describe
list

table x, c(mean y n y median y sd y ) format(%9.2f) /* find mean, etc y for each level of x */

forvalues ix =1(1)4 {
display in yellow "................................x`ix'"
	label variable cat`ix' "Dummy coding of x`ix'"
}



regress y cat*, noconstant /* include all variables who start with cat ; exclude the intercept*/
test cat1 = cat2 /* linear combination .. for nonlinear testnl */
test cat1 = cat3
test cat1 = cat4

test 0.5*cat1 + 0.5*cat2 = cat3
test   5*cat1 + 4*cat2 - 3*cat3 = 2*cat4  /* 5.0 cat1 + 4.0 cat2 - 3.0 cat3 - 2.0 cat4 = 0.0 */
lincom 5*cat1 + 4*cat2 - 3*cat3 - 2*cat4  /* 5.0 cat1 + 4.0 cat2 - 3.0 cat3 - 2.0 cat4 = 0.0 */


regress y cat*, noconstant

*with constant 
regress y cat2-cat4
test _cons = _cons + cat2
test cat2

*******************
* ANOVA Document1.txt
*******************
anova y x 
regress 
regress, coeflegend
test _b[1.x] = _b[2.x] 

lincom _b[1.x]+_b[_cons]  /* level 1 */
lincom _b[2.x]+_b[_cons]  /* level 2 */
lincom _b[3.x]+_b[_cons]  /* level 3 */
lincom _b[4.x]+_b[_cons]  /* level 4 = constant = referent */

*******************
* ANOVA Document2.txt http://www.ats.ucla.edu/stat/stata/library/anova_comp.htm
*******************

import delimited C:\Users\vcarrion\Desktop\vc_IMHR\Dropbox\MAYRA\RAMON\BIO2\\purplework_\Document2.txt, delimiter(space, collapse) clear 
 
save "C:\Users\vcarrion\Desktop\vc_IMHR\Dropbox\MAYRA\RAMON\BIO2\purplework_\document2.dta", replace 
table a b, c(mean y) row col
*--------------------
anova y a b a##b /* overparameterized two-way ANOVA model */
*Cell means ANOVA model
egen c = group(a b)  /*  categorical variable that corresponds to the cells in the two-way table */
table a b, c(mean c)

anova y c, nocons
anova y c
regress

*F-test from the overparameterized ANOVA model using appropriate combinations of single degree-of-freedom tests after the cell means ANOVA model.
*Remember that c 1, 2, and 3 correspond to level 1 of a, c 4, 5, and 6 correspond to level 2 of a, and so on.)

*F-test for the term a of anova y a b a##b
test _b[1.c] + _b[2.c] + _b[3.c] = _b[4.c] + _b[5.c] + _b[6.c]
test _b[1.c] + _b[2.c] + _b[3.c] = _b[7.c] + _b[8.c] + _b[9.c],accum
test _b[1.c] + _b[2.c] + _b[3.c] = _b[10.c] + _b[11.c] + _b[12.c], accum

*F-test for the b term of anova y a b a##b
test _b[1.c]+_b[4.c]+_b[7.c]+_b[10.c]=_b[2.c]+_b[5.c]+_b[8.c]+_b[11.c]
test _b[1.c]+_b[4.c]+_b[7.c]+_b[10.c]=_b[3.c]+_b[6.c]+_b[9.c]+_b[12.c], accum

*F-test for a#b interaction term
test _b[1.c] + _b[5.c] = _b[2.c] + _b[4.c]
test _b[1.c] + _b[6.c] = _b[3.c] + _b[4.c], accum
test _b[1.c] + _b[8.c] = _b[2.c] + _b[7.c], accum
test _b[1.c] + _b[9.c] = _b[3.c] + _b[7.c], accum
test _b[1.c] + _b[11.c] = _b[2.c] + _b[10.c], accum
test _b[1.c] + _b[12.c] = _b[3.c] + _b[10.c], accum


/*
*F-test for the term a of anova y a b a##b
test _b[c[1]] + _b[c[2]] + _b[c[3]] = _b[c[4]] + _b[c[5]] + _b[c[6]]
test _b[c[1]] + _b[c[2]] + _b[c[3]] = _b[c[7]] + _b[c[8]] + _b[c[9]], accum
test _b[c[1]] + _b[c[2]] + _b[c[3]] = _b[c[10]] + _b[c[11]] + _b[c[12]], accum

*F-test for the b term of anova y a b a##b
test _b[c[1]]+_b[c[4]]+_b[c[7]]+_b[c[10]]=_b[c[2]]+_b[c[5]]+_b[c[8]]+_b[c[11]]
test _b[c[1]]+_b[c[4]]+_b[c[7]]+_b[c[10]]=_b[c[3]]+_b[c[6]]+_b[c[9]]+_b[c[12]], accum

*F-test for a#b interaction term
test _b[c[1]] + _b[c[5]] = _b[c[2]] + _b[c[4]]
test _b[c[1]] + _b[c[6]] = _b[c[3]] + _b[c[4]], accum
test _b[c[1]] + _b[c[8]] = _b[c[2]] + _b[c[7]], accum
test _b[c[1]] + _b[c[9]] = _b[c[3]] + _b[c[7]], accum
test _b[c[1]] + _b[c[11]] = _b[c[2]] + _b[c[10]], accum
test _b[c[1]] + _b[c[12]] = _b[c[3]] + _b[c[10]], accum
*/
*--------------------
 
fvset base 4 a /* referent*/
fvset base 3 b /* referent*/

 
anova y a b a##b /* overparameterized two-way ANOVA model */
regress, allbaselevels
 

*The cell mean for level i of a and level j of b is equal to the coefficient for the 
*constant plus the coefficient for a at level i plus the coefficient for b at level j 
*plus the coefficient for a and b at i and j. When a coefficient is dropped in the 
*regression table, the corresponding coefficient is zero. The table below shows the relationship. 
table a b, c(mean y) row col  /* table of cell means (and marginal means). */
 
 

 
 
 
 
*=====================================================

*=====================================================General Social Survey, 1991-- religion
* http://www.thearda.com/Archive/Files/Downloads/GSS1991_DL2.asp
* General Social Survey, 1991.dta

use "C:\Users\vcarrion\Desktop\vc_IMHR\Dropbox\MAYRA\RAMON\BIO2\25JAN17\GeneralSocialSurvey1991.dta", clear

fixvarcaps /* the program I created and we loaded in the beginning */

clonevar weight=formwt 

capture noisily gen iattend_c2= i_attend >=4
tab i_attend iattend

capture noisily gen happy_c2= 1 if happy==1 |happy==2
replace happy_c2=. if happy==9
replace happy_c2=0 if happy==3

capture noisily drop educ_c3
gen educ_c3=.
replace educ_c3= 0 if educ==0 | educ==99 
replace educ_c3= 1 if educ<=12 & educ >0 & educ!=99
replace educ_c3= 2 if educ >12 & educ!=99 & !missing(educ)
tab educ educ_c3, m

/*
*-------------------
tab sex, gen(sex_)
tab1 sex age, plot
stem age, lines(10)
table happy, row
by sex: su age educ, detail
ci means age educ sei, level(99)
tab sex happy, row col cell all

corr age educ prestg80
corr age educ prestg80, covariance
*  Pwcorr command – calculations of correlations are done on a pairwise basis.
pwcorr age educ prestg80, obs sig
* T-tests – ttest command
ttest educ, by (sex) unequal
*Paired samples – testing the hypothesis that one group has the same mean on two variables.
ttest maeduc=paeduc
*/

*One-Way ANOVA – oneway command.
oneway educ race  [w=weight], bon sidak scheffe tabulate



MULTIVARIATE ANALYSES.
*OLS Regression – regress command.
fvset base 0 educ_c3
fvset base 2 sex

reg happy age i.sex##i.educ_c3  marital race [pweight=weight],  cformat(%9.2f) coeflegend allbaselevels

reg happy age i.sex##i.educ_c3  marital race [pweight=weight],  cformat(%9.4f) 
testparm i.sex#i.educ_c3 
lincom 1.sex + 1.sex#0.educ_c3 
lincom 1.sex + 1.sex#1.educ_c3 
lincom 1.sex + 1.sex#2.educ_c3 

lincom 2.sex + 2.sex#0.educ_c3 
lincom 2.sex + 2.sex#1.educ_c3 
lincom 2.sex + 2.sex#2.educ_c3 


/*
*The following commands can be executed after the regress command to perform regression diagnostics.
rvfplot, yline(0) /* to produce a fitted residual versus fitted value plot */
avplots  /*  to produce partial plots*/
hettest  /* – to test for heteroscedasticity*/
vif  /* – to test for multicollinearity*/
*/



*-----------------------------Logistic regression
tab happy happy_c2


logistic happy_c2 age i.sex##i.educ_c3  marital race [pweight=weight],  cformat(%9.4f)

lincom 1.sex + 1.sex#0.educ_c3 
lincom 1.sex + 1.sex#1.educ_c3 
lincom 1.sex + 1.sex#2.educ_c3 

lincom 2.sex + 2.sex#0.educ_c3 
lincom 2.sex + 2.sex#1.educ_c3 
lincom 2.sex + 2.sex#2.educ_c3 


/* help glm....

If you specify both family() and link(), not all combinations	make sense.	You	may	choose	from	the
following combinations:

id  log  logit  probit  clog  pow  opower  nbinomial	loglog  logc
	
Gaussian    x   x                         x
inv. Gau.   x   x                         x
binomial    x   x     x      x       x    x     x	x      x
Poisson     x   x                         x
neg. bin.   x   x                         x              x
gamma       x   x                         x

*/


capture  log close log2
log using "C:\Users\vcarrion\Desktop\vc_IMHR\Dropbox\MAYRA\RAMON\BIO2\25JAN17\logistic_25JAN17.log", replace  text name(log2)
logistic happy_c2 age i.sex##i.educ_c3  marital race [pweight=weight],  cformat(%9.4f) coeflegend allbaselevels vsquish

logistic happy_c2 age i.sex##i.educ_c3  marital race [pweight=weight],  cformat(%9.4f)
lincom 1.sex + 1.sex#0.educ_c3 
lincom 1.sex + 1.sex#1.educ_c3 
lincom 1.sex + 1.sex#2.educ_c3 

lincom 2.sex + 2.sex#0.educ_c3 
lincom 2.sex + 2.sex#1.educ_c3 
lincom 2.sex + 2.sex#2.educ_c3 

glm happy_c2 age i.sex##i.educ_c3  marital race [pweight=weight], cformat(%9.2f) family(binomial) link(logit) eform
lincom 1.sex + 1.sex#0.educ_c3 
lincom 1.sex + 1.sex#1.educ_c3 
lincom 1.sex + 1.sex#2.educ_c3 

lincom 2.sex + 2.sex#0.educ_c3 
lincom 2.sex + 2.sex#1.educ_c3 
lincom 2.sex + 2.sex#2.educ_c3 


glm happy_c2 age i.sex##i.educ_c3  marital race [pweight=weight], cformat(%9.2f) family(poisson) link(log) 
lincom 1.sex + 1.sex#0.educ_c3 
lincom 1.sex + 1.sex#1.educ_c3 
lincom 1.sex + 1.sex#2.educ_c3 

lincom 2.sex + 2.sex#0.educ_c3 
lincom 2.sex + 2.sex#1.educ_c3 
lincom 2.sex + 2.sex#2.educ_c3 


glm happy_c2 age i.sex##i.educ_c3  marital race [pweight=weight], cformat(%9.2f) family(poisson) link(log) eform
lincom 1.sex + 1.sex#0.educ_c3 , eform
lincom 1.sex + 1.sex#1.educ_c3 , eform
lincom 1.sex + 1.sex#2.educ_c3 , eform

lincom 2.sex + 2.sex#0.educ_c3 , eform
lincom 2.sex + 2.sex#1.educ_c3 , eform
lincom 2.sex + 2.sex#2.educ_c3 , eform


capture  log close log2

















