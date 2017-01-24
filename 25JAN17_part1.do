*=====================================================
* vcarrion
* Biostatistics II
* 25JAN17
* example using system data file
*=====================================================
*-----------------------------------------
/*  */


*===================================================== SET UP
		capture noisily program drop fixvarcaps
		program fixvarcaps
				set more off
					foreach x of varlist * {
					 rename `x', lower
					}
				end
				*-------------------------------------
				capture noisily program drop showme
				program showme
							macro list
							return list
							ereturn list
							sreturn list
				di in yellow "creturn list"
		end

		capture program drop seehelp
		program seehelp
				*********************
				* List of helpful commands
				*********************
				di in yellow "net search coefplot"
				di in yellow "help graph"
				di in yellow "findit tablist"
				di in yellow "net search tablist"
				di in yellow "compress"
		end

*-----------------------------------------



*=====================================================EXPLORE DATA
*********************
* DATA SET auto.dta
*********************
tab;e
describe /* */

set more off /* turning automatic MORE off */
codebook /* create a codebook */
*-----------------------------------------Look at the data in the window
browse mpg-turn /* open browse data window with these variables */
br if mpg==15 /* open browse data window with this data */
*-----------------------------------------List variables and the variable labels
describe /* list variables, variable format and variable labels */
*-----------------------------------------
list
list price mpg foreign /* list data  */


tablist foreign price mpg , sort(v) clean  /* list various data */
 net search tablist

*-----------------------------------------
rename *, lower /* Renaming variables */
order foreign mpg price /* Reordering variables */
aorder /* Reordering whole data file */
*-----------------------------------------

**********Labelling variables
tab foreign, gen(foreign_)
label variable car_type "Vehicle Type"
capture label variable car_type "Vehicle Type"

label define yesnofmt 1 "Yes" 0 "No"
label value (foreign_1 foreign_2)  yesnofmt

tablist foreign foreign_1 foreign_2
*-----------------------------------------
*-----------------------------------------
sort mpg /*  sorting*/
tablist foreign rep78
gsort +foreign -rep78, mfirst /* sorting in ascending and descending simultanously */
tablist foreign rep78, sort(v) clean

*-----------------------------------------COMPUTING VARIABLES 
gen lengthsq=length*length  /*  computing a new variable*/
gen price1000=price/1000



tab rep78,m
gen rep78r=rep78
replace rep78r=0 if missing(rep78)
tab rep78r rep78,m


tab price1000,m
gen exp=0
*capture gen exp=0

replace exp=1 if price1000>=10 & !missing(price1000)

*-----------------------------------------
encode make,gen(makenum) 		/*converting string to numeric  */
*-----------------------------------------
clonevar rep78R=rep78 			/* copying a variable data and formats, etc */
replace rep78R=1 if rep78==2
replace rep78R=5 if rep78==4
tab rep78R, gen(rep78R_)

label define rep78Rfmt 1 "Low", modify
label define rep78Rfmt 3 "Med", modify
label define rep78Rfmt 5 "High",modify
label values rep78R rep78Rfmt

tab rep78R ,m 
*----------------------
egen pricecat = cut(price), group(4)
tab pricecat,m

tab foreign
tab foreign, nol
decode foreign, gen (foreign_str)
tab foreign_str,nol

des length
gen length_str=string(length)
des length_str

encode foreign_str, gen (foreign_num)
destring length_str, gen (length_num)

generate gpm=1/mpg /* * Creating a new variable – gpm. */
label var gpm "Gallons per mile"

tostring weight,gen(peso) /* destring vs tostring  */


compress /* compress binary file if possible  */

des rep*

*=========================================================================================Looking at the data
*-----------------------------------------FREQUENCIES PLOTS DESCRIPTION
describe

count

summarize price mpg weight foreign
summarize price mpg weight foreign, detail

centile price, centile(10(10)90)

tab foreign /* tabulations */
tab foreign, nol
capture noisily tab foreign, gen(foreign_)


tab displacement, plot
tab mpg, plot
tab1 foreign mpg pricecat trunk,nolabel m
tab3way pricecat foreign rep78R , alltot 

tab3way pricecat  foreign rep78R, rowpct colp format(%5.3f)

viewsource tab3way.ado /*want to look at the code used in tab3way*/


tab make, nol
encode make, gen(marca)
tab marca
tab marca, nol

*smummaries -> other tables -> compact
tabstat price mpg weight foreign
tabstat headroom rep78 weight length turn displacement, statistics( mean count cv )

by marca, sort : tabstat headroom rep78 weight length turn displacement, statistics( mean count ) missing longstub



*----------------------------------------- 
tab marca, gen(make_)
list  make_46-make_52
list  make_46-make_52, clean

*net search tablist
tablist make_46-make_52, sort(v) clean
*----------------------------------------- 

cls

foreach ic of varlist make_60-make_69 {
	di in yellow ".................................................`ic' ............."
	tab `ic', m
	sum `ic',d
}

*----------------------------------------- 

*----------------------------------------- https://www.ssc.wisc.edu/sscc/pubs/4-24.htm
plot mpg weight 
plot mpg displacement
plot marca weight
*-------------------- 
histogram mpg 
hist weight
histogram mpg, by(foreign)

histogram weight, frequency addlabel normal
histogram weight, frequency addlabel normal xlabel(0(500)5000)

by foreign, sort: tab mpg, plot
*-------------------- 
graph bar (count) mpg, over(foreign)
graph bar mpg, over(foreign) missing percentages
graph bar mpg, over(foreign) missing stack scheme(s1color)
graph bar mpg, over(foreign) missing stack scheme(s1color) scale(.5)


graph box price weight, over(foreign) 

qnorm mpg /* Quantiles of varname against quantiles of normal distribution */

scatter mpg weight || lfit mpg weight ||, by(foreign, total row(1))



**************Univariate descriptives

sktest price mpg weight
ladder mpg
return list
showme

ladder mpg, gen(mpgx)
gladder mpg, fraction
qladder mpg


logit foreign mpg
predict phat
line phat mpg,sort
scatter phat mpg

graph twoway (line phat mpg ,sort) (scatter phat mpg)
hist mpg, width(2.9) frequency kdensity

*-------------------- 

compress
save "C:\Users\vcarrion\Desktop\vc_IMHR\Dropbox\MAYRA\RAMON\BIO2\auto_modified.dta"


*===================================another example of creating tables... 
 webuse byssin
* Three-way table
table workplace smokes race [fw=pop], c(mean prob)
*Add formatting
table workplace smokes race [fw=pop], c(mean prob) format(%9.3f)
*Request supercolumn totals
table workplace smokes race [fw=pop], c(mean prob) format(%9.3f) sc




/*============================SCRATCH
sysuse auto, clear

oprobit rep78 i.foreign mpg price weight
margins foreign, at(mpg=(10(5)50)) predict(outcome(3)) saving(file1, replace)

oprobit rep78 i.foreign mpg
margins foreign, at(mpg=(10(5)50)) predict(outcome(3)) saving(file2, replace)

oprobit rep78 i.foreign mpg gear
margins foreign, at(mpg=(10(5)50)) predict(outcome(3)) saving(file3, replace)

combomarginsplot file1 file2 file3 , ///
    labels("Full model" "Restricted model" "Gear Model") noci
	
	
	
	
	
