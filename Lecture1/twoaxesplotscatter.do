set more off
clear all
set matsize 11000

/*
Project    : Resources, Two Axes Scatter Plot

Description: scatter plot two dependent variables over x with 
             two different vertical axes
             the format could be easily adapted for any twoway type plot

Basics:      
		
This version: 10/24/2014

This .do file: Jorge Luis Garcia
This project : CEHD
*/

// construct fake data
// comment in if you want to test
clear all
// generate data
set obs 10
gen id = _n  

// weights
gen w = rnormal(100,10)
replace w = w/100
global w w 

// x simple ordered index
local b = 10
generate x = _n + `b'

// y, outcome
foreach sd of numlist 1(1)3 {
	gen y`sd' = rnormal(100,`sd')
}

// x,y labels
global y1label y1
global y2label y2
global  xlabel x

// two axes plot
#delimit
twoway (scatter y1 x [aw = $w], msymbol(triangle) mfcolor (gs4) mlcolor(gs4) connect(l) lwidth(medthick) lpattern(solid) lcolor(gs4) yaxis(1))
       (scatter y2 x [aw = $w], msymbol(square)   mfcolor (gs8) mlcolor(gs8) connect(l) lwidth(medthick) lpattern(dash)  lcolor(gs8) yaxis(2))
        , 
		  legend(label(1 $y1label) label(2 $y2label) size(small))
		  xlabel(11[1]20, grid glcolor(gs14)) ylabel(, angle(h) glcolor(gs14))
		  xtitle($xlabel) ytitle($y1label, axis(1)) ytitle($labely2, axis(2))
		  graphregion(color(white)) plotregion(fcolor(white));
#delimit cr 
