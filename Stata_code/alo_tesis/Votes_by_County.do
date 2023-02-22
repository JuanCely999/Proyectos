********************************************************************************
*********************** Lobbying e Ideología: **********************************
****************** Votos para House por Condados *****************************
***************** Autor: Alonso Ramírez Hernández ******************************
********************************************************************************


* Establecer entorno de trabajo
{
	
clear all 
cap log close
set more off

cd "/Users/juanpablo/Documents/Investigacion/Lobbying and Ideology/Lobbying and Ideology"
global dir "/Users/juanpablo/Documents/Investigacion/Lobbying and Ideology/Lobbying and Ideology" // Personal 

global data "$dir/Data"
global original "$data/Original"
global final "$data/Processed"
global code "$dir/Code"
global draft "$dir/Draft"
global tables "$draft/Tables"
global figures "$draft/Figures"	
}
/* Este do File agrupará las votaciones desde nivel de Precint a nivel de condado
para cada distrito congresional */ 

*********************************
*********** ALABAMA **************
*********************************
global years "2002 2004 2006 2008 2010 2012"
foreach year in $years {
	use "$original/Votes_by_Precint/AL_`year'", clear 
	collapse (sum) g`year'_USH_*, by(state year county cd)
	save "$original/Votes_by_Precint/AL_`year'_county", replace
}
*********************************
*********** ALASKA **************
*********************************

global years "2002 2004 2006 2008 2010 2012"

foreach year in $years {
	use "$original/Votes_by_Precint/AK_`year'", clear 
	if "`year'"=="2002"{
		gen cd = 1
		rename ed county 
		collapse (sum) g`year'_USH_*, by(state year county cd)
		tostring county, replace 
		save "$original/Votes_by_Precint/AK_`year'_county", replace 
	}
	else if "`year'">"2002" & "`year'"<="2010"{ 
		gen cd=1
		gen county=substr(precinct_code,1,2)
		collapse (sum) g`year'_USH_*, by(state year county cd)
		save "$original/Votes_by_Precint/AK_`year'_county", replace 
	}
	else {
		rename ld county 
		collapse (sum) g`year'_USH_*, by(state year county cd)
		tostring county, replace 
		save "$original/Votes_by_Precint/AK_`year'_county", replace 
	}
}        

************************************JP
*********************************
*********** ARKANSAS **************
*********************************
global years "2002 2004 2006 2008 2010 2012"
foreach year in $years {
	use "$original/Votes_by_Precint/AK_`year'", clear 
	collapse (sum) g`year'_USH_*, by(state year county cd)
	save "$original/Votes_by_Precint/AL_`year'_county", replace
}    

************************************JP
*********************************
*********** ARIZONA **************
*********************************
global years "2000 2002 2004 2006 2008 2012"
foreach year in $years {
	use "$original/Votes_by_Precint/AZ_`year'", clear 
	collapse (sum) g`year'_USH_*, by(state year county cd)
	save "$original/Votes_by_Precint/AL_`year'_county", replace
}                          

*************************************
*********** CALIFORNIA **************
*************************************
global years "2002 2004 2006 2008 2010"
foreach year in $years {
	use "$original/Votes_by_Precint/CA_`year'", clear 
	rename county_code county 
	collapse (sum) g`year'_USH_*, by(state year county cd)
	tostring county, replace  	
	save "$original/Votes_by_Precint/CA_`year'_county", replace
}

*************************************
************ COLORADO ***************
*************************************

* 2004
use "$original/Votes_by_Precint/CO_2004", clear 

gen precinct_code= regexs(0) if(regexm(precinct,"[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]"))
gen vtd=substr(precinct_code,6,10)
gen county_code=substr(vtd,1,2)
order state year cd county county_code precinct precinct_code vtd
save "$original/Votes_by_Precint/CO_2004", replace 

collapse (sum) g2004_USH_*, by(state year county cd)

save "$original/Votes_by_Precint/CO_2004_county", replace

* 2006
use "$original/Votes_by_Precint/CO_2006", clear 

gen cd=substr(precinct,1,1)
gen vtd=substr(precinct,6,10)
gen county_code=substr(vtd,1,2)
order state year cd county county_code precinct vtd
save "$original/Votes_by_Precint/CO_2006", replace 

collapse (sum) g2006_USH_*, by(state year county cd)

save "$original/Votes_by_Precint/CO_2006_county", replace

*2008 y 2010 
use "$original/Votes_by_Precint/CO_2008", clear 
gen county_code=substr(vtd,1,2)
*merge 1:1 county_code using "$original/Votes_by_Precint/CO_2006", keepusing(county cd)


***************************************
************ CONNECTIUT ***************
***************************************
* Listado de Towns by County 
import excel "$original/Votes_by_Precint/CT_Counties.xlsx", firstrow clear 

replace County=regexr(County,"County","")
replace County = subinstr(County," ","",.)
rename (Town County) (town county)

save "$original/Votes_by_Precint/CT_Counties", replace 

* Generado bases de datos 
global years "2002 2004 2006 2008 2010 2012"
foreach year in $years {
	if "`year'"<"2010"{ 
		use "$original/Votes_by_Precint/CT_`year'", clear 
		merge 1:1 town using "$original/Votes_by_Precint/CT_Counties"
		order state year cd cd2 county town 
		collapse (sum) g`year'_USH_*, by(state year county cd cd2)
		save "$original/Votes_by_Precint/CT_`year'_county", replace
	}
	else {
		use "$original/Votes_by_Precint/CT_`year'", clear 
		replace town="New Milford" if town=="New Millford"
		merge 1:1 town using "$original/Votes_by_Precint/CT_Counties"
		order state year cd cd2 county town 
		collapse (sum) g`year'_USH_*, by(state year county cd cd2)
		save "$original/Votes_by_Precint/CT_`year'_county", replace
	}
}
*************************************
************ DELAWARE ***************
*************************************


*************************************
************ FLORIDA ****************
*************************************

global years "2002 2004 2006 2008 2010 2012"

foreach year in $years {
import delimited "$original/Votes_by_Precint/FLORIDA/FL_`year'.txt", clear 

keep if officedesc=="United States Representative"

gen state="FL"

gen year=substr(electiondate,-4,9)
destring year, replace 
rename (juris1num juris2num canvotes) (cd cd2 g`year'_USH_)
replace partycode="dv" if partycode=="DEM"
replace partycode="rv" if partycode=="REP"
drop electiondate partyname racecode officedesc precincts precinctsreporting cannamelast cannamefirst cannamemiddle

collapse (sum) g`year'_USH_ , by(state year cd cd2 countyname countycode partycode)

reshape wide g`year'_USH_ , i(countyname countycode cd cd2) j(partycode) string
order state year cd cd2 countycode countyname  

if "`g`year'_USH_*'"!="`*dv'" | "`g`year'_USH_*'"!="`*rv'"{
	egen g`year'_USH_ov = rowtotal (g`year'_USH_*)
	}
else{
	egen g`year'_USH_tv = rowtotal (g`year'_USH_dv g`year'_USH_rv)
	}	
	
keep state year cd cd2 countycode countyname *dv *rv *tv

rename countyname county 
	
collapse (sum) g`year'_USH_*, by(state year county cd cd2)
	
save "$original/Votes_by_Precint/FL_`year'_county", replace 
}	
	
*************************************
************ GEORGIA ****************
*************************************	
	
global years "2002 2004 2006 2008 2010"

foreach year in $years {
	import excel "$original/Votes_by_Precint/GEORGIA/GA_`year'.xlsx", firstrow clear 
	save "$original/Votes_by_Precint/GA_`year'_county", replace 
	}
	
use "$original/Votes_by_Precint/GA_2012", clear 

collapse (sum) g2012_USH_*, by(state year county cd cd2 cd3 cd4)

save "$original/Votes_by_Precint/GA_2012_county", replace 


*************************************
************** HAWAI ****************
*************************************	
* PENDIENTE

*************************************
************** IDAHO ****************
*************************************	
global years "2002 2004 2006 2008 2010 2012"
foreach year in $years {
	use "$original/Votes_by_Precint/ID_`year'", clear 
	collapse (sum) g`year'_USH_*, by(state year county cd)
	save "$original/Votes_by_Precint/ID_`year'_county", replace
}

****************************************
************** ILLINOIS ****************
****************************************
* PENDIENTE 

***************************************
************** INDIANA ****************
***************************************
* PENDIENTE

*************************************
************** IOWA *****************
*************************************	
global years "2000 2004 2006 2008 2010 2012"
foreach year in $years {
	use "$original/Votes_by_Precint/IA_`year'", clear 
	collapse (sum) g`year'_USH_*, by(state year county cd)
	save "$original/Votes_by_Precint/IA_`year'_county", replace
}

***************************************
************** KANSAS *****************
***************************************
global years "2000 2002 2004 2006 2008 2010 2012"
foreach year in $years {
	if "`year'"!="2006"{	
	use "$original/Votes_by_Precint/KS_`year'", clear 
	collapse (sum) g`year'_USH_*, by(state year county cd)
	save "$original/Votes_by_Precint/KS_`year'_county", replace
		if "`year'"=="2004"{
			preserve
			keep county cd 
			save "$original/Votes_by_Precint/KS_2004_cd", replace 
			restore
		}
		else {
			
		}
	}
	else {
		use "$original/Votes_by_Precint/KS_`year'", clear 
		merge m:m county using "$original/Votes_by_Precint/KS_2004_cd"
		collapse (sum) g`year'_USH_*, by(state year county cd)
		save "$original/Votes_by_Precint/KS_`year'_county", replace
	}
}          
*****************************************
************** KENTUCKY *****************
*****************************************
global years "2000 2002 2004 2006 2008 2010 2012"

foreach year in $years {
	if "`year'"<"2010"{
		import excel "$original/Votes_by_Precint/KENTUCKY/KY_`year'.xlsx", firstrow clear 
		save "$original/Votes_by_Precint/KY_`year'_county", replace 
	}
	else {
		use "$original/Votes_by_Precint/KY_`year'", clear 
		collapse (sum) g`year'_USH_*, by(state year county cd)
		save "$original/Votes_by_Precint/KY_`year'_county", replace
	}
}

*****************************************
************** LOUISANA *****************
*****************************************	
global years "2000 2004 2006 2008 2010 2012"
foreach year in $years {
	use "$original/Votes_by_Precint/LA_`year'", clear         
	rename parish county 
	collapse (sum) g`year'_USH_*, by(state year county cd*)
	save "$original/Votes_by_Precint/LA_`year'_county", replace
}	


*****************************************
***************** MAINE *****************
*****************************************	
global years "2000 2004 2006 2008 2010 2012"
foreach year in $years {
	use "$original/Votes_by_Precint/ME_`year'", clear         
	collapse (sum) g`year'_USH_*, by(state year county cd*)
	save "$original/Votes_by_Precint/ME_`year'_county", replace
}	
	
*****************************************
************** MARYLAND *****************
*****************************************	
global years "2000 2004 2006 2008 2010 2012"
foreach year in $years {
	use "$original/Votes_by_Precint/MD_`year'", clear         
	collapse (sum) g`year'_USH_*, by(state year county cd*)
	save "$original/Votes_by_Precint/MD_`year'_county", replace
}		
**********************************************
************** MASSACHUSETTS *****************
**********************************************
import excel "$original/Votes_by_Precint/MA_Counties.xlsx", firstrow clear

save "$original/MA_Counties", replace

global years "2004 2006 2008 2010 2012"
foreach year in $years {
	use "$original/Votes_by_Precint/MA_`year'", clear 
	merge m:m town using "$original/MA_Counties"
	collapse (sum) g`year'_USH_*, by(state year county cd*)
	save "$original/Votes_by_Precint/MA_`year'_county", replace
}

*****************************************
************** MICHIGAN *****************
*****************************************
global years "2000 2004 2006 2008 2010"
foreach year in $years {
	use "$original/Votes_by_Precint/MI_`year'", clear         
	collapse (sum) g`year'_USH_*, by(state year county cd*)
	save "$original/Votes_by_Precint/MI_`year'_county", replace
}	
******************************************
************** MINNESOTA *****************
******************************************
global years "2000 2002 2004 2006 2008 2010 2012"
foreach year in $years {
	if "`year'"!="2004"{
		use "$original/Votes_by_Precint/MN_`year'", clear         
		preserve
		collapse (sum) g`year'_USH_*, by(state year county cd*)
		save "$original/Votes_by_Precint/MN_`year'_county", replace
		restore 
			if "`year'"=="2000"{
				preserve
				keep cd county fips ld_code mcd_code vtd precinct
				save "$original/Votes_by_Precint/MN_2000_fips", replace 
				restore
			}
			else {
		
			}
		}
	else {
		use "$original/Votes_by_Precint/MN_`year'", clear  
		merge 1:1 cd fips ld_code mcd_code vtd precinct using "$original/Votes_by_Precint/MN_2000_fips"
		collapse (sum) g`year'_USH_*, by(state year county vtd cd*)
		save "$original/Votes_by_Precint/MN_`year'_county", replace
	}
}		

** Arreglar 2004 
********************************************
************** MISSISSIPPI *****************
********************************************
global years "2004 2006 2008 2010 2012"
foreach year in $years {
	use "$original/Votes_by_Precint/MS_`year'", clear         
	collapse (sum) g`year'_USH_*, by(state year county cd*)
	save "$original/Votes_by_Precint/MS_`year'_county", replace
}	

*****************************************
************** MISSOURI *****************
*****************************************
global years "2004 2006 2008 2010"
foreach year in $years {
	use "$original/Votes_by_Precint/MO_`year'", clear         
	collapse (sum) g`year'_USH_*, by(state year county cd*)
	save "$original/Votes_by_Precint/MO_`year'_county", replace
}
*****************************************
************** MONTANA *****************
*****************************************
global years "2000 2002 2004 2006 2008 2010 2012"
foreach year in $years {
	if "`year'"<"2010"{	
	use "$original/Votes_by_Precint/MT_`year'", clear 
	collapse (sum) g`year'_USH_*, by(state year county cd)
	save "$original/Votes_by_Precint/MT_`year'_county", replace
		if "`year'"=="2008"{
			preserve
			keep county cd 
			save "$original/Votes_by_Precint/MT_2008_cd", replace 
			restore
		}
		else {
			
		}
	}
	else {
		use "$original/Votes_by_Precint/MT_`year'", clear 
		merge m:m county using "$original/Votes_by_Precint/MT_2008_cd"
		collapse (sum) g`year'_USH_*, by(state year county cd)
		drop if year==.
		save "$original/Votes_by_Precint/MT_`year'_county", replace
	}
}  

*****************************************
************** NEBRASKA *****************
*****************************************
global years "2004 2008 2012"
foreach year in $years {
	use "$original/Votes_by_Precint/NE_`year'", clear         
	collapse (sum) g`year'_USH_*, by(state year county cd*)
	save "$original/Votes_by_Precint/NE_`year'_county", replace
}

*****************************************
**************** NEVADA *****************
*****************************************
global years "2000 2002 2004 2006 2008 2010 2012"

foreach year in $years {
	import excel "$original/Votes_by_Precint/NEVADA/NV_`year'.xlsx", firstrow clear 
	gen state ="NV"
	keep state year cd county g2006_USH_*
	drop if year==.
	save "$original/Votes_by_Precint/NV_`year'_county", replace 
	}
************************************************
**************** NEW HAMPSHIRE *****************
************************************************
global years "2000 2002 2004 2006 2008 2010 2012"
foreach year in $years {
	use "$original/Votes_by_Precint/NH_`year'", clear         
	collapse (sum) g`year'_USH_*, by(state year county cd*)
	save "$original/Votes_by_Precint/NH_`year'_county", replace
}

*********************************************
**************** NEW JERSEY *****************
*********************************************
global years "2000 2002 2004 2008"
foreach year in $years {
	use "$original/Votes_by_Precint/NJ_`year'", clear         
	collapse (sum) g`year'_USH_*, by(state year county cd*)
	save "$original/Votes_by_Precint/NJ_`year'_county", replace
}
*Falta 2006 2010 2012
*********************************************
**************** NEW MEXICO *****************
*********************************************
import excel "$original/Votes_by_Precint/NEW MEXICO/NM_2002.xlsx", firstrow clear 
keep cd county county_code  
save "$original/Votes_by_Precint/NM_2002_cd", replace 

global years "2000 2002 2004 2006 2008 2010 2012"
foreach year in $years {
	if "`year'"=="2002" |"`year'"=="2006" |"`year'"=="2010" { 
		import excel "$original/Votes_by_Precint/NM_`year'.xlsx", firstrow clear         
		save "$original/Votes_by_Precint/NM_`year'_county", replace
	}
	else {
		use "$original/Votes_by_Precint/NM_`year'", clear  
	}
}
* TERMINAR
*********************************************
**************** NEW YORK *****************
*********************************************



use "$original/Votes_by_Precint/NY_2006", clear 

collapse (sum) g2012_USH_*, by(state year county cd cd2 cd3 cd4)

save "$original/Votes_by_Precint/GA_2012_county", replace 

	
use "$original/Votes_by_Precint/MN_2000", clear
 

global 


*************************************************
**************** NORTH CAROLINA *****************
*************************************************

global years "2000 2002 2004 2008 2010 2012"
foreach year in $years {
	use "$original/Votes_by_Precint/NC_`year'", clear         
	collapse (sum) g`year'_USH_*, by(state year county cd*)
	save "$original/Votes_by_Precint/NC_`year'_county", replace
}


*************************************************
**************** NORTH DAKOTA *****************
*************************************************

global years "2000 2002 2004 2008 2010 2012"
foreach year in $years {
	use "$original/Votes_by_Precint/ND_`year'", clear 
	gen cd=1
	collapse (sum) g`year'_USH_*, by(state year county cd*)
	save "$original/Votes_by_Precint/NC_`year'_county", replace
}




















