use "Y:\Bearbeitung\Datenaufbereitung\Rekrutierung_2018\Main\online2\aktuell\online2_GIP_Main_Befragungsdaten.dta" , clear


* Familienstand und Nationalität
tab RQD3020
gen ledig = 0
replace ledig = 1 if  RQD3020 == 1
replace ledig = . if RQD3020 == -90
gen verheiratet =  0
replace verheiratet = 1 if RQD3020 == 2 | RQD3020 == 5
replace verheiratet = . if RQD3020 == -90
gen geschieden = 0
replace geschieden = 1 if RQD3020 == 4 |RQD3020 == 7
replace geschieden = . if RQD3020 == -90
gen verwitwet = 0
replace verwitwet = 1 if RQD3020 == 3 | RQD3020 == 6
replace verwitwet = . if RQD3020 == -90
gen marital_cat = . 
replace marital_cat = 1 if verheiratet == 1
replace marital_cat = 2 if geschieden == 1
replace marital_cat = 3 if verwitwet == 1
replace marital_cat = 4 if ledig == 1
lab def mar_lab 1 "married" 2 "divorced" 3 "widowed" 4 "never married"
lab val marital_cat mar_lab

tab RQB3007
gen german = 0 
replace german = 1 if RQB3007 == 1 |  RQB3007 == 2
replace german = . if  RQB3007 ==-90
gen other = 0
replace other = 1 if RQB3007 == 3 |  RQB3007 == 4
replace other = . if  RQB3007 ==-90

tab RQD3021
gen hh1 = 0
replace hh1 = 1 if  RQD3021 == 1
replace hh1 = . if  RQD3021 == -90
gen hh2 = 0
replace hh2 = 1 if  RQD3021 == 2
replace hh2 = . if  RQD3021 == -90
gen hh3 = 0
replace hh3 = 1 if  RQD3021 == 3
replace hh3 = . if  RQD3021 == -90
gen hh4p = 0
replace hh4 = 1 if  RQD3021 == 4 | RQD3021 == 5
replace hh4 = . if  RQD3021 == -90

gen hh_cat = . 
replace hh_cat = 1 if hh1 == 1
replace hh_cat = 2 if hh2 == 1
replace hh_cat = 3 if hh3 == 1
replace hh_cat = 4 if hh4 == 1

save Y:\Bearbeitung\Projekte\Rekrutierung_2018\Data\push_to_web_and_bonus\online2_recoded, replace

