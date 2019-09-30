
use  "Y:\Bearbeitung\Datenaufbereitung\Rekrutierung_2018\Main\part1", clear
drop RQB3018
merge 1:1 id_l using "Y:\Bearbeitung\Datenaufbereitung\Rekrutierung_2018\Main\online1\V1\online1_GIP_original_Variablen.dta"
drop _m
replace RQB3018_o = . if paper == 1
clonevar RQB3018 = RQB3018_o
drop RQB3018_o
merge 1:1 id_l using "Y:\Bearbeitung\Orginal_Daten\Rekrutierung_2018\Main\V1\DGIP07810M_GIP_Beg2018_Teil1_MAINSAMPLE_final_20190301_offene_Fragen_01", keepusing(RQB3003_TXT)
drop _m
merge 1:1 id_l using "Y:\Bearbeitung\Datenaufbereitung\Rekrutierung_2018\Main\paper1\paper1_GIP_original_Variablen.dta"
replace RQB3018 = RQB3018_o if RQB3018 == .
drop _m
merge 1:1 id_l pagnr_frabo using  "Y:\Bearbeitung\Orginal_Daten\Rekrutierung_2018\Papierfragebogen\offene_Angaben_final.dta", keepusing(RQB3003_TXT)
drop if _m == 2
drop _m

*gender
gen gender = RQB3001
replace gender = . if RQB3001 < 0
gen female = 0 
replace female = 1 if gender == 2
replace female = . if gender == .
gen male = 0
replace male = 1 if gender == 1
replace male = . if gender == .

*age
replace RQB3018 = . if RQB3018 > 2018 | RQB3018 == -90
gen age = 2018 - RQB3018 if  RQB3018 != .
gen age16to29 = 0
replace age16to29 = 1 if age >= 16 & age <= 29 
replace age16to29 = . if age == .
gen age30to39 = 0 
replace age30to39 = 1 if age >= 30 & age <= 39 
replace age30to39 = . if age == .
gen age40to49 = 0
replace age40to49 = 1 if age >= 40 & age <= 49 
replace age40to49  = . if age == .
gen age50to59 = 0
replace age50to59 = 1 if age >= 50 & age <= 59 
replace age50to59  = . if age == .
gen age60plus = 0 
replace age60plus = 1 if age >= 60 & age < .
replace age60plus = . if age == .

gen age_cat =  . 
replace age_cat = 1 if age16to29 == 1
replace age_cat = 2 if age30to39 == 1
replace age_cat = 3 if age40to49 == 1
replace age_cat = 4 if age50to59 == 1
replace age_cat = 5 if age60plus == 1

lab def agec 1 "age 16 to 29" 2 "age 30 to 39"  3 "age 40 to 49" 4 "age 50 to 59" 5 "age 60 plus"
lab val age_cat agec

*education
gen edu_low_0 = 0
replace edu_low_0 = 1 if RQB3003==2
gen edu_low_1 = 0
replace edu_low_1 = 1 if RQB3003==3
gen edu_low = 0
replace edu_low = 1 if inlist(RQB3003, 2,3) // kein Abschluss, Hauptschule
replace edu_low = . if RQB3003 < 0 | RQB3003 ==. 
gen edu_mid = 0
replace edu_mid = 1 if  inlist(RQB3003, 1, 4) // Schüler, mittlere Reife
replace edu_mid = . if RQB3003 < 0 | RQB3003 ==.
gen edu_high = 0
replace edu_high = 1 if  inlist(RQB3003, 5,6) // Abitur, Fachabitur
replace edu_high = . if RQB3003 < 0 | RQB3003 ==.
  
* offene Angaben kodieren


#delimit ;
replace edu_low = 1 if (RQB3003_TXT == "Hauptschulabschluss nach Klasse 10" | RQB3003_TXT == "1-jährige Hauswirtschaftsschule zusätzlich" | RQB3003_TXT == "Keine" 
	| RQB3003_TXT == "Teilabschluss 7. Klasse DDR" | RQB3003_TXT == "Grundschule 8 Klasse Konditor Fachmann 3 Jahre 1983" 
	| RQB3003_TXT == "Nur die Grundschule (1- 4 Klasse) in Portugal besucht, d.h. ich besitze keinen Schulabschluss" | RQB3003_TXT == "Hauptschulabschluss 10. Klasse und zwei abgeschl. Berufsausbildungen"
	| RQB3003_TXT == "7 klasse abgang" | RQB3003_TXT == "Hauptschulabschluss + 2 Jahre um Real zu machen" | RQB3003_TXT == "10. Klasse Hauptschulabschluss" 
	| RQB3003_TXT =="Grundschule in der Türkei" | RQB3003_TXT =="Analphabet" | RQB3003_TXT =="Förderschule" 
	| RQB3003_TXT =="Kein Schulabschluss" | RQB3003_TXT =="Sonderschule für Körperbehinderte (5 Jahre), Sonderschule für Lernbehinderte (5 Jahre)" 
	| RQB3003_TXT =="qualifizierter Hauptschulabschluß" | RQB3003_TXT =="Grundschulabschluss" | RQB3003_TXT == "Qualifizierender Hauptschulabschluss" 
	| RQB3003_TXT == "Hauptschulabschluss" | RQB3003_TXT == "Analphabet" | RQB3003_TXT == "7 klasse abgang"
	| RQB3003_TXT == "qualifizierender Hauptschulabschluß" | RQB3003_TXT == "Grundschule" | RQB3003_TXT == "Förderschulabschluss" 
	| RQB3003_TXT == "ERWEITERTEN Haupts.abs." |  RQB3003_TXT == "Quali" | RQB3003_TXT =="Hauptschule/Abschluß" ) & RQB3003 == 7
;
#delimit cr
	

*replace edu_mid	

#delimit ;
replace edu_mid = 1 if (RQB3003_TXT == "High School Abschluss" | RQB3003_TXT == "Meisterschule Industrie" | RQB3003_TXT == "Qualifizierter Hauptschulabschluss 10. Klasse" 
	| RQB3003_TXT == "Realschulabschluss mit Abschluss 12 Klasse (Waldorfschule)" | RQB3003_TXT == "Meisterschule" 
	| RQB3003_TXT == "Fachschulabschluß" | RQB3003_TXT == "med. Fachschule" | RQB3003_TXT == "Gesamtschulabschluss" 
	| RQB3003_TXT == "Abiturient Zurzeit mit einer Mittleren Reife" | RQB3003_TXT == "div. Abendschule div. Berufe" 
	| RQB3003_TXT == "Berufsschulabschluss in Deutschland" | RQB3003_TXT == "Fachschule Kinderpflegerin" | RQB3003_TXT == "Landwirtschaftslehre" 
	| RQB3003_TXT == "Meisterprüfung Goldschmiedehandwerk" |RQB3003_TXT =="mit der abgeschlossenen Berufsausbildung, Mittlere Reife (davor kein Schulabschluß)" 
	| RQB3003_TXT == "Ich komme aus Kroatien. Bei uns ist 8 Klassen Grundschule + 4 Jahre Berufsschule" | RQB3003_TXT == "Bürokauffrau" 
	| RQB3003_TXT == "Meister, Betriebswirt HK, Fachkaufmann Einkauf + Logistik IHK" | RQB3003_TXT == "Berufsschule Handwerk"
	| RQB3003_TXT == "Italienischer Abschluss als Elektriker" | RQB3003_TXT == "Berufsschule" | RQB3003_TXT == "+ BKI" 
	| RQB3003_TXT == "private kaufmännische Schule" 
	| RQB3003_TXT == "Staatlich geprüfter Techniker mit Abschluss 14. Klasse" | RQB3003_TXT == "Meisterprügung in ländlicher Hauswirtschaft"  
	| RQB3003_TXT == "Mehrere Abschlüsse im Handwerk" |RQB3003_TXT == "in Marokko die Schule besucht" 
	| RQB3003_TXT == "(staatl. gep. Betriebswirt/ Gärtnermeister) siehe nächste Seite" 
	| RQB3003_TXT == "Pädagogische Fachschule" | RQB3003_TXT =="Berufsschule 3 Jahre in Wien" 
	| RQB3003_TXT == "3 jährige Lehre" | RQB3003_TXT =="Waldorfschulabschluss u. Realschulabschluss (12. Kl)" 
	| RQB3003_TXT == "Abiturient Zurzeit mit einer Mittleren Reife" | RQB3003_TXT == "Meisterschule Industrie" 
	| RQB3003_TXT == "Ausbildung als staatlich anerkannte Erzieherin" | RQB3003_TXT == "Lehre Chemielaborant" |RQB3003_TXT ==  "Techniker" 
	| RQB3003_TXT == "Wirtschaftsschule - mittlere Reife"| RQB3003_TXT == "Realschule und mache dieses Jahr mein Abitur" 
	| RQB3003_TXT == "Lehre" | RQB3003_TXT == "Polytechnische Oberschule" | RQB3003_TXT == "erweiterter Realabschluss" 
	| RQB3003_TXT == "Gesamtschulabschluss" | RQB3003_TXT == "Ausbildung 3,5 Jahre" | RQB3003_TXT == "couture" 
	| RQB3003_TXT == "Fachschule" | RQB3003_TXT == "Mittlere Reife mit Qualifikation (FORQ)"| RQB3003_TXT == "Höhere Handelsschule"  | RQB3003_TXT == "höhere Handelsschule"
	| RQB3003_TXT == "Meister für Schutz und Sicherheit" | RQB3003_TXT == "Erweiterter Realschulabschluß"  | id_l == 785000901 | id_l == 877002201
	| RQB3003_TXT == "Beruf Schule" | RQB3003_TXT == "Handwerksmeister" | RQB3003_TXT == "Handelsschule" 
	| RQB3003_TXT == "Mittlere Reife" | RQB3003_TXT == "Realschulabschluss, jedoch immer noch Schüler (Abitur)" 
	| RQB3003_TXT == "erweiterter Realschulabschluss" | RQB3003_TXT == "10 klasse" | RQB3003_TXT == "Industriemeister Metall"  ) & RQB3003 == 7
;
#delimit cr


*replace edu_high

#delimit ;
replace edu_high = 1 if (RQB3003_TXT == "Fachabitur in Wirtschaft und Verwaltung" | RQB3003_TXT == "Fachhochschule, Diplom" 
	| RQB3003_TXT == "Staatsexamen" | RQB3003_TXT == "Diplom-Ingenieur FH" |RQB3003_TXT == "+ Studium" | RQB3003_TXT == "Bachelor's degree, Master's degree" 
	| RQB3003_TXT == "Beruf mit Abitur 13 Jahre DDR" | RQB3003_TXT == "12. Klasse Abgangszeugis ohne Abitur" | RQB3003_TXT == "Dipl.-Lehrerin (Gymnasium) a.D." |
	| RQB3003_TXT == "Berufsausbildung mit Abitur" | RQB3003_TXT == "Diplomlehrerin" 
	| RQB3003_TXT == "Abitur mit Abschluss 13. Klasse" | RQB3003_TXT == "Abschluss in Arabischer Sprache" 
	| RQB3003_TXT == "Sowjetische Abitur (UdSSR)" | RQB3003_TXT == "Hochschulaqbschluss -Diplom" |RQB3003_TXT == "Fachhochschulabschluss im Ausland" 
	| RQB3003_TXT == "Universität(Master)" | RQB3003_TXT == "Abitur, Abschluss 13. Klasse (Hochschulreife)" 
	| RQB3003_TXT == "Fachhochschulreife aber inzwischen aufgrund angefangenem Studium mit 60 ECTS auch Fachgebundene Hochschulreife" 
	| RQB3003_TXT == "Universität, Promotion" |RQB3003_TXT == "abgeschlossenes Studium, Promotion" | RQB3003_TXT == "Promotion" 
	| RQB3003_TXT == "Master" | RQB3003_TXT == "Abitur in der Tschechischen Republik" | RQB3003_TXT == "11 Klassen und Studium" 
	| RQB3003_TXT == "Studium absolviert" | RQB3003_TXT == "Studium Universität" | RQB3003_TXT == "Vordiplom  Tourismus Paris" 
	| RQB3003_TXT == "Mittlere Reife + derzeit nebenberufliches Teilzeitstudium zur "Betriebswirtin"" | RQB3003_TXT == "Studium/Staatsexamen" 
	| RQB3003_TXT == "mehrere Weiterbildungen, mehrere Berufe. Krankenschwester, gesetzliche Betreuerin. BWL fast abgeschlossen." | RQB3003_TXT == "baccalauréat scientifique (Frankreich)" 
	| RQB3003_TXT == "Berufsflugzeugführen 2 Jahre/ Justizschule Abschluss/ 2 Jah." | RQB3003_TXT == "University with graduation" 
	| RQB3003_TXT == "anschließend Studium BWL" | RQB3003_TXT == "Diplombetriebswirt" |RQB3003_TXT ==  "Diplom - Uni" |
	| RQB3003_TXT == "Schulicher Teil der Fachhochschulreife" | RQB3003_TXT == "11. Klasse in Russische Föderation" 
	| RQB3003_TXT == "Ich habe ein zweisprachiges Gymnasium in Ungarn besucht, die deutsche Matura erhalten, und darauf basiert habe ich meine Ausbildung als Speditionsaufmann, und Produktionsleiter erfolgreich abgeschlossen." 
	| RQB3003_TXT == "B.o.Sc." | RQB3003_TXT == "Studium an der Fachhochschule mit Abschluss Diplom-Ingenieur" 
	| RQB3003_TXT == "allg. Hochschulreife" | RQB3003_TXT == "Doctor Chem" | RQB3003_TXT == "Bachelor" 
	| RQB3003_TXT == "Student" | RQB3003_TXT == "Abitur 13. Klasse" | RQB3003_TXT == "Diplom" 
	| RQB3003_TXT == "Studied Finance" | RQB3003_TXT == "Master of Science" | RQB3003_TXT == "Universität Abschluss" 
	| RQB3003_TXT == "M.Sc und jetzt PhD Student" | RQB3003_TXT == "Fachhochschulabschluß" | RQB3003_TXT == "Hochschule" 
	| RQB3003_TXT == "Bachelor of Sience / Maschinenbau Ingenieur" | RQB3003_TXT == "Bin momentan Studentin" 
	| RQB3003_TXT == "3. Staatsexamen" | RQB3003_TXT == "Fachhochschule (Dipl-Ing FH)" 
	| RQB3003_TXT == "Kommunikationsdesignerin" | RQB3003_TXT == "Studium" 
	| RQB3003_TXT == "Universität" | RQB3003_TXT == "Year 13 University Entrance (NZ)" 
	| RQB3003_TXT == "Studied Finance" | RQB3003_TXT == "Europäisches Bac"| RQB3003_TXT == "Dipl.Ing. ( FH )" |RQB3003_TXT ==  "Bachelor of Law" 
	| RQB3003_TXT == "Staatlich geprüfter Techniker, Fachrichtung Elektrotechnik, Schwerpunkt Nachrichtentechnik, beinhaltet Abitur" |RQB3003_TXT ==  "Doktor der Physik / Hochschullehrer" 
	| RQB3003_TXT == "Fachhochschulreife aber inzwischen aufgrund angefangenem Studium mit 60 ECTS auch Fachgebundene Hochschulreife" | RQB3003_TXT == "Bachelor of Science" 
	| RQB3003_TXT == "Hochschulabschluss" | RQB3003_TXT == "Fachabitur in Wirtschaft und Verwaltung" 
	| RQB3003_TXT == "Staatsexamen Hochschule"| RQB3003_TXT == "computer engineer degree" 
	| RQB3003_TXT == "M.B.S.E  für auslendische yunge ein kurz berufschule zwischen jahr 1981 bis 1982" 
	| RQB3003_TXT == "Fakultät" | RQB3003_TXT == "Magister" | RQB3003_TXT == "Universität  Abschluss" 
	| RQB3003_TXT == "Fachschulstudium" | RQB3003_TXT == "Berufsausbildung mit Abitur (DDR)" | RQB3003_TXT == "Britische A levels (bei 18)" | inlist(RQB3005,8, 9, 10, 11)) & RQB3003 == 7
;
#delimit cr;


*replace edu_low = . if  (RQB3003_TXT == "A"  & RQB3003 == 7)
*replace edu_mid = . if  (RQB3003_TXT == "A"  & RQB3003 == 7)
*replace edu_high = . if  (RQB3003_TXT == "A"  & RQB3003 == 7)

gen edu_cat = .
replace edu_cat = 1 if edu_low==1
replace edu_cat = 2 if edu_mid==1
replace edu_cat = 3 if edu_high==1

lab def educ 1 "low education" 2 "medium education" 3 "high education"
lab val edu_cat educ

save Y:\Bearbeitung\Projekte\Rekrutierung_2018\Data\push_to_web_and_bonus\part1_recoded, replace

