* Encoding: UTF-8.
* Encoding: .
----------------DATA PREP START------------------------------.
VARIABLE LABELS
Q1.1 "Dear Client"
Q1.2 "Division/ School"
Q1.2_TEXT "Division/ School-TEXT"
Q1.3 "Primary role"
Q1.3_TEXT "Primary role-TEXT"
Q2.1_1 "BPA"
Q2.1_2 "Financial"
Q2.1_3 "HR"
Q2.1_4 "Public Safety"
Q2.1_5 "SIS"
Q2.1_6 "Fund Raising"
Q2.1_7 "UDW"
Q2.1_8 "Others"
Q2.1_8_TEXT "Others-TEXT"
Q2.2_x35 "Length-BPA"
Q2.2_x4 "Length-Financial"
Q2.2_x6 "Length-HR"
Q2.2_x25 "Length-Public Safety"
Q2.2_x13 "Length-SIS"
Q2.2_x2 "Length-Fund Raising"
Q2.2_x3 "Length-UDW"
Q2.2_x15 "Length-Others"
Q2.2_x15_TEXT "Length-Others-TEXT"
Q2.3_x35 "Frequently-BPA"
Q2.3_x4 "Frequently-Financial"
Q2.3_x6 "Frequently-HR"
Q2.3_x25 "Frequently-Public Safety"
Q2.3_x13 "Frequently-SIS"
Q2.3_x2 "Frequently-Fund Raising"
Q2.3_x3 "Frequently-UDW"
Q2.3_x15 "Frequently-Others"
Q2.3_x15_TEXT "Frequently-Others-TEXT"
Q3.1_1_35 "Rate-BPA-Conformance"
Q3.1_2_35 "Rate-BPA-Reliability"
Q3.1_3_35 "Rate-BPA-Assurance"
Q3.1_4_35 "Rate-BPA-Partnership"
Q3.1_5_35 "Rate-BPA-Responsiveness"
Q3.1_1_4 "Rate-Financial-Conformance"
Q3.1_2_4 "Rate-Financial-Reliability"
Q3.1_3_4 "Rate-Financial-Assurance"
Q3.1_4_4 "Rate-Financial-Partnership"
Q3.1_5_4 "Rate-Financial-Responsiveness"
Q3.1_1_6 "Rate-HR-Conformance"
Q3.1_2_6 "Rate-HR-Reliability"
Q3.1_3_6 "Rate-HR-Assurance"
Q3.1_4_6 "Rate-HR-Partnership"
Q3.1_5_6 "Rate-HR-Responsiveness"
Q3.1_1_25 "Rate-Public Safety-Conformance"
Q3.1_2_25 "Rate-Public Safety-Reliability"
Q3.1_3_25 "Rate-Public Safety-Assurance"
Q3.1_4_25 "Rate-Public Safety-Partnership"
Q3.1_5_25 "Rate-Public Safety-Responsiveness"
Q3.1_1_13 "Rate-SIS-Conformance"
Q3.1_2_13 "Rate-SIS-Reliability"
Q3.1_3_13 "Rate-SIS-Assurance"
Q3.1_4_13 "Rate-SIS-Partnership"
Q3.1_5_13 "Rate-SIS-Responsiveness"
Q3.1_1_2 "Rate-Fund Raising-Conformance"
Q3.1_2_2 "Rate-Fund Raising-Reliability"
Q3.1_3_2 "Rate-Fund Raising-Assurance"
Q3.1_4_2 "Rate-Fund Raising-Partnership"
Q3.1_5_2 "Rate-Fund Raising-Responsiveness"
Q3.1_1_3 "Rate-UDW-Conformance"
Q3.1_2_3 "Rate-UDW-Reliability"
Q3.1_3_3 "Rate-UDW-Assurance"
Q3.1_4_3 "Rate-UDW-Partnership"
Q3.1_5_3 "Rate-UDW-Responsiveness"
Q3.1_1_15 "Rate-Others-Conformance"
Q3.1_2_15 "Rate-Others-Reliability"
Q3.1_3_15 "Rate-Others-Assurance"
Q3.1_4_15 "Rate-Others-Partnership"
Q3.1_5_15 "Rate-Others-Responsiveness"
Q3.2_35 "Text-BPA"
Q3.2_4 "Text-Financial"
Q3.2_6 "Text-HR"
Q3.2_25 "Text-Public Safety"
Q3.2_13 "Text-SIS"
Q3.2_2 "Text-Fund Raising"
Q3.2_3 "Text-UDW"
Q3.2_15 "Text-Others"
Q4.1 "Overall Satisfaction Rate"
Q4.2 "Overall Satisfaction Comment"
Q5.1 "Thank you for your feedback."
.

VALUE LABELS
	/Q1.2
		1 "Abu-Dhabi"
		2 "Budget & Planning"
		3 "Finance"
		4 "HR"
		5 "IT"
		6 "Payroll"
		7 "PSO"
		8 "Public Safety"
		9 "School"
		10 "Shanghai"
		11 "Student Services"
		12 "UDAR"
		13 "Others"
	/Q1.2_TEXT
		1 "Abu-Dhabi"
		2 "Budget & Planning"
		3 "Finance"
		4 "HR"
		5 "IT"
		6 "Payroll"
		7 "PSO"
		8 "Public Safety"
		9 "School"
		10 "Shanghai"
		11 "Student Services"
		12 "UDAR"
		13 "Others".


*RECODE Q1.2 (6=3) (ELSE=Copy) INTO Div_School. 
*VARIABLE LABELS  Div_School 'Division/ School1'.

*VARIABLE LEVEL
    Div_School (NOMINAL).

*VALUE LABELS
	/Div_School
		1 "Abu-Dhabi"
		2 "Budget & Planning"
		3 "Finance"
		4 "HR"
		5 "IT"
  6 "Payroll"
		7 "PSO"
		8 "Public Safety"
		9 "School"
  10 "Shanghai"
		11 "Student Services"
		12 "UDAR"
		13 "Others".

*ALTER TYPE
, Div_School (F8).

ALTER TYPE
    Score_sum to Score_weightedStdDev (F10.2), Status (F2).

VARIABLE LEVEL
    Score_sum to Score_weightedStdDev (SCALE).

VARIABLE LEVEL
    Q2.2_x35 to Q3.1_5_15, Q4.1 (ORDINAL).

VARIABLE LEVEL Finished, Status, Q1.1 to Q2.1_8_TEXT, Q2.2_x15_TEXT
    Q2.3_x15_TEXT, Q3.2_35, Q3.2_4, Q3.2_6, Q3.2_25, Q3.2_13,
    Q3.2_2, Q3.2_3, Q3.2_15, Q5.1 (NOMINAL).

VALUE LABELS
	/Status
    0 "Normal response"
    2 "Test response"
    1 "Preview response"
    4 "Test response"
    8 "Possible spam response"
    16 "Qualtrics surveys app"
/Finished
    0 "Closed without submitting"
    1 "Submitted".


RECODE Q2.1_1 Q2.1_2 Q2.1_3 Q2.1_4 Q2.1_5 Q2.1_6 Q2.1_7 Q2.1_8 (SYSMIS=9).

VALUE LABELS
/Q2.1_1 1 "Yes" 0 "No" 9 "NA"
/Q2.1_2 1 "Yes" 0 "No" 9 "NA"
/Q2.1_3 1 "Yes" 0 "No" 9 "NA"
/Q2.1_4 1 "Yes" 0 "No" 9 "NA"
/Q2.1_5 1 "Yes" 0 "No" 9 "NA"
/Q2.1_6 1 "Yes" 0 "No" 9 "NA"
/Q2.1_7 1 "Yes" 0 "No" 9 "NA"
/Q2.1_8 1 "Yes" 0 "No" 9 "NA"
.

*----------------DATA PREP END------------------------------.
*----------------DATA TRANSFORMATION START-------------.

COMPUTE Services_count      = SUM(Q2.1_1 to Q2.1_8).
COMPUTE BPA_pct                = MEAN(Q3.1_1_35 to Q3.1_5_35)/5*100.
COMPUTE Fin_pct                  = MEAN(Q3.1_1_4 to Q3.1_5_4)/5*100.
COMPUTE HR_pct                  = MEAN(Q3.1_1_6 to Q3.1_5_6)/5*100.
COMPUTE PS_pct                  = MEAN(Q3.1_1_25 to Q3.1_5_25)/5*100.
COMPUTE SIS_pct                 = MEAN(Q3.1_1_13 to Q3.1_5_13)/5*100.
COMPUTE FR_pct                  = MEAN(Q3.1_1_2 to Q3.1_5_2)/5*100.
COMPUTE UDW_pct              = MEAN(Q3.1_1_3 to Q3.1_5_3)/5*100.
COMPUTE Others_pct            = MEAN(Q3.1_1_15 to Q3.1_5_15)/5*100.
COMPUTE Conform_pct     = MEAN(Q3.1_1_35, Q3.1_1_4, Q3.1_1_6, Q3.1_1_25, Q3.1_1_13, Q3.1_1_2, Q3.1_1_3, Q3.1_1_15)/5 * 100.
COMPUTE Reliab_pct        = MEAN(Q3.1_2_35, Q3.1_2_4, Q3.1_2_6, Q3.1_2_25, Q3.1_2_13, Q3.1_2_2, Q3.1_2_3, Q3.1_2_15)/5 * 100.
COMPUTE Assur_pct        = MEAN(Q3.1_3_35, Q3.1_3_4, Q3.1_3_6, Q3.1_3_25, Q3.1_3_13, Q3.1_3_2, Q3.1_3_3, Q3.1_3_15)/5 * 100.
COMPUTE Partner_pct      = MEAN(Q3.1_4_35, Q3.1_4_4, Q3.1_4_6, Q3.1_4_25, Q3.1_4_13, Q3.1_4_2, Q3.1_4_3, Q3.1_4_15)/5 * 100.
COMPUTE Respon_pct     = MEAN(Q3.1_5_35, Q3.1_5_4, Q3.1_5_6, Q3.1_5_25, Q3.1_5_13, Q3.1_5_2, Q3.1_5_3, Q3.1_5_15)/5 * 100.

COMPUTE Quality_pct            = MEAN(BPA_pct to Others_pct).

COMPUTE Satisfaction_pct    = Q4.1/5*100.

VARIABLE LEVEL
    Services_count to Quality_pct (SCALE), Satisfaction_pct (ORDINAL).

ALTER TYPE
    Services_count (F8).

ALTER TYPE
    BPA_pct to Satisfaction_pct (F8.1).

VARIABLE LABELS
Services_count	"Services Count"
BPA_pct    		"BPA Pct"
BPA_pct    		"BPA Pct"
Fin_pct    		"Finance Pct"
HR_pct     		"HR Pct"
PS_pct     		"Public Safety Pct"
SIS_pct    		"SIS Pct"
FR_pct     		"Fund Raising Pct"
UDW_pct    		"UDW Pct"
Others_pct 		"Others Pct"
Conform_pct		"Conformance Pct" 
Reliab_pct  	"Reliability Pct"
Assur_pct		"Assurance Pct" 
Partner_pct 	"Partnership Pct"
Respon_pct		"Responsiveness Pct"
Quality_pct  	"Quality of Service Pct"
Satisfaction_pct  	"Satisfaction Pct".

COMPUTE Target_pct=80.
VARIABLE LABELS Target_pct 'Target Pct'.
FORMATS Target_pct (F2.0).

VALUE LABEL Satisfaction_pct
    20    "Very Dissatisfied"
    40    "Dissatisfied"
    60    "Neutral"
    80    "Satisfied"
    100    "Very Satisfied".


USE ALL. 
COMPUTE filter_$=(Finished=1). 
VARIABLE LABELS filter_$ 'Finished=1 (FILTER)'. 
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'. 
FORMATS filter_$ (f1.0). 
FILTER BY filter_$.

SET SUMMARY=None TOLERANCE=1 CTemplate=None CELLSBREAK=10000 
    TABLERENDER=light ROWSBREAK=100 TLook='C:\PROGRA~1\IBM\SPSS\STATIS~1\23\Looks\ClassicAlternate.stt' TFit=Both.

*----------------DATA TRANSFORMATION END-------------.
* Custom Tables - Division/School Demo Analysis. 
CTABLES 
  /VLABELS VARIABLES=Q1.2 DISPLAY=LABEL 
  /TABLE Q1.2 [C][COUNT F40.0, COLPCT.COUNT PCT40.1, TOTALS[COUNT F40.0]] 
  /CATEGORIES VARIABLES=Q1.2 ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER.

* Custom Tables Major Apps Selection Summary. 
CTABLES 
  /VLABELS VARIABLES=Q2.1_1 Q2.1_2 Q2.1_3 Q2.1_4 Q2.1_5 Q2.1_6 Q2.1_7 Q2.1_8 
    DISPLAY=LABEL 
  /TABLE Q2.1_1 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q2.1_2 [C][COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q2.1_3 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q2.1_4 [C][COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q2.1_5 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q2.1_6 [C][COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q2.1_7 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q2.1_8 [C][COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] 
  /SLABELS POSITION=ROW 
  /CLABELS ROWLABELS=OPPOSITE 
  /CATEGORIES VARIABLES=Q2.1_1 Q2.1_2 Q2.1_3 Q2.1_4 Q2.1_5 Q2.1_6 Q2.1_7 Q2.1_8 ORDER=A KEY=VALUE 
    EMPTY=INCLUDE TOTAL=YES POSITION=AFTER.

* Custom Tables for Division and HR Apps. 
CTABLES 
  /VLABELS VARIABLES=Q2.1_1 Q2.1_2 Q2.1_3 Q2.1_4 Q2.1_5 Q2.1_6 Q2.1_7 Q2.1_8 Q1.2 
    DISPLAY=LABEL 
  /TABLE Q2.1_1 [C] + Q2.1_2 [C] + Q2.1_3 [C] + Q2.1_4 [C] + Q2.1_5 [C] + Q2.1_6 [C] + Q2.1_7 [C] + 
    Q2.1_8 [C] BY Q1.2 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] 
  /SLABELS POSITION=ROW VISIBLE=NO 
  /CATEGORIES VARIABLES=Q2.1_1 Q2.1_2 Q2.1_3 Q2.1_4 Q2.1_5 Q2.1_6 Q2.1_7 Q2.1_8 ORDER=A KEY=VALUE 
    EMPTY=EXCLUDE 
  /CATEGORIES VARIABLES=Q1.2 ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER.

* Relationship Division and Major Apps.
GRAPH 
  /BAR(STACK)=N(Q2.1_1) N(Q2.1_2) N(Q2.1_3) N(Q2.1_4) N(Q2.1_5) N(Q2.1_6) N(Q2.1_7) N(Q2.1_8) BY 
    Q1.2 
  /MISSING=LISTWISE.

* Custom Tables for Major Apps Length of Usage. 
CTABLES 
  /VLABELS VARIABLES=Q2.2_x35 Q2.2_x4 Q2.2_x6 Q2.2_x25 Q2.2_x13 Q2.2_x2 Q2.2_x3 Q2.2_x15 
    DISPLAY=LABEL 
  /TABLE Q2.2_x35 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q2.2_x4 [C][COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q2.2_x6 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q2.2_x25 [C][COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q2.2_x13 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q2.2_x2 [C][COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q2.2_x3 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q2.2_x15 [C][COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] 
  /SLABELS POSITION=ROW VISIBLE=NO 
  /CLABELS ROWLABELS=OPPOSITE 
  /CATEGORIES VARIABLES=Q2.2_x35 Q2.2_x4 Q2.2_x6 Q2.2_x25 Q2.2_x13 Q2.2_x2 Q2.2_x3 Q2.2_x15 ORDER=A 
    KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER.

* Custom Tables Major Apps Frequency of Usage. 
CTABLES 
  /VLABELS VARIABLES=Q2.3_x35 Q2.3_x4 Q2.3_x6 Q2.3_x25 Q2.3_x13 Q2.3_x2 Q2.3_x3 Q2.3_x15 
    DISPLAY=LABEL 
  /TABLE Q2.3_x35 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q2.3_x4 [C][COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q2.3_x6 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q2.3_x25 [C][COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q2.3_x13 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q2.3_x2 [C][COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q2.3_x3 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q2.3_x15 [C][COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] 
  /SLABELS POSITION=ROW VISIBLE=NO 
  /CLABELS ROWLABELS=OPPOSITE 
  /CATEGORIES VARIABLES=Q2.3_x35 Q2.3_x4 Q2.3_x6 Q2.3_x25 Q2.3_x13 Q2.3_x2 Q2.3_x3 Q2.3_x15 ORDER=A 
    KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER.

*Overall Satifaction Across different Major Apps Selected: 
* Custom Tables. 
CTABLES 
  /VLABELS VARIABLES=Q4.1 Q2.1_1 Q2.1_2 Q2.1_3 Q2.1_4 Q2.1_5 Q2.1_6 Q2.1_7 Q2.1_8 
    DISPLAY=LABEL 
  /TABLE Q4.1 [C] BY Q2.1_1 [C][COUNT F40.0] + Q2.1_2 [C][COUNT F40.0] + Q2.1_3 [C][COUNT F40.0] + 
    Q2.1_4 [C][COUNT F40.0] + Q2.1_5 [C][COUNT F40.0] + Q2.1_6 [C][COUNT F40.0] + Q2.1_7 [C][COUNT 
    F40.0] + Q2.1_8 [C][COUNT F40.0] 
  /SLABELS POSITION=ROW VISIBLE=NO 
  /CLABELS COLLABELS=OPPOSITE 
  /CATEGORIES VARIABLES=Q4.1 ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER 
  /CATEGORIES VARIABLES=Q2.1_1 Q2.1_2 Q2.1_3 Q2.1_4 Q2.1_5 Q2.1_6 Q2.1_7 Q2.1_8 ORDER=A KEY=VALUE 
    EMPTY=EXCLUDE.

* Custom Tables Overall Satisfaction Rates by Division/School. 
CTABLES 
  /VLABELS VARIABLES=Q1.2 Satisfaction_pct DISPLAY=LABEL 
  /TABLE Q1.2 [C][COUNT F40.0, TOTALS[COUNT F40.0]] BY Satisfaction_pct [C] 
  /SLABELS POSITION=ROW VISIBLE=NO 
  /CATEGORIES VARIABLES=Q1.2 Satisfaction_pct ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES 
    POSITION=AFTER.

*Q1-4.
*Nonparametric Tests: One Sample. 
NPTESTS 
  /ONESAMPLE TEST (BPA_pct Fin_pct HR_pct PS_pct SIS_pct FR_pct UDW_pct Others_pct 
    Conform_pct Reliab_pct Assur_pct Partner_pct Respon_pct Quality_pct Satisfaction_pct) WILCOXON(TESTVALUE=80) 
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
  /CRITERIA ALPHA=0.05 CILEVEL=95.


*RQ2-4.
*Nonparametric Tests: Independent Samples. 
NPAR TESTS
  /WILCOXON=BPA_pct Fin_pct HR_pct PS_pct SIS_pct FR_pct UDW_pct Others_pct Conform_pct Reliab_pct 
    Assur_pct Partner_pct Respon_pct Quality_pct Satisfaction_pct WITH Target_pct Target_pct Target_pct 
    Target_pct Target_pct Target_pct Target_pct Target_pct Target_pct Target_pct Target_pct Target_pct 
    Target_pct Target_pct Target_pct (PAIRED)
  /STATISTICS DESCRIPTIVES QUARTILES
  /MISSING ANALYSIS
  /METHOD=EXACT TIMER(5).

*RQ4.
*Nonparametric Tests: Independent Samples. 
NPTESTS 
  /INDEPENDENT TEST (Satisfaction_pct) GROUP (Q1.2) MEDIAN(TESTVALUE=80 COMPARE=PAIRWISE) 
  /MISSING SCOPE=LISTWISE USERMISSING=EXCLUDE 
  /CRITERIA ALPHA=0.05  CILEVEL=95.

*RQ1-4
*Nonparametric Tests: 2 Related (Paired) Samples Wilcoxon Signed Rank Test. 
NPAR TESTS
  /WILCOXON=BPA_pct Fin_pct HR_pct PS_pct SIS_pct FR_pct UDW_pct Others_pct Conform_pct Reliab_pct 
    Assur_pct Partner_pct Respon_pct Quality_pct Satisfaction_pct WITH Target_pct Target_pct Target_pct 
    Target_pct Target_pct Target_pct Target_pct Target_pct Target_pct Target_pct Target_pct Target_pct 
    Target_pct Target_pct Target_pct (PAIRED)
  /STATISTICS DESCRIPTIVES QUARTILES
  /MISSING ANALYSIS
  /METHOD=EXACT TIMER(5).

* SUMMARY  MEDIANS
* Chart Builder. 
* Chart Builder. 
GGRAPH 
  /GRAPHDATASET NAME="graphdataset" VARIABLES=MEDIANCI(Conform_pct, 95) MEDIANCI(Reliab_pct, 95) 
    MEDIANCI(Assur_pct, 95) MEDIANCI(Partner_pct, 95) MEDIANCI(Respon_pct, 95) MEDIANCI(Quality_pct, 
    95) MISSING=LISTWISE REPORTMISSING=NO 
    TRANSFORM=VARSTOCASES(SUMMARY="#SUMMARY" INDEX="#INDEX" LOW="#LOW" HIGH="#HIGH") 
  /GRAPHSPEC SOURCE=INLINE. 
BEGIN GPL 
  SOURCE: s=userSource(id("graphdataset")) 
  DATA: SUMMARY=col(source(s), name("#SUMMARY")) 
  DATA: INDEX=col(source(s), name("#INDEX"), unit.category()) 
  DATA: LOW=col(source(s), name("#LOW")) 
  DATA: HIGH=col(source(s), name("#HIGH")) 
  GUIDE: axis(dim(2), label("Median")) 
  GUIDE: text.footnote(label("Error Bars: 95% CI")) 
  SCALE: cat(dim(1), include("0", "1", "2", "3", "4", "5")) 
  SCALE: linear(dim(2), include(0)) 
  ELEMENT: interval(position(INDEX*SUMMARY), shape.interior(shape.square)) 
  ELEMENT: interval(position(region.spread.range(INDEX*(LOW+HIGH))), shape.interior(shape.ibeam)) 
END GPL.

GGRAPH 
  /GRAPHDATASET NAME="graphdataset" VARIABLES=MEDIANCI(Conform_pct, 95) MEDIANCI(Reliab_pct, 95) 
    MEDIANCI(Assur_pct, 95) MEDIANCI(Partner_pct, 95) MEDIANCI(Respon_pct, 95) MEDIANCI(Quality_pct, 
    95) MISSING=LISTWISE REPORTMISSING=NO 
    TRANSFORM=VARSTOCASES(SUMMARY="#SUMMARY" INDEX="#INDEX" LOW="#LOW" HIGH="#HIGH") 
  /GRAPHSPEC SOURCE=INLINE. 
BEGIN GPL 
  SOURCE: s=userSource(id("graphdataset")) 
  DATA: SUMMARY=col(source(s), name("#SUMMARY")) 
  DATA: INDEX=col(source(s), name("#INDEX"), unit.category()) 
  DATA: LOW=col(source(s), name("#LOW")) 
  DATA: HIGH=col(source(s), name("#HIGH")) 
  GUIDE: axis(dim(2), label("Median")) 
  GUIDE: text.footnote(label("Error Bars: 95% CI")) 
  SCALE: cat(dim(1), include("0", "1", "2", "3", "4", "5")) 
  SCALE: linear(dim(2), include(0)) 
  ELEMENT: interval(position(INDEX*SUMMARY), shape.interior(shape.square)) 
  ELEMENT: interval(position(region.spread.range(INDEX*(LOW+HIGH))), shape.interior(shape.ibeam)) 
END GPL.
* Custom Tables - Appendix Major Apps details. 
CTABLES 
  /VLABELS VARIABLES=Q3.1_1_4 Q3.1_2_4 Q3.1_3_4 Q3.1_4_4 Q3.1_5_4 DISPLAY=LABEL 
  /TABLE Q3.1_1_4 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1, TOTALS[COUNT F40.0, MEAN, MEDIAN]] + 
    Q3.1_2_4 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1, TOTALS[COUNT F40.0, MEAN, MEDIAN]] + Q3.1_3_4 
    [C][COUNT F40.0, ROWPCT.COUNT PCT40.1, TOTALS[COUNT F40.0, MEAN, MEDIAN]] + Q3.1_4_4 [C][COUNT 
    F40.0, ROWPCT.COUNT PCT40.1, TOTALS[COUNT F40.0, MEAN, MEDIAN]] + Q3.1_5_4 [C][COUNT F40.0, 
    ROWPCT.COUNT PCT40.1, TOTALS[COUNT F40.0, MEAN, MEDIAN]] 
  /SLABELS POSITION=ROW 
  /CLABELS ROWLABELS=OPPOSITE 
  /CATEGORIES VARIABLES=Q3.1_1_4 Q3.1_2_4 Q3.1_3_4 Q3.1_4_4 Q3.1_5_4 ORDER=A KEY=VALUE 
    EMPTY=INCLUDE TOTAL=YES POSITION=AFTER.

*RQ4 Custom Tables - Ratings by Major Apps and Quality Dimensions. 
CTABLES 
  /VLABELS VARIABLES=Q3.1_1_35 Q3.1_2_35 Q3.1_3_35 Q3.1_4_35 Q3.1_5_35 Q3.1_1_4 Q3.1_2_4 Q3.1_3_4 
    Q3.1_4_4 Q3.1_5_4 Q3.1_1_6 Q3.1_2_6 Q3.1_3_6 Q3.1_4_6 Q3.1_5_6 Q3.1_1_25 Q3.1_2_25 Q3.1_3_25 
    Q3.1_4_25 Q3.1_5_25 Q3.1_1_13 Q3.1_2_13 Q3.1_3_13 Q3.1_4_13 Q3.1_5_13 Q3.1_1_2 Q3.1_2_2 Q3.1_3_2 
    Q3.1_4_2 Q3.1_5_2 Q3.1_1_3 Q3.1_2_3 Q3.1_3_3 Q3.1_4_3 Q3.1_5_3 Q3.1_1_15 Q3.1_2_15 Q3.1_3_15 
    Q3.1_4_15 Q3.1_5_15 
    DISPLAY=LABEL 
  /TABLE Q3.1_1_35 [C][COUNT F40.0] + Q3.1_2_35 [C][COUNT F40.0] + Q3.1_3_35 [C][COUNT F40.0] + 
    Q3.1_4_35 [C][COUNT F40.0] + Q3.1_5_35 [C][COUNT F40.0] + Q3.1_1_4 [C][COUNT F40.0] + Q3.1_2_4 
    [C][COUNT F40.0] + Q3.1_3_4 [C][COUNT F40.0] + Q3.1_4_4 [C][COUNT F40.0] + Q3.1_5_4 [C][COUNT 
    F40.0] + Q3.1_1_6 [C][COUNT F40.0] + Q3.1_2_6 [C][COUNT F40.0] + Q3.1_3_6 [C][COUNT F40.0] + 
    Q3.1_4_6 [C][COUNT F40.0] + Q3.1_5_6 [C][COUNT F40.0] + Q3.1_1_25 [C][COUNT F40.0] + Q3.1_2_25 
    [C][COUNT F40.0] + Q3.1_3_25 [C][COUNT F40.0] + Q3.1_4_25 [C][COUNT F40.0] + Q3.1_5_25 [C][COUNT 
    F40.0] + Q3.1_1_13 [C][COUNT F40.0] + Q3.1_2_13 [C][COUNT F40.0] + Q3.1_3_13 [C][COUNT F40.0] + 
    Q3.1_4_13 [C][COUNT F40.0] + Q3.1_5_13 [C][COUNT F40.0] + Q3.1_1_2 [C][COUNT F40.0] + Q3.1_2_2 
    [C][COUNT F40.0] + Q3.1_3_2 [C][COUNT F40.0] + Q3.1_4_2 [C][COUNT F40.0] + Q3.1_5_2 [C][COUNT 
    F40.0] + Q3.1_1_3 [C][COUNT F40.0] + Q3.1_2_3 [C][COUNT F40.0] + Q3.1_3_3 [C][COUNT F40.0] + 
    Q3.1_4_3 [C][COUNT F40.0] + Q3.1_5_3 [C][COUNT F40.0] + Q3.1_1_15 [C][COUNT F40.0] + Q3.1_2_15 
    [C][COUNT F40.0] + Q3.1_3_15 [C][COUNT F40.0] + Q3.1_4_15 [C][COUNT F40.0] + Q3.1_5_15 [C][COUNT 
    F40.0] 
  /SLABELS VISIBLE=NO 
  /CLABELS ROWLABELS=OPPOSITE 
  /CATEGORIES VARIABLES=Q3.1_1_35 Q3.1_2_35 Q3.1_3_35 Q3.1_4_35 Q3.1_5_35 Q3.1_1_4 Q3.1_2_4 
    Q3.1_3_4 Q3.1_4_4 Q3.1_5_4 Q3.1_1_6 Q3.1_2_6 Q3.1_3_6 Q3.1_4_6 Q3.1_5_6 Q3.1_1_25 Q3.1_2_25 
    Q3.1_3_25 Q3.1_4_25 Q3.1_5_25 Q3.1_1_13 Q3.1_2_13 Q3.1_3_13 Q3.1_4_13 Q3.1_5_13 Q3.1_1_2 Q3.1_2_2 
    Q3.1_3_2 Q3.1_4_2 Q3.1_5_2 Q3.1_1_3 Q3.1_2_3 Q3.1_3_3 Q3.1_4_3 Q3.1_5_3 Q3.1_1_15 Q3.1_2_15 
    Q3.1_3_15 Q3.1_4_15 Q3.1_5_15 ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER.

* Custom Tables.
CTABLES
  /VLABELS VARIABLES=Q3.1_1_35 Q3.1_2_35 Q3.1_3_35 Q3.1_4_35 Q3.1_5_35 Q3.1_1_4 Q3.1_2_4 Q3.1_3_4 
    Q3.1_4_4 Q3.1_5_4 Q3.1_1_6 Q3.1_2_6 Q3.1_3_6 Q3.1_4_6 Q3.1_5_6 Q3.1_1_25 Q3.1_2_25 Q3.1_3_25 
    Q3.1_4_25 Q3.1_5_25 Q3.1_1_13 Q3.1_2_13 Q3.1_3_13 Q3.1_4_13 Q3.1_5_13 Q3.1_1_2 Q3.1_2_2 Q3.1_3_2 
    Q3.1_4_2 Q3.1_5_2 Q3.1_1_3 Q3.1_2_3 Q3.1_3_3 Q3.1_4_3 Q3.1_5_3 Q3.1_1_15 Q3.1_2_15 Q3.1_3_15 
    Q3.1_4_15 Q3.1_5_15 
    DISPLAY=LABEL
  /TABLE Q3.1_1_35 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_2_35 [C][COUNT 
    F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_3_35 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN 
    COMMA40.1]] + Q3.1_4_35 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_5_35 [C][COUNT 
    F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_1_4 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN 
    COMMA40.1]] + Q3.1_2_4 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_3_4 [C][COUNT 
    F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_4_4 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN 
    COMMA40.1]] + Q3.1_5_4 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_1_6 [C][COUNT 
    F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_2_6 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN 
    COMMA40.1]] + Q3.1_3_6 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_4_6 [C][COUNT 
    F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_5_6 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN 
    COMMA40.1]] + Q3.1_1_25 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_2_25 [C][COUNT 
    F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_3_25 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN 
    COMMA40.1]] + Q3.1_4_25 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_5_25 [C][COUNT 
    F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_1_13 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN 
    COMMA40.1]] + Q3.1_2_13 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_3_13 [C][COUNT 
    F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_4_13 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN 
    COMMA40.1]] + Q3.1_5_13 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_1_2 [C][COUNT 
    F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_2_2 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN 
    COMMA40.1]] + Q3.1_3_2 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_4_2 [C][COUNT 
    F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_5_2 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN 
    COMMA40.1]] + Q3.1_1_3 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_2_3 [C][COUNT 
    F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_3_3 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN 
    COMMA40.1]] + Q3.1_4_3 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_5_3 [C][COUNT 
    F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_1_15 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN 
    COMMA40.1]] + Q3.1_2_15 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_3_15 [C][COUNT 
    F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q3.1_4_15 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN 
    COMMA40.1]] + Q3.1_5_15 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]]
  /CLABELS ROWLABELS=OPPOSITE
  /CATEGORIES VARIABLES=Q3.1_1_35 Q3.1_2_35 Q3.1_3_35 Q3.1_4_35 Q3.1_5_35 Q3.1_1_4 Q3.1_2_4 
    Q3.1_3_4 Q3.1_4_4 Q3.1_5_4 Q3.1_1_6 Q3.1_2_6 Q3.1_3_6 Q3.1_4_6 Q3.1_5_6 Q3.1_1_25 Q3.1_2_25 
    Q3.1_3_25 Q3.1_4_25 Q3.1_5_25 Q3.1_1_13 Q3.1_2_13 Q3.1_3_13 Q3.1_4_13 Q3.1_5_13 Q3.1_1_2 Q3.1_2_2 
    Q3.1_3_2 Q3.1_4_2 Q3.1_5_2 Q3.1_1_3 Q3.1_2_3 Q3.1_3_3 Q3.1_4_3 Q3.1_5_3 Q3.1_1_15 Q3.1_2_15 
    Q3.1_3_15 Q3.1_4_15 Q3.1_5_15 ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /TITLES
    TITLE='Major Applications Mean Quality Ratings'.

* Custom Tables.
CTABLES
  /VLABELS VARIABLES=Q3.1_1_35 Q3.1_2_35 Q3.1_3_35 Q3.1_4_35 Q3.1_5_35 Q3.1_1_4 Q3.1_2_4 Q3.1_3_4 
    Q3.1_4_4 Q3.1_5_4 Q3.1_1_6 Q3.1_2_6 Q3.1_3_6 Q3.1_4_6 Q3.1_5_6 Q3.1_1_25 Q3.1_2_25 Q3.1_3_25 
    Q3.1_4_25 Q3.1_5_25 Q3.1_1_13 Q3.1_2_13 Q3.1_3_13 Q3.1_4_13 Q3.1_5_13 Q3.1_1_2 Q3.1_2_2 Q3.1_3_2 
    Q3.1_4_2 Q3.1_5_2 Q3.1_1_3 Q3.1_2_3 Q3.1_3_3 Q3.1_4_3 Q3.1_5_3 Q3.1_1_15 Q3.1_2_15 Q3.1_3_15 
    Q3.1_4_15 Q3.1_5_15 
    DISPLAY=LABEL
  /TABLE Q3.1_1_35 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_2_35 [C][COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q3.1_3_35 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_4_35 [C][COUNT F40.0, 
    ROWPCT.COUNT PCT40.1] + Q3.1_5_35 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_1_4 [C][COUNT 
    F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_2_4 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_3_4 [C][COUNT 
    F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_4_4 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_5_4 [C][COUNT 
    F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_1_6 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_2_6 [C][COUNT 
    F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_3_6 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_4_6 [C][COUNT 
    F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_5_6 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_1_25 
    [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_2_25 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + 
    Q3.1_3_25 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_4_25 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] 
    + Q3.1_5_25 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_1_13 [C][COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q3.1_2_13 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_3_13 [C][COUNT F40.0, 
    ROWPCT.COUNT PCT40.1] + Q3.1_4_13 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_5_13 [C][COUNT 
    F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_1_2 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_2_2 [C][COUNT 
    F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_3_2 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_4_2 [C][COUNT 
    F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_5_2 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_1_3 [C][COUNT 
    F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_2_3 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_3_3 [C][COUNT 
    F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_4_3 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_5_3 [C][COUNT 
    F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_1_15 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_2_15 
    [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_3_15 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + 
    Q3.1_4_15 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_5_15 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1]
  /SLABELS POSITION=ROW
  /CLABELS ROWLABELS=OPPOSITE
  /CATEGORIES VARIABLES=Q3.1_1_35 Q3.1_2_35 Q3.1_3_35 Q3.1_4_35 Q3.1_5_35 Q3.1_1_4 Q3.1_2_4 
    Q3.1_3_4 Q3.1_4_4 Q3.1_5_4 Q3.1_1_6 Q3.1_2_6 Q3.1_3_6 Q3.1_4_6 Q3.1_5_6 Q3.1_1_25 Q3.1_2_25 
    Q3.1_3_25 Q3.1_4_25 Q3.1_5_25 Q3.1_1_13 Q3.1_2_13 Q3.1_3_13 Q3.1_4_13 Q3.1_5_13 Q3.1_1_2 Q3.1_2_2 
    Q3.1_3_2 Q3.1_4_2 Q3.1_5_2 Q3.1_1_3 Q3.1_2_3 Q3.1_3_3 Q3.1_4_3 Q3.1_5_3 Q3.1_1_15 Q3.1_2_15 
    Q3.1_3_15 Q3.1_4_15 Q3.1_5_15 ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /TITLES
    TITLE='Major Applications Quality Attribute Rates'.

* Custom Tables. 
CTABLES 
  /VLABELS VARIABLES=Q2.2_x35 Q2.2_x4 Q2.2_x6 Q2.2_x25 Q2.2_x13 Q2.2_x2 Q2.2_x3 Q2.2_x15 
    DISPLAY=LABEL 
  /TABLE Q2.2_x35 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q2.2_x4 [C][COUNT F40.0, 
    TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q2.2_x6 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN 
    COMMA40.1]] + Q2.2_x25 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q2.2_x13 [C][COUNT 
    F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q2.2_x2 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN 
    COMMA40.1]] + Q2.2_x3 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] + Q2.2_x15 [C][COUNT 
    F40.0, TOTALS[COUNT F40.0, MEAN COMMA40.2]] 
  /CLABELS ROWLABELS=OPPOSITE 
  /CATEGORIES VARIABLES=Q2.2_x35 Q2.2_x4 Q2.2_x6 Q2.2_x25 Q2.2_x13 Q2.2_x2 Q2.2_x3 Q2.2_x15 ORDER=A 
    KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
 /TITLES
    TITLE='Major Applications Mean Length of Usage'.

DATASET ACTIVATE DataSet1.
* Custom Tables.
CTABLES
  /VLABELS VARIABLES=Q2.2_x35 Q2.2_x4 Q2.2_x6 Q2.2_x25 Q2.2_x13 Q2.2_x2 Q2.2_x3 Q2.2_x15 
    DISPLAY=LABEL
  /TABLE Q2.2_x35 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1, TOTALS[COUNT F40.0, ROWPCT.COUNT PCT40.1, 
    MEDIAN]] + Q2.2_x4 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1, TOTALS[COUNT F40.0, ROWPCT.COUNT PCT40.1, 
    MEDIAN]] + Q2.2_x6 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1, TOTALS[COUNT F40.0, ROWPCT.COUNT PCT40.1, 
    MEDIAN]] + Q2.2_x25 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1, TOTALS[COUNT F40.0, ROWPCT.COUNT 
    PCT40.1, MEDIAN]] + Q2.2_x13 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1, TOTALS[COUNT F40.0, 
    ROWPCT.COUNT PCT40.1, MEDIAN]] + Q2.2_x2 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1, TOTALS[COUNT F40.0, 
    ROWPCT.COUNT PCT40.1, MEDIAN]] + Q2.2_x3 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1, TOTALS[COUNT F40.0, 
    ROWPCT.COUNT PCT40.1, MEDIAN]] + Q2.2_x15 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1, TOTALS[COUNT 
    F40.0, ROWPCT.COUNT PCT40.1, MEDIAN]]
  /SLABELS POSITION=ROW
  /CLABELS ROWLABELS=OPPOSITE
  /CATEGORIES VARIABLES=Q2.2_x35 Q2.2_x4 Q2.2_x6 Q2.2_x25 Q2.2_x13 Q2.2_x2 Q2.2_x3 Q2.2_x15 ORDER=A 
    KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
 /TITLES
    TITLE='Major Applications Length of Usage'.



* Custom Tables.
CTABLES
  /VLABELS VARIABLES=Q2.3_x35 Q2.3_x4 Q2.3_x6 Q2.3_x25 Q2.3_x13 Q2.3_x2 Q2.3_x3 Q2.3_x15 
    DISPLAY=LABEL
  /TABLE Q2.3_x35 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEDIAN COMMA40.2]] + Q2.3_x4 [C][COUNT F40.0, 
    TOTALS[COUNT F40.0, MEDIAN COMMA40.2]] + Q2.3_x6 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEDIAN 
    COMMA40.1]] + Q2.3_x25 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEDIAN COMMA40.2]] + Q2.3_x13 [C][COUNT 
    F40.0, TOTALS[COUNT F40.0, MEDIAN COMMA40.2]] + Q2.3_x2 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEDIAN 
    COMMA40.1]] + Q2.3_x3 [C][COUNT F40.0, TOTALS[COUNT F40.0, MEDIAN COMMA40.2]] + Q2.3_x15 [C][COUNT 
    F40.0, TOTALS[COUNT F40.0, MEDIAN COMMA40.2]]
  /CLABELS ROWLABELS=OPPOSITE
  /CATEGORIES VARIABLES=Q2.3_x35 Q2.3_x4 Q2.3_x6 Q2.3_x25 Q2.3_x13 Q2.3_x2 Q2.3_x3 Q2.3_x15 ORDER=A 
    KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
 /TITLES
    TITLE='Major Applications MEDIAN Frequency of Usage'.


* Custom Tables.
CTABLES
  /VLABELS VARIABLES=Q3.1_1_35 Q3.1_1_4 Q3.1_1_6 Q3.1_1_25 Q3.1_1_13 Q3.1_1_2 Q3.1_1_3 Q3.1_1_15 
    DISPLAY=LABEL
  /TABLE Q3.1_1_35 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_1_4 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q3.1_1_6 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_1_25 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q3.1_1_13 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_1_2 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q3.1_1_3 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_1_15 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1]
  /SLABELS POSITION=ROW
  /CLABELS ROWLABELS=OPPOSITE
  /CATEGORIES VARIABLES=Q3.1_1_35 Q3.1_1_4 Q3.1_1_6 Q3.1_1_25 Q3.1_1_13 Q3.1_1_2 Q3.1_1_3 Q3.1_1_15 
    ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
 /TITLES
    TITLE='Quality Conformance Rates'.


* Custom Tables.
CTABLES
  /VLABELS VARIABLES=Q3.1_2_35 Q3.1_2_4 Q3.1_2_6 Q3.1_2_25 Q3.1_2_13 Q3.1_2_2 Q3.1_2_3 Q3.1_2_15 
    DISPLAY=LABEL
  /TABLE Q3.1_2_35 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_2_4 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q3.1_2_6 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_2_25 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q3.1_2_13 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_2_2 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q3.1_2_3 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_2_15 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1]
  /SLABELS POSITION=ROW
  /CLABELS ROWLABELS=OPPOSITE
  /CATEGORIES VARIABLES=Q3.1_2_35 Q3.1_2_4 Q3.1_2_6 Q3.1_2_25 Q3.1_2_13 Q3.1_2_2 Q3.1_2_3 Q3.1_2_15 
    ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
 /TITLES
    TITLE='Quality Responsiveness Rates'.

* Custom Tables.
CTABLES
  /VLABELS VARIABLES=Q3.1_3_35 Q3.1_3_4 Q3.1_3_6 Q3.1_3_25 Q3.1_3_13 Q3.1_3_2 Q3.1_3_3 Q3.1_3_15 
    DISPLAY=LABEL
  /TABLE Q3.1_3_35 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_3_4 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q3.1_3_6 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_3_25 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q3.1_3_13 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_3_2 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q3.1_3_3 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_3_15 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1]
  /SLABELS POSITION=ROW
  /CLABELS ROWLABELS=OPPOSITE
  /CATEGORIES VARIABLES=Q3.1_3_35 Q3.1_3_4 Q3.1_3_6 Q3.1_3_25 Q3.1_3_13 Q3.1_3_2 Q3.1_3_3 Q3.1_3_15 
    ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
 /TITLES
    TITLE='Quality Assurance Rates'.

* Custom Tables.
CTABLES
  /VLABELS VARIABLES=Q3.1_4_35 Q3.1_4_4 Q3.1_4_6 Q3.1_4_25 Q3.1_4_13 Q3.1_4_2 Q3.1_4_3 Q3.1_4_15 
    DISPLAY=LABEL
  /TABLE Q3.1_4_35 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_4_4 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q3.1_4_6 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_4_25 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q3.1_4_13 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_4_2 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q3.1_4_3 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_4_15 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1]
  /SLABELS POSITION=ROW
  /CLABELS ROWLABELS=OPPOSITE
  /CATEGORIES VARIABLES=Q3.1_4_35 Q3.1_4_4 Q3.1_4_6 Q3.1_4_25 Q3.1_4_13 Q3.1_4_2 Q3.1_4_3 Q3.1_4_15 
    ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
 /TITLES
    TITLE='Quality Partnership Rates'.

* Custom Tables.
CTABLES
  /VLABELS VARIABLES=Q3.1_5_35 Q3.1_5_4 Q3.1_5_6 Q3.1_5_25 Q3.1_5_13 Q3.1_5_2 Q3.1_5_3 Q3.1_5_15 
    DISPLAY=LABEL
  /TABLE Q3.1_5_35 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_5_4 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q3.1_5_6 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_5_25 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q3.1_5_13 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_5_2 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1] + Q3.1_5_3 [COUNT F40.0, ROWPCT.COUNT PCT40.1] + Q3.1_5_15 [COUNT F40.0, ROWPCT.COUNT 
    PCT40.1]
  /SLABELS POSITION=ROW
  /CLABELS ROWLABELS=OPPOSITE
  /CATEGORIES VARIABLES=Q3.1_5_35 Q3.1_5_4 Q3.1_5_6 Q3.1_5_25 Q3.1_5_13 Q3.1_5_2 Q3.1_5_3 Q3.1_5_15 
    ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
 /TITLES
    TITLE='Quality Responsiveness Rates'.

*========ANALYSIS START=========.
* ANOVA for two independent variables (Division IT and Roles) with 
*    different dimensions - Overall Rate, Overall Mean, Conformance, Responsiveness, Responsiveness, Assurance, Partnership
* ANOVA compared the variance (variability in scores) between different groups (believed to be due to 
*    indipendent variables - Div IT and Rols), with the variability within each of the groups (believed to be due to chance).

*1) Frequency % Stacked Bar for Overall Rating by Divisions
*2) Frequency % Stacked Bar for Overall Rating by Primary Roles.

* Chart Builder. 
GGRAPH 
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Q1.2[name="Q1_2"] COUNT()[name="COUNT"] Satisfied 
    MISSING=LISTWISE REPORTMISSING=NO 
  /GRAPHSPEC SOURCE=INLINE
BEGIN GPL 
  SOURCE: s=userSource(id("graphdataset")) 
  DATA: Q1_2=col(source(s), name("Q1_2"), unit.category()) 
  DATA: COUNT=col(source(s), name("COUNT")) 
  DATA: Satisfied=col(source(s), name("Satisfied"), unit.category()) 
  GUIDE: axis(dim(1), label("Division/School")) 
  GUIDE: axis(dim(2), label("Percent")) 
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("Satisfied")) 
  SCALE: cat(dim(1), include("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12" 
, "13")) 
  SCALE: linear(dim(2), include(0)) 
  SCALE: cat(aesthetic(aesthetic.color.interior), include("0", "1")) 
  ELEMENT: interval.stack(position(summary.percent(Q1_2*COUNT, base.coordinate(dim(1)))), 
    color.interior(Satisfied), shape.interior(shape.square)) 
END GPL.

GGRAPH 
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Q4.1[name="Q4_1"] COUNT()[name="COUNT"] 
    Q1.3[name="Q1_3"] MISSING=LISTWISE REPORTMISSING=NO 
  /GRAPHSPEC SOURCE=INLINE. 
BEGIN GPL 
  SOURCE: s=userSource(id("graphdataset")) 
  DATA: Q4_1=col(source(s), name("Q4_1"), unit.category()) 
  DATA: COUNT=col(source(s), name("COUNT")) 
  DATA: Q1_3=col(source(s), name("Q1_3"), unit.category()) 
  GUIDE: axis(dim(1), label("Overall Rate")) 
  GUIDE: axis(dim(2), label("Percent")) 
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("Primary role")) 
  SCALE: linear(dim(2), include(0)) 
  SCALE: cat(aesthetic(aesthetic.color.interior), include("1", "2", "3", "4", "5", "6")) 
  ELEMENT: interval.stack(position(summary.percent(Q4_1*COUNT, base.all(acrossPanels()))), 
    color.interior(Q1_3), shape.interior(shape.square)) 
END GPL.

*3) Chi-Square test to determine whether there is a significant difference between overall rate with respect to IT and Non-IT respondents.
*4).	Chi-Square test to determine whether there is a significant difference between overall rate with respect to Primary Roles.
CROSSTABS 
  /TABLES=Division_IT Q1.3 BY Q4.1 
  /FORMAT=AVALUE TABLES 
  /STATISTICS=CHISQ CC ETA CORR BTAU 
  /CELLS=COUNT EXPECTED ROW PROP 
  /COUNT ROUND CELL.

*5) Frequency % Distribution Stacked Bar for Overall Mean by Division_IT.
*6) Frequency % Distribution Stacked Bar for Overall Mean by Primary Roles.

GGRAPH 
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Overall_mean Division_IT MISSING=LISTWISE 
    REPORTMISSING=NO 
  /GRAPHSPEC SOURCE=INLINE. 
BEGIN GPL 
  SOURCE: s=userSource(id("graphdataset")) 
  DATA: Overall_mean=col(source(s), name("Overall_mean")) 
  DATA: Division_IT=col(source(s), name("Division_IT"), unit.category()) 
  GUIDE: axis(dim(1), label("Overall_mean")) 
  GUIDE: axis(dim(2), label("Frequency")) 
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("Division_IT")) 
  SCALE: cat(aesthetic(aesthetic.color.interior), include("0", "1")) 
  ELEMENT: interval.stack(position(summary.count(bin.rect(Overall_mean))), 
    color.interior(Division_IT), shape.interior(shape.square)) 
  ELEMENT: line(position(density.normal(Overall_mean)), color("Normal")) 
END GPL.
GGRAPH 
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Overall_mean Q1.3[name="Q1_3"] MISSING=LISTWISE 
    REPORTMISSING=NO 
  /GRAPHSPEC SOURCE=INLINE. 
BEGIN GPL 
  SOURCE: s=userSource(id("graphdataset")) 
  DATA: Overall_mean=col(source(s), name("Overall_mean")) 
  DATA: Q1_3=col(source(s), name("Q1_3"), unit.category()) 
  GUIDE: axis(dim(1), label("Overall_mean")) 
  GUIDE: axis(dim(2), label("Frequency")) 
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("Primary role")) 
  SCALE: cat(aesthetic(aesthetic.color.interior), include("1", "2", "3", "4", "5", "6")) 
  ELEMENT: interval.stack(position(summary.count(bin.rect(Overall_mean))), color.interior(Q1_3), 
    shape.interior(shape.square)) 
  ELEMENT: line(position(density.normal(Overall_mean)), color("Normal")) 
END GPL.

*Gaps between perception and expectations toward Service Quality Overall Mean and Rating by Division_IT (Independent Samples T-Test)
T-TEST GROUPS=Division_IT(0 1) 
  /MISSING=ANALYSIS 
  /VARIABLES=Q4.1 Overall_mean 
  /CRITERIA=CI(.95).

*ANOVA Test - Significant differences in service quality in the mean various among the responses given by IT and NOT_IT..
ONEWAY Overall_mean Q4.1 Conformance_Mean Responsiveness_Mean Assurance_Mean Partnership_Mean 
    Responsiveness_Mean BY Division_IT 
  /STATISTICS DESCRIPTIVES EFFECTS HOMOGENEITY 
  /PLOT MEANS 
  /MISSING ANALYSIS.

* Check if the assumption of normality is met - One Sample Chi Square for Overall Rate Likert Item and K-S Test for all scale variables.
NPTESTS 
  /ONESAMPLE TEST (Q4.1, Overall_mean Conformance_Mean Responsiveness_Mean Assurance_Mean Partnership_Mean Responsiveness_Mean) 
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
  /CRITERIA ALPHA=0.05 CILEVEL=95.

* Alpha Coefficients and intercorrelations among subscale on SERVUAL: Check the internal consistency (Responsiveness) of Likert scales.
*measure of internal consistency ("Responsiveness"). It is most commonly used when you have multiple 
*Likert questions in a survey/questionnaire that form a scale and you wish to determine if the scale is reliable.


* H0: No significant differences between customer's satsfaction and quality of services for IT and NOT IT or by roles.

*========ANALYSIS END=========.

DATASET ACTIVATE.
TABLES
                /TABLE    
                Q3.2_5_1      +Q3.2_1_1      +Q3.2_2_1     +Q3.2_3_1      +Q3.2_4_1    +
                Q3.2_5_2      +Q3.2_1_2      +Q3.2_2_2     +Q3.2_3_2      +Q3.2_4_2    +
                Q3.2_5_3      +Q3.2_1_3      +Q3.2_2_3     +Q3.2_3_3      +Q3.2_4_3    +
                Q3.2_5_4      +Q3.2_1_4      +Q3.2_2_4     +Q3.2_3_4      +Q3.2_4_4    +
                Q3.2_5_5      +Q3.2_1_5      +Q3.2_2_5     +Q3.2_3_5      +Q3.2_4_5    +
                Q3.2_5_6      +Q3.2_1_6      +Q3.2_2_6     +Q3.2_3_6      +Q3.2_4_6    +
                Q3.2_5_7      +Q3.2_1_7      +Q3.2_2_7     +Q3.2_3_7      +Q3.2_4_7    +
                Q3.2_5_8      +Q3.2_1_8      +Q3.2_2_8     +Q3.2_3_8      +Q3.2_4_8    +
                Q3.2_5_11    +Q3.2_1_11    +Q3.2_2_11    +Q3.2_3_11    +Q3.2_4_11    +
                Q3.2_5_9      +Q3.2_1_9      +Q3.2_2_9     +Q3.2_3_9      +Q3.2_4_9    +
                Q3.2_5_12    +Q3.2_1_12    +Q3.2_2_12    +Q3.2_3_12    +Q3.2_4_12    +
                Q3.2_5_13    +Q3.2_1_13    +Q3.2_2_13    +Q3.2_3_13    +Q3.2_4_13    +
                Q3.2_5_14    +Q3.2_1_14    +Q3.2_2_14    +Q3.2_3_14    +Q3.2_4_14    +
                Q3.2_5_10    +Q3.2_1_10    +Q3.2_2_10    +Q3.2_3_10    +Q3.2_4_10    +
                Q3.2_5_16    +Q3.2_1_16    +Q3.2_2_16    +Q3.2_3_16    +Q3.2_4_16    +
                Q3.2_5_15    +Q3.2_1_15    +Q3.2_2_15    +Q3.2_3_15    +Q3.2_4_15
                by    (labels)   >    (statistics)
                /STATISTICS    COUNT('qcnt'(F3.0))    CPCT('qpct')
                /TITLE='Summary Results of Q3.2 - Quality of Service'.

*Run Analyze/Nonparametric Tests/1-Sample K-S Test to test null hypothesis that distribution is normal for all the variables.

*QUALITY DIMENSIONS ANALYSIS.

* Custom Tables. 
CTABLES 
  /VLABELS VARIABLES=Q2.1_1 Q2.1_2 Q2.1_3 Q2.1_4 Q2.1_5 Q2.1_6 Q2.1_7 Q2.1_8 Q4.1 
    DISPLAY=LABEL 
  /TABLE Q2.1_1 [C][COUNT F40.0] + Q2.1_2 [C][COUNT F40.0] + Q2.1_3 [C][COUNT F40.0] + Q2.1_4 
    [C][COUNT F40.0] + Q2.1_5 [C][COUNT F40.0] + Q2.1_6 [C][COUNT F40.0] + Q2.1_7 [C][COUNT F40.0] + 
    Q2.1_8 [C][COUNT F40.0] BY Q4.1 [C] 
  /SLABELS VISIBLE=NO 
  /CLABELS ROWLABELS=OPPOSITE 
  /CATEGORIES VARIABLES=Q2.1_1 Q2.1_2 Q2.1_3 Q2.1_4 Q2.1_5 Q2.1_6 Q2.1_7 Q2.1_8 ORDER=A KEY=VALUE 
    EMPTY=EXCLUDE 
  /CATEGORIES VARIABLES=Q4.1 ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER.




DATASET ACTIVATE DataSet1.
* Custom Tables.
CTABLES
  /VLABELS VARIABLES=Q2.1_1 Q2.1_2 Q2.1_3 Q2.1_4 Q2.1_5 Q2.1_6 Q2.1_7 Q2.1_8 Q4.1 
    DISPLAY=LABEL
  /TABLE Q2.1_1 [COUNT F40.0] + Q2.1_2 [COUNT F40.0] + Q2.1_3 [COUNT F40.0] + Q2.1_4 [COUNT F40.0] 
    + Q2.1_5 [COUNT F40.0] + Q2.1_6 [COUNT F40.0] + Q2.1_7 [COUNT F40.0] + Q2.1_8 [COUNT F40.0] BY Q4.1
  /SLABELS VISIBLE=NO
  /CLABELS ROWLABELS=OPPOSITE
  /CATEGORIES VARIABLES=Q2.1_1 Q2.1_2 Q2.1_3 Q2.1_4 Q2.1_5 Q2.1_6 Q2.1_7 Q2.1_8 ORDER=A KEY=VALUE 
    EMPTY=EXCLUDE
  /CATEGORIES VARIABLES=Q4.1 ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER.

DATASET ACTIVATE DataSet1.

