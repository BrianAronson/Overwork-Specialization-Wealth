*import and run from R;
proc import datafile="C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/uqrregres1919.csv"
     out=rreg
     dbms=csv;
run;
proc mianalyze parms=Rreg;
*    modeleffects Intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared Year1 HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40Ho Year1HeadWorkHoursOve;
    modeleffects Intercept Income HSGraduate SomeCollege Bachelors AdvancedDegree black latino other age agesquared Children ChildrenSquared Year1 HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40Ho Year1HeadWorkHoursOve;
	by quantile;
	Title "2 - Quantile Regression";
	ods output ParameterEstimates=temp;
run;

proc export 
	  data=work.temp
	  dbms=xlsx 
outfile="C:\Users\bda13\Desktop\Sociology\Papers in Progress\Work Hours\Archived tables\uqrnetworth.xlsx"	  replace;
run;



/*remove marital variables;
Married
Separated
Divorced
Widowed
LivingWithPartner
*/




