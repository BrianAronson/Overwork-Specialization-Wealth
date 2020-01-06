data scftemp;
	set scftemp;
	if NWPercentile=0 then NWPercentile=10;
run;

*Investment research;
		proc sort data=scftemp;
			by NWPercentile _Imputation_;
		run;
		proc logistic data=scftemp;
			*model iself (event='1')=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year1 HeadWorkHoursOver40 year1*HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse;
			model iself (event='1')=Income HSGraduate SomeCollege Bachelors AdvancedDegree black latino other age agesquared Children ChildrenSquared year1 HeadWorkHoursOver40 year1*HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse;
			weight wgt;
			by NWPercentile _Imputation_;
			ods output ParameterEstimates=RegressionParameters;
			Title ".";
		run;
		proc sort data=RegressionParameters;
		by NWPercentile;
		run;
		proc mianalyze parms=RegressionParameters;
		    modeleffects intercept Income HSGraduate SomeCollege Bachelors AdvancedDegree black latino other age agesquared Children ChildrenSquared year1 HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS year1*HeadWorkHoursO;
			by NWPercentile;
			Title "3 - log homeownership";
*			Title "3 - log stockownership";
*			Title "3 - log cashaccountownership";
			ods output ParameterEstimates=temp;
		run;
		proc export 
			  data=work.temp
			  dbms=xlsx 
			  outfile="C:\Users\bda13\Desktop\Sociology\Papers in Progress\Work Hours\Archived tables\Investment research.xlsx" 
			  replace;
		run;



	*Late Debt payments;
		proc logistic data=scftemp;
			*model late (event='1')=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year1 HeadWorkHoursOver40 year1*HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse;
			model late (event='1')=Income HSGraduate SomeCollege Bachelors AdvancedDegree black latino other age agesquared Children ChildrenSquared year1 HeadWorkHoursOver40 year1*HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse;
			weight wgt;
			by NWPercentile _Imputation_;
			ods output ParameterEstimates=RegressionParameters;
			Title ".";
		run;
		proc sort data=RegressionParameters;
		by NWPercentile;
		run;
		proc mianalyze parms=RegressionParameters;
		    modeleffects intercept Income HSGraduate SomeCollege Bachelors AdvancedDegree black latino other age agesquared Children ChildrenSquared year1 HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS year1*HeadWorkHoursO;
			by NWPercentile;
			Title "3 - log homeownership";
*			Title "3 - log stockownership";
*			Title "3 - log cashaccountownership";
			ods output ParameterEstimates=temp;
		run;
		proc export 
			  data=work.temp
			  dbms=xlsx 
			  outfile="C:\Users\bda13\Desktop\Sociology\Papers in Progress\Work Hours\Archived tables\late debts.xlsx" 
			  replace;
		run;

