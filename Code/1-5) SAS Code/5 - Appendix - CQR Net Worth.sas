*Table 2.1 - Quantile Regression;
		proc sort data=scftemp;
			by _Imputation_;
		run;
		proc quantreg data=scftemp algorithm=simplex ci=resampling;
			model networth=Income HSGraduate SomeCollege Bachelors AdvancedDegree black latino other age agesquared Children ChildrenSquared year1 HeadWorkHoursOver40 year1*HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse
			/quantile=.1 .25 .5 .75 .9 .99;
			weight wgt;
			by _Imputation_;
			ods output ParameterEstimates=RegressionParameters;
			Title ".";
		run;
		proc sort data=RegressionParameters;
			by quantile;
		run;
		proc mianalyze parms=RegressionParameters;
		    modeleffects intercept Income HSGraduate SomeCollege Bachelors AdvancedDegree black latino other age agesquared Children ChildrenSquared year1 HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS year1*HeadWorkHoursO ;
			by quantile;
			Title "2 - Quantile Regression";
			ods output ParameterEstimates=temp;
		run;
		proc export 
			  data=work.temp
			  dbms=xlsx 
			  outfile="C:\Users\bda13\Desktop\cqr.xlsx" 
			  replace;
run;
