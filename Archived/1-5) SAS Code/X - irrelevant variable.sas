*****************MODELS*****************;

*Table 1 - Descriptive Stats;
		proc means data=scftemp; *Total Sample;
					var networth HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse;
					by _Imputation_;
					weight wgt;
					output out=mimeans 
						mean=networth HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse
						stderr=se_networth se_HSGraduate se_SomeCollege se_Bachelors se_AdvancedDegree se_black se_latino se_other se_Married se_Separated se_Divorced se_Widowed se_LivingWithPartner se_age se_agesquared se_Children se_ChildrenSquared se_se_year se_HeadWorkHoursOver40 se_HomemakerSpouse
						std=std_networth std_HSGraduate std_SomeCollege std_Bachelors std_AdvancedDegree std_black std_latino std_other std_Married std_Separated std_Divorced std_Widowed std_LivingWithPartner std_age std_agesquared std_Children std_ChildrenSquared std_std_year std_HeadWorkHoursOver40 std_HomemakerSpouse;
						Title ".";
				run;
		proc mianalyze data=mimeans;
					modeleffects networth HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse;
					stderr se_networth se_HSGraduate se_SomeCollege se_Bachelors se_AdvancedDegree se_black se_latino se_other se_Married se_Separated se_Divorced se_Widowed se_LivingWithPartner se_age se_agesquared se_Children se_ChildrenSquared se_se_year se_HeadWorkHoursOver40 se_HomemakerSpouse;
					Title "1 - Descriptive Statistics";
				run;
		*proc mianalyze data=mimeans;
		*		modeleffects std_networth std_HSGraduate std_SomeCollege std_Bachelors std_AdvancedDegree std_black std_latino std_other std_Married std_Separated std_Divorced std_Widowed std_LivingWithPartner std_age std_agesquared std_Children std_ChildrenSquared std_std_year std_HeadWorkHoursOver40 std_HomemakerSpouse;
		*		stderr se_networth se_HSGraduate se_SomeCollege se_Bachelors se_AdvancedDegree se_black se_latino se_other se_Married se_Separated se_Divorced se_Widowed se_LivingWithPartner se_age se_agesquared se_Children se_ChildrenSquared se_se_year se_HeadWorkHoursOver40 se_HomemakerSpouse;
		*		run;

proc sort data=scftemp;
			by _Imputation_;
			run;
*Table 2 - Quantile Regression;
		proc quantreg data=scftemp algorithm=simplex ci=resampling;
			model networth=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year1 HeadWorkHoursOver40 year1*HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse
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
		    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year1 HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS year1*HeadWorkHoursO ;
			by quantile;
			Title "2 - Quantile Regression";
			ods output ParameterEstimates=temp;
		run;
		proc export 
			  data=work.temp
			  dbms=xlsx 
			  outfile="C:\Users\bdaro_000\Desktop\temp" 
			  replace;
run;

*Exploratory stats
	*1a - Quantile Regression by year groups;
		proc sort data=scftemp;
			by yearinterval _Imputation_;
			run;
		proc quantreg data=scftemp algorithm=simplex ci=resampling;
			model networth=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse
			/quantile=.1 .25 .5 .75 .9 .99;
			weight wgt;
			by yearinterval _Imputation_;
			ods output ParameterEstimates=RegressionParameters1;
			Title ".";
		run;
		proc sort data=RegressionParameters1;
			by yearinterval quantile ;
		run;
		proc mianalyze parms=RegressionParameters1;
		    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
			by yearinterval quantile;
			Title "1a - By year group";
		run;
		proc sort data=scftemp;
			by _Imputation_;
		run;
		*Problem with these results is that it is not clear how to interpret them.;
	*1b - quantile regression with year interactions;
		proc quantreg data=scftemp algorithm=simplex ci=resampling;
			model networth=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse year*HeadWorkHoursOver40 year*HomemakerSpouse year*HeadWorkHoursOver40*HomemakerSpouse
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
		    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS year*HeadWor*Homemak year*HeadWorkHoursOv year*HomemakerSpouse;
			by quantile;
			Title "1b-Year interaction";
		run;
*1c - quantile regression by cohort;
	*Greatest;
		proc quantreg data=scftemp algorithm=simplex ci=resampling;
			model networth=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse
			/quantile=.1 .25 .5 .75 .9 .99;
			weight wgt;
			by _Imputation_;
			where Greatest=1;
			ods output ParameterEstimates=RegressionParameters;
			Title ".";
		run;
		proc sort data=RegressionParameters;
			by quantile;
		run;
		proc mianalyze parms=RegressionParameters;
		    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
			by quantile;
			Title "1c-Greatest";
		run;
		*Babyboomer;
		proc quantreg data=scftemp algorithm=simplex ci=resampling;
			model networth=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse
			/quantile=.1 .25 .5 .75 .9 .99;
			weight wgt;
			by _Imputation_;
			where Babyboomer=1;
			ods output ParameterEstimates=RegressionParameters;
			Title ".";
		run;
		proc sort data=RegressionParameters;
		by quantile;
		run;
		proc mianalyze parms=RegressionParameters;
		    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
			by quantile;
			Title "1c-BabyBoomer";
		run;
		*Genx;
		proc quantreg data=scftemp algorithm=simplex ci=resampling;
			model networth=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse
			/quantile=.1 .25 .5 .75 .9 .99;
			weight wgt;
			by _Imputation_;
			where GenX=1;
			ods output ParameterEstimates=RegressionParameters;
			Title ".";
		run;
		proc sort data=RegressionParameters;
		by quantile;
		run;
		proc mianalyze parms=RegressionParameters;
		    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
			by quantile;
			Title "1c-GenX";
		run;
		*Geny;
		proc quantreg data=scftemp algorithm=simplex ci=resampling;
			model networth=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse
			/quantile=.1 .25 .5 .75 .9 .99;
			weight wgt;
			by _Imputation_;
			where GenY=1;
			ods output ParameterEstimates=RegressionParameters;
			Title ".";
		run;
		proc sort data=RegressionParameters;
		by quantile;
		run;
		proc mianalyze parms=RegressionParameters;
		    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
			by quantile;
			Title "1c-GenY";
		run;


*4 quantile regression for other wealth components;
	*4a - Stocks;
		proc quantreg data=scftemp algorithm=simplex ci=resampling;
			model StockValue=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse
			/quantile= .75 .9 .99;
			weight wgt;
			by _Imputation_;
			ods output ParameterEstimates=RegressionParameters;
			Title ".";
		run;
		proc sort data=RegressionParameters;
			by quantile;
		run;
		proc mianalyze parms=RegressionParameters;
		    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
			by quantile;
			Title "4a - Stocks Quantile";
		run;
	*4b - HomeEquity;
		proc quantreg data=scftemp algorithm=simplex ci=resampling;
			model HomeEquity=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse
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
		    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
			by quantile;
			Title "4b - HomeEquity Quantile";
		run;
	*4c - PAssetsInFinance;
		proc quantreg data=scftemp algorithm=simplex ci=resampling;
			model PAssetsInFinance=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse
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
		    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
			by quantile;
			Title "4c - PAssetsInFinance Quantile";
		run;
	*4d - Debt;
		proc quantreg data=scftemp algorithm=simplex ci=resampling;
			model Debt=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse
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
		    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
			by quantile;
			Title "4d - Debt Quantile";
		run;
	*4e - Income;
		proc quantreg data=scftemp algorithm=simplex ci=resampling;
			model Income=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse
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
		    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
			by quantile;
			Title "4e - Income Quantile";
		run;


*ChildUnder3, living with Parents or siblings;
	proc freq data=scftemp;
	tables homemakerspouse;
	where ChildUnder3Dummy=1;
	run;


proc sort data=scftemp;
			by _Imputation_;
			run;


*Table 5a - Wealth by homemaker spouse and infant;
		proc quantreg data=scftemp algorithm=simplex ci=resampling;
			model networth=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse ChildUnder3Dummy ChildUnder3Dummy*homemakerspouse
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
		    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS ChildUnder3Dummy Homemaker*ChildUnder;
			by quantile;
			Title "2 - Quantile Regression";
		run;

		proc freq data=scftemp;
		tables NWPercentile;
		run;

proc sort data=scftemp;
by NWPercentile _Imputation_;
run;

*Table 5b - homemaker spouse by quantile;
		proc logistic data=scftemp;
			model homemakerspouse(event='1')=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other age agesquared Children ChildrenSquared year HeadWorkHoursOver40 ChildUnder3Dummy ParentOrSibling ChildUnder3Dummy*HeadWorkHoursOver40;
			weight wgt;
			by NWPercentile _Imputation_;
			where Married=1;
			ods output ParameterEstimates=RegressionParameters;
			Title ".";
		run;
		proc sort data=RegressionParameters;
		by NWPercentile;
		run;
		proc mianalyze parms=RegressionParameters;
		    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other age agesquared Children ChildrenSquared year HeadWorkHoursOver40 ChildUnder3Dummy ParentOrSibling HeadWorkH*ChildUnder;
			by NWPercentile;
			Title "2 - Quantile Regression";
		run;


proc sort data=scftemp;
by NWPercentile ChildUnder3;
run;

proc means data=scftemp;
var networth;
by NWPercentile;
weight wgt;
run;


proc contents data=scftemp;

run;


proc freq data=scftemp;
table networth99;
run;




*Predict asset ownership in different asset types, controlling for wealth.

*Predict household income by quantile and specialization





/*
*2 - Logistic regression for getting into 1%;
*These don't work for various reasons - they show factors that distinguish the 1% from other percentiles, but they are not direct measurements of utility.;

	*a. year interactions;
			*log odds for being in net worth one percent;
				proc logistic data=scftemp; *unfortunately, ods cannot work with noprint;
					model OnePercent(event='1')=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse;
					weight wgt;
					by _Imputation_;
					ods output ParameterEstimates=RegressionParameters;
					run;
				proc mianalyze parms=RegressionParameters;
				    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
					run;
			*log odds for being in net worth one percent with year interactions;
				proc logistic data=scftemp;
					model OnePercent(event='1')=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse year*HeadWorkHoursOver40 year*HomemakerSpouse year*HeadWorkHoursOver40*HomemakerSpouse;
					weight wgt;
					by _Imputation_;
					ods output ParameterEstimates=RegressionParameters;
					run;
				proc mianalyze parms=RegressionParameters;
				    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS year*HeadWor*Homemak year*HeadWorkHoursOv year*HomemakerSpouse ;
					run;
			*log odds for being in net worth 10 percent;
				proc logistic data=scftemp;
					model twentyfivePercent(event='1')=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse;
					weight wgt;
					by _Imputation_;
					ods output ParameterEstimates=RegressionParameters;
					run;
				proc mianalyze parms=RegressionParameters;
				    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
					run;
			*log odds for being in net worth 10 percent with year interactions;
				proc logistic data=scftemp;
					model NinetyPercent(event='1')=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse year*HeadWorkHoursOver40 year*HomemakerSpouse year*HeadWorkHoursOver40*HomemakerSpouse;
					weight wgt;
					by _Imputation_;
					ods output ParameterEstimates=RegressionParameters;
					run;
				proc mianalyze parms=RegressionParameters;
				    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS year*HeadWor*Homemak year*HeadWorkHoursOv year*HomemakerSpouse ;
					run;
	*b. year groups - log odds for being in net worth one percent;
			*years 89-95;
				proc logistic data=scftemp;
					model OnePercent(event='1')=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse; 
					weight wgt;
					by _Imputation_;
					where year<1996;
					ods output ParameterEstimates=RegressionParameters;
					run;
				proc mianalyze parms=RegressionParameters;
				    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
					run;
			*years 98-04;
				proc logistic data=scftemp;
					model OnePercent(event='1')=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse;
					weight wgt;
					by _Imputation_;
					where year>1995 & year<2005;
					ods output ParameterEstimates=RegressionParameters;
					run;
				proc mianalyze parms=RegressionParameters;
				    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
					run;
			*years 07-13;
				proc logistic data=scftemp;
					model tenPercent(event='1')=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse ;
					weight wgt;
					by _Imputation_;
					where year>2006;
					ods output ParameterEstimates=RegressionParameters;
					run;
				proc mianalyze parms=RegressionParameters;
				    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
					run;
*3 - Cohort effects;
	*model wealth attainment by cohort
			*GenY (only contains 4 observations in one percent!); 
				proc logistic data=scftemp;
					model OnePercent(event='1')=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse;
					weight wgt;
					by _Imputation_;
					where GenY=1;
					ods output ParameterEstimates=RegressionParameters;
					run;
				proc mianalyze parms=RegressionParameters;
				    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
					run;
			*GenX; 
				proc logistic data=scftemp;
					model OnePercent(event='1')=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse;
					weight wgt;
					by _Imputation_;
					where GenX=1;
					ods output ParameterEstimates=RegressionParameters;
					run;
				proc mianalyze parms=RegressionParameters;
				    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
					run;
			*Babyboomer; 
				proc logistic data=scftemp;
					model OnePercent(event='1')=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse;
					weight wgt;
					by _Imputation_;
					where Babyboomer=1;
					ods output ParameterEstimates=RegressionParameters;
					run;
				proc mianalyze parms=RegressionParameters;
				    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
					run;
			*Greatest (only contains 4 observations in one percent!); 
				proc logistic data=scftemp;
					model OnePercent(event='1')=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse;
					weight wgt;
					by _Imputation_;
					where Greatest=1;
					ods output ParameterEstimates=RegressionParameters;
					run;
				proc mianalyze parms=RegressionParameters;
				    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS;
					run;












*New 

















*/



*OLD - PREDICT WEALTH VIA OLS;
		/*
		data scftemp;
		set scftemp;
		loggednetworth1=log(networth + 227019001); *positive skew method;
		loggednetworth2=log10(2837016154-networth); *negative skew method;
		if networth>0 then loggednetworth3=log10(networth); *joint skew method (part 1);
		if networth<0 then loggednetworth3=-log10(-networth); *joint skew method (part 2);
		if networth=0 then loggednetworth3=log10(networth+1); *joint skew method (part 3), could also just replace then statement with "0";
		cubednetworth=SIGN(networth)*ABS(networth)**(1/3); *more common way of dealing with skew
		run;

		proc means;
		var LessThanHS HSGraduate SomeCollege Bachelors AdvancedDegree white black latino other Married Separated Divorced NeverMarried Widowed age agesquared Children year RHoursPerWeek Homemaker;
		run;

		proc reg data=scftemp outest=regressions covout; 
		model cubednetworth= HSGraduate SomeCollege Bachelors AdvancedDegree white latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children year HeadWorkHours HomemakerSpouse;
		weight wgt;
		by _Imputation_;
		run;
		*reference variables = less than high school=education, black=race, age=<25, NeverMarried=marital status;

		 proc mianalyze data=regressions;
		        modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year HeadWorkHours HomemakerSpouse;
		        run;
		*/




*****************Archive*****************;
/*
		*Net Worth Percentiles; 	Proc sort data=SCFTemp;	*Sort by Percentile and year;
				by NWPercentile year;
				run;
		Proc Means mean Data=SCFTemp;	*Work Hours and weeks per year controlling for old age and employment; 
			var RHoursPerWeek RWeeksPerYear;
			Title 'Respondent Work Employed';
			by  NWPercentile year;
			where Age25to64=1 & RWorkStatus="Employed";
			Weight wgt;	
		Proc Means mean Data=SCFTemp;	*Rate of Dual Employment; 
			var BothEmployed;
			Title 'Both Employed';
			by  NWPercentile year;
			where  Age25to64=1;
			Weight wgt;
			run;

		Proc Means mean Data=SCFTemp;	*Rate of Homemaker Spouse; 
			var Homemaker;
			Title 'Homemaker Spouse';
			by  NWPercentile year;
			where  Age25to64=1;
			Weight wgt;
			run;

		proc freq;  *Confirmation that homemakers don't work;
			tables SHoursPerWeek;
			where Homemaker=1;
			run;

		Proc Means mean Data=SCFTemp;	*Rate of Retirement before 65; 
			var RRetired SRetired;
			Title 'Retirement Rate';
			by  NWPercentile year;
			where  Age25to64=1;
			Weight wgt;
			run;

		Proc Means mean Data=SCFTemp;	*Percent of Assets in Financial categories by  NWPercentile over time; 
			var PAssetsInStocks PAssetsInFinance PIncomeCapitalGains PIncomeDividends;
			Title 'Measuring Financialization';
			by  NWPercentile year;
			where  Age25to64=1;
			Weight wgt;
			run;

		*Income Percentiles; 	Proc sort data=SCFTemp;	*Sort by Percentile and year;
				by IPercentile year;
				run;

		Proc Means mean Data=SCFTemp;	*Work Hours and weeks per year controlling for old age and employment; 
			var RHoursPerWeek RWeeksPerYear;
			Title 'Respondent Work Employed';
			by  IPercentile year;
			where Age25to64=1 & RWorkStatus="Employed";
			Weight wgt;
			run;
			
		Proc Means mean Data=SCFTemp;	*Rate of Dual Employment; 
			var BothEmployed;
			Title 'Both Employed';
			by  IPercentile year;
			where  Age25to64=1;
			Weight wgt;
			run;

		Proc Means mean Data=SCFTemp;	*Rate of Homemaker Spouse; 
			var Homemaker;
			Title 'Homemaker Spouse';
			by  IPercentile year;
			where  Age25to64=1;
			Weight wgt;
			run;

		proc freq;  *Confirmation that homemakers don't work;
			tables SHoursPerWeek;
			where Homemaker=1;
			run;

		Proc Means mean Data=SCFTemp;	*Rate of Retirement before 65; 
			var RRetired SRetired;
			Title 'Retirement Rate';
			by  IPercentile year;
			where  Age25to64=1;
			Weight wgt;
			run;

		Proc Means mean Data=SCFTemp;	*Percent of Assets in Financial categories by  IPercentile over time; 
			var PAssetsInStocks PAssetsInFinance PIncomeCapitalGains PIncomeDividends;
			Title 'Measuring Financialization';
			by  IPercentile year;
			where  Age25to64=1;
			Weight wgt;
			run;
















		/*Obselete code;

		Proc Means mean Data=SCFTemp;	*Work Hours and weeks per year controlling for old age; 
			var RHoursPerWeek RWeeksPerYear;
			Title 'Respondent Work Old Age';
			by  NWPercentile year;
			where Age25to64=1;
			Weight wgt;
			run;

		Proc Means mean Data=SCFTemp;	*Work Hours and weeks per year; 
			var RHoursPerWeek RWeeksPerYear;
			Title 'Respondent Work';
			by  NWPercentile year;
			Weight wgt;
			run;

		proc univariate plot; *Plot Distribution of Stock Ownership over time;
		var PAssetsInStocks;
		Title 'Distribution of Stock Ownership over time';
		where  NWPercentile="99";
		by year;
		run;

		Proc Means mean Data=SCFTemp;	*Work Hours and weeks per year of Couple by  NWPercentile and year for controlling for old age; 
			var CHoursPerWeek CWeeksPerYear;
			Title 'Couple Work Old Age';
			by  NWPercentile year;
			where Age25to64=1;
			Weight wgt;
			run;

		Proc Means mean Data=SCFTemp;	*Work Hours and weeks per year of Couple by  NWPercentile and year when both employed controlling for old age and Employment ; 
			var CHoursPerWeek CWeeksPerYear;
			Title 'Couple Work Employed';
			by  NWPercentile year;
			where SWorkStatus="Employed" & RWorkStatus="Employed" & Age25to64=1;
			Weight wgt;
			run;




		Proc Means mean Data=SCFTemp; *Demographic Stats;
				var EducBelowHS Age25to64 Married Children White Black Hispanic Other ;
				by year;
				Weight wgt;
				run;





		Proc Means mean Data=SCFTemp;	*Work Hours and weeks per year of Couple by  NWPercentile and year (even for those without a spouse); 
			var CHoursPerWeek CWeeksPerYear;
			Title 'Couple Work';
			by  NWPercentile year;
			where;
			Weight wgt;
			run;


		Proc Means mean Data=SCFTemp;	*Work Hours and weeks per year of Couple by  NWPercentile and year (for those with a spouse - biased upwards); 
			var CHoursPerWeek CWeeksPerYear;
			Title 'Couple Work';
			by  NWPercentile year;
			where SWorkStatus ~= "No Spous";
			Weight wgt;
			run;

		Proc Means mean median P90 P99 Data=SCFTemp; *SCF Income and Wealth;

				var Income Networth Debt Assets;
				by year;
				Weight wgt;
				*where age>34 & age<55 & white=1;	
				run;
			
				Proc Means mean median P90 P99 Data=SCFTemp; *SCF Homeownership;
				var Homeowner Mortgages HomeEquity HomeDebt;
				by year;
				Weight wgt;
				run;
				Proc Means mean median P90 P99 Data=SCFTemp; *SCF Liquid assets;

				var StockMutual InterestBank Checking IRAor401k;
				by year;
				Weight wgt;
				run;
				proc freq data=scftemp; *Frequencies of networth categories;
				Title ' NWPercentiles';
				tables  NWPercentile;
				run;

				proc means; *1. Two way figures: Proportion of Assets in stocks and median value of stocks by  NWPercentile over time.;
				var PAssetsInFinance;
				where age>24;
				weight wgt;
				by  NWPercentile year;
				run;
					*b;		proc means;
							var StockValue;
							where age>24;
							weight wgt;
							by  NWPercentile year;
							run;
					*c;		proc means;
							var mma;
							where age>24;
							weight wgt;
							by  NWPercentile year;
							run;
					*d;		proc means;
							var IraValue;
							where age>24;
							weight wgt;
							by  NWPercentile year;
							run;
					*e;		proc means;
							var LifeInsuranceValue;
							where age>24;
							weight wgt;
							by  NWPercentile year;
							run;
					*F;		proc means;
							var Networth;
							where age>24;
							weight wgt;
							by  NWPercentile year;
							run;

			*/
		*/
