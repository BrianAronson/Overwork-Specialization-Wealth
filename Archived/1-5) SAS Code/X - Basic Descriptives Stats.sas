
*Table 1 - Descriptive Stats;
		proc sort data=scftemp;
			by _Imputation_;
		run;
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
			ods output ParameterEstimates=temp;
		run;
		proc export 
			  data=work.temp
			  dbms=xlsx 
			  outfile="C:\Users\bdaro_000\Sociology\Papers\Lisa Projects\Paper - 1% Work Hours Paper\Figures and Tables\1 - Descriptives" 
			  replace;
		run;

		
****************************************************

*Table 2 - Quantile Regression;
		proc sort data=scftemp;
			by _Imputation_;
		run;
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
			  outfile="C:\Users\bdaro_000\Sociology\Papers\Lisa Projects\Paper - 1% Work Hours Paper\Figures and Tables\2 - Wealth Quantile Regression" 
			  replace;
run;


****************************************************

*Stats for figures (quantile regression for other wealth components:
	*3a - HomeEquity;
		proc quantreg data=scftemp algorithm=simplex ci=resampling;
			model HomeEquity=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year1 HeadWorkHoursOver40 year1*HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse
			/quantile=.99;
			weight wgt;
			by _Imputation_;
			ods output ParameterEstimates=RegressionParameters;
			Title ".";
		run;
		proc sort data=RegressionParameters;
			by quantile;
		run;
		proc mianalyze parms=RegressionParameters;
		    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year1 HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS year1*HeadWorkHoursO;
		*	by quantile;
			Title "4a - HomeEquity Quantile";
			ods output ParameterEstimates=temp;
		run;
		proc export 
			  data=work.temp
			  dbms=xlsx 
			  outfile="C:\Users\bdaro_000\Sociology\Papers\Lisa Projects\Paper - 1% Work Hours Paper\Figures and Tables\3a - Home Equity Quantile Regression" 
			  replace;
		run;

	*3b - Financial assets;
		proc quantreg data=scftemp algorithm=simplex ci=resampling;
			model fin=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year1 HeadWorkHoursOver40 year1*HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse
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
		    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year1 HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS year1*HeadWorkHoursO;
			by quantile;
			Title "4b - Financial Quantile";
			ods output ParameterEstimates=temp;
		run;
		proc export 
			  data=work.temp
			  dbms=xlsx 
			  outfile="C:\Users\bdaro_000\Sociology\Papers\Lisa Projects\Paper - 1% Work Hours Paper\Figures and Tables\3b - Financial Assets Quantile Regression" 
			  replace;
		run;


****************************************************

*logistic regressions;
	proc sort data=scftemp;
		by NWPercentile _Imputation_;
	run;
	*HomeOwner StockMutual InterestBank Checking1;
		proc logistic data=scftemp;
			model InterestBank (event='1')=HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year1 HeadWorkHoursOver40 year1*HeadWorkHoursOver40 HomemakerSpouse HeadWorkHoursOver40*HomemakerSpouse;
			weight wgt;
			by NWPercentile _Imputation_;
			ods output ParameterEstimates=RegressionParameters;
			Title ".";
		run;
		proc sort data=RegressionParameters;
		by NWPercentile;
		run;
		proc mianalyze parms=RegressionParameters;
		    modeleffects intercept HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year1 HeadWorkHoursOver40 HomemakerSpouse HeadWorkH*HomemakerS year1*HeadWorkHoursO;
			by NWPercentile;
			Title "3 - log homeownership";
*			Title "3 - log stockownership";
*			Title "3 - log cashaccountownership";
		run;


****************************************************




*Other Descriptive Stats;
		proc sort data=scftemp;
			by NWPercentile homemaker _Imputation_;
		run;
		proc means data=scftemp; *Total Sample;
			var htrad late internet knowl icall idont ifinplan ifinpro ifriendwork iinternet imagznews imailadtv iself iother ;
			by NWPercentile homemaker _Imputation_;
			weight wgt;
			output out=mimeans 
				mean=htrad late internet knowl icall idont ifinplan ifinpro ifriendwork iinternet imagznews imailadtv iself iother 
				stderr=htrad_se late_se internet_se knowl_se icall_se idont_se ifinplan_se ifinpro_se ifriendwork_se iinternet_se imagznews_se imailadtv_se iself_se iother;
				Title ".";
		run;
		proc sort data=mimeans;
			by NWPercentile homemaker ;
		run;
		proc mianalyze data=mimeans;
			modeleffects htrad late internet knowl icall idont ifinplan ifinpro ifriendwork iinternet imagznews imailadtv iself iother;
			stderr htrad_se late_se internet_se knowl_se icall_se idont_se ifinplan_se ifinpro_se ifriendwork_se iinternet_se imagznews_se imailadtv_se iself_se iother;
			by NWPercentile homemaker ;
			Title "Other Descriptive Statistics";
			ods output ParameterEstimates=temp;
		run;
		proc export 
			  data=work.temp
			  dbms=xlsx 
			  outfile="C:\Users\bdaro_000\Sociology\Papers\Lisa Projects\Paper - 1% Work Hours Paper\Figures and Tables\Other Descriptives" 
			  replace;
		run;




htrad (traded in last year)
late (had late payments in last year)
internet (used internet to do financials)
knowl (knowledge of personal finances)

information for investment decisions:
htrad late internet knowl icall idont ifinplan ifinpro ifriendwork iinternet imagznews imailadtv iself iother


