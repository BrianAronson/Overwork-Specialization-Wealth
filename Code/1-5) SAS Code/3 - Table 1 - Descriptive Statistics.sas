*Table 1 - Descriptive Stats;
data scftemp;
	set scftemp;
	if NWPercentile=0 then NWPercentile=10;
run;

proc freq data=scftemp; table Married	Separated	Divorced	Widowed	LivingWithPartner; run;

proc freq data=scftemp; table NWPercentile; run;


proc sort data=scftemp;
			by NWPercentile _Imputation_;
		run;
		proc means data=scftemp; *Total Sample;
			var HeadWorkHoursOver40	HomemakerSpouse	age	HSGraduate	SomeCollege	Bachelors	AdvancedDegree	white	black	latino	other	Married	Separated	Divorced	Widowed	LivingWithPartner	Children	year	networth	StockValue	HomeEquity	iself late income2;
			by NWPercentile _Imputation_;
			weight wgt;
			output out=mimeans 
				mean= HeadWorkHoursOver40	HomemakerSpouse	age	HSGraduate	SomeCollege	Bachelors	AdvancedDegree	white	black	latino	other	Married	Separated	Divorced	Widowed	LivingWithPartner	Children	year	networth	StockValue	HomeEquity	iself late income2
				stderr=se_HeadWorkHoursOver40 se_HomemakerSpouse se_age se_HSGraduate se_SomeCollege se_Bachelors se_AdvancedDegree se_white se_black se_latino se_other se_Married se_Separated se_Divorced se_Widowed se_LivingWithPartner se_Children se_year se_networth se_StockValue se_HomeEquity se_iself se_late se_income2; 
				Title ".";
		run;
		proc mianalyze data=mimeans;
			modeleffects HeadWorkHoursOver40	HomemakerSpouse	age	HSGraduate	SomeCollege	Bachelors	AdvancedDegree	white	black	latino	other	Married	Separated	Divorced	Widowed	LivingWithPartner	Children	year	networth	StockValue	HomeEquity	iself late income2;
			stderr se_HeadWorkHoursOver40 se_HomemakerSpouse se_age se_HSGraduate se_SomeCollege se_Bachelors se_AdvancedDegree se_white se_black se_latino se_other se_Married se_Separated se_Divorced se_Widowed se_LivingWithPartner se_Children se_year se_networth se_StockValue se_HomeEquity se_iself se_late se_income2;
			by NWPercentile;
			Title "1 - Descriptive Statistics";
			ods output ParameterEstimates=temp;
		run;
		proc export 
			  data=work.temp
			  dbms=xlsx 
			  outfile="C:\Users\bda13\Desktop\Sociology\Papers in Progress\Work Hours\Figures and Tables\1 - Descriptives" 
			  replace;
		run;
