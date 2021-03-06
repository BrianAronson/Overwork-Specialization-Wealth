Data SCFTemp;
	Set ScfFull.Combined2; *1989-2016;
	*Education; 
		if educ<8 then Education=0;
			else if educ<9 then Education=1;
			else if educ<12 then Education=2;
			else if educ<13 then Education=3;
			else if educ<16 then Education=4;
		if Education=0 then LessThanHS=1;
			else if Education~=0 & Education~=. then LessThanHS=0;
		if Education=1 then HSGraduate=1;
			else if Education~=1 & Education~=. then HSGraduate=0;
		if Education=2 then SomeCollege=1;
			else if Education~=2 & Education~=. then SomeCollege=0;
		if Education=3 then Bachelors=1;
			else if Education~=3 & Education~=. then Bachelors=0;
		if Education=4 then AdvancedDegree=1;
			else if Education~=4 & Education~=. then AdvancedDegree=0;
	*Race;
		if race=1 then White=1;
			else White=0;
		if race=2 then Black=1;
			else Black=0;
		if race=3 then Latino=1;
			else Latino=0;
		if race=5 then Other=1;
			else Other=0;
	*Age Category;
		if age>24 & age<65 then Age25to64=1;
			else Age25to64=0;
	*Other Age Categories;
		if age=>25 & age<40 then Age25to39=1; else Age25to39=0;
		if age=>40 & age<50 then Age40to49=1; else Age40to49=0;
		if age=>50 & age<60 then Age50to59=1; else Age50to59=0;
		if age=>60 & age<70 then Age60to69=1; else Age60to69=0;
		if age=>70 then Age70P=1; else Age70P=0;
		agesquared=age**2;
	*Generations;
		if year-age>1981 then GenY=1; else GenY=0;
		if year-age<1982 & year-age>1964 then GenX=1; else GenX=0;
		if year-age<1965 & year-age>1945 then BabyBoomer=1; else BabyBoomer=0;
		if year-age<1946 then Greatest=1; else Greatest=0;
	*Married;
		if Married=2 then Married=0;
	*Children;
		if kids<5 then Children=kids;
			else Children=5;
		Childrensquared=Children**2;
	*Wealth Variables;
		Assets=Networth+Debt;
	*Home and Mortgage;
		HomeEquity=homeeq;
		HomeDebt=NH_MORT;
		HomeOwner = hhouses;
		if homeeq~=0 then HomeOwner1=1;
			else if homeeq=. | homeeq=0 then Homeowner1=0;
		if housecl~=2 then HomeOwner1=1;
			else Homeowner1=0;
		if HomeOwner1=1 & PAYMORT1=0 then Mortgages=0;
			else if Homeowner1=1 & PAYMORT1>0 & PAYMORT2>0 & PAYMORT3>0 then Mortgages=3 ;
			else if Homeowner1=1 & PAYMORT1>0 & PAYMORT2>0 & PAYMORT3=0 then Mortgages=2 ;
			else if Homeowner1=1 & PAYMORT1>0 & PAYMORT2=0 & PAYMORT3=0 then Mortgages=1 ;
			else Mortgages=0; *This is redundant.  Models on mortgages should exclude non-home owners.;
		OtherRealEstateValue = nnresre;
	*Liquid Asset Rates of Ownership; 
		if hstocks=1 | deq>0 then StockMutual=1;
			else StockMutual=0;
		if mma>0 then InterestBank=1;
			else InterestBank=0;
		if nochk=0 then Checking1=1;
			else Checking1=0;
		If irakh>0 then IRAor401k=1;
			Else  IRAor401k=0;
	*Liquid Asset and Debt value;
		CheckingValue = checking;
		SavingsPlanValue = thrift;
		StockValue = deq;
		IraValue = irakh;
		LifeInsuranceValue = cashli;
		OtherAssetValue = othnfin;
		SecuredDebtValue = mrthel;
		CreditCardDebtValue = ccbal;
		BusinessValue = bus;

	*Other Variables;
		Female=hhsex-1;
		Unemployed = 1 - lf;
		if OCCAT2=1 then Manager=1;
		else Manager=0;
	*Cars;
		CarDebt = VEH_INST;
		VehicleValue = vehic;
		OwnCar = own;
		NumberCarOwned = nown;
		if NumberCarOwned>5 then NumberCarOwned=5;
	*Percent Assets in Stocks;
	     PAssetsInStocks=(StockValue) /(Assets + .001);
	     if PAssetsInStocks=. then PAssetsInStocks=0;
	     if PAssetsInStocks<0 then PAssetsInStocks=0;
	     if PAssetsInStocks>1 then PAssetsInStocks=1;
	*Percent Assets in Finance;
	     PAssetsInFinance=fin/ (Assets + .001);
	     if PAssetsInFinance=. then PAssetsInFinance=0;
	     if PAssetsInFinance<0 then PAssetsInFinance=0;
	     if PAssetsInFinance>1 then PAssetsInFinance=1;
	*Other proportions;
		PStockValue = (StockValue+.001)/(assets+.001);
			if StockValue<2 then PStockValue=0;
			if PStockValue>1 then PStockValue=1;
		PDebt = (Debt+.001)/(assets+.001); 
			if debt<2 then PDebt=0;
			if PDebt>1 then PDebt=1;
		PHomeequity=(Homeequity+.001)/(assets+.001); 
			if Homeequity<2 then PHomeequity=0;
			if PHomeequity>1 then PHomeequity=1;
		PLifeInsuranceValue=(LifeInsuranceValue+.001)/(assets+.001); 
			if LifeInsuranceValue<2 then PLifeInsuranceValue=0;
			if PLifeInsuranceValue>1 then PLifeInsuranceValue=1;
		PIncome=(Income+.001)/(assets+.001);
			if Income<2 then PIncome=0;
			if PIncome>1 then PIncome=1;
		POtherRealEstateValue=(OtherRealEstateValue+.001)/(assets+.001);
			if OtherRealEstateValue<2 then POtherRealEstateValue=0;
			if POtherRealEstateValue>1 then POtherRealEstateValue=1;
		Pwageinc=(wageinc+.001)/(assets+.001);
			if wageinc<2 then Pwageinc=0;
			if Pwageinc>1 then Pwageinc=1;
		PIncWages=(wageinc+.001)/(Income+001); 
			if wageinc<2 then PIncWages=0;
			if PIncWages>1 then PIncWages=1;
		Pbond=(bond+.001)/(assets+.001);
			if bond<2 then bond=0;
			if Pbond>1 then Pbond=1;
		Pbus=(bus+.001)/(assets+.001); 
			if bus<2 then Pbus=0;
			if Pbus>1 then Pbus=1;
	*Income sources;
		StockGains=KGSTMF;
		CapitalGains=KGINC;
		Salary=WAGEINC;
		Dividends=INTDIVINC;
		PIncomeCapitalGains=CapitalGains/(Income+.01);
        PIncomeDividends=Dividends/(Income+.01);
        PIncomeSalary=Salary/(Income+.01);      
	*logged variables;
		Array SkewedVariable (17) income networth debt assets homeequity CheckingValue SavingsPlanValue StockValue IraValue LifeInsuranceValue OtherRealEstateValue BusinessValue OtherAssetValue SecuredDebtValue HomeDebt CreditCardDebtValue CarDebt;
		Array LoggedVariable (17) loggedincome loggednetworth loggeddebt loggedassets loggedhomeequity loggedCheckingValue loggedSavingsPlanValue loggedStockValue loggedIraValue loggedLifeInsuranceValue loggedOtherRealEstateValue loggedBusinessValue loggedOtherAssetValue loggedSecuredDebtValue loggedHomeDebt loggedCreditCardDebtValue loggedCarDebt;
		Do i = 1 to 17;
			if SkewedVariable(i)>0 then LoggedVariable(i)=log(SkewedVariable(i));  *This without if statement would produce an error and missing number for those with negative and 0 networth.;
			if SkewedVariable(i)<0 then LoggedVariable(i)=-log(ABS(SkewedVariable(i))); *This gives me negative logged networth;
			if SkewedVariable(i)=0 then LoggedVariable(i)=0; *This is not actually true, but log of 1=0, and 0 is pretty close to 1...;
		END;
	*cubed variables;
		cubednetworth=SIGN(networth)*ABS(networth)**(1/3);
	*Year variables;
		if year<1996 then yearinterval=1; 
		else if year<2005 then yearinterval=2;
		else yearinterval=3;
		year1=year-1989;
	*SCFFull variables;
		*Work status;
			*Match work status for old years;
				if X4100>10 & X4100<20 then X4100=1;
				if X4100=20 | X4100=21 then X4100=2;
				if X4100=30 then X4100=3;
				if X4100=50 then X4100=7;
				if X4100=52 then X4100=6;
				if X4100=70 then X4100=4;
				if X4100=80 then X4100=5;
				if X4100>89 then X4100=1;
				if X4700>10 & X4700<20 then X4700=1;
				if X4700=20 | X4700=21 then X4700=2;
				if X4700=30 then X4700=3;
				if X4700=50 then X4700=7;
				if X4700=52 then X4700=6;
				if X4700=70 then X4700=4;
				if X4700=80 then X4700=5;
				if X4700>89 then X4700=1;
			*Match variable name for old years;
				if X6670=. then X6670=X4100;
				if X6678=. then X6678=X4700;
			*Delete duplicate variables;
				drop X4100;
				drop X4700;
			*Create/rename work status variables;
				RHoursPerWeek=X4110;
				SHoursPerWeek=X4710;
				CHoursPerWeek=X4110+X4710;
				RWeeksPerYear=X4111;
				SWeeksPerYear=X4711;
				CWeeksPerYear=X4111+X4711;
				TRWorkStatus=X6670;
				TSWorkStatus=X6678;
			*Reduce/rename categories;
				if TRWorkStatus=2 then RWorkStatus="Temp Laid Off"; 
				if TRWorkStatus=1 then RWorkStatus="Employed"; 
				if TRWorkStatus=3 then RWorkStatus="Unemployed"; 
				if TRWorkStatus=4 then RWorkStatus="Student"; 
				if TRWorkStatus=5 then RWorkStatus="Homemaker"; 
				if TRWorkStatus=6 then RWorkStatus="Disabled"; 
				if TRWorkStatus=7 then RWorkStatus="Retired"; 
				if TRWorkStatus=8 then RWorkStatus="On Sick Leave"; 
				if TRWorkStatus=10 then RWorkStatus="Volunteer"; 
				if TRWorkStatus=16 then RWorkStatus="Other"; 
				if TRWorkStatus=-7 then RWorkStatus="Other"; 
				if TSWorkStatus=2 then SWorkStatus="Temp Laid Off"; 
				if TRWorkStatus=0 then RWorkStatus="Other"; 				
				if TSWorkStatus=1 then SWorkStatus="Employed"; 
				if TSWorkStatus=3 then SWorkStatus="Unemployed"; 
				if TSWorkStatus=4 then SWorkStatus="Student"; 
				if TSWorkStatus=5 then SWorkStatus="Homemaker"; 
				if TSWorkStatus=6 then SWorkStatus="Disabled"; 
				if TSWorkStatus=7 then SWorkStatus="Retired"; 
				if TSWorkStatus=8 then SWorkStatus="On Sick Leave"; 
				if TSWorkStatus=10 then SWorkStatus="Volunteer"; 
				if TSWorkStatus=16 then SWorkStatus="Other"; 
				if TSWorkStatus=-7 then SWorkStatus="Other"; 
				if TSWorkStatus=0 then SWorkStatus="No Spouse"; 	
			*Create dummy variables;
				if SWorkStatus="Homemaker" then Homemaker=1; else Homemaker=0;
				if RWorkStatus="Retired" then RRetired=1; else RRetired=0;
				if SWorkStatus="Employed" & RWorkStatus="Employed" then BothEmployed=1; else BothEmployed=0;
				if SWorkStatus="Retired" then SRetired=1; else SRetired=0;
				HomemakerSpouse=Homemaker;
		*Work hours;
			HeadWorkHours=RHoursPerWeek;
			if HeadWorkHours>80 then HeadWorkHours=80;
			HeadWorkHoursSquared=HeadWorkHours**2;
			if HeadWorkHours>40 then HeadWorkHoursOver40=HeadWorkHours-40;
			else if HeadWorkHours<41 then HeadWorkHoursOver40=0;
		*Inheritance;
			if X5801=1 then ReceivedInheritance=1;
				else ReceivedInheritance=0;
			NumberInheritances=X5802;
			InheritanceType1= X5803;
			InheritanceType2= X5808;
			InheritanceType3= X5813;
			InheritanceValue1= X5804;
			InheritanceValue2= X5809;
			InheritanceValue3= X5814;
			YearReceived1=X5805;
			YearReceived2=X5810;
			YearReceived3=X5815;
			InheritFrom1=X5806;
			InheritFrom2=X5811;
			InheritFrom3=X5816;
			OtherInheritance=X5818;
			ExpectInheritance=X5819;
			AmountExpected=X5821;
			TotalInheritance=InheritanceValue1+InheritanceValue2+InheritanceValue3+OtherInheritance;
			InheritanceNWRatio=TotalInheritance/(Networth+.01);
			if cashli>0 then anyli=1; else anyli=0;
			if kginc>0 then kginc1=1; else kginc1=0;

		*Marital status;
			if X8023=1 then Married=1; else Married=0;
			if X8023=2 then LivingWithPartner=1; else LivingWithPartner=0;
			if X8023=3 then Separated=1; else Separated=0;
			if X8023=4 then Divorced=1; else Divorced=0;
			if X8023=5 then Widowed=1; else Widowed=0;
			if X8023=6|X8023=7|X8023=8 then NeverMarried=1; else NeverMarried=0;
			
		*family member variables;
			Array family (10) boarder spouse parent otherrelative grandchild child grandparent aunt cousin sibling;
			Array code (10) _temporary_ (34 2 6 29 5 4 7 8 9 11);
			Do i = 1 to 10;
				if X8020=code(i) then family(i)=1; else family(i)=0; if X102=code(i) then family(i)=family(i)+1; if X108=code(i) then family(i)=family(i)+1; if X114=code(i) then family(i)=family(i)+1; if X120=code(i) then family(i)=family(i)+1; if X126=code(i) then family(i)=family(i)+1; if X132=code(i) then family(i)=family(i)+1; if X202=code(i) then family(i)=family(i)+1; if X208=code(i) then family(i)=family(i)+1; if X214=code(i) then family(i)=family(i)+1; if X220=code(i) then family(i)=family(i)+1; if X226=code(i) then family(i)=family(i)+1;
			End;
			if parent>0 | sibling>0 then ParentOrSibling=1; else ParentOrSibling=0;
			*Count how many children a family has that are under 3 years old;
				ChildUnder3=0; *must update SCFULL.Combined2 with age data for this to work;
				Array SCFFamilyMember (12) X8020 X102 X108 X114 X120 X126 X132 X202 X208 X214 X220 X226;
				Array SCFChildAge (12) X8022 X104 X110 X116 X122 X128 X134 X204 X210 X216 X222 X228;
				Array ChildNumber (12) Child1 Child2 Child3 Child4 Child5 Child6 Child7 Child8 Child9 Child10 Child11 Child12;
				Array ChildAge (12) Child1Age Child2Age Child3Age Child4Age Child5Age Child6Age Child7Age Child8Age Child9Age Child10Age Child11Age Child12Age;
				Do i = 1 to 12;
					if SCFFamilyMember(i)=4 then ChildNumber(i)=1; else ChildNumber(i)=0;
					if ChildNumber(i)=1 then ChildAge(i)=SCFChildAge(i);
					if ChildAge(i)<3 & ChildAge(i)~=. then ChildUnder3=ChildUnder3+1;
				End;
				if ChildUnder3>0 then ChildUnder3Dummy=1; else ChildUnder3Dummy=0;
		*Create proper SCF imputation names;
			_Imputation_=1*substr(Y1,length(Y1),1);
		*Drop redundant variables;
			drop X2001 X2002 X2003 X2004 X2009 X2505 X2506 X2605 X2606 X2623 X4020 X4024 X4028 X4022 X4026 X4030 X4110 X4710 X6670 X6678 X4111 X4711 X108 X114 X120 X126 X132 X202 X208 X214 X220 X226 X5801 X5802 X6703 X5803 X5808 X5813 X5804 X5809 X5814 X5805 X5810 X5815 X5806 X5811 X5816 X5818 X5819 X5821;
run;

*Determine Percentile Cutoffs;
	proc univariate data=SCFTEMP noprint;
	    var networth income PAssetsInStocks HomeEquity StockValue;
	    output out=percentiles1 pctlpts=99 90 75 50 25 10 pctlpre=networth income PAssetsInStocks HomeEquity StockValue;
	    by year;
	    weight wgt;
	run;

*Merge Percentiles into SCFTemp;
	DATA SCFTemp;
		merge SCFTemp Percentiles1;
		by year;
	*create networth percentile categorical variables;
		if networth>=networth99 then NWPercentile="99";
		else if networth>=networth90 then NWPercentile="90";
		else if networth>=networth75 then NWPercentile="75";
		else if networth>=networth50 then NWPercentile="50";
		else if networth>=networth25 then NWPercentile="25";
		else if networth>=networth10 then NWPercentile="10";
		else NWPercentile="0";
	*create income percentile categorical variables;
		if income>=income99 then IPercentile="99";
		else if income>=income90 then IPercentile="90";
		else if income>=income75 then IPercentile="75";
		else if income>=income50 then IPercentile="50";
		else if income>=income25 then IPercentile="25";
		else if income>=income10 then IPercentile="10";
		else IPercentile="0";

	*create networth percentile categorical variables;
		if HomeEquity>=HomeEquity99 then HEPercentile="99";
		else if HomeEquity>=HomeEquity90 then HEPercentile="90";
		else if HomeEquity>=HomeEquity75 then HEPercentile="75";
		else if HomeEquity>=HomeEquity50 then HEPercentile="50";
		else if HomeEquity>=HomeEquity25 then HEPercentile="25";
		else if HomeEquity>=HomeEquity10 then HEPercentile="10";
		else HEPercentile="0";
	*create networth percentile categorical variables;
		if StockValue>=StockValue99 then SVPercentile="99";
		else if StockValue>=StockValue90 then SVPercentile="90";
		else if StockValue>=StockValue75 then SVPercentile="75";
		else if StockValue>=StockValue50 then SVPercentile="50";
		else if StockValue>=StockValue25 then SVPercentile="25";
		else if StockValue>=StockValue10 then SVPercentile="10";
		else SVPercentile="0";

	*Create percentile dummy variables;
		if networth>networth99 then OnePercent=1;
			else OnePercent=0;
		if networth>networth90 then TenPercent=1;
			else TenPercent=0;
		if networth>networth75 then TwentyfivePercent=1;
			else TwentyfivePercent=0;
			run;


*Sort data by imputation variable;
	proc sort data=scftemp threads;
		by _Imputation_ year;
	run;

/**Save Copy for R Visuals;
	Data ScfFull.SCFforR;
	set SCFTemp;
	run;
*/

/**Save Copy in csv format for Modeling in R;
	proc export data=scftemp
		   outfile='C:\Users\bda13\Desktop\Sociology\Statistics\SCF\SCF Full Data Files\rscffull.csv'
		   dbms=csv
		   replace;
	run;
*/


*TEMPORARY UNDO;


	proc stdize data=scftemp out=scfstand;
         var HSGraduate SomeCollege Bachelors AdvancedDegree black latino other Married Separated Divorced Widowed LivingWithPartner age agesquared Children ChildrenSquared year1 HeadWorkHoursOver40 HomemakerSpouse;
      run;


*Save Copy in csv format for Modeling in R;
	  *	(keep=RWorkStatus Female NWPercentile IPercentile _Imputation_ wgt HeadWorkHoursOver40	HomemakerSpouse	age	HSGraduate	SomeCollege	Bachelors	AdvancedDegree	white	black	latino	other	Married	Separated	Divorced	Widowed	LivingWithPartner	Children	year	networth	StockValue	HomeEquity	iself late )
;
	proc export data=scftemp
			(keep=RWorkStatus SWorkStatus Female NWPercentile IPercentile _Imputation_ wgt HeadWorkHoursOver40	HomemakerSpouse	age	HSGraduate	SomeCollege	Bachelors	AdvancedDegree	white	black	latino	other	Married	Separated	Divorced	Widowed	LivingWithPartner	Children	year	networth	StockValue	HomeEquity	iself late WAGEINC X4112 X4712 X4105 X4705 SHoursPerWeek year1 HEPercentile SVPercentile Income ifinplan ifinpro)
		   outfile='C:\Users\bda13\Desktop\Sociology\Statistics\SCF\SCF Full Data Files\rscf.csv'
		   dbms=csv
		   replace;
	run;
*/



*limit sample to households with male, non-retired heads, where the spouses are married;
	data scftemp;
		set scftemp;
		if Female=1 then delete;
		if RWorkStatus~="Employed" then delete;
		if MARRIED=0 then delete;
		if X4712>0 & SWorkStatus="Homemaker" then delete;
		income2=income;
		Income=loggedincome;
	run;
