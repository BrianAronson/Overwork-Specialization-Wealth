*Description: 
	The following macro reads the frb SCF extract and complete fulls files,
	saves copies of these files to .sas7bdat format, appends the cross sections 
	into one large data file, and creates a merged file that includes both the 
	SCF extract and SCF full variables. 

Directions: 
	The only input required from the user is in Step 1: the user must specify the
	directories of the SCF extract and SCF Full data files. Do not use quotations.

Other Notes: 
	-Step 7 is optional and currently commented out. This step creates and saves all
	variables from the Full SCF and extract SCF. This file works, but it is rather
	unwieldy.

	-Step 8 is the alternative to Step 7. This step creates and saves a dataset
	containing all variables from the SCF extract, and custom variables from the 
	full SCF. This is recommended for use. It currently includes all variables that 
	I have used in my analyses. You may want to modify it per your convenience.

	-Steps 3, 5, 7 and 8 assume that the SCF extract files have been downloaded. I have
	included these on the flash drive. These steps can be commented out if desired, but 
	this is not recommended. The precoded variables from the SCF extract file are very 
	convenienient to use. Additionally, it is recommended that the SCF extract files be
	downloaded rather than generated with the Federal Reserve System's distributed macro.
	Running this macro takes much more time than simply downloading and extracting the 
	files.

	-You may be tempted to to cut and paste Steps 5 through 7 into other programs that 
	modifying the variables and run your descriptive statistics. I would not recommend 
	this. Step 6 can take anywhere between 3-15 minutes to run on current hardware.;


*1 - (Requires user input) Set directories of SCF files;
	%let extractp= C:\Users\bda13\Desktop\Sociology\Statistics\SCF\SCF Extract Data Files; *Extract files;
	%let fullp= C:\Users\bda13\Desktop\Sociology\Statistics\SCF\SCF Full Data Files; *Full files;

*2 - Assign Library (place to store data);
	libname ScfExtr "&extractp";	
	libname ScfFull "&fullp";

*3 - Import/convert SCF extract files;
		filename importin "&extractp\Scfp1989";
		PROC CIMPORT infile=importin library=ScfExtr memtype=data;
		RUN;
		filename importin "&extractp\Scfp1992";
		PROC CIMPORT infile=importin library=ScfExtr memtype=data;
		RUN;
		filename importin "&extractp\Scfp1995";
		PROC CIMPORT infile=importin library=ScfExtr memtype=data;
		RUN;
		filename importin "&extractp\Scfp1998";
		PROC CIMPORT infile=importin library=ScfExtr memtype=data;
		RUN;
		filename importin "&extractp\Scfp2001";
		PROC CIMPORT infile=importin library=ScfExtr memtype=data;
		RUN;
		filename importin "&extractp\Scfp2004";
		PROC CIMPORT infile=importin library=ScfExtr memtype=data;
		RUN;
		filename importin "&extractp\Scfp2007";
		PROC CIMPORT infile=importin library=ScfExtr memtype=data;
		RUN;
		filename importin "&extractp\Scfp2010";
		PROC CIMPORT infile=importin library=ScfExtr memtype=data;
		RUN;
		filename importin "&extractp\Scfp2013";
		PROC CIMPORT infile=importin library=ScfExtr memtype=data;
		RUN;
		filename importin "&extractp\Scfp2016";
		PROC CIMPORT infile=importin library=ScfExtr memtype=data;
		RUN;
		
*4 - Import/convert Full SCF files;
	*import SCF files;
		filename importin "&fullp\Scf89";
		PROC CIMPORT infile=importin library=ScfFull memtype=data;
		RUN;
		filename importin "&fullp\Scf92";
		PROC CIMPORT infile=importin library=ScfFull memtype=data;
		RUN;
		filename importin "&fullp\Scf95";
		PROC CIMPORT infile=importin library=ScfFull memtype=data;
		RUN;
		filename importin "&fullp\Scf98";
		PROC CIMPORT infile=importin library=ScfFull memtype=data;
		RUN;
		filename importin "&fullp\Scf2001";
		PROC CIMPORT infile=importin library=ScfFull memtype=data;
		RUN;
		filename importin "&fullp\Scf2004";
		PROC CIMPORT infile=importin library=ScfFull memtype=data;
		RUN;
		filename importin "&fullp\Scf2007";
		PROC CIMPORT infile=importin library=ScfFull memtype=data;
		RUN;
		filename importin "&fullp\Scf2010";
		PROC CIMPORT infile=importin library=ScfFull memtype=data;
		RUN;
		filename importin "&fullp\Scf2013";
		PROC CIMPORT infile=importin library=ScfFull memtype=data;
		RUN;
		filename importin "&fullp\Scf2016";
		PROC CIMPORT infile=importin library=ScfFull memtype=data;
		RUN;

*5 - Append the individual cross-sections of the SCF extract files into one dataset;
		DATA ScfExtr.ScfExtr;
			set ScfExtr.Scfp1989 ScfExtr.Scfp1992 ScfExtr.Scfp1995 ScfExtr.Scfp1998 ScfExtr.Scfp2001 ScfExtr.Scfp2004 ScfExtr.Scfp2007 ScfExtr.Scfp2010 ScfExtr.Scfp2013 ScfExtr.Scfp2016 indsname=year1; 
			year=1*substr(year1,ANYDIGIT(year1)); *Creates a year variable;
			Dataset="Extract";
			*fix household IDs for old years;
				if YY1=. then YY1=XX1*1;
				if Y1=. then Y1=X1*1;
			*Delete duplicate ID variables;
				drop X1;
				drop XX1;
		run; 

*6 - Append the individual cross-sections of the Full SCF files into one dataset;
	*The lengths of some variables differ over time. To prevent truncation, sql syntax is used. This assigns each variable length to the longest across each file;
			proc sql;
				create table SCFFull.SCFFull as
				select *, 1989 as year
				from ScfFull.P89i6 
				OUTER UNION CORR
				select *, 1992 as year
				from ScfFull.P92i4 
				OUTER UNION CORR
				select *, 1995 as year
				from ScfFull.P95i6
				OUTER UNION CORR
				select *, 1998 as year
				from ScfFull.P98i6 
				OUTER UNION CORR
				select *, 2001 as year
				from ScfFull.P01i6 
				OUTER UNION CORR
				select *, 2004 as year
				from ScfFull.P04i6
				OUTER UNION CORR
				select *, 2007 as year
				from ScfFull.P07i6 
				OUTER UNION CORR
				select *, 2010 as year
				from ScfFull.P10i6
				OUTER UNION CORR
				select *, 2013 as year
				from ScfFull.P13i6
				OUTER UNION CORR
				select *, 2016 as year
				from ScfFull.P16i6;
			quit;
	*Make minor necessary tweaks;
		DATA SCFFull.SCFFull;
			set SCFFull.SCFFull; 
			*fix household IDs for old years;
				if YY1=. then YY1=XX1*1;
				if Y1=. then Y1=X1*1;
			*Delete duplicate ID variables;
				drop X1;
				drop XX1;
			*Indicate dataset;
				Dataset="Full";
		run; 
/*
*7 - (Optional and not recommended) Merge the full data with the extract data and save as new dataset;
	*Sort each dataset;
		proc sort data=ScfFull.ScfFull threads;
			by year Y1 YY1;
		run;
		proc sort data=ScfExtr.ScfExtr threads;
			by year Y1 YY1;
		run;
	*Merge ;
		Data ScfFull.Combined;
			merge ScfExtr.ScfExtr ScfFull.ScfFull;
	       		run;
*/

*8 - (Optional but recommended) Create a subset dataset, containing variables of interest from SCF.Full, then merge with SCFExtr;
	*Create temporary dataset with variables of interest;
		DATA temp;
			set SCFFull.ScfFull (keep=  year YY1 Y1 X2001 X2002 X2003 X2004 X2009 X2505 X2506 X2605 X2606 X2623 X4020 X4024 X4028 X4022 X4026 X4030 X4110 X4710 X6670 X6678 X4111 X4711 X108 X114 X120 X126 X132 X202 X208 X214 X220 X226 X4100 X4700 X5801 X5802 X6703 X5803 X5808 X5813 X5804 X5809 X5814 X5805 X5810 X5815 X5806 X5811 X5816 X5818 X5819 X5821
			X6526 X504 X3737 X3743 X3749 X3755 X3761 X5731 X8020 X102 X108 X114 X120 X126 X132 X202 X208 X214 X220 X226 X7021 X8023 X5906 X101 X3731 X3737 X3743 X3749 X3755 X3761 X2505 X2605 X1223 x7801 X5733 X4115 X6403 X5801 X5802 X5803 X5808 X5813 X5804 X5809 X5814 X5805 X5810 X5815 X5806 X5811 X5816 X8022 X104 X110 X116 X122 X128 X134 X204 X210 X216 X222 X228
X4112 X4712 X4105 X4705);

		run;
	*Sort each dataset;
		proc sort data=temp threads;
			by year Y1 YY1;
		run;
		proc sort data=ScfExtr.ScfExtr threads;
			by year Y1 YY1;
		run;
	*Merge ;
		Data ScfFull.Combined2;
			merge ScfExtr.ScfExtr temp;
	     run;
