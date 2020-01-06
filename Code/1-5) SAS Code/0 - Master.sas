*Description: 
	The following macro runs all of the user's SCF SAS programs. 

*Directions: 
	The only input required from the user is in Step 1: the user must specify the
	directories of the SCF extract and SCF Full data files. Do not use quotations.

*0 - Prep
		*0a - Prep workspace and cpu resources;
			OPTIONS THREADS;
			PROC DATASETS lib=work kill nolist memtype=data;
			quit;

		*0b - (Requires user input) Set directories of SCF files and prorgrams;
			*File paths;
				%let extractp= C:\Users\bda13\Desktop\Sociology\Statistics\SCF\SCF Extract Data Files; *Extract files;
				%let fullp= C:\Users\bda13\Desktop\Sociology\Statistics\SCF\SCF Full Data Files; *Full files;
			*Program paths;
				%let program= C:\Users\bda13\Desktop\Sociology\Papers in Progress\Work Hours\Code; *programs;

		*0c - Assign Library (place to store data);
			libname ScfExtr "&extractp";	
			libname ScfFull "&fullp";

*1 - Combine data and create year variables. Must modify this program's directories to use;
	*%Include "&program/1 - Prep Data.sas";

*2 - Create Variables for SCF;
	%Include "&program/2 - Create Variables for SCF.sas";

*4 - Basic Descriptives Stats;
*	&%Include "&program/3 - Basic Descriptives Stats.sas";
	
