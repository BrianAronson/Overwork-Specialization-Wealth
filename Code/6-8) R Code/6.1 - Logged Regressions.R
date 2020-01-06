#0 - Prep
    #a - load libraries
        library(data.table)
        library(mice)
        library(quantreg)
        library(foreign)
        library(gtools)
        library(Hmisc)
  #      library(uqr)
    #b - load data
        scf<-read.csv("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/rscf.csv")
  
#1 - prep data
    #a - uncapitalize and capitalize where it makes sense to
        scf$Networth<-scf$NETWORTH
        scf$Other<-scf$OTHER
        scf$Married<-scf$MARRIED
        scf$Age<-scf$AGE
        scf$AgeSquared<-scf$agesquared
        scf$ChildrenSquared<-scf$Childrensquared
        scf$Year1<-scf$year1
    #b - identify variables of interest
        vars<- c("Networth","HSGraduate","SomeCollege","Bachelors","AdvancedDegree","Black",
        "Latino","Other","Married","Separated","Divorced","Widowed","LivingWithPartner",
        "Age","AgeSquared","Children","ChildrenSquared","Year1","HeadWorkHoursOver40","HomemakerSpouse")
    #c - convert to data table for speed
        scf<-data.table(scf)
    #d - separate data into separate lists based on replicates
        perc<-sort(unique(scf$NWPercentile))
        scf$NWPercentile<-factor(scf$NWPercentile,levels=perc)
    #e - within replicate lists, split by quantile 
        lscf<-split(scf,scf$X_Imputation_)
        for(i in 1:length(lscf)){
          lscf[[i]]<-split(lscf[[i]],lscf[[i]]$NWPercentile)
        }
        scf$ISELF
#3 - Standard quantile regression
    #run models
        qr<-(vector(7,mode="list"))
        for(i in 1:length(qr)){
          qr[[i]]<-(vector(5,mode="list"))
        }
        for(j in 1:length(perc)){
          for(i in 1:5){
            qr[[j]][[i]]<-(glm(formula=(ISELF~HSGraduate+SomeCollege+Bachelors+
                                AdvancedDegree+ Black+Latino+Other+Married+Separated+Divorced+
                                Widowed+LivingWithPartner+Age+AgeSquared+Children+
                                ChildrenSquared+Year1+HeadWorkHoursOver40+HomemakerSpouse+
                                HeadWorkHoursOver40*HomemakerSpouse + Year1*HeadWorkHoursOver40)
                     ,data=lscf[[i]][[j]]
                     ,family=binomial))
            }
         }

    #This is just for making plots...
        for(i in 1:length(qr)){
            temp<-lapply(qr[[i]],summary)
            temp<-lapply(temp,"[[","coefficients")
            temp<-c(lapply(temp,as.data.frame))
            regres<-data.frame(
                "_Imputation_"=rep(1:5,each=length(qr[[i]][[1]]$coefficients)),
                Quantile=perc[i],
                Parameter=rep(names(qr[[i]][[1]]$coefficients),5),
                DF=1,
                Estimate=c(sapply(qr[[i]],"[[","coefficients")),
                "Std Err"=c(sapply(temp,"[[",2))
        )
        write.csv(regres,(paste("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/",i,"regres.csv",sep = "")))
        }
        head(regres)
        
        
