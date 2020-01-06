#0 - library
    library(readxl)
    library(XML)
    library(RCurl)
    library(rlist)
    library(stringr)
    library(weights)
    library(ggplot2)
    library(directlabels)
    library(Hmisc)
    library(cowplot)
    library(data.table)
    library(ggthemes)
    library(haven)
    
#1 - functions
    weighted.median<-function(var,weight) as.numeric(wtd.quantile(var,weight,.5))
    weighted.sd<-function(var,weight) sqrt(wtd.var(var,weight))
    
#2 - examine homemaker earnings.
    scf<-read.csv("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/rscf.csv")
    scf<-scf[scf$RWorkStatus=="Employed" & scf$Female==0,]
      cor(scf$WAGEINC,scf$X4112)
      table(scf$X4712[scf$SWorkStatus=="Homemaker"]!=0)
      table(scf$X4712[scf$SWorkStatus=="Homemaker"]>1000)
      summary(scf$X4712[scf$SWorkStatus=="Employed"])
      summary(scf$X4712[scf$SWorkStatus=="Homemaker"])
      summary(scf$X4712[scf$SWorkStatus=="Homemaker" & !(scf$X4712<=0)])
      quantile(scf$X4712[scf$SWorkStatus=="Homemaker"],c(1:50)/50)
      quantile(scf$X4712[scf$SWorkStatus=="Employed"],c(1:50)/50)
        # it turns out some of the female homemakers were making wage income...
  #b) how many homemakers who don't report making money also say they are getting paid?
      table(scf$X4705[scf$SWorkStatus=="Homemaker"]==1 | scf$X4712[scf$SWorkStatus=="Homemaker"]>0)
      prop.table(table(scf$X4705[scf$SWorkStatus=="Homemaker"]==1 | scf$X4712[scf$SWorkStatus=="Homemaker"]>0))
      table(scf$X4705[scf$SWorkStatus=="Homemaker"]==1 & scf$X4712[scf$SWorkStatus=="Homemaker"]<=0)
        #a few of them
  #c) hours per week of these women?
      table(scf$SHoursPerWeek[scf$SWorkStatus=="Homemaker"])
      summary(scf$SHoursPerWeek[scf$SWorkStatus=="Homemaker"])
      summary(scf$SHoursPerWeek[scf$SWorkStatus=="Homemaker" & (scf$X4705==1 | scf$X4712)])
      summary(scf$SHoursPerWeek[scf$SWorkStatus=="Homemaker" & !(scf$X4705==1 | scf$X4712)])
      
#3 - examine female breadwinners
      scf<-read.csv("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/rscf.csv")
      a<-scf[scf$Female==1 & scf$RWorkStatus=="Homemaker",]
      600/238880
      a<-scf[scf$Female==1,]
      table(a$SWorkStatus)
          #there are 8 households where the female is a head and a homemaker, and the male is not a head but employed.
      a<-scf[scf$Female==1,]
      table(a$SWorkStatus)
          #there are 13 households where the female is a head the male is the homemaker.
          #this leaves a grey area that I won't want to discuss (e.g. wife employed and male retired)
      
#4 - Reduce sample to just married people
    scf<-read.csv("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/rscf.csv")
    scf<-scf[scf$Female==0 & scf$RWorkStatus=="Employed",]
    prop.table(table(scf$MARRIED))
        #kills a quarter of the sample. Not too bad.
  
#5 - Financial planners by wealth
    prop.table(table(scf$IFINPRO,scf$NWPercentile),2)
    prop.table(table(scf$IFINPLAN,scf$NWPercentile),2)
    prop.table(table(scf$ISELF,scf$NWPercentile),2)
      
    
#6 - household heads
    scf<-read.csv("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/rscf.csv")
    scf<-scf[scf$Female==0,]
    table(scf$RWorkStatus=="Homemaker" & scf$SWorkStatus=="Employed")
    table(scf$RWorkStatus=="Homemaker" & scf$SWorkStatus=="Employed",scf$NWPercentile)
    455/5
#X - Look at poor people who overwork.
    # scf<-read.csv("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/rscf.csv")
    # scf<-scf[scf$Female==0 & scf$RWorkStatus=="Employed" & scf$MARRIED==1,]
    # 
    # scfpr0<-scf[scf$NWPercentile<10 & scf$HeadWorkHoursOver40==0,]
    # scfpr.5<-scf[scf$NWPercentile<10 & scf$HeadWorkHoursOver40>0,]
    # scfpr1<-scf[scf$NWPercentile<10 & scf$HeadWorkHoursOver40>0 & scf$HeadWorkHoursOver40<10,]
    # scfpr2<-scf[scf$NWPercentile<10 & scf$HeadWorkHoursOver40>=10  & scf$HeadWorkHoursOver40<20,]
    # scfpr3<-scf[scf$NWPercentile<10 & scf$HeadWorkHoursOver40>=20,]
    # 
    #   paste(round(weighted.mean(scfpr0$AGE,scfpr0$WGT),2),
    #         round(weighted.median(scfpr0$AGE,scfpr0$WGT),2))
    #   paste(round(weighted.mean(scfpr.5$AGE,scfpr.5$WGT),2),
    #         round(weighted.median(scfpr.5$AGE,scfpr.5$WGT),2))
    #   # paste(round(weighted.mean(scfpr1$AGE,scfpr1$WGT),2),
    #   #       round(weighted.median(scfpr1$AGE,scfpr1$WGT),2))
    #   # paste(round(weighted.mean(scfpr2$AGE,scfpr2$WGT),2),
    #   #       round(weighted.median(scfpr2$AGE,scfpr2$WGT),2))
    #   # paste(round(weighted.mean(scfpr3$AGE,scfpr3$WGT),2),
    #   #       round(weighted.median(scfpr3$AGE,scfpr3$WGT),2))
    #   # wtd.t.test(x=scfpr0$AGE[scfpr0$X_Imputation_==1], weight=scfpr0$WGT[scfpr0$X_Imputation_==1], y=scfpr.5$AGE[scfpr.5$X_Imputation_==1],weighty=scfpr.5$WGT[scfpr.5$X_Imputation_==1])
    #       #Overworkers are slightly younger.
    #   
    #   paste(round(weighted.mean(scfpr0$White,scfpr0$WGT),2),
    #         round(weighted.median(scfpr0$White,scfpr0$WGT),2))
    #   paste(round(weighted.mean(scfpr.5$White,scfpr.5$WGT),2),
    #         round(weighted.median(scfpr.5$White,scfpr.5$WGT),2))
    #   # paste(round(weighted.mean(scfpr1$White,scfpr1$WGT),2),
    #   #       round(weighted.median(scfpr1$White,scfpr1$WGT),2))
    #   # paste(round(weighted.mean(scfpr2$White,scfpr2$WGT),2),
    #   #       round(weighted.median(scfpr2$White,scfpr2$WGT),2))
    #   # paste(round(weighted.mean(scfpr3$White,scfpr3$WGT),2),
    #   #       round(weighted.median(scfpr3$White,scfpr3$WGT),2))
    #   # wtd.t.test(x=scfpr0$White[scfpr0$X_Imputation_==1], weight=scfpr0$WGT[scfpr0$X_Imputation_==1], y=scfpr.5$White[scfpr.5$X_Imputation_==1],weighty=scfpr.5$WGT[scfpr.5$X_Imputation_==1])
    #       #poor overworkers are more white.
    #   paste(round(weighted.mean(scfpr0$SomeCollege,scfpr0$WGT),2),
    #         round(weighted.median(scfpr0$SomeCollege,scfpr0$WGT),2))
    #   paste(round(weighted.mean(scfpr.5$SomeCollege,scfpr.5$WGT),2),
    #         round(weighted.median(scfpr.5$SomeCollege,scfpr.5$WGT),2))
    #   
    #   paste(round(weighted.mean(scfpr0$Bachelors,scfpr0$WGT),2),
    #         round(weighted.median(scfpr0$Bachelors,scfpr0$WGT),2))
    #   paste(round(weighted.mean(scfpr.5$Bachelors,scfpr.5$WGT),2),
    #         round(weighted.median(scfpr.5$Bachelors,scfpr.5$WGT),2))
    #   
    #   paste(round(weighted.mean(scfpr0$AdvancedDegree,scfpr0$WGT),2),
    #         round(weighted.median(scfpr0$AdvancedDegree,scfpr0$WGT),2))
    #   paste(round(weighted.mean(scfpr.5$AdvancedDegree,scfpr.5$WGT),2),
    #         round(weighted.median(scfpr.5$AdvancedDegree,scfpr.5$WGT),2))
    #   
    #   
    #   
    #   paste(round(weighted.mean(scfpr0$hssc,scfpr0$WGT),2),
    #         round(weighted.median(scfpr0$hssc,scfpr0$WGT),2))
    #   paste(round(weighted.mean(scfpr.5$hssc,scfpr.5$WGT),2),
    #         round(weighted.median(scfpr.5$hssc,scfpr.5$WGT),2))
    #   # paste(round(weighted.mean(scfpr1$Bachelors,scfpr1$WGT),2),
    #   #       round(weighted.median(scfpr1$Bachelors,scfpr1$WGT),2))
    #   # paste(round(weighted.mean(scfpr2$Bachelors,scfpr2$WGT),2),
    #   #       round(weighted.median(scfpr2$Bachelors,scfpr2$WGT),2))
    #   # paste(round(weighted.mean(scfpr3$Bachelors,scfpr3$WGT),2),
    #   #       round(weighted.median(scfpr3$Bachelors,scfpr3$WGT),2))
    #   # wtd.t.test(x=scfpr0$Bachelors[scfpr0$X_Imputation_==1], weight=scfpr0$WGT[scfpr0$X_Imputation_==1], y=scfpr.5$Bachelors[scfpr.5$X_Imputation_==1],weighty=scfpr.5$WGT[scfpr.5$X_Imputation_==1])
    #       #slight overworkers are more college educated; extreme overworkers are not.
    # 
    #   
    #   scfpr.5$hssc<-ifelse(scfpr.5$HSGraduate==1 | scfpr.5$SomeCollege==1,1,0)
    #   scfpr0$hssc<-ifelse(scfpr0$HSGraduate==1 | scfpr0$SomeCollege==1,1,0)
    #   
    #   scfpr.5$lshs<-ifelse(scfpr.5$HSGraduate==1 | scfpr.5$SomeCollege==1 | scfpr.5$Bachelors==1 | scfpr.5$AdvancedDegree==1,0,1)
    #   scfpr.5$lshs<-ifelse(scfpr.5$HSGraduate==1 | scfpr.5$SomeCollege==1 | scfpr.5$Bachelors==1 | scfpr.5$AdvancedDegree==1,0,1)
    #   
    #   table(scfpr0$HSGraduate==1 | scfpr0$SomeCollege==1 | scfpr0$Bachelors==1 | scfpr0$AdvancedDegree==1) 
    #         
    #   scf$Bachelors
    #   scf$AdvancedDegree
    #   
    # summary(scf$White)
    # summary(scfpr0$White)
    # summary(scfpr1$White)
    # summary(scfpr2$White)
    # summary(scfpr3$White)
    # 
    # 
    # summary(scfpr$NETWORTH)
    # scf$HeadWorkHoursOver40
    # 
    # 