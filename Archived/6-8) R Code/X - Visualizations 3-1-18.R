rm(list=ls())
gc()
library(ggplot2)
# library(sas7bdat)
library(directlabels)
load("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/scfforr.rdata")
scf<-scfforr #change name
rm(scfforr) #kill old one

summary(scf$AmountExpected)
summary(scf$TotalInheritance)
summary(scf$year)
#Fix outliers in inheritance ratios - Some of these codings are negative because some participants have negative NW. This is not problematic for 1%
scf$InheritanceNWRatio<-ifelse(is.na(scf$InheritanceNWRatio),0,ifelse(scf$InheritanceNWRatio<(0),(0),ifelse(scf$InheritanceNWRatio>1,1,scf$InheritanceNWRatio)))
#Fix Outliers in capital gains  -  Poor people screw this up somehow
scf$PIncomeCapitalGains<-ifelse(scf$IPercentile=="0" & scf$PIncomeCapitalGains<0,0,ifelse(scf$IPercentile=="0" & scf$PIncomeCapitalGains>1,1,scf$PIncomeCapitalGains))
#Fix Outliers in dividends  -  90th percentile in 1995 screw this up somehow
scf$PIncomeDividends<-ifelse(scf$NWPercentile=="90" & scf$PIncomeDividends<0,0,ifelse(scf$NWPercentile=="90" & scf$PIncomeDividends>1,1,scf$PIncomeDividends))
#Create variables
scf$Retired<-ifelse(scf$RWorkStatus=="Retired",1,0)
scf$ageC<-ifelse(scf$Age25to39==1,"25",ifelse(scf$Age40to49==1,"40",ifelse(scf$Age50to59==1,"50",ifelse(scf$Age60to69==1,"60","70"))))
scf$ageC<-ifelse(scf$age<25 & scf$age>20,"20",ifelse(scf$age<30,"25",ifelse(scf$age<35,"30",ifelse(scf$age<40,"35",ifelse(scf$age<45,"40",ifelse(scf$age<50,"45",ifelse(scf$age<55,"50",ifelse(scf$age<60,"55",ifelse(scf$age<65,"60",ifelse(scf$age<70,"65",ifelse(scf$age<75,"70","")))))))))))
#Percentiles to character
scf$NWPercentile<-as.character(scf$NWPercentile)
scf$IPercentile<-as.character(scf$IPercentile)
# Control for young age, old age, and employment status 
# Cscf1<-scf[scf$RWorkStatus=="Employed",]
# Cscf2<-scf[scf$Age25to64==1,]
# Cscf3<-scf[scf$NWPercentile=="99",]
# Cscf4<-scf[scf$age<60,]
# Cscf1$RHoursPerWeek80<-ifelse(Cscf1$RHoursPerWeek>80,80,Cscf1$RHoursPerWeek)
# Cscf5<-Cscf1[Cscf1$RHoursPerWeek>40,]
# #Make mean hours per week variable by percentile by year (with controls (For those employed and between ages 25 and 65)
# Cscf11<-aggregate(cbind(RHoursPerWeek, RWeeksPerYear)~year+NWPercentile, FUN=mean, data=Cscf1)
# Cscf12<-aggregate(cbind(RHoursPerWeek, RWeeksPerYear)~year+IPercentile, FUN=mean, data=Cscf1)
# Cscf13<-aggregate(cbind(Homemaker)~year+NWPercentile, FUN=mean, data=Cscf1)
# Cscf14<-aggregate(cbind(Homemaker)~year+RHoursPerWeek80+NWPercentile+IPercentile, FUN=mean, data=Cscf5)
# 
# #Make mean hours per week variable by percentile by year without controls
# scf11<-aggregate(cbind(Retired, Homemaker, Age25to64, InheritanceNWRatio, Manager, PIncomeCapitalGains, PIncomeSalary, PIncomeDividends)~year+NWPercentile, FUN=mean, data=scf)
# scf12<-aggregate(cbind(Retired, Homemaker, Age25to64, InheritanceNWRatio, Manager, PIncomeCapitalGains, PIncomeSalary, PIncomeDividends)~year+IPercentile, FUN=mean, data=scf)
# scf22<-aggregate(cbind(Retired)~year+NWPercentile, FUN=mean, data=Cscf2) #Retirement under age 65
# scf23<-aggregate(cbind(Retired)~year+ageC, FUN=mean, data=Cscf3) #Retirement age for 99P
# scf24<-aggregate(cbind(Retired,AmountExpected)~year, FUN=mean, data=Cscf3) #Retirement under age 60

#scf<-scf[scf$Female==0,]

#########################  Figures for paper  #########################

setwd("C:/Users/bdaro_000/Sociology/Papers/Lisa Projects/Paper - 1% Work Hours Paper/Figures and Tables/")

library(Hmisc)
#Rate of homemaker spouses by percentile over time
    #Determine weighted means
        tempscf<-scf[scf$RWorkStatus=="Employed" & scf$Female==0,]
      #remove 25th percentile
        tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="25","0",tempscf$NWPercentile)
        tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="75","50-89",tempscf$NWPercentile)
        tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="50","50-89",tempscf$NWPercentile)
        tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="0","0-49",tempscf$NWPercentile)
        tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="90","90-99",tempscf$NWPercentile)
        tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="99","99-100",tempscf$NWPercentile)
        
      #Create empty df
          aggscf<-as.data.frame(matrix(nrow=length(unique(tempscf$year))*length(unique(tempscf$NWPercentile)),ncol=1))
          names(aggscf)<-"Homemaker"
          aggscf$Year<-rep(unique(tempscf$year),length(unique(tempscf$NWPercentile)))
          aggscf$NWPercentile<-rep(sort(unique(tempscf$NWPercentile)),each=length(unique(tempscf$year)))
          aggscf$Var<-0
      #Find weighted means and variances
          for (i in 1:nrow(aggscf)){
            aggscf$Homemaker[i]<-wtd.mean(tempscf$Homemaker[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]],tempscf$wgt[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]])
            aggscf$Var[i]<-wtd.var(tempscf$Homemaker[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]],tempscf$wgt[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]])
          }
      #Determine SEM while accounting for fact that numbers are below 100.
          aggscf$SEM<-sqrt(100*aggscf$Var)/100
      #Plot graph
          p<-ggplot(aggscf, aes((Year), y=Homemaker))
          pdf(file="Fig - Changing Rate of Homemaker Spouses by Wealth Percentile.pdf", height=6, width=11)
              p+ geom_line(aes(group=NWPercentile, color=NWPercentile),size=2)+
                scale_colour_grey(name="Net Worth Percentile",start=.75,end=0)+
                scale_y_continuous(labels=scales::percent)+ #,limits = c(0,.51)
                xlim(1988,2017)+
                theme_bw()+
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                      legend.position="none", title = element_text(size=18),
                      axis.text.x = element_text(size=14),  axis.text.y = element_text(size=14),  
                      axis.title.x= element_text(size=14,margin = margin(t = 20, r = 0, b = 0, l = 0)),  
                      axis.title.y= element_text(size=14,margin = margin(t = 0, r = 20, b = 0, l = 0)))+
                ggtitle("Changing Rate of Homemaker Spouses by Wealth Percentile")+
                geom_dl(aes(label = paste("  ",NWPercentile,"%  ",sep="")), method = list(c('first.bumpup'),dl.combine(first.points), cex = .9)) +
                geom_dl(aes(label = paste("  ",NWPercentile,"%  ",sep="")), method = list(c('last.bumpup'),dl.combine(last.points), cex = .9)) +
                #geom_errorbar(aes(ymin = Homemaker - SEM, ymax = Homemaker + SEM), width=0.3)+
                labs(x="Year", y="Percent Homemakers")
          dev.off()
      
          
#Rate of work hours by age and percentile
    #Determine weighted means
        tempscf<-scf[scf$age>29 & scf$age<75 & scf$Female==0,]#[scf$RWorkStatus=="Employed" & scf$Age25to64==1,] #scf$RHoursPerWeek>32
            # remove responses above 80 and below 0
                tempscf$RHoursPerWeek80<-ifelse(tempscf$RHoursPerWeek>80,80,tempscf$RHoursPerWeek)

            #remove 25th and 75th percentile
                 tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="25","0",tempscf$NWPercentile)
                 tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="90","90-98",tempscf$NWPercentile)
                 tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="0","0-49",tempscf$NWPercentile)
                 tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="50","50-89",tempscf$NWPercentile)
                 tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="75","50-89",tempscf$NWPercentile)
                 tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="99","99-100",tempscf$NWPercentile)
                 tempscf$year<-as.numeric(tempscf$ageC)
            #Create empty df
                aggscf<-as.data.frame(matrix(nrow=length(unique(tempscf$year))*length(unique(tempscf$NWPercentile)),ncol=1))
                names(aggscf)<-"WorkHours"
                aggscf$Year<-rep(unique(tempscf$year),length(unique(tempscf$NWPercentile)))
                aggscf$NWPercentile<-rep(sort(unique(tempscf$NWPercentile)),each=length(unique(tempscf$year)))
                aggscf$Var<-0
            #Find weighted means and variances
                for (i in 1:nrow(aggscf)){
                  aggscf$WorkHours[i]<-wtd.mean(tempscf$RHoursPerWeek80[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]],tempscf$wgt[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]])
                  aggscf$SEM[i]<-sqrt(wtd.var(tempscf$RHoursPerWeek80[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]],tempscf$wgt[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]]))
                }
            #Change age labels
                aggscf$Year<-ifelse(aggscf$Year==30,"30-34",ifelse(aggscf$Year==35,"35-39",ifelse(aggscf$Year==40,"40-44",ifelse(aggscf$Year==45,"45-49",ifelse(aggscf$Year==50,"50-54",ifelse(aggscf$Year==55,"55-59",ifelse(aggscf$Year==60,"60-64",ifelse(aggscf$Year==65,"65-69",ifelse(aggscf$Year==70,"70-74","")))))))))

            #Plot graph
                p<-ggplot(aggscf, aes(y=WorkHours,x=Year))
                pdf(file="Fig - Average Work Hours by Age and Wealth Percentile.pdf", height=6, width=11)
                    p+geom_bar(aes(fill = NWPercentile), position = "dodge", stat="identity")+
                    theme_bw()+
                    scale_fill_grey(start = .9, end = 0)+
                    labs(x="Age Range", y="Average Breadwinner Work Hours",fill="Percentile")+
                    ggtitle("Average Work Hours by Age and Wealth Percentile")+
                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.line = element_line(colour = "black"),
                        legend.text=element_text(size=12), title = element_text(size=18),
                        legend.title=element_text(size=14),
                        axis.text.x = element_text(size=14),  axis.text.y = element_text(size=14),
                        axis.title.x= element_text(size=14,margin = margin(t = 20, r = 0, b = 0, l = 0)),
                        axis.title.y= element_text(size=14.5,margin = margin(t = 0, r = 20, b = 0, l = 0)))
                dev.off()
                


#Networth allocated to asset types by percentile
    #subset data
        tempscf<-scf[scf$RWorkStatus=="Employed" & scf$Female==0,]
        #remove 0th percentile
            tempscf<-tempscf[tempscf$NWPercentile!="0",]
    #Find weighted means and variances
        #Create empty df
            tempdf<-data.frame(Percentile=rep(sort(unique(tempscf$NWPercentile)),4),Specialized=rep(rep(c(0,1),each=5),2),Asset.Type=rep(c("Stocks and Mutual Funds","Home Equity"),each=10))
        for (i in 1:nrow(tempdf)){
          if(i<11){
            tempdf$Value[i]<-wtd.mean(tempscf$PStockValue[tempscf$HomemakerSpouse==tempdf$Specialized[i] & tempscf$NWPercentile==tempdf$Percentile[i]],tempscf$wgt[tempscf$NWPercentile==tempdf$Percentile[i] & tempscf$HomemakerSpouse==tempdf$Specialized[i]])
          }else{
            tempdf$Value[i]<-wtd.mean(tempscf$PHomeequity[tempscf$HomemakerSpouse==tempdf$Specialized[i] & tempscf$NWPercentile==tempdf$Percentile[i]],tempscf$wgt[tempscf$NWPercentile==tempdf$Percentile[i] & tempscf$HomemakerSpouse==tempdf$Specialized[i]])
          }
        }
    #Change indicator for specialization
        tempdf$Specialized<-ifelse(tempdf$Specialized=="0","Not Specialized  ","Specialized")
    #Change levels for Asset.Type
        tempdf$Asset.Type<-factor(tempdf$Asset.Type,levels = c(as.character(unique(tempdf$Asset.Type))))
    #Plot
        p <- ggplot(data = tempdf, aes(x = Percentile, y = Value))
        pdf(file="Fig - Average Percent of Net Worth Allocated to Asset Type by Wealth Percentile.pdf", height=6, width=11)
            p + geom_bar(aes(fill = as.factor(Specialized)), position = "dodge", stat="identity")+
            facet_wrap(~Asset.Type, scales='free')+
            scale_y_continuous(labels=scales::percent)+
            theme_bw()+
            scale_fill_grey(start = .75, end = 0)+
            labs(x="Wealth Percentile", y="Percent of Net Worth Allocated",fill="")+
            ggtitle("Average Percent of Net Worth Allocated to Asset Type by Wealth Percentile")+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"),
                  legend.text=element_text(size=11), title = element_text(size=14),
                  legend.position="bottom",
                  axis.text.x = element_text(size=14),  axis.text.y = element_text(size=14),
                  axis.title.x= element_text(size=14,margin = margin(t = 20, r = 0, b = 0, l = 0)),
                  axis.title.y= element_text(size=14,margin = margin(t = 0, r = 20, b = 0, l = 0)),
                  strip.text.x = element_text(size = 14))
        dev.off()

        
    #Quantile parameter estimates
        df<-read.table("clipboard",sep="\t",header=T)
        #show homemaker spouse effect in thousands
            df[df$Parameter=="HomemakerSpouse",c("Estimate","Std.Err")]<-df[df$Parameter=="HomemakerSpouse",c("Estimate","Std.Err")]
            df[df$Parameter=="HeadWorkH*HomemakerS",c("Estimate","Std.Err")]<-df[df$Parameter=="HomemakerSpouse",c("Estimate","Std.Err")]
            
        df$conf.low<-df$Estimate-df$Std.Err*1.96
        df$conf.high<-df$Estimate+df$Std.Err*1.96
        df1<-df[df$Type=="Home Equity",]
        
        
        head(df)
        p <- ggplot(df1[df1$Percentile==0.99,], aes(y=Estimate,x=reorder(Parameter, Estimate)))
        p + geom_pointrange(aes(ymin=conf.low, ymax=conf.high))
#          geom_hline() +
#          coord_flip()

        
        
    #Investment behaviors by homemaker/networth percentile
        
        
        #Networth allocated to asset types by percentile
            df<-read.table("clipboard",sep="\t",header=T)
            df$Behavior<-factor(df$Behavior,levels = c(as.character(unique(df$Behavior))))
            df$Percentile<-factor(df$Percentile,levels = c(as.character(unique(df$Percentile))))
            names(df)
            #Plot
                p <- ggplot(data = df, aes(x = Percentile, y = Estimate))
                pdf(file="Fig - Financial Behaviors by Wealth Percentile and Specialization.pdf", height=6, width=11)
                    p + geom_bar(aes(fill = as.factor(Specialized)), position = "dodge", stat="identity")+
                    facet_wrap(~Behavior, scales='free')+
                    scale_y_continuous(labels=scales::percent)+
                    theme_bw()+
                    scale_fill_grey(start = .75, end = 0)+
                    labs(x="Wealth Percentile", y="Rate of Financial Behavior",fill="")+
                    ggtitle("Financial Behaviors by Wealth Percentile and Specialization")+
                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.background = element_blank(), axis.line = element_line(colour = "black"),
                          legend.text=element_text(size=11), title = element_text(size=14),
                          legend.position="bottom",
                          axis.text.x = element_text(size=14),  axis.text.y = element_text(size=14),
                          axis.title.x= element_text(size=14,margin = margin(t = 20, r = 0, b = 0, l = 0)),
                          axis.title.y= element_text(size=14,margin = margin(t = 0, r = 20, b = 0, l = 0)),
                          strip.text.x = element_text(size = 14))
                dev.off()
        
                

# ##REMOVED - The data kind of shows what I want, but the interpretation is a little murky. Rates of specialization are correlated with wealth (not surprising), but rates of specialization increase slightly more for the 75th and 90th percentiles when they have small kids.
#     #Work specialization child under 3 by percentile
#         tempscf<-scf[scf$Female==0 & scf$married==1,]#[scf$RWorkStatus=="Employed" & scf$Age25to64==1,] #scf$RHoursPerWeek>32
#         #Create coordinates by percentile
#             df<-as.data.frame(matrix(nrow=length(unique(tempscf$Homemaker))*length(unique(tempscf$NWPercentile)),ncol=1))
#             names(df)<-"Child"
#             df$Child<-rep(c(0,1),nrow(df)/2)
#             df$Percentile<-rep(sort(unique(tempscf$NWPercentile)),each=2)
#             for(i in 1:nrow(df)){
#               df$Specialized[i]<-wtd.mean(tempscf$Homemaker[tempscf$NWPercentile==df$Percentile[i] & tempscf$ChildUnder3Dummy==df$Child[i]],tempscf$wgt[tempscf$NWPercentile==df$Percentile[i] & tempscf$ChildUnder3Dummy==df$Child[i]])
#               df$SEM[i]<-sqrt(100*wtd.var(tempscf$Homemaker[tempscf$NWPercentile==df$Percentile[i] & tempscf$ChildUnder3Dummy==df$Child[i]],tempscf$wgt[tempscf$NWPercentile==df$Percentile[i] & tempscf$ChildUnder3Dummy==df$Child[i]]))/100
#             }
#         #Plot difference in specialization by percentile
#             p<-ggplot(df, aes(y=Specialized,x=Percentile))
#             p+geom_bar(aes(fill = as.factor(Child)), position = "dodge", stat="identity")    
#         #Calculate percent increase in specialization by   
#             S1<-df[df$Child==1,]
#             S0<-df[df$Child==0,]
#             S0$dif<-(1-S0$Specialized)/(1-S1$Specialized)-1
#         #Plot % differences
#             p<-ggplot(S0, aes(y=dif,x=Percentile))
#             p+geom_bar(position = "dodge", stat="identity")    
    
    

        

        summary(tempscf$ChildUnder3Dummy)
        

##REMOVED - The effect is pretty uniform, except for in the 1% (this seems to suggest that specialization does not motivate more work hours)
#     Work hours by specialization by percentile
#         tempscf<-scf[scf$age>29 & scf$age<75 & scf$RWorkStatus=="Employed",]#[scf$RWorkStatus=="Employed" & scf$Age25to64==1,] #scf$RHoursPerWeek>32
#         # remove responses above 80 and below 0
#             tempscf$RHoursPerWeek80<-ifelse(tempscf$RHoursPerWeek>80,80,tempscf$RHoursPerWeek)
#         #Create coordinates by percentile
#             df<-as.data.frame(matrix(nrow=length(unique(tempscf$Homemaker))*length(unique(tempscf$NWPercentile)),ncol=1))
#             names(df)<-"Hours"
#             df$Specialized<-rep(c(0,1),nrow(df)/2)
#             df$Percentile<-rep(sort(unique(tempscf$NWPercentile)),each=2)
#             for(i in 1:nrow(df)){
#               df$Hours[i]<-wtd.mean(tempscf$RHoursPerWeek80[tempscf$NWPercentile==df$Percentile[i] & tempscf$Homemaker==df$Specialized[i]],tempscf$wgt[tempscf$NWPercentile==df$Percentile[i] & tempscf$Homemaker==df$Specialized[i]])
#             }
#         #Plot     
#             ggplot(df,aes(x=Hours,y=Specialized,group=Percentile))+geom_line()
            
            
#                 
#       #Homemakers in 1% - Less homemakers in all groups except 99 percent; controls make little substantive difference
#       p<-ggplot(Cscf13, aes((year), y=Homemaker))
#       p+ geom_line(aes(group=NWPercentile, color=NWPercentile),size=1)+
#         scale_colour_grey(name="Net Worth Percentile",start=.75,end=0)+
#         theme_bw()+
#         theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12))+
#         ggtitle("Rate of Homemaker Spouses by Percentile")+
#         ggsave(file="Rate of Homemaker Spouses by Percentile.pdf",dpi=300, height=6, width=9)
#       #Hours per week in 1%  -   Top Decile works more than others
#       p<-ggplot(Cscf11, aes(x=(year), y=RHoursPerWeek))
#         p+ geom_line(aes(group=NWPercentile, color=NWPercentile),size=1)+
#         scale_colour_grey(name="Net Worth Percentile",start=.75,end=0)+
#         theme_bw()+
#         theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12))+
#         ggtitle("Average Household Head's Work Hours by Percentile")              
#         ggsave(file="Average Household Head's Work Hours by Percentile.pdf",dpi=300, height=6, width=9)
#         
#       #Homemakers by hours per week
#         p<-ggplot(Cscf14, aes(x=(RHoursPerWeek80), y=Homemaker))
#         p+geom_bar(stat = "identity", width=5)
#         p+geom_smooth()
#         
#         p+geom_smooth(aes(group=IPercentile, color=IPercentile))
#           geom_text(aes(label = NWPercentile, vjust = -1, size=1))              
#         
#       
# ###################### Somewhat Interesting #####################
#     #Age25to64   -   99 percentile seems to be older
#       p<-ggplot(scf11, aes(x=(year), y=Age25to64))
#       p+ geom_line(aes(group=NWPercentile, color=NWPercentile))+
#         geom_text(aes(label = NWPercentile, vjust = -1, size=1))              
#     #Retired   -   more of 99 percentile is retired (suggests they are older)
#       p<-ggplot(scf11, aes(x=(year), y=Retired))
#       p+ geom_line(aes(group=NWPercentile, color=NWPercentile))+
#         geom_text(aes(label = NWPercentile, vjust = -1, size=1)) +
#         ggtitle("Retirement Rate by percentile")       
#         ggsave(file="Retirement Rate by percentile.pdf")
#     #Mean rate of retirement by year and agegroup in 99P  -   Suggests that more people in 99P are retiring across age groups 
#       p<-ggplot(scf23, aes(x=(year), y=Retired))
#       p+ geom_line(aes(group=ageC, color=ageC))+
#         geom_text(aes(label = ageC, vjust = -1, size=1)) +
#         ggtitle("Retirement Rate of 1% by age group")
#         ggsave(file="Retirement Rate of 1P by age group.pdf")
#     #Retired but under 60   -   Much more 99 percentile under age 60 retired.
#       p<-ggplot(Cscf3, aes(x=(jitter(year)), y=Retired))
#       p+ geom_smooth() +
#         ggtitle("Retirement Rate of 1% under age 60")
#         ggsave(file="Retirement Rate of 1% under age 60.pdf")
#     #Manager
#       p<-ggplot(scf11, aes(x=(year), y=Manager))
#       p+ geom_line(aes(group=NWPercentile, color=NWPercentile))+
#         geom_text(aes(label = NWPercentile, vjust = -1, size=1))
#     #Salary
#       p<-ggplot(scf11, aes(x=(year), y=PIncomeSalary))
#       p+ geom_line(aes(group=NWPercentile, color=NWPercentile))+
#         geom_text(aes(label = NWPercentile, vjust = -1, size=1))
#       ggtitle("Percent income from Salary by percentile")              
#       ggsave(file="Percent income from Salary by percentile.pdf")
#     #Capital gains
#       p<-ggplot(scf11, aes(x=(year), y=PIncomeCapitalGains))
#       p+ geom_line(aes(group=NWPercentile, color=NWPercentile))+
#         geom_text(aes(label = NWPercentile, vjust = -1, size=1))
#       ggtitle("Percent income from capital gains by percentile")              
#       ggsave(file="Percent income from capital gains by percentile.pdf")
#     #Dividends
#       p<-ggplot(scf11, aes(x=(year), y=PIncomeDividends))
#       p+ geom_line(aes(group=NWPercentile, color=NWPercentile))+
#         geom_text(aes(label = NWPercentile, vjust = -1, size=1))
#       ggtitle("Percent income from dividends by percentile")              
#       ggsave(file="Percent income from dividends by percentile.pdf")
#       
# #########################  LESS Interesting  #########################
#   #Networth
#     #Inheritance ratio  -  Barely anyone inherits a substantial portion of what they are worth.
#       p<-ggplot(Cscf3, aes(x=(jitter(year)), y=InheritanceNWRatio))
#       p+ geom_point()+geom_smooth()              
#     #Amount expected to inherit ratio to current net worth.
#       p<-ggplot(scf, aes(x=(jitter(year)), y=AmountExpected))
#       p+ geom_point()+geom_smooth()              
#       
#     #Weeks Per year
#       p<-ggplot(Cscf11, aes(x=(year), y=RWeeksPerYear))
#       p+ geom_line(aes(group=NWPercentile, color=NWPercentile))+
#         geom_text(aes(label = NWPercentile, vjust = -1, size=1))              
#     #Retired but under 65   -   more 99 percentile retired under age 65.
#       p<-ggplot(scf22, aes(x=(year), y=Retired))
#       p+ geom_line(aes(group=NWPercentile, color=NWPercentile))+
#         geom_text(aes(label = NWPercentile, vjust = -1, size=1))              
#         
#   #Income (no unique trends from networth)
#     #Age25to65 -  Less 99 Percent are aged 25-64, suggesting they are older
#       p<-ggplot(scf12, aes(x=(year), y=Age25to64))
#       p+ geom_line(aes(group=IPercentile, color=IPercentile))+
#         geom_text(aes(label = IPercentile, vjust = -1, size=1))                
#     #Retired  -   more of 99 percentile is retired (suggests they are older)
#       p<-ggplot(scf12, aes(x=(year), y=Retired))
#       p+ geom_line(aes(group=IPercentile, color=IPercentile))+
#         geom_text(aes(label = IPercentile, vjust = -1, size=1))              
#       come
#     #Hours per week
#       p<-ggplot(Cscf12, aes(x=(year), y=RHoursPerWeek))
#       p+ geom_line(aes(group=IPercentile, color=IPercentile))+
#         geom_text(aes(label = IPercentile, vjust = -1, size=1))              
#     #Weeks Per year
#       p<-ggplot(Cscf12, aes(x=(year), y=RWeeksPerYear))
#       p+ geom_line(aes(group=IPercentile, color=IPercentile))+
#         geom_text(aes(label = IPercentile, vjust = -1, size=1))              
#     #Homemaker
#       p<-ggplot(scf12, aes(x=(year), y=Homemaker))
#       p+ geom_line(aes(group=IPercentile, color=IPercentile))+
#         geom_text(aes(label = IPercentile, vjust = -1, size=1))              
#       #Manager
#       p<-ggplot(scf12, aes(x=(year), y=Manager))
#       p+ geom_line(aes(group=IPercentile, color=IPercentile))+
#         geom_text(aes(label = IPercentile, vjust = -1, size=1))
#       #Salary
#       p<-ggplot(scf12, aes(x=(year), y=PIncomeSalary))
#       p+ geom_line(aes(group=IPercentile, color=IPercentile))+
#         geom_text(aes(label = IPercentile, vjust = -1, size=1))
#       #Capital gains
#       p<-ggplot(scf12, aes(x=(year), y=PIncomeCapitalGains))
#       p+ geom_line(aes(group=IPercentile, color=IPercentile))+
#         geom_text(aes(label = IPercentile, vjust = -1, size=1))
#       #Dividends
#       p<-ggplot(scf12, aes(x=(year), y=PIncomeDividends))
#       p+ geom_line(aes(group=IPercentile, color=IPercentile))+
#         geom_text(aes(label = IPercentile, vjust = -1, size=1))
#       
#       
# 
#       
#             
#       
# #loess relationships
# #hours
#   #Networth
#   p<-ggplot(Cscf1, aes(x=(jitter(year)), y=RHoursPerWeek, color=NWPercentile))
#   p + geom_smooth(
#                   (aes(group=NWPercentile,
#                        color=(NWPercentile=="90")))) +
#     scale_color_manual(values=c("grey70", "firebrick", "blue"),
#                        labels=c("Other Percentiles", "90th Percentile"))
#   #income
#   p<-ggplot(Cscf1, aes(x=(jitter(year)), y=RHoursPerWeek, color=IPercentile))
#   p + geom_smooth(
#     (aes(group=IPercentile,
#          color=(IPercentile=="90")))) +
#     scale_color_manual(values=c("grey70", "firebrick", "blue"),
#                        labels=c("Other Percentiles", "90th Percentile"))
# 
#   
# #Weeks
#   #Networth
#   pdf(file="Weeks Per Year.pdf", height=10, width=15)
#   p<-ggplot(Cscf1, aes(x=(jitter(year)), y=RWeeksPerYear, color=NWPercentile))
#   p + geom_smooth(
#     (aes(group=NWPercentile,
#          color=(NWPercentile=="90")))) +
#     scale_color_manual(values=c("grey70", "firebrick"),
#                        labels=c("Other Percentiles", "90th Percentile"))
# dev.off()  
#   #income
#   p<-ggplot(Cscf1, aes(x=(jitter(year)), y=RWeeksPerYear, color=IPercentile))
#   p + geom_smooth(
#     (aes(group=IPercentile,
#          color=(IPercentile=="90")))) +
#     scale_color_manual(values=c("grey70", "firebrick", "blue"),
#                        labels=c("Other Percentiles", "90th Percentile"))
# 
# #Loess with scatter
#   #Networth
#   pdf(file="Loess and Scatter.pdf", height=10, width=15)
#   
# p<-ggplot(Cscf1, aes(x=(jitter(year)), y=RHoursPerWeek, color=NWPercentile))
# print(  p + geom_point(aes(group=NWPercentile,
#             color=(NWPercentile=="99")))  +
#       geom_smooth(aes(group=NWPercentile,
#             color=(NWPercentile=="99"))) +
#       scale_color_manual(values=c("grey70", "firebrick"),
#             labels=c("Other Percentiles", "99th Percentile")))
#   dev.off()
# 
#   
#   
#   
#   
#   
# #Plot quantile regression results with bar graph
#   
# HeadWorkHours=c(383,861,1943,5957,17058,11695)
# HomemakerSpouse=c(-2086,-3369,-8126,-88,64575,3887891)
# Interaction=c(94,34,1953,12580,42435,257067)
# 
# HeadWorkHours=c(7.27,7.82,8.06,10.03,7.82,4.07)
# HomemakerSpouse=c(-1.53,-1.60,-1.71,-0.01,1.21,3.44)
# Interaction=c(0.68,0.12,2.28,5.07,3.62,1.73)
# 
# 
# 
# 
# 
# Value=c(HeadWorkHours,HomemakerSpouse,Interaction)
# Quantile=c(.1,.25,.5,.75,.9,.99,.1,.25,.5,.75,.9,.99,.1,.25,.5,.75,.9,.99)
# Variable=c("HeadWorkHours","HeadWorkHours","HeadWorkHours","HeadWorkHours","HeadWorkHours","HeadWorkHours","HomemakerSpouse","HomemakerSpouse","HomemakerSpouse","HomemakerSpouse","HomemakerSpouse","HomemakerSpouse","Interaction","Interaction","Interaction","Interaction","Interaction","Interaction")
# data<-data.frame(Value,Quantile,Variable)
#   
# ggplot(data, aes(Quantile, Value)) +   
#   geom_bar(aes(fill = Variable), position = "dodge", stat="identity")  
# 
# p<-ggplot(data, aes(x=(Quantile), y=HeadWorkHours))
# p+geom_bar(stat = "identity")
# 
# 
# 
# #by year plot
# data<-read.csv("C:/Users/bdaro_000/Sociology/Papers/Lisa Projects/Paper - 1% Work Hours Paper/Figures and Tables/by year2.csv", sep=",")
# P99<-data[data$quantile==.99,]
# P99$ParYear<-paste(P99$Parameter,P99$yearinterval)
# P99<-P99[order(P99$ParYear),]
# 
# p<-ggplot(P99, aes(ParYear, Estimate,group=Parameter)) 
#   p+geom_line(size=1) + facet_wrap(~Parameter, scales="free") + geom_ribbon(aes(ymin=lowerlimit,ymax=upperlimit),alpha=0.3)
# 
# #Different by year plot
# data<-read.csv("C:/Users/bdaro_000/Sociology/Papers/Lisa Projects/Paper - 1% Work Hours Paper/Figures and Tables/by year2.csv", sep=",")
# P99<-data[data$Parameter=="HeadWorkHoursOver40" |data$Parameter=="HomemakerSpouse" |data$Parameter=="HeadWorkH*HomemakerS",]
# P99$ParQuantile<-paste(P99$Parameter,P99$quantile)
# P99<-P99[order(P99$ParQuantile),]
# P99$yearinterval<-ifelse(P99$yearinterval==1,"89-95",ifelse(P99$yearinterval==2,"98-04","07-13"))
# P99<-P99[order(P99$ParQuantile),]
# 
#   interaction(Parameter,quantile)
#   p<-ggplot(P99, aes(yearinterval, Estimate,group=ParQuantile)) 
#   p+
#     geom_line(size=1) + 
#     facet_wrap(~ParQuantile, scales="free",nrow=3) + 
#     geom_ribbon(aes(ymin=lowerlimit,ymax=upperlimit),alpha=0.3) +
#     scale_x_discrete(limits=c("89-95","98-04","07-13"))
#   
#   
#   interaction(HomemakerSpouse,quantile)
#   
#   
# p<-ggplot(data=P99, aes(yearinterval, Estimate, group=Parameter))
# p+geom_line(size=1) + facet_wrap(~quantile, scales="free") + geom_ribbon(aes(ymin=lowerlimit,ymax=upperlimit),alpha=0.3)
# 
# + facet_wrap(~Parameter, scales="free") + geom_ribbon(aes(ymin=lowerlimit,ymax=upperlimit),alpha=0.3)
# 
# 
# #by cohort plot
# data<-read.csv("C:/Users/bdaro_000/Sociology/Papers/Lisa Projects/Paper - 1% Work Hours Paper/Figures and Tables/by cohort.csv", sep=",")
# P99<-data[data$quantile==.99,]
# P99$generation<-ifelse(P99$generation=="greatest","1 greatest", ifelse (P99$generation=="babyboomer","2 babyboomer", ifelse(P99$generation=="genX","3 genx",0)))
# P99$ParGen<-paste(P99$Parameter,P99$generation)
# P99<-P99[order(P99$ParGen),]
# 
# p<-ggplot(P99, aes(ParGen, Estimate,group=Parameter)) 
#   p+geom_line(size=1) + facet_wrap(~Parameter, scales="free") + geom_ribbon(aes(ymin=lowerlimit,ymax=upperlimit),alpha=0.3)
#   
#   
# #by type plot predicted probabilities
#   #read data
#     data<-read.csv("C:/Users/bdaro_000/Sociology/Papers/Lisa Projects/Paper - 1% Work Hours Paper/Figures and Tables/by type.csv", sep=",")
#   #stock predicted probabilities
#       quantile<-c(.75,.90,.99) #stocks don't have any lower quantiles than these
#       stockpredictions<-list()
#       for(k in 1:length(quantile)){
#         #define variables
#           intercept<-data$Estimate[data$Parameter=="intercept" & data$type=="stocks" & data$quantile==quantile[k]]
#           HomemakerSpouse<-data$Estimate[data$Parameter=="HomemakerSpouse" & data$type=="stocks" & data$quantile==quantile[k]]
#           HeadWorkHours<-data$Estimate[data$Parameter=="HeadWorkHoursOver40" & data$type=="stocks" & data$quantile==quantile[k]]
#           interaction<-data$Estimate[data$Parameter=="HeadWorkH*HomemakerS" & data$type=="stocks" & data$quantile==quantile[k]]
#           #Create data frame of predicted probabilities by work hours and homemaker spouse
#             stockpredictions[[k]]<-data.frame(Value=NA,HomemakerSpouse=rep(0:1,each=21),WorkHours=rep(0:20,2),type="stocks",quantile=quantile[k])
#             for (i in 0:1){
#               for (j in 0:20){
#                 stockpredictions[[k]]$Value[i*21+j+1]<-intercept+(i*HomemakerSpouse)+(j*HeadWorkHours)+(i*j*interaction)
#               }
#             }
#       }
# 
#     #income predicted probabilities
#       quantile<-c(.25,.5,.75,.90,.99)
#       incomepredictions<-list()
#       for(k in 1:length(quantile)){
#         #define variables
#         intercept<-data$Estimate[data$Parameter=="intercept" & data$type=="income" & data$quantile==quantile[k]]
#         HomemakerSpouse<-data$Estimate[data$Parameter=="HomemakerSpouse" & data$type=="income" & data$quantile==quantile[k]]
#         HeadWorkHours<-data$Estimate[data$Parameter=="HeadWorkHoursOver40" & data$type=="income" & data$quantile==quantile[k]]
#         interaction<-data$Estimate[data$Parameter=="HeadWorkH*HomemakerS" & data$type=="income" & data$quantile==quantile[k]]
#         married<-data$Estimate[data$Parameter=="Married" & data$type=="income" & data$quantile==quantile[k]]
#         age<-data$Estimate[data$Parameter=="age" & data$type=="income" & data$quantile==quantile[k]]
#         agesquared<-data$Estimate[data$Parameter=="agesquared" & data$type=="income" & data$quantile==quantile[k]]
#         Bachelors<-data$Estimate[data$Parameter=="Bachelors" & data$type=="income" & data$quantile==quantile[k]]
#         year<-data$Estimate[data$Parameter=="year" & data$type=="income" & data$quantile==quantile[k]]
#         
#         #Create data frame of predicted probabilities by work hours and homemaker spouse
#         incomepredictions[[k]]<-data.frame(Value=NA,HomemakerSpouse=rep(0:1,each=21),WorkHours=rep(0:20,2),type="income",quantile=quantile[k])
#         for (i in 0:1){
#           for (j in 0:20){
#             incomepredictions[[k]]$Value[i*21+j+1]<-intercept+married+Bachelors+year*2000+(i*HomemakerSpouse)+(j*HeadWorkHours)+(i*j*interaction)
#           }
#         }
#       }
#       
#       #networth predicted probabilities
#       quantile<-c(.25,.5,.75,.90,.99) #need to remake networth quantiles for the .1 decile
#       networthpredictions<-list()
#       for(k in 1:length(quantile)){
#         #define variables
#         intercept<-data$Estimate[data$Parameter=="intercept" & data$type=="networth" & data$quantile==quantile[k]]
#         HomemakerSpouse<-data$Estimate[data$Parameter=="HomemakerSpouse" & data$type=="networth" & data$quantile==quantile[k]]
#         HeadWorkHours<-data$Estimate[data$Parameter=="HeadWorkHoursOver40" & data$type=="networth" & data$quantile==quantile[k]]
#         interaction<-data$Estimate[data$Parameter=="HeadWorkH*HomemakerS" & data$type=="networth" & data$quantile==quantile[k]]
#         married<-data$Estimate[data$Parameter=="Married" & data$type=="networth" & data$quantile==quantile[k]]
#         age<-data$Estimate[data$Parameter=="age" & data$type=="networth" & data$quantile==quantile[k]]
#         agesquared<-data$Estimate[data$Parameter=="agesquared" & data$type=="networth" & data$quantile==quantile[k]]
#         Bachelors<-data$Estimate[data$Parameter=="Bachelors" & data$type=="networth" & data$quantile==quantile[k]]
#         year<-data$Estimate[data$Parameter=="year" & data$type=="networth" & data$quantile==quantile[k]]
#         #Create data frame of predicted probabilities by work hours and homemaker spouse
#         networthpredictions[[k]]<-data.frame(Value=NA,HomemakerSpouse=rep(0:1,each=21),WorkHours=rep(0:20,2),type="networth",quantile=quantile[k])
#         for (i in 0:1){
#           for (j in 0:20){
#             networthpredictions[[k]]$Value[i*21+j+1]<-intercept+married+Bachelors+year*2000+(i*HomemakerSpouse)+(j*HeadWorkHours)+(i*j*interaction)
#           }
#         }
#       }
#       
# #convert  predicted probabilities to one large dataframe
#     tempnetworthdf<-networthpredictions[[1]]
#     tempincomedf<-incomepredictions[[1]]
#       for (i in 2:length(networthpredictions)){
#         tempnetworthdf<- rbind(tempnetworthdf,networthpredictions[[i]])    
#         tempincomedf<- rbind(tempincomedf,incomepredictions[[i]])    
#       }
#     preprob<-rbind(tempnetworthdf,tempincomedf)
#     preprob2<-preprob[preprob$quantile!=.99,]
#       
#         p<-ggplot(data=tempincomedf, aes(WorkHours, Value, group=interaction(HomemakerSpouse,quantile)))
#         p+geom_line(size=1.5,aes(color=factor(quantile),linetype=factor(HomemakerSpouse)))
# 
#         p<-ggplot(data=tempnetworthdf, aes(WorkHours, Value, group=interaction(HomemakerSpouse,quantile)))
#         p+geom_line(size=1.5,aes(color=factor(quantile),linetype=factor(HomemakerSpouse)))
#         
#         
#         p<-ggplot(data=preprob2, aes(WorkHours, Value, group=interaction(HomemakerSpouse,quantile)))
#         p+geom_line(size=1.5,aes(color=factor(quantile),linetype=factor(HomemakerSpouse)))+ facet_wrap(~type, scales="free")
#         
#               
#       + facet_wrap(~Parameter, scales="free") + geom_ribbon(aes(ymin=lowerlimit,ymax=upperlimit),alpha=0.3)
#       
#       i=0 
#       j=0
#       
#          
#     unique(data$Parameter)
#     P99<-data[data$quantile==.99,]
# #  P99$generation<-ifelse(P99$generation=="greatest","1 greatest", ifelse (P99$generation=="babyboomer","2 babyboomer", ifelse(P99$generation=="genX","3 genx",0)))
#   P99$ParGen<-paste(P99$Parameter,P99$generation)
#   P99<-P99[order(P99$ParGen),]
#   
#   p<-ggplot(P99, aes(ParGen, Estimate,group=Parameter)) 
#   p+geom_line(size=1) + facet_wrap(~Parameter, scales="free") + geom_ribbon(aes(ymin=lowerlimit,ymax=upperlimit),alpha=0.3)
#   
#     
#   
#   
#   P992<-P99[P99$Parameter=="HeadWorkHoursOver40" | P99$Parameter=="HomemakerSpouse" | P99$Parameter=="HeadWorkH*HomemakerS",]
#   P992$ParYear<-paste(P992$Parameter,P992$yearinterval)
#   P992<-P992[order(P992$ParYear),]
#   

#1% threshold over time
    options(scipen = 999)
    library(data.table)
    df<-data.table(scf)
    as.data.frame(table(df$NWPercentile,df$year))
    temp<-df[,c("year","networth25","networth50","networth75","networth90","networth99")]
    temp<-temp[!duplicated(df$year),]
  #viz
    pdf(file="Fun - 1p Net Worth Thresholds over Time.pdf", height=6, width=11)
    p <- ggplot(data = temp, aes(x = year, y = networth99/1000000))
    p + geom_bar(position = "dodge", stat="identity")+
      labs(x="Year", y="1% Net Worth Threshold (millions)")+
      ggtitle("1% Net Worth Threshold over time")
    dev.off()

#percentile threshold growth rate over time
    #data prep
        tempdf<-data.frame(year=rep(temp$year,5),value=c(temp$networth25,temp$networth50,temp$networth75,temp$networth90,temp$networth99),Percentile=rep(sort(unique(df$NWPercentile))[-1],each=10))
        temp<-tempdf[seq(1,50,10),]$value
        tempdf$agggrowth<-rep(temp,each=10)
        tempdf$agggrowth<-tempdf$value/tempdf$agggrowth
        tempdf$agggrowth<-tempdf$agggrowth-1
        tempdf$agggrowth<-tempdf$agggrowth
    #graph
        pdf(file="Fun - Aggregate Percentile Threshold Growth over Time.pdf", height=6, width=11)
        p<-ggplot(tempdf, aes((year), y=agggrowth))
        p+ geom_line(aes(group=Percentile, color=Percentile),size=2)+
          labs(x="Year", y="Percent Threshold Increase over 1989")+
          ggtitle("Net Worth Threshold Aggregate Growth over Time by Percentile ")+
          scale_colour_grey(name="Net Worth Percentile",start=.75,end=0)+
          scale_y_continuous(labels=scales::percent)+ #,limits = c(0,.51)
          xlim(1988,2017)+
          theme_bw()+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"),
                title = element_text(size=18),
                axis.text.x = element_text(size=14),  axis.text.y = element_text(size=14),
                axis.title.x= element_text(size=14,margin = margin(t = 20, r = 0, b = 0, l = 0)),
                axis.title.y= element_text(size=14,margin = margin(t = 0, r = 20, b = 0, l = 0)))
        dev.off()


