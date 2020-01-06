table(scf$HomemakerSpouse)

#0 - Prep
    #a - libraries
        library(ggplot2)
        library(directlabels)
        library(Hmisc)
        library(cowplot)
        library(data.table)
        library(ggthemes)
        library(readxl)
        library(XML)
        library(RCurl)
        library(rlist)
        library(stringr)
        library(weights)
        library(haven)

    #b - load data; rename data; set work directory
        scf<-read.csv("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/rscf.csv")
        scf<-scf[scf$RWorkStatus=="Employed" & scf$Female==0,]
        scf<-scf[scf$MARRIED==1,]
        scf<-scf[!((scf$X4712>0) & scf$SWorkStatus=="Homemaker"),]
        
        105252/5

        setwd("C:/Users/bda13/Desktop/Sociology/Papers in Progress/Work Hours/Figures and Tables/")
        #c) read data
        # scf <- read_sas("scf2016.sas7bdat")

#1 - transform/create necessary variables
    #a -Percentiles to character
        scf$NWPercentile<-as.character(scf$NWPercentile)
        scf$IPercentile<-as.character(scf$IPercentile)

#2 - Figure 1 - Changing Rate of Homemaker Spouses by Wealth Percentile
    #a - Prep data
        #Determine weighted means
          tempscf<-scf[scf$RWorkStatus=="Employed" & scf$Female==0,]
        #remove 25th and 10th percentile
          tempsnwperc<-tempscf$NWPercentile
          tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="25","0",tempscf$NWPercentile)
          tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="10","0",tempscf$NWPercentile)
          tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="75","75-89",tempscf$NWPercentile)
          tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="50","0-74",tempscf$NWPercentile)
          tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="0","0-74",tempscf$NWPercentile)
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
            aggscf$Homemaker[i]<-wtd.mean(tempscf$Homemaker[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]],tempscf$WGT[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]])
            #aggscf$Homemaker[i]<-mean(tempscf$Homemaker[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]])
            aggscf$Var[i]<-wtd.var(tempscf$Homemaker[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]],tempscf$wgt[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]])
          }
      #Determine SEM while accounting for fact that numbers are below 100.
          aggscf$SEM<-sqrt(100*aggscf$Var)/100
          class(aggscf$Year)
          
          
          scale_y_continuous(labels=function(x) paste0(x*10,"%"))
          aggscf$NWPercentile
          
    #b - Plot graph
        p<-ggplot(aggscf, aes(Year, y=Homemaker))
        png(file="C:/Users/bda13/Desktop/Figure 1 - Changing Rate of Homemaker Spouses by Wealth Percentile.png", height=6.7, width=11, units="in",res=300)
            p+ geom_smooth(aes(group=NWPercentile, color=NWPercentile,linetype=NWPercentile),se=F,size=3,span=.7)+
            #geom_line(aes(group=NWPercentile, color=NWPercentile),size=2)
                scale_colour_grey(name="Net Worth Percentile",start=.85,end=0)+
                scale_y_continuous(labels=function(x) paste0(x*100,"%"),breaks = ((0:6)/10),limits = c(0,.6))+ #,limits = c(0,.51)
                scale_x_continuous(limits=c(1988,2017), breaks=c(1989,1992,1995,1998,2001,2004,2007,2010,2013,2016))+
              theme_bw()+
                theme(#panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),
                      axis.line = element_line(colour = "black"),
                      legend.position="none",
                      plot.title = element_text(size=18,face = "bold"),
                      axis.text.x = element_text(size=13.5),
                      axis.text.y = element_text(size=13.5),
                      axis.title.x= element_text(size=17,margin = margin(t = 20, r = 0, b = 0, l = 0)),
                      axis.title.y= element_text(size=17,margin = margin(t = 0, r = 20, b = 0, l = 0)))+
                # ggtitle("Figure 1 - Changing Rate of Homemaker Spouses by Wealth Percentile")+
                # geom_dl(aes(label = paste("  ",NWPercentile,"%  ",sep="")), method = list(c('first.bumpup'),dl.combine(first.points), cex = .9)) +
                geom_dl(aes(label = paste("  ",NWPercentile,"%  ",sep="")), method = list(c('last.bumpup'),dl.combine(last.points), cex = .9)) +
                # geom_errorbar(aes(ymin = Homemaker - SEM, ymax = Homemaker + SEM), width=0.3)+
                labs(x="Year", y="Percentage Homemakers")
        dev.off()




wtd.t.test(x=scfpr0$AGE[scfpr0$X_Imputation_==1], weight=scfpr0$WGT[scfpr0$X_Imputation_==1], y=scfpr.5$AGE[scfpr.5$X_Imputation_==1],weighty=scfpr.5$WGT[scfpr.5$X_Imputation_==1])
perc<-unique(aggscf$NWPercentile)
#alternative for making sure the bottom isn't too heterogenous
perc<-unique(tempsnwperc)
tempscf$NWPercentile<-tempsnwperc

x=7
perc[x]
sub1<-tempscf$year==2016 & tempscf$NWPercentile==perc[x] & tempscf$X_Imputation_==1
a1<-tempscf$Homemaker[sub1]
b1<-tempscf$WGT[sub1]
sub2<-tempscf$year==1989 & tempscf$NWPercentile==perc[x] & tempscf$X_Imputation_==1
a2<-tempscf$Homemaker[sub2]
b2<-tempscf$WGT[sub2]
wtd.t.test(x=a1,weight=b1,y=a2,weighty=b2)

#all trends from the 75th to 99th percentiles are significan