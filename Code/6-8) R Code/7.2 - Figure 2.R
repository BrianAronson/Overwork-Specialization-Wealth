
#1 - library
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

#2 - Figure 2 - Parameter estimates
  #a) prep data
      scf<-read.csv("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/rscf.csv")
      scf<-scf[scf$RWorkStatus=="Employed" & scf$Female==0  & scf$MARRIED==1,]
      scf<-scf[!((scf$X4712>0) & scf$SWorkStatus=="Homemaker"),]
      
      setwd("C:/Users/bda13/Desktop/Sociology/Papers in Progress/Work Hours/Archived tables")
      {
      #read and bind results
         {uqr1<-read_xlsx("uqrnetworth.xlsx")
          uqr2<-read_xlsx("uqrhomeequity.xlsx")
          uqr3<-read_xlsx("uqrstocks.xlsx")
          uqr4<-read_xlsx("Investment research.xlsx")
          uqr5<-read_xlsx("late debts.xlsx")
          uqr1<-uqr1[uqr1$Parm %in% c("HeadWorkHoursOver40","HomemakerSpouse","HeadWorkHoursOver40Ho","Year1HeadWorkHoursOve"),]
          uqr2<-uqr2[uqr2$Parm %in% c("HeadWorkHoursOver40","HomemakerSpouse","HeadWorkHoursOver40Ho","Year1HeadWorkHoursOve"),]
          uqr3<-uqr3[uqr3$Parm %in% c("HeadWorkHoursOver40","HomemakerSpouse","HeadWorkHoursOver40Ho","Year1HeadWorkHoursOve"),]
          uqr4<-uqr4[uqr4$Parm %in% c("HeadWorkHoursOver40","HomemakerSpouse","HeadWorkH*HomemakerS","year1*HeadWorkHoursO"),]
          uqr5<-uqr5[uqr5$Parm %in% c("HeadWorkHoursOver40","HomemakerSpouse","HeadWorkH*HomemakerS","year1*HeadWorkHoursO"),]
          uqr1$DV<-"Net Worth"
          uqr2$DV<-"Home Equity"
          uqr3$DV<-"Stocks"
          uqr4$DV<-"Research"
          uqr5$DV<-"Late Debts"
          names(uqr4)[1]<-"Quantile"
          names(uqr5)[1]<-"Quantile"
          keeps<-names(uqr5)
          uqr1<-uqr1[,keeps]
          uqr2<-uqr2[,keeps]
          uqr3<-uqr3[,keeps]
          uqr4<-uqr4[,keeps]
          uqr<-rbind(uqr1,uqr2,uqr3,uqr4,uqr5)
          #uqr5 causing issues; missing rows
          uqr<-as.data.frame(uqr)
      #create CI variable
          uqr$CIlower<-uqr$Estimate-uqr$StdErr*1.96
          uqr$CIupper<-uqr$Estimate+uqr$StdErr*1.96
      #create color variable
          uqr$Significant<-factor(ifelse(sign(uqr$CIlower)==sign(uqr$CIupper),1,0))
        #rename variables
          uqr$Variable<-uqr$Parm
          uqr$Variable[uqr$Variable=="HeadWorkH*HomemakerS" | uqr$Variable=="HeadWorkHoursOver40Ho"]<-"Overwork * Specialization"
          uqr$Variable[uqr$Variable=="HeadWorkHoursOver40"]<-"Overwork"
          uqr$Variable[uqr$Variable=="HomemakerSpouse"]<-"Specialization"
          uqr$Variable[uqr$Variable=="year1*HeadWorkHoursO" | uqr$Variable=="Year1HeadWorkHoursOve"]<-"Overwork * Year"
      #keep variable order
          uqr$Variable<-factor(uqr$Variable,levels=c("Overwork * Year","Overwork * Specialization","Overwork","Specialization"))
      #fix quantiles
          uqr$Quantile<-as.numeric(uqr$Quantile)
          uqr$Quantile[uqr$Quantile<1]<-uqr$Quantile[uqr$Quantile<1]*100
          uqr$Quantile<-factor(uqr$Quantile,levels=unique(uqr$Quantile))
          
          
          uqr2$tValue
          
      #standardize variables
        #prep scf
            scf<-scf[scf$RWorkStatus=="Employed" & scf$Female==0,]
            scf$Specialization<-scf$HomemakerSpouse
            scf$Overwork<-scf$HeadWorkHoursOver40
            scf$`Overwork * Specialization`<-scf$Specialization*scf$Overwork
            scf$`Overwork * Year`<-scf$Overwork*scf$year1
        #prep new variable
            uqr$sdEstimate<-0
            i=1
        #prep function
            newfun<-function(x,per,p) sqrt(wtd.var(scf[,x][scf[,per]==p]))
            
        #change percentiles to match current dataset
            a<-wtd.quantile(scf$NETWORTH,scf$WGT,c(.1,.25,.5,.75,.9,.99,1))
            scf$NWPercentile[scf$NETWORTH<=a[7]]<-99
            scf$NWPercentile[scf$NETWORTH<a[6]]<-90
            scf$NWPercentile[scf$NETWORTH<a[5]]<-75
            scf$NWPercentile[scf$NETWORTH<a[4]]<-50
            scf$NWPercentile[scf$NETWORTH<a[3]]<-25
            scf$NWPercentile[scf$NETWORTH<a[2]]<-10
            scf$NWPercentile[scf$NETWORTH<a[1]]<-0
            
            a<-wtd.quantile(scf$HomeEquity,scf$WGT,c(.1,.25,.5,.75,.9,.99,1))
            scf$HEPercentile[scf$HomeEquity<=a[7]]<-99
            scf$HEPercentile[scf$HomeEquity<a[6]]<-90
            scf$HEPercentile[scf$HomeEquity<a[5]]<-75
            scf$HEPercentile[scf$HomeEquity<a[4]]<-50
            scf$HEPercentile[scf$HomeEquity<a[3]]<-25
            scf$HEPercentile[scf$HomeEquity<a[2]]<-10
            scf$HEPercentile[scf$HomeEquity<a[1]]<-0
            
            a<-wtd.quantile(scf$StockValue,scf$WGT,c(.1,.25,.5,.75,.9,.99,1))
            scf$SVPercentile[scf$StockValue<=a[7]]<-99
            scf$SVPercentile[scf$StockValue<a[6]]<-90
            scf$SVPercentile[scf$StockValue<a[5]]<-75
            scf$SVPercentile[scf$StockValue<a[4]]<-50
            scf$SVPercentile[scf$StockValue<a[3]]<-25
            scf$SVPercentile[scf$StockValue<a[2]]<-10
            scf$SVPercentile[scf$StockValue<a[1]]<-0
            
        #start iteration for each variable, quantile, and percentile
            for(i in 1:nrow(uqr)){
              x<-as.character(uqr$Variable[i])
              per<-ifelse(uqr$DV[i]=="Net Worth","NWPercentile",
                   ifelse(uqr$DV[i]=="Research","NWPercentile",
                   ifelse(uqr$DV[i]=="Late Debts","NWPercentile",
                   ifelse(uqr$DV[i]=="Home Equity","HEPercentile",
                   "SVPercentile"))))
              p<-as.character(uqr$Quantile[i])
              uqr$sdEstimate[i]<-newfun(x=x,per=per,p=p)
            }
            # #manually set home equity and stocks to NA where relevant
            # uqr$DV=="Home Equity"
            # uqr$Quantile=="10th" | uqr$Quantile=="25th"
            # uqr$sdEstimate
            
            # uqr[uqr$DV=="Stocks",] 
        #standardize
            uqr$stEstimate<-uqr$Estimate*uqr$sdEstimate
            uqr$stCIlower<-uqr$stEstimate-uqr$StdErr*uqr$sdEstimate*1.96
            uqr$stCIupper<-uqr$stEstimate+uqr$StdErr*uqr$sdEstimate*1.96
            uqr$stSignificant<-(ifelse(sign(uqr$stCIlower)==sign(uqr$stCIupper),1,0))
        #rename quantiles
            levels(uqr$Quantile)<-c("10th   ","25th   ","50th   ","75th   ","90th   ","99th   ")

        #create rounding functions
            mround <- function(x,base){
              base*round(x/base)
            }
            mcieling <- function(x,base){
              base*ceiling(x/base)
            }
            mfloor <- function(x,base){
              base*floor(x/base)
            }
        }
        #Create y axis labels
          labs<-c(expression("Overwork" %*% "Year"),expression("Overwork" %*% "Specialization"),"Overwork","Specialization")
          
  #b) Visualize data
      #i) create plot for axis titles and variable names
          plotfun1<-function(x){
            ggplot(uqr[uqr$DV==unique(uqr$DV)[x],], aes(y=stEstimate,x=Variable))+
              coord_flip()+
              geom_hline(yintercept = 0)+
              theme_bw() +
              theme(
                axis.ticks.y= element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"),
                text=element_text(size=15),
                title =element_text(size=15, face='bold'),
                plot.title = element_text(hjust=.5),
                legend.position="none",
                plot.margin = margin(t=.5, r=.2, b=.5, l=.5, "cm"),
                axis.text.x = element_blank(),
                axis.title.y = element_blank(),
                axis.title.x = element_blank(),
                strip.placement = "outside",
                strip.background = element_blank(),
                strip.text.y = element_text(angle = 180,face="bold",size="17")
              )+
              facet_wrap(~Quantile,ncol=1,strip.position="left")+
              ggtitle("")+
              scale_x_discrete(labels= labs)+
              geom_blank()
          }
    #ii) create remainder of plots
          x=2
        plotfun2<-function(x){
          mi<-mfloor(min(uqr$stCIlower[uqr$DV==unique(uqr$DV)[x]],na.rm=T),.5)
          ma<-mcieling(max(uqr$stCIupper[uqr$DV==unique(uqr$DV)[x]],na.rm=T),.5)
            ggplot(uqr[uqr$DV==unique(uqr$DV)[x],], aes(y=stEstimate,x=(Variable)))+
            coord_flip()+
            # geom_pointrange(aes(ymin=stCIlower,ymax=stCIupper),size=1.5,fill=ifelse(uqr$stSignificant[uqr$DV==unique(uqr$DV)[x]]==0,"white","darkgrey"),color=ifelse(uqr$stSignificant[uqr$DV==unique(uqr$DV)[x]]==0,"grey70","black"),shape=73, fatten = 6)+
            # geom_pointrange(aes(ymin=stCIlower,ymax=stCIupper),size=.75,fill=ifelse(uqr$stSignificant[uqr$DV==unique(uqr$DV)[x]]==0,"white","darkgrey"),color=ifelse(uqr$stSignificant[uqr$DV==unique(uqr$DV)[x]]==0,"grey70","black")) +
              geom_linerange(aes(ymin=stCIlower,ymax=stCIupper),width=.5,
                            size=.75,color=ifelse(uqr$stSignificant[uqr$DV==unique(uqr$DV)[x]]==0,"grey50","black"))+
              geom_point(size=3.5,color=ifelse(uqr$stSignificant[uqr$DV==unique(uqr$DV)[x]]==0,"white","black"))+
              geom_point(size=3.5,shape=1)+
              
            geom_hline(yintercept = 0)+
            theme_bw() +
            theme(axis.ticks.y= element_blank(),
              # panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"),
              text=element_text(size=15),
              title =element_text(size=15, face='bold'),
              plot.title = element_text(hjust=.5),
              legend.position="none",
              plot.margin = margin(t=.5, r=.2, b=.5, l=.3, "cm"),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              strip.text = element_blank()
            )+
            facet_wrap(~Quantile,ncol=1)+
            scale_x_discrete(labels= labs)+
            ggtitle(unique(uqr$DV)[x])+
            #manually edit axis limits
              # if(x==1){
              #   scale_y_continuous(limits = c(-.6,1.1),breaks =seq(from=-.5,to=1,by=.5))
              # }else if(x==2){
              #   scale_y_continuous(limits = c(-.11,.21),breaks =seq(from=-.1,to=.25,by=.1))
              # }else if(x==3){
              #   scale_y_continuous(limits = c(-.7,.7),breaks =seq(from=-.5,to=.5,by=.5))
              # }else if(x==4){
              #   scale_y_continuous(limits = c(-1.05,.8),breaks =seq(from=-1,to=.5,by=.5))
              # }else if(x==5){
              #   scale_y_continuous(limits = c(-.7,1.2),breaks =seq(from=-.5,to=1,by=.5))
              # }
            if(x==1){
              scale_y_continuous(limits = c(-.6,.6),breaks =seq(from=-.5,to=.5,by=.5),function(x) sprintf("%.2f", x))
            }else if(x==2){
              scale_y_continuous(limits = c(-.6,.6),breaks =seq(from=-.5,to=.5,by=.5),function(x) sprintf("%.2f", x))
            }else if(x==3){
              scale_y_continuous(limits = c(-.6,.6),breaks =seq(from=-.5,to=.5,by=.5),function(x) sprintf("%.2f", x))
            }else if(x==4){
              scale_y_continuous(limits = c(-1.2,1.2),breaks =seq(from=-1,to=1,by=1),function(x) sprintf("%.2f", x))
            }else if(x==5){
              scale_y_continuous(limits = c(-1.2,1.2),breaks =seq(from=-1,to=1,by=1),function(x) sprintf("%.2f", x))
            }
        }
        uqr$Quantile[uqr$DV=="Home Equity"]
        plotfun2(3)
        x=3
        a<-uqr[uqr$DV==unique(uqr$DV)[x],]
        a$stCIlower
        sort(a$stCIlower)
        sort(a$stCIupper)
      
    #iii) manually edit things to fit in margins better
       { a<-uqr$stCIupper[uqr$DV==unique(uqr$DV)[4]][24]
        b<-uqr$stCIlower[uqr$DV==unique(uqr$DV)[4]][24]
        uqr$stCIupper[uqr$DV==unique(uqr$DV)[4]][24]<-a-.05
        uqr$stCIlower[uqr$DV==unique(uqr$DV)[4]][24]<-b+.05

        a<-uqr$stCIupper[uqr$DV==unique(uqr$DV)[4]][c(1,4)]
        b<-uqr$stCIlower[uqr$DV==unique(uqr$DV)[4]][c(1,4)]
        uqr$stCIupper[uqr$DV==unique(uqr$DV)[4]][c(1,4)]<-a-.15
        uqr$stCIlower[uqr$DV==unique(uqr$DV)[4]][c(1,4)]<-b+.15

        a<-uqr$stCIupper[uqr$DV==unique(uqr$DV)[3]][13]
        b<-uqr$stCIlower[uqr$DV==unique(uqr$DV)[3]][13]
        uqr$stCIupper[uqr$DV==unique(uqr$DV)[3]][13]<-a-.04
        uqr$stCIlower[uqr$DV==unique(uqr$DV)[3]][13]<-b+.04

        a<-uqr$stCIupper[uqr$DV==unique(uqr$DV)[3]][21]
        b<-uqr$stCIlower[uqr$DV==unique(uqr$DV)[3]][21]
        uqr$stCIupper[uqr$DV==unique(uqr$DV)[3]][21]<-a-.02
        uqr$stCIlower[uqr$DV==unique(uqr$DV)[3]][21]<-b+.02
        
        uqr[uqr$DV==unique(uqr$DV)[3],]$stCIlower[22]<-uqr[uqr$DV==unique(uqr$DV)[3],]$stCIlower[22]-.03
        uqr[uqr$DV==unique(uqr$DV)[3],]$stCIupper[22]<-uqr[uqr$DV==unique(uqr$DV)[3],]$stCIupper[22]-.03
        uqr[uqr$DV==unique(uqr$DV)[3],]$stEstimate[22]<-uqr[uqr$DV==unique(uqr$DV)[3],]$stEstimate[22]-.03
        

        a<-uqr$stCIupper[uqr$DV==unique(uqr$DV)[5]][21]
        b<-uqr$stCIlower[uqr$DV==unique(uqr$DV)[5]][21]
        uqr$stCIupper[uqr$DV==unique(uqr$DV)[5]][21]<-a-.05
        uqr$stCIlower[uqr$DV==unique(uqr$DV)[5]][21]<-b+.05

        a<-uqr$stCIupper[uqr$DV==unique(uqr$DV)[5]][22]
        b<-uqr$stCIlower[uqr$DV==unique(uqr$DV)[5]][22]
        uqr$stCIupper[uqr$DV==unique(uqr$DV)[5]][22]<-a-.05
        uqr$stCIlower[uqr$DV==unique(uqr$DV)[5]][22]<-b+.05
       }
    #iv) manually edit to make significandots not overlap 0
        {
        sub1<-uqr$DV!=unique(uqr$DV)[2]
        sub2<-abs(uqr$stEstimate[sub1])<.2 & !is.na(uqr$stEstimate[sub1]) & uqr$stSignificant[sub1]==1
        uqr$stEstimate[sub1][sub2]<-uqr$stEstimate[sub1][sub2]+sign(uqr$stEstimate[sub1][sub2])*.05
        uqr$stCIlower[sub1][sub2]<-uqr$stCIlower[sub1][sub2]+sign(uqr$stEstimate[sub1][sub2])*.05
        uqr$stCIupper[sub1][sub2]<-uqr$stCIupper[sub1][sub2]+sign(uqr$stEstimate[sub1][sub2])*.05

        sub1<-uqr$DV==unique(uqr$DV)[2]
        sub2<-abs(uqr$stEstimate[sub1])<.4 & !is.na(uqr$stEstimate[sub1]) & uqr$stSignificant[sub1]==1
        uqr$stEstimate[sub1][sub2]<-uqr$stEstimate[sub1][sub2]+sign(uqr$stEstimate[sub1][sub2])*.01
        uqr$stCIlower[sub1][sub2]<-uqr$stCIlower[sub1][sub2]+sign(uqr$stEstimate[sub1][sub2])*.01
        uqr$stCIupper[sub1][sub2]<-uqr$stCIupper[sub1][sub2]+sign(uqr$stEstimate[sub1][sub2])*.01

        sub1<-uqr$DV==unique(uqr$DV)[3]
        uqr$stEstimate[sub1]
        }

    #v) kill null stocks findings
        uqr[uqr$DV=="Stocks" & (as.character(uqr$Quantile)=="50th   "),c("sdEstimate","stEstimate","stCIlower","stCIupper","stSignificant")]<-NA
        uqr[uqr$DV=="Home Equity" & ((as.character(uqr$Quantile)=="25th   ") | (as.character(uqr$Quantile)=="10th   ")),c("sdEstimate","stEstimate","stCIlower","stCIupper","stSignificant")]<-NA
        
    #vi) Change names of quantiles
        levels(uqr$Quantile)<-c(".10    ",".25    ",".50    ",".75    ",".90    ",".99    ")

    #vi) alter interaction names
        # uqr$stCIlower[uqr$DV!=unique(uqr$DV)[2]]<-uqr$stCIlower[uqr$DV==unique(uqr$DV)[1] & uqr$Quantile==unique(uqr$Quantile)[4]][2:4]+.02
        # uqr$stCIupper[uqr$DV==unique(uqr$DV)[1] & uqr$Quantile==unique(uqr$Quantile)[4]][2:4]<-uqr$stCIupper[uqr$DV==unique(uqr$DV)[1] & uqr$Quantile==unique(uqr$Quantile)[4]][2:4]+.02
        # uqr$stEstimate[uqr$DV==unique(uqr$DV)[1] & uqr$Quantile==unique(uqr$Quantile)[4]][2:4]<-uqr$stEstimate[uqr$DV==unique(uqr$DV)[1] & uqr$Quantile==unique(uqr$Quantile)[4]][2:4]+.02
        # uqr$stCIlower[uqr$DV==unique(uqr$DV)[2] & uqr$Quantile==unique(uqr$Quantile)[2]][3:4]<-uqr$stCIlower[uqr$DV==unique(uqr$DV)[2] & uqr$Quantile==unique(uqr$Quantile)[2]][3:4]+.0015
        # uqr$stCIupper[uqr$DV==unique(uqr$DV)[2] & uqr$Quantile==unique(uqr$Quantile)[2]][3:4]<-uqr$stCIupper[uqr$DV==unique(uqr$DV)[2] & uqr$Quantile==unique(uqr$Quantile)[2]][3:4]+.0015
        # uqr$stEstimate[uqr$DV==unique(uqr$DV)[2] & uqr$Quantile==unique(uqr$Quantile)[2]][3:4]<-uqr$stEstimate[uqr$DV==unique(uqr$DV)[2] & uqr$Quantile==unique(uqr$Quantile)[2]][3:4]+.0015
    #vii) plot all
        m0<-plotfun1(1)
        m1<-plotfun2(1)
        m2<-plotfun2(2)
        m3<-plotfun2(3)
        m4<-plotfun2(4)
        m5<-plotfun2(5)
        title <- ggdraw() + draw_label("Figure 2 - Model Estimates for All Dependent Variables", fontface='bold',size=24)
        p<-plot_grid(m0,m1,m2,m3,m4,m5,ncol=6,rel_widths=c(1.1, 1,1,1,1,1)) #put all figures together; make space for y title
        p<-plot_grid(title, p,nrow=2,rel_heights=c(0.06, 1)) #put title above graph
        p<-p+draw_label("Quantile", 0.04, .908,fontface = "bold",size=18) #put quantile above quantile label
        xtitle <- ggdraw() + draw_label(" ", fontface='bold',size=18)
        p<-plot_grid(p,xtitle,nrow=2,rel_heights=c(1,0.06)) #make room for x title
        p<-p+ draw_label("Parameter Estimates", 0.595, 0.05,size=18)
    #viii) save results
        save_plot("C:/Users/bda13/Desktop/1Fig 2 - Parameter Estimates.png",p,
        base_height=10,
        base_aspect_ratio = 1.5)
  }
