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
    
    
#2 - Plot predicted estimates
    scf<-read.csv("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/rscf.csv")
    scf<-scf[scf$RWorkStatus=="Employed" & scf$Female==0  & scf$MARRIED==1,]
    scf<-scf[!((scf$X4712>0) & scf$SWorkStatus=="Homemaker"),]
    scf$INCOME<-sign(scf$INCOME)*log(abs(scf$INCOME))
    scf$INCOME[is.na(scf$INCOME)]<-0
    
    setwd("C:/Users/bda13/Desktop/Sociology/Papers in Progress/Work Hours/Archived tables")
    {
    uqr1<-read_xlsx("uqrnetworth.xlsx")
    uqr2<-read_xlsx("uqrhomeequity.xlsx")
    uqr3<-read_xlsx("uqrstocks.xlsx")
    uqr4<-read_xlsx("Investment research.xlsx")
    uqr5<-read_xlsx("late debts.xlsx")
    uqr1$DV<-"Net Worth"
    uqr2$DV<-"Home Equity"
    uqr3$DV<-"Stocks"
    uqr4$DV<-"Research"
    uqr5$DV<-"Late Debts"
    names(uqr4)[1]<-"Quantile"
    names(uqr5)[1]<-"Quantile"
    uqr<-rbind(uqr1,uqr2,uqr3,uqr4,uqr5)
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
        #uqr$Variable<-factor(uqr$Variable,levels=c("Specialization","Overwork","Overwork * Specialization","Overwork * Year"))
    #fix quantiles
        uqr$Quantile<-as.numeric(uqr$Quantile)
        uqr$Quantile[uqr$Quantile>1]<-uqr$Quantile[uqr$Quantile>1]/100
        uqr$Quantile<-factor(uqr$Quantile,levels=unique(uqr$Quantile))
        levels(uqr$Quantile)<-c(".10",".25",".50",".75",".90",".99")
        }
    #change quantile variables
        scf$NWPercentile2<-scf$NWPercentile
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
    
    i=1
    j=1
    
    tempdfl<-list()
    for(j in 1:length(unique(uqr$DV))){
    #predict wealth by quantile
        lpdf<-list()
        pdfl<-list()
        quantiles<-unique(uqr$Quantile)
        # max=c(10,100,500,2000,8000,80000)
        for(i in 1:length(quantiles)){
            a<-uqr
            percentiledf<-a[a$Quantile==quantiles[i],]
            #subset to unique DV
                percentiledf<-percentiledf[percentiledf$DV==unique(percentiledf$DV)[[j]],]

            names(percentiledf)[18]<-"Parameter"
            sort(unique(percentiledf$Parameter))
            percentiledf$Parameter[percentiledf$Parameter=="intercept"]<-"Intercept"
            percentiledf$Parameter[percentiledf$Parameter=="year1"]<-"Year1"
            percentiledf$Parameter[percentiledf$Parameter=="year1*HeadWorkHoursO"]<-"Year1HeadWorkHoursOve"
            
            #determine modal values for intercept
                #subset data to info in percentile
                    tscf<-scf
                    if(j==1){
                        tscf$percentile<-tscf$NWPercentile
                    }
                    if(j==2){
                      tscf$percentile<-tscf$HEPercentile
                    }
                    if(j==3){
                      if(i<3){
                        next()
                      }
                      tscf$percentile<-tscf$SVPercentile
                    }
                    if(j==4 | j==5){
                      tscf$percentile<-tscf$NWPercentile2
                    }
                    tscf$percentile[tscf$percentile==0]<-10
                    tquantiles<-c(10,25,50,75,90,99)
                    tscf<-tscf[tscf$percentile==tquantiles[i],]
                    
                    # table(tscf$SVPercentile)
                    
                exp(10.7)
                #Income
                    tincome<-as.numeric(wtd.quantile(tscf$INCOME,tscf$WGT,.5)) #median
                #Education
                    a<-c(wtd.mean(tscf$HSGraduate,tscf$WGT),
                    wtd.mean(tscf$SomeCollege,tscf$WGT),
                    wtd.mean(tscf$Bachelors,tscf$WGT),
                    wtd.mean(tscf$AdvancedDegree,tscf$WGT))
                    teduc<-c("HSGraduate","SomeCollege","Bachelors","AdvancedDegree")[which(a==max(a))] #mode
                #race
                    a<-c(wtd.mean(tscf$Black,tscf$WGT),
                         wtd.mean(tscf$Latino,tscf$WGT),
                         wtd.mean(tscf$OTHER,tscf$WGT),
                         wtd.mean(tscf$White,tscf$WGT))
                    trace<-c("black","latino","other","white")[which(a==max(a))] #mode
                #Age
                    tage<-as.numeric(wtd.quantile(tscf$AGE,tscf$WGT,.5)) #median
                #children
                    tChildren<-as.numeric(wtd.quantile(tscf$Children,tscf$WGT,.5)) #median
                #year1
                    tyear1<-27
                    
                    
            #determine base intercept
                base<-
                percentiledf$Estimate[percentiledf$Parameter=="Intercept"]+
                (percentiledf$Estimate[percentiledf$Parameter=="Income"]*tincome)+
                percentiledf$Estimate[percentiledf$Parameter==teduc]+
                (percentiledf$Estimate[percentiledf$Parameter=="age"]*tage)+
                (percentiledf$Estimate[percentiledf$Parameter=="agesquared"]*(tage^2))+
                (percentiledf$Estimate[percentiledf$Parameter=="Year1"]*tyear1)+
                (percentiledf$Estimate[percentiledf$Parameter=="Children"]*tChildren)+
                (percentiledf$Estimate[percentiledf$Parameter=="Children"]*tChildren^2)
                
                if(trace!="white"){
                  base<-base+percentiledf$Estimate[percentiledf$Parameter==trace]
                }
            # #Make insignificant DV estimates have less effect
            #     percentiledf$Estimate[percentiledf$Significant==0]<-percentiledf$Estimate[percentiledf$Significant==0]*.2

            #create df of probability expectation given work hours (40-60)
                pdf<-data.frame(WorkHours=40:60)
                pdf$Wealth<-(base+
                  (pdf$WorkHours-40)*percentiledf$Estimate[percentiledf$Parameter=="Overwork"]+
                  (27*(pdf$WorkHours-40)*percentiledf$Estimate[percentiledf$Parameter=="Overwork * Year"])
                  )#^3
            #do again but for specialized households
                pdf2<-data.frame(WorkHours=40:60)
                pdf2$Wealth<-(base+
                  (pdf2$WorkHours-40)*percentiledf$Estimate[percentiledf$Parameter=="Overwork"]+
                  27*(pdf2$WorkHours-40)*percentiledf$Estimate[percentiledf$Parameter=="Overwork * Year"]+
                  percentiledf$Estimate[percentiledf$Parameter=="Specialization"]+
                  (pdf2$WorkHours-40)*percentiledf$Estimate[percentiledf$Parameter=="Overwork * Specialization"]
                  )#^3
            #rbind
                pdf$Specialization<-0
                pdf2$Specialization<-1
                pdf<-rbind(pdf,pdf2)
                pdf$Specialization<-factor(pdf$Specialization)
                pdf$Quantile<-quantiles[i]
    #                pdf$Wealth<-pdf$Wealth/1000
                pdfl[[i]]<-pdf
        }
        tempdf<-do.call(rbind,pdfl)
        tempdf$Quantile<-paste("Quantile = ",tempdf$Quantile,sep="")
        tempdfl[[j]]<-tempdf
        print(j)
    }

    tempdfl[[1]]$DV<-"Net Worth"
    tempdfl[[2]]$DV<-"Home Equity"
    tempdfl[[3]]$DV<-"Stocks"
    tempdfl[[4]]$DV<-"Research"
    tempdfl[[5]]$DV<-"Late Debts"
    plotdf<-do.call(rbind,tempdfl)

    #Plot
        scaleFUN <- function(x) sprintf("%.1f", x)
        plotfun<-function(xr){
            p<-ggplot(tempdfl[[xr]], aes(x=WorkHours, y=Wealth,group=Specialization,color=Specialization))
            {p+ geom_line(size=2)+
                    scale_colour_grey()+
                    scale_x_continuous(breaks = c(40,50,60))+
                    theme_pander(base_size =14)+
                    theme(
                          axis.line.x = element_line(colour = "black"),
                          axis.line.y = element_line(colour = "black"),
                          plot.title = element_text(size=20,hjust=0),
                          # axis.title.x=element_text(size=18, margin = margin(t = 20, r = 0, b = 2, 0)),
                          # axis.title.y=element_text(size=20, margin = margin(t = 0, r = 20, b = 0, 0)),
                          # axis.text.y=element_text(size=14, margin = margin(t = 0, r = 0, b = 0, 0)),
                          # axis.text.x= element_text(size=14, margin = margin(t = 0, r = 0, b = 0, 0)),
                          axis.title.x=element_blank(),
            #              axis.title.y=element_blank(),
                          #axis.text.y=element_blank(),
                          axis.text.x=element_blank(),
                          strip.text.x = element_blank(),
    #                     legend.direction = "horizontal", legend.position = "bottom", legend.text =element_text(size=16), legend.title =element_text(size=16),
                          legend.position="none",
                          panel.spacing = unit(2.5, "lines"),
                          axis.ticks= element_blank(),
    #                      panel.grid.minor = element_blank(),
    #                     strip.text = element_text(size=16,face = "bold"),
                          plot.margin = unit(c(.5, t=0, b=.2, 0.5),"lines"))+
    #                ggtitle("Figure 1 - Predicted Wealth by Quantile, Work Hours, and Specialization")+
    #                labs(x="Work Hours", y="Predicted Net Worth (Logged)")+
                facet_wrap(~Quantile,nrow=1)+
                scale_y_continuous(labels=scaleFUN)+
                      if(xr==1){
                        labs(x="", y=expression(atop("Net Worth", "(logged)")))
                      }else if(xr==2){
                        labs(x="", y=expression(atop("Home Equity", "(logged)")))
                      }else if(xr==3){
                        labs(x="", y=expression(atop("Stock Value", "(logged)")))
                      }else if(xr==4){
                        labs(x="", y=expression(atop("Research", "(log odds)")))
                      }else if(xr==5){
                        labs(x="", y=expression(atop("Late Debts", "(log odds)")))
                      }

            }
        }

          m01<-ggplot(tempdfl[[1]], aes(x=WorkHours, y=Wealth,group=Specialization,color=Specialization))+
          # geom_line(size=2)+
              scale_colour_grey()+
              #scale_y_continuous(breaks = c(0:10*2))+
              scale_x_continuous(breaks = c(40,50,60))+
              theme_pander(base_size =14)+
              theme(
                axis.line.x = element_line(colour = "black"),
                axis.line.y = element_line(colour = "black"),
                plot.title = element_text(size=20,hjust=0),
                # axis.title.x=element_text(size=18, margin = margin(t = 20, r = 0, b = 2, 0)),
                # axis.title.y=element_text(size=20, margin = margin(t = 0, r = 20, b = 0, 0)),
                # axis.text.y=element_text(size=14, margin = margin(t = 0, r = 0, b = 0, 0)),
                # axis.text.x= element_text(size=14, margin = margin(t = 0, r = 0, b = 0, 0)),
                axis.title.x=element_blank(),
                # axis.title.y=element_blank(),
                # axis.text.y=element_blank(),
                axis.text.x=element_blank(),
                # strip.text.x = element_blank(),
                #                     legend.direction = "horizontal", legend.position = "bottom", legend.text =element_text(size=16), legend.title =element_text(size=16),
                legend.position="none",
                panel.spacing = unit(2.5, "lines"),
                axis.ticks= element_blank(),
                #                      panel.grid.minor = element_blank(),
                #                     strip.text = element_text(size=16,face = "bold"),
                plot.margin = unit(c(.5, t=.2, b=.2, 0.5),"lines"))+
              #                ggtitle("Figure 1 - Predicted Wealth by Quantile, Work Hours, and Specialization")+
              #                labs(x="Work Hours", y="Predicted Net Worth (Logged)")+
              facet_wrap(~Quantile,nrow=1)+
            geom_blank()+
            labs(x="", y=expression(atop("", "")))


          m02<-ggplot(tempdfl[[1]], aes(x=WorkHours, y=Wealth,group=Specialization,color=Specialization))+
            # geom_line(size=2)+
            scale_colour_grey()+
            #scale_y_continuous(breaks = c(0:10*2))+
            scale_x_continuous(breaks = c(40,50,60))+
            theme_pander(base_size =14)+
            theme(
              axis.line.x = element_line(colour = "white"),
              axis.line.y = element_line(colour = "white"),
              plot.title = element_text(size=20,hjust=0),
               axis.title.x=element_text(size=18, margin = margin(t = 20, r = 0, b = 2, 0)),
              axis.title.y=element_text(size=20, margin = margin(t = 0, r = 20, b = 0, 0)),
              # axis.text.y=element_text(size=14, margin = margin(t = 0, r = 0, b = 0, 0)),
               axis.text.x= element_text(size=14, margin = margin(t = 0, r = 0, b = 0, 0)),
              #axis.title.x=element_blank(),
              #axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              #axis.text.x=element_blank(),
              strip.text.x = element_blank(),
              #                     legend.direction = "horizontal", legend.position = "bottom", legend.text =element_text(size=16), legend.title =element_text(size=16),
              legend.position="none",
              panel.spacing = unit(2.5, "lines"),
              axis.ticks= element_blank(),
                                    panel.grid.minor = element_blank(),
              #                     strip.text = element_text(size=16,face = "bold"),
              plot.margin = unit(c(.5, t=0, b=.2, 0.5),"lines"))+
            #                ggtitle("Figure 1 - Predicted Wealth by Quantile, Work Hours, and Specialization")+
            #                labs(x="Work Hours", y="Predicted Net Worth (Logged)")+
            labs(x="Work Hours Per Week", y=" \n \n")+
            facet_wrap(~Quantile,nrow=1)+
            geom_blank()

          #hide home equity and stocks where no clear estimates
          tempdfl[[2]]$Wealth[tempdfl[[2]]$Quantile=="Quantile = .10" | tempdfl[[2]]$Quantile=="Quantile = .25"]<-NA
          tempdfl[[3]]$Wealth[tempdfl[[3]]$Quantile=="Quantile = .10" | tempdfl[[3]]$Quantile=="Quantile = .25" | tempdfl[[3]]$Quantile=="Quantile = .50"]<-NA
          #add missing rows to stcoks
              trows<-(nrow(tempdfl[[2]])-nrow(tempdfl[[3]]))
              tempdfl[[3]]<-rbind(tempdfl[[2]][1:trows,],tempdfl[[3]])
              tempdfl[[3]]$DV[trows]<-"Stocks"



      #plot all
          m1<-plotfun(1)
          m2<-plotfun(2)
          m3<-plotfun(3)
          m4<-plotfun(4)
          m5<-plotfun(5)
          p<-plot_grid(m1,m2,m3,m4,m5,ncol=1)
          p<-plot_grid(m01,p,ncol=1,rel_heights=c(0.06,1))
          p<-plot_grid(p,m02,ncol=1,rel_heights=c(1,0.15))
          title <- ggdraw() + draw_label("Figure 3 - Predicted Values of Dependent Variables by Quantile", fontface='bold',size=20)
          p<-plot_grid(title, p,nrow=2,rel_heights=c(0.07, 1)) #put title above graph
          p<-plot_grid(p,scale = .98)

          #save
          save_plot("C:/Users/bda13/Desktop/Fig 3 - Predicted Values of Dependent Variables by Quantile.png",p,
                    base_height=14,
                    base_aspect_ratio = .78)

