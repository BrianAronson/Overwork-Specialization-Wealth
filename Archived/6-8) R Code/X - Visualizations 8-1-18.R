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
    #b - load data; rename data; set work directory
        scf<-read.csv("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/rscf.csv")
        setwd("C:/Users/bda13/Desktop/Sociology/Papers in Progress/Work Hours/Figures and Tables/")

        # library(Hmisc)
        # temp<-scf[scf$AGE<31 & scf$AGE>27,]
        # temp<-temp[temp$year>2012,]
        # temp<-temp[temp$Education>0,]
        # wtd.quantile(temp$NETWORTH,weights = temp$WGT)
        # wtd.quantile(temp$NETWORTH,weights = temp$WGT,c(1:20)/20)
        
#1 - transform/create necessary variables
    #a -Percentiles to character
        scf$NWPercentile<-as.character(scf$NWPercentile)
        scf$IPercentile<-as.character(scf$IPercentile)
# 
#         tempscf<-scf[scf$RWorkStatus=="Employed" & scf$Female==0,]        
#         vars<-c("MARRIED","Separated", "Divorced", "Widowed","LivingWithPartner")
#         mat<-matrix(0,length(vars),length(vars))
#         for(i in 1:length(vars)){
#           for(j in 1:length(vars)){
#             mat[i,j]<-table(tempscf[,vars[i]],tempscf[,vars[j]])[2,2]
#           }
#         }
    
# #Figure 1 - Networth allocated to asset types by percentile
#     #a - prep data
#         #subset data
#             tempscf<-scf[scf$RWorkStatus=="Employed" & scf$Female==0,]
#             #remove 0th percentile
#                 tempscf<-tempscf[tempscf$NWPercentile!="0",]
#             #change key variables to be percent of networth rather than percent of assets
#                 perc<-function(x,y){
#                   z<-x/y
#                   z[z>1]<-1
#                   z[z<0]<-0
#                   return(z)
#                 }
#                 tempscf$PStockValue<-perc(tempscf$StockValue,tempscf$NETWORTH)
#                 tempscf$PHomeequity<-perc(tempscf$HomeEquity,tempscf$NETWORTH)
#                 
#         #Find weighted means and variances
#             #Create empty df
#                 tempdf<-data.frame(Percentile=rep(sort(unique(tempscf$NWPercentile)),4),Specialized=rep(rep(c(0,1),each=6),2),Asset.Type=rep(c("Stocks and Mutual Funds","Home Equity"),each=12))
#             for (i in 1:nrow(tempdf)){
#               if(i<11){
#                 tempdf$Value[i]<-wtd.mean(tempscf$PStockValue[tempscf$HomemakerSpouse==tempdf$Specialized[i] & tempscf$NWPercentile==tempdf$Percentile[i]],tempscf$wgt[tempscf$NWPercentile==tempdf$Percentile[i] & tempscf$HomemakerSpouse==tempdf$Specialized[i]])
#               }else{
#                 tempdf$Value[i]<-wtd.mean(tempscf$PHomeequity[tempscf$HomemakerSpouse==tempdf$Specialized[i] & tempscf$NWPercentile==tempdf$Percentile[i]],tempscf$wgt[tempscf$NWPercentile==tempdf$Percentile[i] & tempscf$HomemakerSpouse==tempdf$Specialized[i]])
#               }
#             }
#         #Change indicator for specialization
#             tempdf$Specialized<-ifelse(tempdf$Specialized=="0","Not Specialized  ","Specialized")
#         #Change levels for Asset.Type
#             tempdf$Asset.Type<-factor(tempdf$Asset.Type,levels = c(as.character(unique(tempdf$Asset.Type))))
#     #b - Plot
#         p <- ggplot(data = tempdf, aes(x = Percentile, y = Value))
#         pdf(file="Figure 1 - Average Percent of Net Worth Allocated to Asset Type by Wealth Percentile.pdf", height=6, width=11)
#             p + geom_bar(aes(fill = as.factor(Specialized)), position = "dodge", stat="identity")+
#             facet_wrap(~Asset.Type, scales='free')+
#             scale_y_continuous(labels=scales::percent)+
#             theme_bw()+
#             scale_fill_grey(start = .75, end = 0)+
#             labs(x="Wealth Percentile", y="Percent of Net Worth Allocated",fill="")+
#             ggtitle("Figure 1 - Average Percent of Net Worth Allocated to Asset Type by Wealth Percentile")+
#             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                   panel.background = element_blank(), axis.line = element_line(colour = "black"),
#                   legend.text=element_text(size=11), title = element_text(size=14),
#                   legend.position="bottom",
#                   axis.text.x = element_text(size=14),  axis.text.y = element_text(size=14),
#                   axis.title.x= element_text(size=14,margin = margin(t = 20, r = 0, b = 0, l = 0)),
#                   axis.title.y= element_text(size=14,margin = margin(t = 0, r = 20, b = 0, l = 0)),
#                   strip.text.x = element_text(size = 14))
#         dev.off()

        
    # #Figure 2 - Financial Behaviors by Wealth Percentile and Specialization
    #     #a - grab variables of interest
    #         names(scf)<-tolower(names(scf))
    #         scf$temp<-((scf$imagznews | scf$imailadtv | scf$idont | scf$ise)*1)
    #         scf$temp<-((scf$ifinplan | scf$ifinpro)*1)
    #         scf$temp<-((scf$idont | scf$imailadtv)*1)
    #         #mean(scf$iself[scf$networth>1000000&scf$HomemakerSpouse==1],na.rm = T)
    #         df<-scf[,c("wgt","homemakerspouse","nwpercentile","kginc1","imailadtv","late","homeowner","hstocks","anyli","temp","iself")]
    #         df<-data.table(df)
    # #        table(scf$kginc1)
    #         # table(scf$kginc[scf$networth>8000000 &scf$HomemakerSpouse==1]>2)
    #         #prop.table(table(scf$equitinc[scf$networth<8000000&scf$HomemakerSpouse==0]>0))
    #     #b - get row means of variable by specialization and percentile
    #         a<-df[,.(kginc1=weighted.mean(iself,wgt,na.rm=T),imailadtv=weighted.mean(imailadtv,wgt,na.rm=T),late=weighted.mean(late,wgt,na.rm=T),HomeOwner=weighted.mean(homeowner,wgt,na.rm=T),hstocks=weighted.mean(hstocks,wgt,na.rm=T),anyli=weighted.mean(anyli,wgt,na.rm=T)),by=list(nwpercentile,homemakerspouse)]
    #     #c -rename first two columns
    #         names(a)[1]<-"Percentile"
    #         names(a)[2]<-"Specialized"
    #     #d - split results, rename columns, and append on top of each other
    #         a1<-a[,c(1:3)]
    #         a2<-a[,c(1,2,4)]
    #         a3<-a[,c(1,2,5)]
    #         a4<-a[,c(1,2,6)]
    #         a5<-a[,c(1,2,7)]
    #         a6<-a[,c(1,2,8)]
    #         a1$Behavior<-"Researches Investments"
    #         a2$Behavior<-"Ads Inform Investments"
    #         a3$Behavior<-"Had Late Debt Payments"
    #         a4$Behavior<-"Owns Home"
    #         a5$Behavior<-"Owns Stocks"
    #         a6$Behavior<-"Owns Life Insurance"
    #         names(a1)[3]<-"Estimate"
    #         names(a2)[3]<-"Estimate"
    #         names(a3)[3]<-"Estimate"
    #         names(a4)[3]<-"Estimate"
    #         names(a5)[3]<-"Estimate"
    #         names(a6)[3]<-"Estimate"
    #         df<-rbind(a1,a2)
    #         df<-rbind(df,a3)
    #         df<-rbind(df,a4)
    #         df<-rbind(df,a5)
    #         df<-rbind(df,a6)
    #     #e - set levels (order) of behavior
    #         order<-c("Owns Home","Owns Life Insurance","Owns Stocks","Researches Investments","Ads Inform Investments","Had Late Debt Payments")
    #         df$Behavior = factor(df$Behavior,levels=order)
    #     #f - kill variable of non-interest
    # #        df<-df[df$Behavior!="Sold any investments in last year",]
    #     #g - Plot
    #         p <- ggplot(data = df, aes(x = Percentile, y = Estimate))
    #         pdf(file="Figure 2 - Financial Behaviors by Wealth Percentile and Specialization.pdf", height=6, width=11)
    #         p + geom_bar(aes(fill = as.factor(Specialized)), position = "dodge", stat="identity")+
    #           facet_wrap(~Behavior)+
    #           scale_y_continuous(labels=scales::percent)+
    #           theme_bw()+
    #           scale_fill_grey(start = .75, end = 0)+
    #           labs(x="Wealth Percentile", y="Rate of Financial Behavior",fill="")+
    #           ggtitle("Figure 2 - Financial Behaviors by Wealth Percentile and Specialization")+
    #           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    #                 panel.background = element_blank(), axis.line = element_line(colour = "black"),
    #                  legend.text=element_text(size=11), title = element_text(size=14),
    #                 # legend.position="bottom",
    #                 axis.text.x = element_text(size=14),  axis.text.y = element_text(size=14),
    #                 axis.title.x= element_text(size=14,margin = margin(t = 20, r = 0, b = 0, l = 0)),
    #                 axis.title.y= element_text(size=14,margin = margin(t = 0, r = 20, b = 0, l = 0)),
    #                 strip.text.x = element_text(size = 14))+
    #                 guides(fill=guide_legend(title="Specialized"))
    #         dev.off()
    #     
    #combine the two graphs above
          # names(df)
          # names(tempdf)
          # names(tempdf)<-c("Percentile","Specialized","Behavior","Estimate")
          # tempdf2<-rbind(df,tempdf)
          # tempdf2$Specialized<-as.numeric(ifelse(tempdf2$Specialized=="Specialized",1,ifelse(tempdf2$Specialized=="Not Specialized  ",0,tempdf2$Specialized)))
          # tempdf2<-tempdf2[tempdf2$Percentile!=0]
          # tempdf2<-tempdf2[tempdf2$Behavior!="Owns Stocks" & tempdf2$Behavior!="Ads Inform Investments"]
          # tempdf2$Estimate[tempdf2$Behavior=="Had Late Debt Payments"]<-1-tempdf2$Estimate[tempdf2$Behavior=="Had Late Debt Payments"]
          # tempdf2$Behavior<-as.character(tempdf2$Behavior)
          # tempdf2$Behavior[tempdf2$Behavior=="Had Late Debt Payments"]<-"No Late Debt Payments"
          # order<-c("Home Equity","Stocks and Mutual Funds", "Owns Home","Owns Life Insurance","Researches Investments","No Late Debt Payments")
          # tempdf2$Behavior = factor(tempdf2$Behavior,levels=order)
          # tempdf2$Percentile<-as.numeric(as.character(tempdf2$Percentile))
          # tempdf2$Percentile<-as.factor(tempdf2$Percentile)
          # tempdf2<-tempdf2[order(tempdf2$Behavior),]
          # tempdf2$ymin<-rep(c(0.6,0,0,.6,0,0),each=10)
          # #plot
          # p <- ggplot(data = tempdf2, aes(x = Percentile, y = Estimate))
          # pdf(file="Figure 2 - Financial Behaviors by Wealth Percentile and Specialization.pdf", height=9, width=8)
          # p + geom_bar(aes(fill = as.factor(Specialized)), position = "dodge", stat="identity")+
          #   facet_wrap(~Behavior,ncol=2,scales = "free")+
          #   scale_y_continuous(labels=scales::percent)+
          #   theme_bw()+
          #   scale_fill_grey(start = .75, end = 0)+
          #   labs(x="Wealth Percentile", y="Rate of Financial Behavior",fill="")+
          #   ggtitle("Figure 2 - Financial Behaviors by Wealth Percentile and Specialization")+
          #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          #         panel.background = element_blank(), axis.line = element_line(colour = "black"),
          #         legend.text=element_text(size=13), title = element_text(size=12.8),
          #         # legend.position="bottom", axis.title = element_blank(),
          #         axis.text.x = element_text(size=12),  axis.text.y = element_text(size=14),
          #         axis.title.x= element_text(size=14,margin = margin(t = 20, r = 0, b = 0, l = 0)),
          #         axis.title.y= element_text(size=14,margin = margin(t = 0, r = 20, b = 0, l = 0)),
          #         legend.position="bottom",
          #         strip.text.x = element_text(size = 14))+
          #   theme(plot.margin = unit(c(1, 3, 0.5, 0.5),"lines"))+
          #   guides(fill=guide_legend(title="Specialized"))+
          #   geom_blank(aes(y = ymin))
          # dev.off()
          # 
          

#Figure 4 - Changing Rate of Homemaker Spouses by Wealth Percentile
    #a - Prep data  
        #Determine weighted means
          tempscf<-scf[scf$RWorkStatus=="Employed" & scf$Female==0,]
        #remove 25th and 10th percentile
          tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="25","0",tempscf$NWPercentile)
          tempscf$NWPercentile<-ifelse(tempscf$NWPercentile=="10","0",tempscf$NWPercentile)
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
            aggscf$Homemaker[i]<-wtd.mean(tempscf$Homemaker[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]],tempscf$WGT[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]])
            #aggscf$Homemaker[i]<-mean(tempscf$Homemaker[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]])
            aggscf$Var[i]<-wtd.var(tempscf$Homemaker[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]],tempscf$wgt[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]])
          }
      #Determine SEM while accounting for fact that numbers are below 100.
          aggscf$SEM<-sqrt(100*aggscf$Var)/100
          class(aggscf$Year)
  #b - Plot graph
        p<-ggplot(aggscf, aes(Year, y=Homemaker))
        png(file="C:/Users/bda13/Desktop/Figure 1 - Changing Rate of Homemaker Spouses by Wealth Percentile.png", height=7, width=11, units="in",res=300)
            p+ geom_smooth(aes(group=NWPercentile, color=NWPercentile),se=F,size=3,span=.7)+
            #geom_line(aes(group=NWPercentile, color=NWPercentile),size=2)
                scale_colour_grey(name="Net Worth Percentile",start=.75,end=0)+
                scale_y_continuous(labels=scales::percent,breaks = ((0:5)/10),limits = c(0,.5))+ #,limits = c(0,.51)
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
                ggtitle("Figure 1 - Changing Rate of Homemaker Spouses by Wealth Percentile")+
                geom_dl(aes(label = paste("  ",NWPercentile,"%  ",sep="")), method = list(c('first.bumpup'),dl.combine(first.points), cex = .9)) +
                geom_dl(aes(label = paste("  ",NWPercentile,"%  ",sep="")), method = list(c('last.bumpup'),dl.combine(last.points), cex = .9)) +
                #geom_errorbar(aes(ymin = Homemaker - SEM, ymax = Homemaker + SEM), width=0.3)+
                labs(x="Year", y="Percentage Homemakers")
        dev.off()
  #c - Alternative with loess 
        # tempscf2<-tempscf[,c("NWPercentile","year","HomemakerSpouse","wgt")]
        # p<-ggplot(tempscf2, aes((year), y=HomemakerSpouse, weight=wgt))
        # p+ geom_smooth(aes(group=NWPercentile, color=NWPercentile),size=2,se = F)
        
        plot(density())
        boxplot(scf$NETWORTH)
# #Figure 3 - predicted probabilities
#     #read predictions
#         uqr<-read.csv("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/uqrregres6618.csv")
#         uqr<-as.data.table(uqr)
#     #average predictions across imputations
#         a<-uqr[,.(Estimate=mean(Estimate)),by=list(Parameter,Quantile)]
#     #predict wealth by quantile
#         lpdf<-list()
#         pdfl<-list()
#         quantiles<-unique(a$Quantile)
#         max=c(10,100,500,2000,8000,80000)
#         for(i in 1:length(quantiles)){
#             percentiledf<-a[a$Quantile==quantiles[i]]
#             #determine base intercept
#                 base<-
#                 percentiledf$Estimate[percentiledf$Parameter=="Intercept"]+
#                 percentiledf$Estimate[percentiledf$Parameter=="Bachelors"]+
#                 percentiledf$Estimate[percentiledf$Parameter=="Married"]+
#                 (percentiledf$Estimate[percentiledf$Parameter=="Age"]*35)+
#                 (percentiledf$Estimate[percentiledf$Parameter=="AgeSquared"]*(35^2))+
#                 (percentiledf$Estimate[percentiledf$Parameter=="Year1"]*27)
#             #create df of probability expectation given work hours (40-60)
#                 pdf<-data.frame(WorkHours=40:60)
#                 pdf$Wealth<-(base+
#                   (pdf$WorkHours-40)*percentiledf$Estimate[percentiledf$Parameter=="HeadWorkHoursOver40"]+
#                   27*(pdf$WorkHours-40)*percentiledf$Estimate[percentiledf$Parameter=="Year1HeadWorkHoursOver40"]
#                   )#^3
#             #do again but for specialized households
#                 pdf2<-data.frame(WorkHours=40:60)
#                 pdf2$Wealth<-(base+
#                   (pdf2$WorkHours-40)*percentiledf$Estimate[percentiledf$Parameter=="HeadWorkHoursOver40"]+
#                   27*(pdf2$WorkHours-40)*percentiledf$Estimate[percentiledf$Parameter=="Year1HeadWorkHoursOver40"]+
#                   percentiledf$Estimate[percentiledf$Parameter=="HomemakerSpouse"]+
#                   (pdf2$WorkHours-40)*percentiledf$Estimate[percentiledf$Parameter=="HeadWorkHoursOver40HomemakerSpouse"]
#                   )#^3
#             #rbind
#                 pdf$Specialization<-0
#                 pdf2$Specialization<-1
#                 pdf<-rbind(pdf,pdf2)
#                 pdf$Specialization<-factor(pdf$Specialization)
#                 pdf$Quantile<-quantiles[i]
# #                pdf$Wealth<-pdf$Wealth/1000
#                 pdfl[[i]]<-pdf
#         }
#         tempdf<-do.call(rbind,pdfl)
#         tempdf$Quantile<-paste("Quantile = ",tempdf$Quantile,sep="")
#         #Plot
#             p<-ggplot(tempdf, aes(x=WorkHours, y=Wealth,group=Specialization,color=Specialization))
# 
#             pdf(file="Figure 1 - Predicted Wealth by Quantile, Work Hours, and Specialization.pdf", height=7, width=12)
#             {p+ geom_line(size=3)+
#                     scale_colour_grey()+
#                     scale_y_continuous(breaks = c(0:10*2))+
#                     scale_x_continuous(breaks = c(40,50,60))+
#                     theme_pander(base_size =14)+
#                     theme(
#                           axis.line = element_line(colour = "black"),
#                           plot.title = element_text(size=20,hjust=0),
#                           axis.title.x=element_text(size=18, margin = margin(t = 20, r = 0, b = 2, 0)),
#                           axis.title.y=element_text(size=20, margin = margin(t = 0, r = 20, b = 0, 0)),
#                           axis.text.y=element_text(size=14, margin = margin(t = 0, r = 0, b = 0, 0)),
#                           axis.text.x= element_text(size=14, margin = margin(t = 0, r = 0, b = 0, 0)),
#                           legend.direction = "horizontal", legend.position = "bottom", legend.text =element_text(size=16), legend.title =element_text(size=16),
# #                          legend.position="none",
#                           panel.spacing = unit(2.5, "lines"),
#                           strip.text = element_text(size=16,face = "bold"),
#                           plot.margin = unit(c(1, 1, 0.5, 0.5),"lines"))+
#                     ggtitle("Figure 1 - Predicted Wealth by Quantile, Work Hours, and Specialization")+
#                     labs(x="Work Hours", y="Predicted Net Worth (Logged)")+
#                     facet_wrap(~Quantile,nrow=1,strip.position = "top")
#             }
#             dev.off()
# 
# 
#         title <- ggdraw() + draw_label("Predicted Wealth by Quantile, Work Hours, and Specialization", fontface='bold')
#         p<-plot_grid(ncol=3,lpdf[[1]],lpdf[[2]],lpdf[[3]],lpdf[[4]],lpdf[[5]],lpdf[[6]])
#         p<-plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
#         save_plot("predicted probabilities.pdf",plot=p,base_height=11)
                  


# #Rate of work hours by specialization and percentile
# #Determine weighted means
#     library(wCorr)
#     weightedCorr(x=tempscf$NETWORTH,y=tempscf$HeadWorkHoursOver40,weights=tempscf$WGT,method = c("Pearson"))
#     weightedCorr(x=tempscf$NETWORTH,y=tempscf$Homemaker,weights=tempscf$WGT,method = c("Pearson"))
#     scf$NWPercentile
#     tempscf<-data.table(tempscf)
#     a<-tempscf[,.(Overwork=mean(HeadWorkHoursOver40)),by=c("Homemaker","NWPercentile")]
#       #Create empty df
#           aggscf<-as.data.frame(matrix(nrow=length(unique(tempscf$year))*length(unique(tempscf$NWPercentile)),ncol=1))
#           names(aggscf)<-"WorkHours"
#           aggscf$Year<-rep(unique(tempscf$year),length(unique(tempscf$NWPercentile)))
#           aggscf$NWPercentile<-rep(sort(unique(tempscf$NWPercentile)),each=length(unique(tempscf$year)))
#           aggscf$Var<-0
#       #Find weighted means and variances
#           for (i in 1:nrow(aggscf)){
#             aggscf$WorkHours[i]<-wtd.mean(tempscf$RHoursPerWeek80[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]],tempscf$wgt[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]])
#             aggscf$SEM[i]<-sqrt(wtd.var(tempscf$RHoursPerWeek80[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]],tempscf$wgt[tempscf$year==aggscf$Year[i] & tempscf$NWPercentile==aggscf$NWPercentile[i]]))
#           }
#       #Change age labels
#           aggscf$Year<-ifelse(aggscf$Year==30,"30-34",ifelse(aggscf$Year==35,"35-39",ifelse(aggscf$Year==40,"40-44",ifelse(aggscf$Year==45,"45-49",ifelse(aggscf$Year==50,"50-54",ifelse(aggscf$Year==55,"55-59",ifelse(aggscf$Year==60,"60-64",ifelse(aggscf$Year==65,"65-69",ifelse(aggscf$Year==70,"70-74","")))))))))
#           tempscf$HeadWorkHours[tempscf$HeadWorkHours<0]<-0
#           a<-as.data.frame(table(tempscf$YY1,tempscf$year))
#             #Plot graph
#           p<-ggplot(tempscf, aes(y=WorkHours,x=Year))
#           pdf(file="Fig - Average Work Hours by Age and Wealth Percentile.pdf", height=6, width=11)
#               p+geom_bar(aes(fill = NWPercentile), position = "dodge", stat="identity")+
#               theme_bw()+
#               scale_fill_grey(start = .9, end = 0)+
#               labs(x="Age Range", y="Average Breadwinner Work Hours",fill="Percentile")+
#               ggtitle("Average Work Hours by Age and Wealth Percentile")+
#               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                   panel.background = element_blank(), axis.line = element_line(colour = "black"),
#                   legend.text=element_text(size=12), title = element_text(size=18),
#                   legend.title=element_text(size=14),
#                   axis.text.x = element_text(size=14),  axis.text.y = element_text(size=14),
#                   axis.title.x= element_text(size=14,margin = margin(t = 20, r = 0, b = 0, l = 0)),
#                   axis.title.y= element_text(size=14.5,margin = margin(t = 0, r = 20, b = 0, l = 0)))
#           dev.off()
# sum(tempscf$Homemaker)/5
# sum(1-tempscf$Homemaker)/5
# 
# 
# cor(scf$HeadWorkHoursOver40,scf$Homemaker)

        
        
#Figure 3 - Parameter estimates
    setwd("C:/Users/bda13/Desktop/Sociology/Papers in Progress/Work Hours/")
    scf<-read.csv("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/rscf.csv")

    {
    library(readxl)    
    library(XML)
    library(RCurl)
    library(rlist)
    library(stringr)
    library(weights)
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
    uqr$Variable<-factor(uqr$Variable,levels=c("Overwork * Year","Overwork * Specialization","Overwork","Specialization"))
#fix quantiles
    uqr$Quantile<-as.numeric(uqr$Quantile)
    uqr$Quantile[uqr$Quantile<1]<-uqr$Quantile[uqr$Quantile<1]*100
    uqr$Quantile<-factor(uqr$Quantile,levels=unique(uqr$Quantile))
#standardize variables
    #prep scf
        scf<-scf[scf$RWorkStatus=="Employed" & scf$Female==0,]
        scf$Specialization<-scf$HomemakerSpouse
        scf$Overwork<-scf$HeadWorkHoursOver40
        scf$`Overwork * Specialization`<-scf$Specialization*scf$Overwork
        scf$`Overwork * Year`<-scf$Overwork*scf$year1
    #find sds for each variable in each quantile
    #NOTE: MY PERCENTILE CUTS ARE DIFFERENT FOR EACH MODEL. 
        #THE PERCENTILES FOR QUANTILE REGRESSIONS ARE BASED ON 
        #DISTRIBUTION OF ONLY THOSE IN SAMPLE; THE FULL SAMPLE IS 
        #USED FOR LOGISTIC REGRESSIONS; THE FOLLOWING ARE SLIGHTLY WRONG
    #prep new variable
        uqr$sdEstimate<-0
    #prep function
        newfun<-function(x,per,p) sqrt(wtd.var(scf[,x][scf[,per]==p]))
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
        # x<-"Overwork"
        # per<-"NWPercentile"
        # p<-25
        # which(uqr$Quantile==25,uqr$DV=="Investment Research")
    #alter for home equity and stocks
        table(scf$HEPercentile)
        table(scf$SVPercentile)
        table(scf$NWPercentile)
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
      
#Visualize
    #create plot for axis titles and variable names
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
    #create other plots
    plotfun2<-function(x){
      mi<-mfloor(min(uqr$stCIlower[uqr$DV==unique(uqr$DV)[x]],na.rm=T),.5)
      ma<-mcieling(max(uqr$stCIupper[uqr$DV==unique(uqr$DV)[x]],na.rm=T),.5)
        ggplot(uqr[uqr$DV==unique(uqr$DV)[x],], aes(y=stEstimate,x=(Variable)))+
        geom_pointrange(aes(ymin=stCIlower,ymax=stCIupper),size=1.5,fill=ifelse(uqr$stSignificant[uqr$DV==unique(uqr$DV)[x]]==0,"white","darkgrey"),color=ifelse(uqr$stSignificant[uqr$DV==unique(uqr$DV)[x]]==0,"grey70","black"),shape=73, fatten = 6) + coord_flip()+
        geom_hline(yintercept = 0)+
        theme_bw() +
        theme(axis.ticks.y= element_blank(),
          panel.grid.minor = element_blank(),
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
          if(x==1){
            scale_y_continuous(limits = c(-.6,1.1),breaks =seq(from=-.5,to=1,by=.5))
          }else if(x==2){
            scale_y_continuous(limits = c(-.11,.21),breaks =seq(from=-.1,to=.25,by=.1))
          }else if(x==3){
            scale_y_continuous(limits = c(-.7,.7),breaks =seq(from=-.5,to=.5,by=.5))
          }else if(x==4){
            scale_y_continuous(limits = c(-1.05,.8),breaks =seq(from=-1,to=.5,by=.5))
          }else if(x==5){
            scale_y_continuous(limits = c(-.7,1.2),breaks =seq(from=-.5,to=1,by=.5))
          }
    }

    #manually edit things to fit in margins well
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
        
        a<-uqr$stCIupper[uqr$DV==unique(uqr$DV)[5]][21]
        b<-uqr$stCIlower[uqr$DV==unique(uqr$DV)[5]][21]
        uqr$stCIupper[uqr$DV==unique(uqr$DV)[5]][21]<-a-.05
        uqr$stCIlower[uqr$DV==unique(uqr$DV)[5]][21]<-b+.05
        
        a<-uqr$stCIupper[uqr$DV==unique(uqr$DV)[5]][22]
        b<-uqr$stCIlower[uqr$DV==unique(uqr$DV)[5]][22]
        uqr$stCIupper[uqr$DV==unique(uqr$DV)[5]][22]<-a-.05
        uqr$stCIlower[uqr$DV==unique(uqr$DV)[5]][22]<-b+.05
       }
    #manually edit to make significandots not overlap 0    
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
        }
    
    sub1<-uqr$DV==unique(uqr$DV)[3]
    uqr$stEstimate[sub1]
#kill null stocks findings
    uqr[uqr$DV=="Stocks" & (as.character(uqr$Quantile)=="75th   " | as.character(uqr$Quantile)=="50th   "),c("sdEstimate","stEstimate","stCIlower","stCIupper","stSignificant")]<-NA

#Change names of quantiles
    levels(uqr$Quantile)<-c(".10    ",".25    ",".50    ",".75    ",".90    ",".99    ")

#alter interaction names
    
    # uqr$stCIlower[uqr$DV!=unique(uqr$DV)[2]]<-uqr$stCIlower[uqr$DV==unique(uqr$DV)[1] & uqr$Quantile==unique(uqr$Quantile)[4]][2:4]+.02
    # uqr$stCIupper[uqr$DV==unique(uqr$DV)[1] & uqr$Quantile==unique(uqr$Quantile)[4]][2:4]<-uqr$stCIupper[uqr$DV==unique(uqr$DV)[1] & uqr$Quantile==unique(uqr$Quantile)[4]][2:4]+.02
    # uqr$stEstimate[uqr$DV==unique(uqr$DV)[1] & uqr$Quantile==unique(uqr$Quantile)[4]][2:4]<-uqr$stEstimate[uqr$DV==unique(uqr$DV)[1] & uqr$Quantile==unique(uqr$Quantile)[4]][2:4]+.02
    # uqr$stCIlower[uqr$DV==unique(uqr$DV)[2] & uqr$Quantile==unique(uqr$Quantile)[2]][3:4]<-uqr$stCIlower[uqr$DV==unique(uqr$DV)[2] & uqr$Quantile==unique(uqr$Quantile)[2]][3:4]+.0015
    # uqr$stCIupper[uqr$DV==unique(uqr$DV)[2] & uqr$Quantile==unique(uqr$Quantile)[2]][3:4]<-uqr$stCIupper[uqr$DV==unique(uqr$DV)[2] & uqr$Quantile==unique(uqr$Quantile)[2]][3:4]+.0015
    # uqr$stEstimate[uqr$DV==unique(uqr$DV)[2] & uqr$Quantile==unique(uqr$Quantile)[2]][3:4]<-uqr$stEstimate[uqr$DV==unique(uqr$DV)[2] & uqr$Quantile==unique(uqr$Quantile)[2]][3:4]+.0015
#plot all
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
#save    
    save_plot("C:/Users/bda13/Desktop/Fig 2 - Parameter Estimates.png",p,
              base_height=10,
              base_aspect_ratio = 1.5)
        
}
    setwd(c('..',getwd()))
    
            
#Figure 4: Plot predicted estimates
    setwd("C:/Users/bda13/Desktop/Sociology/Papers in Progress/Work Hours/")
   {uqr1<-read_xlsx("uqrnetworth.xlsx")
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
        #determine base intercept
            base<-
            percentiledf$Estimate[percentiledf$Parameter=="Intercept"]+
            percentiledf$Estimate[percentiledf$Parameter=="Bachelors"]+
            percentiledf$Estimate[percentiledf$Parameter=="Married"]+
            (percentiledf$Estimate[percentiledf$Parameter=="age"]*40)+
            (percentiledf$Estimate[percentiledf$Parameter=="agesquared"]*(40^2))+
            (percentiledf$Estimate[percentiledf$Parameter=="Year1"]*27)
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
    
      tempdfl[[2]]$Wealth[tempdfl[[2]]$Quantile=="Quantile = .10" | tempdfl[[2]]$Quantile=="Quantile = .25"]<-NA
      tempdfl[[3]]$Wealth[tempdfl[[3]]$Quantile=="Quantile = .10" | tempdfl[[2]]$Quantile=="Quantile = .25" | tempdfl[[2]]$Quantile=="Quantile = .50" | tempdfl[[2]]$Quantile=="Quantile = .75"]<-NA      

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
      
    # #Figure X - predicted probabilities
    # #read predictions
    #     uqr<-read.csv("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/uqrregres.csv")
    #     uqr<-as.data.table(uqr)
    # #average predictions across imputations
    #     a<-uqr[,.(Estimate=mean(Estimate)),by=list(Parameter,Quantile)]
    # #predict wealth by quantile
    #     lpdf<-list()
    #     pdfl<-list()
    #     quantiles<-unique(a$Quantile)
    #     max=c(10,100,500,2000,8000,80000)
    #     for(i in 1:length(quantiles)){
    #         percentiledf<-a[a$Quantile==quantiles[i]]
    #         #determine base intercept
    #             base<-
    #             percentiledf$Estimate[percentiledf$Parameter=="Intercept"]+
    #             percentiledf$Estimate[percentiledf$Parameter=="Bachelors"]+
    #             percentiledf$Estimate[percentiledf$Parameter=="Married"]+
    #             (percentiledf$Estimate[percentiledf$Parameter=="Age"]*35)+
    #             (percentiledf$Estimate[percentiledf$Parameter=="AgeSquared"]*(35^2))+
    #             (percentiledf$Estimate[percentiledf$Parameter=="Year1"]*27)
    #         #create df of probability expectation given work hours (40-60)
    #             pdf<-data.frame(WorkHours=40:60)
    #             pdf$Wealth<-(base+
    #               (pdf$WorkHours-40)*percentiledf$Estimate[percentiledf$Parameter=="HeadWorkHoursOver40"]+
    #               27*(pdf$WorkHours-40)*percentiledf$Estimate[percentiledf$Parameter=="Year1HeadWorkHoursOver40"]
    #               )#^3
    #         #do again but for specialized households
    #             pdf2<-data.frame(WorkHours=40:60)
    #             pdf2$Wealth<-(base+
    #               (pdf2$WorkHours-40)*percentiledf$Estimate[percentiledf$Parameter=="HeadWorkHoursOver40"]+
    #               27*(pdf2$WorkHours-40)*percentiledf$Estimate[percentiledf$Parameter=="Year1HeadWorkHoursOver40"]+
    #               percentiledf$Estimate[percentiledf$Parameter=="HomemakerSpouse"]+
    #               (pdf2$WorkHours-40)*percentiledf$Estimate[percentiledf$Parameter=="HeadWorkHoursOver40HomemakerSpouse"]
    #               )#^3
    #         #rbind
    #             pdf$Specialization<-0
    #             pdf2$Specialization<-1
    #             pdf<-rbind(pdf,pdf2)
    #             pdf$Specialization<-factor(pdf$Specialization)
    #             pdf$Quantile<-quantiles[i]
    # #                pdf$Wealth<-pdf$Wealth/1000
    #             pdfl[[i]]<-pdf
    #     }
    #     tempdf<-do.call(rbind,pdfl)
    #     tempdf$Quantile<-paste("Quantile = ",tempdf$Quantile,sep="")
    #     #Plot
    #         p<-ggplot(tempdf, aes(x=WorkHours, y=Wealth,group=Specialization,color=Specialization))
    # 
    #         pdf(file="Figure 1 - Predicted Wealth by Quantile, Work Hours, and Specialization.pdf", height=7, width=12)
    #         {p+ geom_line(size=3)+
    #                 scale_colour_grey()+
    #                 scale_y_continuous(breaks = c(0:10*2))+
    #                 scale_x_continuous(breaks = c(40,50,60))+
    #                 theme_pander(base_size =14)+
    #                 theme(
    #                       axis.line = element_line(colour = "black"),
    #                       plot.title = element_text(size=20,hjust=0),
    #                       axis.title.x=element_text(size=18, margin = margin(t = 20, r = 0, b = 2, 0)),
    #                       axis.title.y=element_text(size=20, margin = margin(t = 0, r = 20, b = 0, 0)),
    #                       axis.text.y=element_text(size=14, margin = margin(t = 0, r = 0, b = 0, 0)),
    #                       axis.text.x= element_text(size=14, margin = margin(t = 0, r = 0, b = 0, 0)),
    #                       legend.direction = "horizontal", legend.position = "bottom", legend.text =element_text(size=16), legend.title =element_text(size=16),
    # #                          legend.position="none",
    #                       panel.spacing = unit(2.5, "lines"),
    #                       strip.text = element_text(size=16,face = "bold"),
    #                       plot.margin = unit(c(1, 1, 0.5, 0.5),"lines"))+
    #                 ggtitle("Figure 1 - Predicted Wealth by Quantile, Work Hours, and Specialization")+
    #                 labs(x="Work Hours", y="Predicted Net Worth (Logged)")+
    #                 facet_wrap(~Quantile,nrow=1,strip.position = "top")
    #         }
    #         dev.off()
    # 
    # 
    #     title <- ggdraw() + draw_label("Predicted Wealth by Quantile, Work Hours, and Specialization", fontface='bold')
    #     p<-plot_grid(ncol=3,lpdf[[1]],lpdf[[2]],lpdf[[3]],lpdf[[4]],lpdf[[5]],lpdf[[6]])
    #     p<-plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
    #     save_plot("predicted probabilities.pdf",plot=p,base_height=11)
      
      
      
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
          geom_blank()
      }
      #create other plots
      plotfun2<-function(x){
        mi<-mfloor(min(uqr$stCIlower[uqr$DV==unique(uqr$DV)[x]],na.rm=T),.5)
        ma<-mcieling(max(uqr$stCIupper[uqr$DV==unique(uqr$DV)[x]],na.rm=T),.5)
          ggplot(uqr[uqr$DV==unique(uqr$DV)[x],], aes(y=stEstimate,x=(Variable)))+
          geom_pointrange(aes(ymin=stCIlower,ymax=stCIupper),size=1.5,fill=ifelse(uqr$stSignificant[uqr$DV==unique(uqr$DV)[x]]==0,"white","darkgrey"),shape=21) + coord_flip()+
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
            plot.margin = margin(t=.5, r=.2, b=.5, l=.3, "cm"),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            strip.text = element_blank()
          )+
          facet_wrap(~Quantile,ncol=1)+
          ggtitle(unique(uqr$DV)[x])+
          #manually edit axis limits
            if(x==1){
              scale_y_continuous(limits = c(-.6,1.1),breaks =seq(from=-.5,to=1,by=.5))
            }else if(x==2){
              scale_y_continuous(limits = c(-.11,.21),breaks =seq(from=-.1,to=.25,by=.1))
            }else if(x==3){
              scale_y_continuous(limits = c(-.7,.7),breaks =seq(from=-.5,to=.5,by=.5))
            }else if(x==4){
              scale_y_continuous(limits = c(-1.05,.8),breaks =seq(from=-1,to=.5,by=.5))
            }else if(x==5){
              scale_y_continuous(limits = c(-.7,1.2),breaks =seq(from=-.5,to=1,by=.5))
            }
      }
      