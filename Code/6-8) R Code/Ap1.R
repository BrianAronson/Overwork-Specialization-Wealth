#sensitivity test for demographics on wealth in the lowest quantile.

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

    
#0 - Prep
    {
    #a - load libraries
        library(data.table)
        library(quantreg)
        library(foreign)
        library(gtools)
        library(Hmisc)

    #b - load data
        #scf<-read.csv("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/rscf.csv")
        scf<-read.csv("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/rscf.csv")
        scf<-scf[scf$RWorkStatus=="Employed" & scf$Female==0,]
    #NEW kill unmarried folks
        scf<-scf[scf$MARRIED==1,]
        scf<-scf[!((scf$X4712>0) & scf$SWorkStatus=="Homemaker"),]
        
    }
#1 - prep data
    {
        # scf$INCOME<-sign(scf$INCOME)*log(abs(scf$INCOME))
        # scf$INCOME[is.na(scf$INCOME)]<-0
      #a - uncapitalize and capitalize where it makes sense to
        scf$Networth<-scf$NETWORTH
        scf$Other<-scf$OTHER
        scf$Married<-scf$MARRIED
        scf$Age<-scf$AGE
        scf$AgeSquared<-scf$agesquared
        scf$ChildrenSquared<-scf$Childrensquared
        #alternatively
            scf$AgeSquared<-scf$Age^2
            scf$ChildrenSquared<-scf$Children^2
        scf$Year1<-scf$year1
    #b - identify variables of interest
        vars<- c("Networth","HSGraduate","SomeCollege","Bachelors","AdvancedDegree","Black",
                 "Latino","Other","Married","Separated","Divorced","Widowed","LivingWithPartner",
                 "Age","AgeSquared","Children","ChildrenSquared","Year1","HeadWorkHoursOver40","HomemakerSpouse")
        vars[which(is.na(match(vars, names(scf))))]
        
        scf<-scf[,c(vars,"NWPercentile","X_Imputation_","WGT","HomeEquity","StockValue")]
    #c - convert to data table for speed
        scf<-data.table(scf)
      # #temporary supplemental - make low wealth households basically identical on key demographics.
      #   scf$hssc<-ifelse(scf$HSGraduate==1 | scf$SomeCollege==1,1,0)
      #   scf<-scf[scf$NWPercentile>1 | scf$hssc==1,]
      #   table(scf$NWPercentile)
    #d - separate data into separate lists based on replicates
        lscf<-split(scf,scf$X_Imputation_)
    }
    
#2 - Create unconditional quantile regression functions
    {
        urq<-function (formula, data, tau = NULL, kernel = NULL, cre = NULL,
                id = NULL, SURVEYWGTS){
          tempwgts<-SURVEYWGTS
          data$tempwgts<-SURVEYWGTS
          if (is.null(kernel))
            kernel <- "gaussian"
          if (is.null(tau))
            tau <- 1:9/10
          indicator <- function(condition) ifelse(condition, 1, 0)
          c1 = NULL
          idx.dep = which(colnames(data) == all.vars(formula)[1])
          q <- wtd.quantile(x = data[, idx.dep], probs = tau,weights = tempwgts)
          f <- density(data[, idx.dep], kernel = kernel,weights = tempwgts/sum(tempwgts))
          fq <- approx(f$x, f$y, q)$y
          for (i in 1:length(tau)) {
            RIF = q[i] + ((tau[i] - indicator(data[, idx.dep] < q[i]))/fq[i])
            data2 = data
            data2[, idx.dep] = RIF
            if (!is.null(cre)) {
              if (is.null(id)) {
                print("id variable is missing")
              }
              vars = all.vars(cre)
              new = rep(NA, length(vars))
              old = all.vars(formula)[2:length(all.vars(formula))]
              dep = all.vars(formula)[1]
              for (i in 1:length(vars)) {
                data2[, dim(data2)[2] + 1] <- ave(data[, which(colnames(data2) ==
                                                                 vars[i])], group = data2[, which(colnames(data2) ==
                                                                                                    id)])
                new[i] = colnames(data2)[dim(data2)[2]] <- paste(vars[i],
                                                                 sep = "", "_M")
              }
              formula_cre = paste(dep, "~", paste(c(old, new),
                                                  collapse = " + "))
              lm = lm(formula_cre, data2, weights = tempwgts)
              c = coef(lm)
              c1 = rbind(c1, c)
              data2[, idx.dep] = data[, idx.dep]
            }
            else {
              lm = lm(formula, data2, weights = tempwgts)
              c = coef(lm)
              c1 = rbind(c1, c)
              data2 = NULL
            }
          }
          RIF = t(c1)
          colnames(RIF) <- paste("tau=", tau)
          if (!is.null(cre)) {
            formula = formula_cre
          }
          if (!is.null(cre)) {
            data = data2
          }
          fit = list(coefficients = RIF, tau = tau, formula = formula,
                     id = id, data = data)
          fit$call <- sys.call()
          class(fit) <- "urq"
          return(fit)
        }

        urqb<-function (data = data, tau = tau, formula = formula, kernel = NULL,
                cluster = cluster,SURVEYWGTS=NULL) {
          if(is.null(SURVEYWGTS)){
            SURVEYWGTS<-as.null()
          }else{
            data$SURVEYWGTS<-SURVEYWGTS
          }
          if (is.null(kernel))
            kernel <- "gaussian"
          indicator <- function(condition) ifelse(condition, 1, 0)
          c1 = NULL
          for (i in 1:length(tau)) {
            formula = as.formula(formula)
            as.data.frame(data)
            idx.dep = which(colnames(data) == all.vars(formula)[1])
            data2 = data
            q <- wtd.quantile(data2[, idx.dep], tau, weights = data$wts,
                              normwt = TRUE)
            f <- density(data[, idx.dep], kernel = kernel, weights = data$wts)
            fq <- approx(f$x, f$y, q)$y
            RIF = q[i] + ((tau[i] - indicator(data2[, idx.dep] <
                                                q[i]))/fq[i])
            data2[, idx.dep] = RIF
            lm = lm(formula, data2, weights = SURVEYWGTS) #added option for weighting
            c = coef(lm)
            c1 = rbind(c1, c)
            data2 = NULL
          }
          RIF = t(c1)
          colnames(RIF) <- paste("tau=", tau)
          fit = list(coefficients = RIF, tau = tau, formula = formula)
          class(fit) <- "urq"
          return(fit)
        }

        urqCI<-function (data, R = 20, seed = NULL, colour = NULL, confidence = NULL,
                graph = TRUE, cluster = NULL, BC = FALSE, SURVEYWGTS=NULL){
          if(is.null(SURVEYWGTS)){
            SURVEYWGTS<-as.null()
          }else{
            data$SURVEYWGTS<-SURVEYWGTS
          }
          if (is.null(seed)) {
            seed <- runif(1, 0, .Machine$integer.max)
          }
          set.seed(seed)
          boot = matrix(ncol = dim(data$coefficients)[1] * length(data$tau),
                        nrow = R)
          cat("Bootstrapping\n")
          tail(data$data$wts)
          for (i in 1:R) {
            data$data$wts = as.numeric(rdirichlet(1, rep(1, dim(data$data)[1])))
            boot[i, ] = as.numeric(urqb(data = data$data, tau = data$tau,
                                        formula = data$formula, kernel = NULL,SURVEYWGTS=SURVEYWGTS)$coefficients) #added option for weighting
            if (!i%%50)
              cat(".")
          }
          tail(boot[1,])
          if (R <= 50)
            cat("Number of replications is small, please consider increasing it.")
          names.data = rownames(data$coefficients)
          if (is.null(confidence))
            confidence <- 0.95
          alpha = 1 - confidence
          se = apply(boot, 2, sd, na.rm = TRUE)
          colnames(boot) = paste(rep(rownames(data$coefficients), times = dim(data$coefficients)[2]))
          if (is.null(colour))
            colour <- "lightblue"
          mar <- c(2, 2, 2, 1.6)
          par(mfrow = n2mfrow(dim(data$coefficients)[1] - 1), mar = mar)
          par(oma = c(0, 0, 3, 0))
          se = apply(boot, 2, sd, na.rm = TRUE)
          data.vector = as.numeric(data$coefficients)
          tP <- 2 * pt(-abs(data.vector/se), dim(boot)[1] - 1)
          lower <- data.vector + qt(alpha/2, dim(boot)[1] - 1) * se
          upper <- data.vector + qt(1 - alpha/2, dim(boot)[1] - 1) *
            se
          recap = cbind(data.vector, lower, upper, tP, se, rep(data$tau,
                                                              each = dim(data$coefficients)[1]))
          colnames(recap) = c("Coef", "t Lower", "t Upper", "P>|t|",
                              "Std.Err.", "tau")
          z0 <- sapply(1:dim(boot)[2], function(x) qnorm(mean(boot[,
                                                                   x] <= data.vector[x], na.rm = TRUE)))
          a = rep(0, length(z0))
          q.lb = sapply(1:dim(boot)[2], function(x) pnorm(z0[x] + (z0[x] +
                                                                     qnorm(alpha/2))/(1 - a[x] * (z0[x] + qnorm(alpha/2)))))
          q.ub = sapply(1:dim(boot)[2], function(x) pnorm(z0[x] + (z0[x] +
                                                                     qnorm(1 - alpha/2))/(1 - a[x] * (z0[x] + qnorm(1 - alpha/2)))))
          type = 7
          BClower <- sapply(1:dim(boot)[2], function(x) quantile(boot[,
                                                                      x], q.lb[x], na.rm = TRUE, type = type))
          BCupper <- sapply(1:dim(boot)[2], function(x) quantile(boot[,
                                                                      x], q.ub[x], na.rm = TRUE, type = type))
          recap = cbind(recap, BClower, BCupper, apply(boot, 2, median,
                                                       na.rm = TRUE))
          cairo_pdf(height = 7, width = 10, onefile = T)
          if (isTRUE(graph) && isTRUE(length(data$tau) > 1)) {
            for (i in c(2:dim(data$coefficients)[1])) {
              cond = names.data[i]
              cfi <- recap[which(rownames(recap) == cond), ]
              plot(rep(data$tau, 2), c(cfi[, 2], cfi[, 3]), xaxt = "n",
                   type = "n", main = cond, xlab = "", ylab = "",
                   ylim = c(min = min(cfi, na.rm = TRUE), max = max(cfi,
                                                                    na.rm = TRUE)))
              axis(1, at = data$tau, labels = data$tau, cex.axis = 0.8,
                   las = 2)
              polygon(c(data$tau, rev(data$tau)), c(cfi[, 2], rev(cfi[,
                                                                    3])), col = colour, border = NA)
              points(data$tau, cfi[, 1], type = "b", lty = "longdash",
                     cex = 0.5, pch = 20, col = "blue")
              abline(h = 0)
              if (isTRUE(BC)) {
                points(data$tau, cfi[, 7], type = "b", lty = "longdash",
                       cex = 0.5, pch = 20, col = "black")
                points(data$tau, cfi[, 8], type = "b", lty = "longdash",
                       cex = 0.5, pch = 20, col = "black")
                points(data$tau, cfi[, 9], type = "b", lty = "longdash",
                       cex = 0.5, pch = 20, col = "orange")
              }
            }
            dev.off()
          }
          fit = list(results = recap, bootstrap = boot)
          class(fit) <- "urqCI"
          return(fit)
        }
    }
#3 - Run UQR functions
    {
  #wealth
    lresults<-list()
    lregres<-list()
    for(i in 1:length(lscf)){
        tempdf<-as.data.frame(lscf[[i]])
        # tempdf<-tempdf[tempdf$NWPercentile==10,]
        tempdf$Networth<-sign(tempdf$Networth)*log(abs(tempdf$Networth))
        tempdf$Networth[is.na(tempdf$Networth)]<-0
        rifreg <- urq(formula=(Networth~HSGraduate+SomeCollege+Bachelors+
                                 AdvancedDegree+ Black+Latino+Other+Married+Separated+Divorced+
                                 Widowed+LivingWithPartner+Age+AgeSquared+Children+
                                 ChildrenSquared+Year1+HeadWorkHoursOver40+HomemakerSpouse+
                                 HeadWorkHoursOver40*HomemakerSpouse + Year1*HeadWorkHoursOver40),
                      SURVEYWGTS=tempdf$WGT,
                      data=tempdf,
                    tau = c(0.1))

        summary=urqCI(data = rifreg,R = 500,graph = TRUE,seed = 1200,SURVEYWGTS=tempdf$WGT)
        lresults[[i]]<-summary$results
        lregres[[i]]<-data.frame(Estimate=summary$results[,1],Quantile=summary$results[,6],`_Imputation_`=i,Parameter=row.names(summary$results),DF=1,Std.Err=summary$results[,5])
    }
    #a<-round(summary$results,4)
    #bind results
        uqrregres<-do.call(rbind,lregres)
    #rename columns for SAS
        names(uqrregres)[3]<-"_Imputation_"
        names(uqrregres)[6]<-"StdErr"
    #rename variables for SAS (i.e. get rid of weird characters)
        uqrregres$Parameter<-as.character(uqrregres$Parameter)
        uqrregres$Parameter<-gsub("[():]","",uqrregres$Parameter)
    #sort by quantile
        uqrregres<-uqrregres[order(uqrregres$Quantile),]
    #remove NA parameters; replace variables names with those that are compatible in SAS.
        uqrregres[1:10,]
        uqrregres<-uqrregres[!is.na(uqrregres$Estimate),]
        uqrregres$Parameter[uqrregres$Parameter=="HeadWorkHoursOver40HomemakerSpouse"]<-"HeadWorkHoursOver40Ho"
        uqrregres$Parameter[uqrregres$Parameter=="Year1HeadWorkHoursOver40"]<-"Year1HeadWorkHoursOve"
        uqrregres1<-uqrregres
    #export to csv
        write.csv(uqrregres,("uqrregresNW1-1720.csv"))
    }
    

    #wealth
      lresults<-list()
      lregres<-list()
      for(i in 1:length(lscf)){
        tempdf<-as.data.frame(lscf[[i]])
        
        #adjust quantile to be the same cutoff as earlier
          quant<-wtd.quantile(tempdf$Networth,tempdf$wgt,.1)
        
        
        #further subset
            tempdf$hssc<-ifelse(tempdf$HSGraduate==1 | tempdf$SomeCollege==1,1,0)
            # tempdf<-tempdf[tempdf$NWPercentile==10,]
            tempdf<-tempdf[!(tempdf$hssc==0 & tempdf$NWPercentile==10),]
            newquant<-round(rank(tempdf$Networth)[round(tempdf$Networth/100)==round(quant/100)][1]/length(tempdf$Networth),3)
            
            newquant<-round(wtd.rank(tempdf$Networth,tempdf$WGT)[round(tempdf$Networth/100)==round(quant/100)][1]/max(wtd.rank(tempdf$Networth,tempdf$WGT)),3)
            
          
          tempdf$Networth<-sign(tempdf$Networth)*log(abs(tempdf$Networth))
          tempdf$Networth[is.na(tempdf$Networth)]<-0
          rifreg <- urq(formula=(Networth~HSGraduate+SomeCollege+Bachelors+
                                   AdvancedDegree+ Black+Latino+Other+Married+Separated+Divorced+
                                   Widowed+LivingWithPartner+Age+AgeSquared+Children+
                                   ChildrenSquared+Year1+HeadWorkHoursOver40+HomemakerSpouse+
                                   HeadWorkHoursOver40*HomemakerSpouse + Year1*HeadWorkHoursOver40),
                        SURVEYWGTS=tempdf$WGT,
                        data=tempdf,
                      tau = c(newquant))

          summary=urqCI(data = rifreg,R = 500,graph = TRUE,seed = 1200,SURVEYWGTS=tempdf$WGT)
          lresults[[i]]<-summary$results
          lregres[[i]]<-data.frame(Estimate=summary$results[,1],Quantile=summary$results[,6],`_Imputation_`=i,Parameter=row.names(summary$results),DF=1,Std.Err=summary$results[,5])
      }
      #a<-round(summary$results,4)
      #bind results
          uqrregres<-do.call(rbind,lregres)
      #rename columns for SAS
          names(uqrregres)[3]<-"_Imputation_"
          names(uqrregres)[6]<-"StdErr"
      #rename variables for SAS (i.e. get rid of weird characters)
          uqrregres$Parameter<-as.character(uqrregres$Parameter)
          uqrregres$Parameter<-gsub("[():]","",uqrregres$Parameter)
      #sort by quantile
          uqrregres<-uqrregres[order(uqrregres$Quantile),]
      #remove NA parameters; replace variables names with those that are compatible in SAS.
          uqrregres[1:10,]
          uqrregres<-uqrregres[!is.na(uqrregres$Estimate),]
          uqrregres$Parameter[uqrregres$Parameter=="HeadWorkHoursOver40HomemakerSpouse"]<-"HeadWorkHoursOver40Ho"
          uqrregres$Parameter[uqrregres$Parameter=="Year1HeadWorkHoursOver40"]<-"Year1HeadWorkHoursOve"
          uqrregres2<-uqrregres
      #export to csv
          write.csv(uqrregres,("uqrregresNW2-1720.csv"))
          
#RUN SAS STEPS    
    #3 - Appendix 3 - UQR Net worth comparisons.sas
          curdir<-getwd()
          "C:/Users/bda13/Desktop/Sociology/Papers in Progress/Work Hours/Code/3 - Appendix 3 - UQR Net worth comparisons.sas"
          setwd("C:/Program Files/SASHome/SASFoundation/9.4")
          return.code <- shell("sas.exe -SYSIN C:/Users/bda13/Desktop/Sociology/Papers in Progress/Work Hours/Code/3 - Appendix 3 - UQR Net worth comparisons.sas")
          
                  
#2 - Figure 2 - Parameter estimates
  #a) prep data
      scf<-read.csv("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/rscf.csv")
      scf<-scf[scf$RWorkStatus=="Employed" & scf$Female==0 & scf$MARRIED==1,]
      scf<-scf[!((scf$X4712>0) & scf$SWorkStatus=="Homemaker"),]
      setwd("C:/Users/bda13/Desktop/Sociology/Papers in Progress/Work Hours/Archived tables")
      {
      #read and bind results
         {uqr1<-read_xlsx("uqrnetworthap1.xlsx")
          uqr2<-read_xlsx("uqrnetworthap2.xlsx")
          uqr1<-uqr1[uqr1$Parm %in% c("HeadWorkHoursOver40","HomemakerSpouse","HeadWorkHoursOver40Ho","Year1HeadWorkHoursOve"),]
          uqr2<-uqr2[uqr2$Parm %in% c("HeadWorkHoursOver40","HomemakerSpouse","HeadWorkHoursOver40Ho","Year1HeadWorkHoursOve"),]
          uqr1$DV<-"Net Worth (Full sample)"
          uqr2$DV<-"Net Worth (Balanced sample)"
          uqr<-rbind(uqr1,uqr2)
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
        #start iteration for each variable, quantile, and percentile
            for(i in 1:nrow(uqr)){
              x<-as.character(uqr$Variable[i])
              per<-"NWPercentile"
              p<-10
              uqr$sdEstimate[i]<-newfun(x=x,per=per,p=p)
            }
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
                text=element_text(size=12),
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
              # facet_wrap(~Quantile,ncol=1,strip.position="left")+
              ggtitle("")+
              scale_x_discrete(labels= labs)+
              geom_blank()
          }
    #ii) create remainder of plots
          x=1
        plotfun2<-function(x){
          mi<-mfloor(min(uqr$stCIlower[uqr$DV==unique(uqr$DV)[x]],na.rm=T),.5)
          ma<-mcieling(max(uqr$stCIupper[uqr$DV==unique(uqr$DV)[x]],na.rm=T),.5)
            ggplot(uqr[uqr$DV==unique(uqr$DV)[x],], aes(y=stEstimate,x=(Variable)))+
            coord_flip()+
            # geom_pointrange(aes(ymin=stCIlower,ymax=stCIupper),size=1.5,fill=ifelse(uqr$stSignificant[uqr$DV==unique(uqr$DV)[x]]==0,"white","darkgrey"),color=ifelse(uqr$stSignificant[uqr$DV==unique(uqr$DV)[x]]==0,"grey70","black"),shape=73, fatten = 6)+
            geom_pointrange(aes(ymin=stCIlower,ymax=stCIupper),size=.6,fill=ifelse(uqr$stSignificant[uqr$DV==unique(uqr$DV)[x]]==0,"white","darkgrey"),color=ifelse(uqr$stSignificant[uqr$DV==unique(uqr$DV)[x]]==0,"grey70","black")) +
            geom_hline(yintercept = 0)+
            theme_bw() +
            theme(axis.ticks.y= element_blank(),
              # panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"),
              text=element_text(size=10),
              title =element_text(size=8, face='bold'),
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
            }
        }
        graphics.off()
        
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
    
        # title <- ggdraw() + draw_label("Figure 2 - Model Estimates for All Dependent Variables", fontface='bold',size=24)
        p<-plot_grid(m0,m1,m2,ncol=3,rel_widths=c(.8, 1,1)) #put all figures together; make space for y title
        # p<-plot_grid(title, p,nrow=2,rel_heights=c(0.06, 1)) #put title above graph
        # p<-p+draw_label("Quantile", 0.04, .908,fontface = "bold",size=18) #put quantile above quantile label
        # xtitle <- ggdraw() + draw_label(" ", fontface='bold',size=18)
        # p<-plot_grid(p,xtitle,nrow=2,rel_heights=c(1,0.06)) #make room for x title
        # p<-p+ draw_label("Parameter Estimates", 0.595, 0.05,size=18)
    #viii) save results
        save_plot("C:/Users/bda13/Desktop/App - balanced low wealth.png",p,
        base_height=3,
        base_aspect_ratio = 2)
  }