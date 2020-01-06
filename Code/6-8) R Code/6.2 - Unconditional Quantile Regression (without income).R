
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
        setwd("/misc/utopia3/bda13/lanhome/SCF/")
        scf<-read.csv("/misc/utopia3/bda13/lanhome/SCF/rscf.csv")
        scf<-scf[scf$RWorkStatus=="Employed" & scf$Female==0,]
    #NEW kill unmarried folks
        scf<-scf[scf$MARRIED==1,]
        scf<-scf[!((scf$X4712>0) & scf$SWorkStatus=="Homemaker"),]
        
    }
    
    # wtd.quantile(scf$HomeEquity,scf$WGT,(1:20)/20)  
    
    
#1 - prep data
    {
        scf$INCOME<-sign(scf$INCOME)*log(abs(scf$INCOME))
        scf$INCOME[is.na(scf$INCOME)]<-0
      #a - uncapitalize and capitalize where it makes sense to
        scf$Networth<-scf$NETWORTH
        scf$Other<-scf$OTHER
        scf$Married<-scf$MARRIED
        scf$Age<-scf$AGE
        # scf$AgeSquared<-scf$agesquared
        # scf$ChildrenSquared<-scf$Childrensquared
        scf$AgeSquared<-scf$Age^2
        scf$ChildrenSquared<-scf$Children^2
        scf$Year1<-scf$year1
        
    #b - identify variables of interest
        vars<- c("Networth","HSGraduate","SomeCollege","Bachelors","AdvancedDegree","Black",
                 "Latino","Other","Married","Separated","Divorced","Widowed","LivingWithPartner",
                 "Age","AgeSquared","Children","ChildrenSquared","Year1","HeadWorkHoursOver40","HomemakerSpouse")
        vars[is.na(match(vars, names(scf)))]
        vars2<-c("NWPercentile","X_Imputation_","WGT","HomeEquity","StockValue")
        vars2[is.na(match(vars2, names(scf)))]
        
        
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
        tempdf$Networth<-sign(tempdf$Networth)*log(abs(tempdf$Networth))
        tempdf$Networth[is.na(tempdf$Networth)]<-0
        rifreg <- urq(formula=(Networth~HSGraduate+SomeCollege+Bachelors+
                                 AdvancedDegree+ Black+Latino+Other+Married+Separated+Divorced+
                                 Widowed+LivingWithPartner+Age+AgeSquared+Children+
                                 ChildrenSquared+Year1+HeadWorkHoursOver40+HomemakerSpouse+
                                 HeadWorkHoursOver40*HomemakerSpouse + Year1*HeadWorkHoursOver40),
                      SURVEYWGTS=tempdf$WGT,
                      data=tempdf,
                    tau = c(0.1,.25,.5,.75,.9,.99))

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

    #export to csv
        write.csv(uqrregres,("uqrregres1919noinc.csv"))
    }
        
        
#home equity    
    {
    lresults<-list()
    lregres<-list()
    for(i in 1:length(lscf)){
        tempdf<-as.data.frame(lscf[[i]])
        tempdf$HomeEquity<-sign(tempdf$HomeEquity)*log(abs(tempdf$HomeEquity))
        tempdf$HomeEquity[is.na(tempdf$HomeEquity)]<-0
        
        #set .25 quantile so that intercept is 0
            wtd.quantile(tempdf$HomeEquity,tempdf$WGT,(1:20)/20)
            rnk<-wtd.rank(tempdf$HomeEquity,tempdf$WGT)
            newquant<-rnk[round(tempdf$HomeEquity,4)==round(quant,4)][1]/max(rnk,na.rm=T)
            newquant<-rnk[round(tempdf$HomeEquity,4)==round(min(tempdf$HomeEquity[tempdf$HomeEquity>0]),4)][1]/max(rnk,na.rm=T)-.001
            
        # table(round(tempdf$HomeEquity,4)==round(quant,4))
        
        rifreg <- urq(formula=(HomeEquity~HSGraduate+SomeCollege+Bachelors+
                                 AdvancedDegree+ Black+Latino+Other+Married+Separated+Divorced+
                                 Widowed+LivingWithPartner+Age+AgeSquared+Children+
                                 ChildrenSquared+Year1+HeadWorkHoursOver40+HomemakerSpouse+
                                 HeadWorkHoursOver40*HomemakerSpouse + Year1*HeadWorkHoursOver40),
                      SURVEYWGTS=tempdf$WGT,
                      data=tempdf,
                    tau = c(0.1,newquant,.5,.75,.9,.99))

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
        
    #export to csv
        write.csv(uqrregres,("uqrregres1919noincHome.csv"))
    }
    
    i=1
#stocks
    {
    lresults<-list()
    lregres<-list()
    for(i in 1:length(lscf)){
        tempdf<-as.data.frame(lscf[[i]])
        tempdf$StockValue<-sign(tempdf$StockValue)*log(abs(tempdf$StockValue))
        tempdf$StockValue[is.na(tempdf$StockValue)]<-0
        #set .75 quantile so that intercept is 0
            wtd.quantile(tempdf$StockValue,tempdf$WGT,(1:20)/20)
            rnk<-wtd.rank(tempdf$StockValue,tempdf$WGT)
            newquant<-rnk[round(tempdf$StockValue,4)==round(quant,4)][1]/max(rnk)
            newquant<-rnk[round(tempdf$StockValue,4)==round(min(tempdf$StockValue[tempdf$StockValue>0]),4)][1]/max(rnk,na.rm=T)-.001

        rifreg <- urq(formula=(StockValue~HSGraduate+SomeCollege+Bachelors+
                                 AdvancedDegree+ Black+Latino+Other+Married+Separated+Divorced+
                                 Widowed+LivingWithPartner+Age+AgeSquared+Children+
                                 ChildrenSquared+Year1+HeadWorkHoursOver40+HomemakerSpouse+
                                 HeadWorkHoursOver40*HomemakerSpouse + Year1*HeadWorkHoursOver40),
                      SURVEYWGTS=tempdf$WGT,
                      data=tempdf,
                    tau = c(0.1,.25,.5,.72,.9,.99))
        rifreg$coefficients
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
        uqrregres$Quantile[uqrregres$Quantile==.72]<-.75
    #remove NA parameters; replace variables names with those that are compatible in SAS.
        uqrregres[1:10,]
        uqrregres<-uqrregres[!is.na(uqrregres$Estimate),]
        uqrregres$Parameter[uqrregres$Parameter=="HeadWorkHoursOver40HomemakerSpouse"]<-"HeadWorkHoursOver40Ho"
        uqrregres$Parameter[uqrregres$Parameter=="Year1HeadWorkHoursOver40"]<-"Year1HeadWorkHoursOve"
        View(uqrregres)
    #export to csv
        write.csv(uqrregres,("uqrregres1919noincStocks.csv"))
    }
    

# #INCOME
#     {
#     lresults<-list()
#     lregres<-list()
#     for(i in 1:length(lscf)){
#         tempdf<-as.data.frame(lscf[[i]])
#         # tempdf$INCOME<-sign(tempdf$INCOME)*log(abs(tempdf$INCOME))
#         # tempdf$INCOME[is.na(tempdf$INCOME)]<-0
#         rifreg <- urq(formula=(INCOME~HSGraduate+SomeCollege+Bachelors+
#                                  AdvancedDegree+ Black+Latino+Other+Married+Separated+Divorced+
#                                  Widowed+LivingWithPartner+Age+AgeSquared+Children+
#                                  ChildrenSquared+Year1+HeadWorkHoursOver40+HomemakerSpouse+
#                                  HeadWorkHoursOver40*HomemakerSpouse + Year1*HeadWorkHoursOver40),
#                       SURVEYWGTS=tempdf$WGT,
#                       data=tempdf,
#                     tau = c(0.1,.25,.5,.72,.9,.99))
#         rifreg$coefficients
#         summary=urqCI(data = rifreg,R = 500,graph = TRUE,seed = 1200,SURVEYWGTS=tempdf$WGT)
#         lresults[[i]]<-summary$results
#         lregres[[i]]<-data.frame(Estimate=summary$results[,1],Quantile=summary$results[,6],`_Imputation_`=i,Parameter=row.names(summary$results),DF=1,Std.Err=summary$results[,5])
#     }
#     #a<-round(summary$results,4)
#     #bind results
#         uqrregres<-do.call(rbind,lregres)
#     #rename columns for SAS
#         names(uqrregres)[3]<-"_Imputation_"
#         names(uqrregres)[6]<-"StdErr"
#     #rename variables for SAS (i.e. get rid of weird characters)
#         uqrregres$Parameter<-as.character(uqrregres$Parameter)
#         uqrregres$Parameter<-gsub("[():]","",uqrregres$Parameter)
#     #sort by quantile
#         uqrregres<-uqrregres[order(uqrregres$Quantile),]
#         uqrregres$Quantile[uqrregres$Quantile==.72]<-.75
#     #remove NA parameters; replace variables names with those that are compatible in SAS.
#         uqrregres[1:10,]
#         uqrregres<-uqrregres[!is.na(uqrregres$Estimate),]
#         uqrregres$Parameter[uqrregres$Parameter=="HeadWorkHoursOver40HomemakerSpouse"]<-"HeadWorkHoursOver40Ho"
#         uqrregres$Parameter[uqrregres$Parameter=="Year1HeadWorkHoursOver40"]<-"Year1HeadWorkHoursOve"
#         View(uqrregres)
#     #export to csv
#         write.csv(uqrregres,("uqrregres1919Income.csv"))
#     }
    
        # #2 - run some basic descriptives        
        #     #a - create weighted variance and sd functions  
        #         weighted.var <- function(x, w, na.rm = FALSE) {
        #           if (na.rm) {
        #             w <- w[i <- !is.na(x)]
        #             x <- x[i]
        #           }
        #           sum.w <- sum(w)
        #           sum.w2 <- sum(w^2)
        #           mean.w <- sum(x * w) / sum(w)
        #           (sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2, na.rm =na.rm)
        #         }
        #         weighted.SE<-function(x,w,na.rm=F){
        #                               sqrt(weighted.var(x,w,na.rm=F))/sqrt(length(x))    
        #         }
        #         weighted.SE(lscf$`1`$Networth,lscf$`1`$WGT,na.rm = T)
        #     #b - get means for each variable of interest in each replicate
        #         #i - prep empty lists
        #             means<-vector(mode="list",length=length(vars))
        #             variances<-vector(mode="list",length=length(vars))
        #         #ii - run on each variable in vars of interest
        #             for(i in 1:length(vars)) {
        #               #prep element of list as a vector
        #                   means[[i]]<-vector()
        #                   variances[[i]]<-vector()
        #               for(j in 1:length(lscf)){
        #                   #grab means and variance
        #                       means[[i]][j]<-weighted.mean(as.data.frame(lscf[[j]])[,c(vars[i])],lscf[[j]]$WGT)
        #                       variances[[i]][j]<-weighted.var(as.data.frame(lscf[[j]])[,c(vars[i])],lscf[[j]]$WGT)
        #               }
        #             }
        #     #c - pool results
        #         #Grab the mean mean; the mean variance, and the between imputation variance
        #             resultsdf<-as.data.frame(matrix(nrow=length(vars),ncol=3))
        #             names(resultsdf) <- c("Mean","Variance","BtwImp Var")
        #             for(i in 1:nrow(resultsdf)){
        #               temp<-pool.scalar(means[[i]], variances[[i]], n=nrow(lscf[[1]]), method = "rubin")
        #               resultsdf$Mean[i]<-temp$qbar
        #               resultsdf$Variance[i]<-temp$ubar
        #               resultsdf$`BtwImp Var`[i]<-temp$b
        #             }
        #             resultsdf<-round(resultsdf,3)
        # 
        # #3 - Standard quantile regression
        #     #run models
        #         qr<-list()
        #         for(i in 1:length(lscf)){
        #             tweight<-lscf[[i]]$WGT
        #             qr[[i]] <- rq(formula=(Networth~HSGraduate+SomeCollege+Bachelors+
        #               AdvancedDegree+ Black+Latino+Other+Married+Separated+Divorced+
        #               Widowed+LivingWithPartner+Age+AgeSquared+Children+
        #               ChildrenSquared+Year1+HeadWorkHoursOver40+HomemakerSpouse+
        #               HeadWorkHoursOver40*HomemakerSpouse + Year1*HeadWorkHoursOver40),
        # #              weights=tweight,
        #               data=lscf[[i]],
        #               tau = 0.9)
        #         }
        # 
        #     #I don't quite trust the mice pooling function, so I'm just going to export the results to SAS...
        #         temp<-lapply(qr,summary)
        #         temp<-lapply(temp,"[[","coefficients")
        #         temp<-c(lapply(temp,as.data.frame))
        #         regres<-data.frame(
        #             "_Imputation_"=rep(1:5,each=length(qr[[1]]$coefficients)),
        #             Quantile=0.9,
        #             Parameter=rep(names(qr[[1]]$coefficients),5),
        #             DF=1,
        #             Estimate=c(sapply(qr,"[[","coefficients")),
        #             "Std Err"=c(sapply(temp,"[[",2))
        #         )
        #         write.csv(regres,("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/regres.csv"))
        #         head(regres)
        #         