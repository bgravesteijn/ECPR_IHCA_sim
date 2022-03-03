model <- function(thresholds=c(2,3,4,9), pm.df=NULL, cohort.df=NULL, parallel=FALSE){
  ####################SETUP#################################
  theme_set(new = theme_bw())
  
  #baseline mortality rates per age
  CBS<-data.table(read.csv("data/CBS lifetable.csv", sep = ";"))
  CBS$Leeftijd<-as.numeric(substr(CBS$Leeftijd, start = 1, stop=2))
  
  
  RateToProb <- function(r, t) {
    # Function to convert rates to probabilities
    # Arg:
    # r: the annual input rate
    # t: the cycle length of your model in years
    #
    # Retrun:
    # probability for the time interval
    1 - exp(-r * t)
  }
  
  probtoodds<-function(x){
    # Function to convert probabilities to odds
    # Arg:
    # x: probability
    x/(1-x)
  }
  
  oddstoprob<-function(x){
    # Function to convert odds to probability
    # Arg:
    # x: odds
    x/(x+1)
  }
  LY<-data.frame(NE=rep(NA, nrow(pm.df)), 
                 EACCI_lo=NA,
                 NE_QALY=NA,
                 EACCI_lo_QALY=NA,
                 costNE=NA,
                 costEACCI_lo=NA)
  LY.l <- vector("list", length = length(thresholds))
  names(LY.l) <- paste("thr", thresholds, sep="")
  
  if(parallel){
    #For parallel computing
    library(doParallel)  
    n_cores <- detectCores() - 1
    cl <- makeCluster(n_cores)  
    registerDoParallel(cl)  
  }
  
  
  for(thr in thresholds){
    message(paste("Current threshold =", thr))
    res_iter_now <- foreach(p = 1:nrow(pm.df), .combine = rbind) %dopar% {
      message(paste(p/nrow(pm.df)*100, "% of current PSA done"))
      #time<-Sys.time()
      for(i in 1:nrow(cohort.df)){
        message(paste(i/nrow(cohort.df)*100, "% of current cohort done"))
        agecat<-ifelse(cohort.df[i,]$age<50,0,NA)
        agecat<-ifelse(cohort.df[i,]$age>=50&cohort.df[i,]$age<60,1,agecat)
        agecat<-ifelse(cohort.df[i,]$age>=60&cohort.df[i,]$age<70,2,agecat)
        agecat<-ifelse(cohort.df[i,]$age>=70&cohort.df[i,]$age<80,3,agecat)
        agecat<-ifelse(cohort.df[i,]$age>=80,4,agecat)
        age_CCI<-(agecat+cohort.df[i,]$cci)
        
        p.DNR                     <- ifelse(i<75, pm.df$p.DNR_75_min[p],
                                            ifelse(i>=75&i<85, oddstoprob(probtoodds(pm.df$p.DNR_75_min[p])*pm.df$or.DNR_75_84[p]), 
                                                   ifelse(i>=85, oddstoprob(probtoodds(pm.df$p.DNR_75_min)*pm.df$or.DNR_85_plus[p])))) #The probability of DNR status, depending on age
        p.Die_CPR_ind             <- oddstoprob(probtoodds(pm.df$p.Die_CPR[p])*exp(pm.df$b.Die_CPR_age_CCI[p]*age_CCI))
        p.Die_CPR_c.ind           <- oddstoprob(probtoodds(p.Die_CPR_ind)*pm.df$or.Die_c.ind_ECMO[p])
        p.Die_CPR_ind_20pl_ECMO   <- pm.df$p.Die_CPR[p]*pm.df$rr.ECMO[p]+pm.df$p.Compl[p]*pm.df$p.Die_Compl[p]
        p.Die_CPR_ind_20pl_noECMO <- pm.df$p.Die_CPR[p]
        
        #Markov model each combination of age CCI and sex, parameter input
        state.names<-c("Alive", "Dead", "Complication")
        n.s.<-length(state.names)
        cycle.length<-20
        year<-seq(0, (cycle.length-1), by=1)
        
        ar.trans<-array(NA, dim = c(n.s., n.s., cycle.length), dimnames = list(state.names, state.names, year))
        
        ### The probability to die per year
        ar.trans["Alive", "Dead", 1:cycle.length]<- seq(RateToProb(r=(ifelse(cohort.df[i,]$gender==1, 
                                                                             CBS[CBS$Leeftijd==ifelse(cohort.df[i,]$age>99, 
                                                                                                  99,              
                                                                                                  cohort.df[i,]$age),]$Man_kans,
                                                                             CBS[CBS$Leeftijd==ifelse(cohort.df[i,]$age>99, 
                                                                                                  99,
                                                                                                  cohort.df[i,]$age),]$Vrouw_kans)), 
                                                                   t=1),
                                                        RateToProb(r=(ifelse(cohort.df[i,]$gender==1,
                                                                             CBS[CBS$Leeftijd==ifelse(cohort.df[i,]$age +
                                                                                                    cycle.length>99, 
                                                                                                  99,
                                                                                                  (cohort.df[i,]$age+cycle.length)), 
                                                                                 ]$Man_kans,
                                                                             CBS[CBS$Leeftijd==ifelse(cohort.df[i,]$age+
                                                                                                    cycle.length>99, 
                                                                                                  99,
                                                                                                  (cohort.df[i,]$age+
                                                                                                     cycle.length)),
                                                                                 ]$Vrouw_kans)), 
                                                                   t=1),
                                                        length.out = cycle.length)
        ar.trans["Complication", "Dead", 1:cycle.length] <- ar.trans["Alive", "Dead", 1:cycle.length]
        
        
        # the probability to stay in the same state
        ar.trans["Alive", "Alive", 1:cycle.length] <- rep(1, cycle.length)-ar.trans["Alive", "Dead", 1:cycle.length]
        ar.trans["Complication", "Complication", 1:cycle.length] <- rep(1, cycle.length)-ar.trans["Complication", "Dead", 1:cycle.length]
        
        # the probability to receive complication
        ar.trans["Alive", "Complication", 1:cycle.length] <- rep(0, cycle.length)
        # the probability to not have a complication anymore
        ar.trans["Complication", "Alive", 1:cycle.length] <- rep(0, cycle.length)
        
        #absorbing state
        ar.trans["Dead", "Dead", 1:cycle.length]<-rep(1, (cycle.length))
        ar.trans["Dead", "Alive", 1:cycle.length]<-rep(0, (cycle.length))
        ar.trans["Dead", "Complication", 1:cycle.length]<-rep(0, (cycle.length))
        
        #REWARD MATRIXES
        rwd.LY<-matrix(c(1,1,1,0,0,0,1,1,1), nrow = 3, byrow = TRUE)
        if(cohort.df[i,]$gender==1){
          rwd.QALY<-matrix(c(pm.df$utility.men[p],
                             pm.df$utility.men[p],
                             pm.df$utility.men[p],
                             0,0,0,
                             pm.df$utility.men[p],
                             pm.df$utility.men[p],
                             pm.df$utility.men[p]), nrow = 3, byrow = TRUE)
          
        }else{
          rwd.QALY<-matrix(c(pm.df$utility.women[p],
                             pm.df$utility.women[p],
                             pm.df$utility.women[p],
                             0,0,0,
                             pm.df$utility.women[p],
                             pm.df$utility.women[p],
                             pm.df$utility.women[p]), nrow = 3, byrow = TRUE)
          
        }
        
        
        
        
        #Decision tree outcome No ECMO
        p.Die_NE<-p.DNR*pm.df$p.Die_DNR[p]+
          (1-p.DNR)*((1-pm.df$p.ECMO_c.ind[p])*(pm.df$p.rosc_20min[p]*p.Die_CPR_ind+
                                                  (1-pm.df$p.rosc_20min[p])*p.Die_CPR_ind_20pl_noECMO)+
                       pm.df$p.ECMO_c.ind[p]*p.Die_CPR_c.ind)
        p.compl_NE <- 0
        s0<-c((1-p.Die_NE-p.compl_NE),p.Die_NE, p.compl_NE)
        l.markov.int<-dampack::CalculateMarkovTrace(M = ar.trans, p0 = s0, n.cycles = cycle.length)
        cohort.df[i,]$LYNE<-dampack::ExpectedValue(trans = l.markov.int$trans, rwd =rwd.LY , half = TRUE)
        cohort.df[i,]$QALYNE<-dampack::ExpectedValue(trans = l.markov.int$trans, rwd =rwd.QALY , half = TRUE)
        cohort.df[i,]$costNE       <- 0
        
        #Decision tree outcome ECMO under threshold
        p.Die_EACCI_lo<-p.DNR*pm.df$p.Die_DNR[p]+
          (1-p.DNR)*(pm.df$p.ECMO_c.ind[p]*p.Die_CPR_c.ind+
                       (1-pm.df$p.ECMO_c.ind[p])*(pm.df$p.rosc_20min[p]*p.Die_CPR_ind+
                                                    (1-pm.df$p.rosc_20min[p])*ifelse(age_CCI<thr, 
                                                                                     p.Die_CPR_ind_20pl_ECMO,
                                                                                     p.Die_CPR_ind_20pl_noECMO)))
        p.compl_EACCI_LO <- (1-p.DNR)*((1-pm.df$p.ECMO_c.ind[p])*((1-pm.df$p.rosc_20min[p])*ifelse(age_CCI<thr, 
                                                                                                   pm.df$p.Compl[p],0)))
        s0<-c((1-p.Die_EACCI_lo-p.compl_EACCI_LO),p.Die_EACCI_lo, p.compl_EACCI_LO )
        l.markov.int<-dampack::CalculateMarkovTrace(M = ar.trans, p0 = s0, n.cycles = cycle.length)
        cohort.df[i,]$LYEACCI_lo<-dampack::ExpectedValue(trans = l.markov.int$trans, rwd =rwd.LY , half = TRUE)
        cohort.df[i,]$QALYEACCI_lo<-dampack::ExpectedValue(trans = l.markov.int$trans, rwd =rwd.QALY , half = TRUE)
        cohort.df[i,]$costEACCI_lo <- (1-p.DNR)*((1-pm.df$p.ECMO_c.ind[p])*
                                                   ((1-pm.df$p.rosc_20min[p])*ifelse(age_CCI<thr, 
                                                                                     pm.df[p,]$c.ECMO,0)))
        
      }
      
      res_iteration <- c(pm.df[p,],
                         p_die_NE       = p.Die_NE,
                         p_die_EACCI_lo = p.Die_EACCI_lo,
                         NE             = mean(cohort.df$LYNE),
                         EACCI_lo       = mean(cohort.df$LYEACCI_lo),
                         costNE         = mean(cohort.df$costNE),
                         costEACCI_lo   = mean(cohort.df$costEACCI_lo),
                         NE_QALY        = mean(cohort.df$QALYNE),
                         EACCI_lo_QALY  = mean(cohort.df$QALYEACCI_lo))
      return(res_iteration)
      
      #Sys.time()-time    
    }
    
    LY.l[[paste("thr", thr, sep="")]] <- data.frame(res_iter_now)
 
  }
  return(LY.l)
}

