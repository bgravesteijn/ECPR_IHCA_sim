make_ce_table <- function(ce){
  ce <- data.table(ce)
  ##ICERs###
  cqaly    <- ce[,median(cost/QALY), by=strategy]
  cqaly$lo <- ce[,quantile(cost/QALY, probs=0.025), by=strategy][,2]
  cqaly$hi <- ce[,quantile(cost/QALY, probs=0.975), by=strategy][,2]
  
  calc.icer <- function(str1=NULL, str2=NULL, measure=NULL){
    if(measure==1){
      res <- median((ce$cost[ce$strategy==str2]-ce$cost[ce$strategy==str1])/(ce$QALY[ce$strategy==str2]-ce$QALY[ce$strategy==str1])) 
    }else{
      if(measure==2){
        res <- quantile((ce$cost[ce$strategy==str2]-ce$cost[ce$strategy==str1])/(ce$QALY[ce$strategy==str2]-ce$QALY[ce$strategy==str1]), probs=0.025)
      }else{
        if(measure==3){
          res <- quantile((ce$cost[ce$strategy==str2]-ce$cost[ce$strategy==str1])/(ce$QALY[ce$strategy==str2]-ce$QALY[ce$strategy==str1]), probs=0.975)
        }
      }
    }
    return(res)
  }
  
  ICER <- round(data.frame(ICER=c(calc.icer(str1 = "NE", str2 = "EALL", measure = 1),
                                  NA,
                                  calc.icer(str1 = "NE", str2 = "thr2", measure = 1),
                                  calc.icer(str1 = "NE", str2 = "thr3", measure = 1),
                                  calc.icer(str1 = "NE", str2 = "thr4", measure = 1)),
                           lo=c(calc.icer(str1 = "NE", str2 = "EALL", measure = 2),
                                NA,
                                calc.icer(str1 = "NE", str2 = "thr2", measure = 2),
                                calc.icer(str1 = "NE", str2 = "thr3", measure = 2),
                                calc.icer(str1 = "NE", str2 = "thr4", measure = 2)),
                           hi=c(calc.icer(str1 = "NE", str2 = "EALL", measure = 3),
                                NA,
                                calc.icer(str1 = "NE", str2 = "thr2", measure = 3),
                                calc.icer(str1 = "NE", str2 = "thr3", measure = 3),
                                calc.icer(str1 = "NE", str2 = "thr4", measure = 3))),1)
  icer <- c(paste(ICER$ICER, " (", ICER$lo, " - ", ICER$hi, ")", sep=""))
  
  cost     <- paste(round(ce[,median(cost), by=strategy]$V1,1)," (",round(ce[,quantile(cost, probs=0.025), by=strategy]$V1,1)," - ",round(ce[,quantile(cost, probs=0.975), by=strategy]$V1,1), ")", sep="")
  
  QALY     <- paste(round(ce[,median(QALY), by=strategy]$V1,2)," (",round(ce[,quantile(QALY, probs=0.025), by=strategy]$V1,2)," - ",round(ce[,quantile(QALY, probs=0.975), by=strategy]$V1,2), ")", sep="")
  
  costqaly <- paste(round(ce[,median(cost/QALY), by=strategy]$V1,1)," (",round(ce[,quantile(cost/QALY, probs=0.025), by=strategy]$V1,1)," - ",round(ce[,quantile(cost/QALY, probs=0.975), by=strategy]$V1,1), ")", sep="")
  
  result <- cbind(cost, QALY, costqaly, icer)
  result <- result[c(2,3,4,5,1), ]
  rownames(result) <- c("NE", "thr2", "thr3", "thr4", "EALL")
}

