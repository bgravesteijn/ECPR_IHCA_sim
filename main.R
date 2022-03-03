library(data.table)
library(ggplot2)
library(dampack) #devtools::install_github("feralaes/dampack")
library(hesim)
library(scales)
library(knitr)

## Load cohort
load("data/cohort.rdata")

## Load parameters
load("data/parameters.rdata")
load("data/prm.rdata")

## Load the model function
source("functions/model.R")

## Run the model
set.seed(123)
res <- model(thresholds = c(2,3,4,9), #Standard, if you change this, the code below needs to change as well
             pm.df      = prm #[sample(1:nrow(prm),100,replace=FALSE),], #You can sample some parameter sets randomly to save computing time
             ,
             cohort.df  = cohort #[sample(1:nrow(cohort),100,replace=FALSE),] #You can sample some patients randomly to save computing time
             , 
             parallel   = TRUE) #To use multiple cores in your computer for faster computing.

## Summarize results
ce.res <- data.table(data.frame(strategy=c(rep("NE", nrow(res$thr2)),
                                           rep("thr2", nrow(res$thr2)),
                                           rep("thr3", nrow(res$thr2)),
                                           rep("thr4", nrow(res$thr2)),
                                           rep("EALL", nrow(res$thr2))),
                            QALY=c(unlist(res$thr2$NE_QALY), 
                                   unlist(res$thr2$EACCI_lo_QALY),
                                   unlist(res$thr3$EACCI_lo_QALY),
                                   unlist(res$thr4$EACCI_lo_QALY), 
                                   unlist(res$thr9$EACCI_lo_QALY)),
                            cost=c(unlist(res$thr2$costNE), 
                                   unlist(res$thr2$costEACCI_lo), 
                                   unlist(res$thr3$costEACCI_lo), 
                                   unlist(res$thr4$costEACCI_lo), 
                                   unlist(res$thr9$costEACCI_lo)),
                            sim=rep(1:nrow(res$thr2), 5),
                            grp="IHCA"))

source("functions/ce_table.R")
make_ce_table(ce = ce.res)

source("functions/ceac_plot.R")
ce_plot(ce = ce.res, 
        plot = 1)

ce_plot(ce = ce.res, 
        plot = 2, 
        vline=c(9500,12500))

