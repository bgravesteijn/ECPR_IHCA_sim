library(data.table)
library(ggplot2)
library(dampack)
library(hesim)
library(scales)
library(knitr)

## Load cohort
load("data/cohort.rdata")

## Load parameters
load("data/prm.rdata")

## Load the model function
source("functions/model.R")

## Run the model
res <- model(pm.df = prm, cohort.df = cohort)

ce.res <- data.table(data.frame(strategy=c(rep("NE", nrow(res$thr2)),
                                       rep("thr2", nrow(res$thr2)), 
                                       rep("thr3", nrow(res$thr2)), 
                                       rep("thr4", nrow(res$thr2)),
                                       rep("EALL", nrow(res$thr2))),
                            QALY=c(res$thr2$NE_QALY, res$thr2$EACCI_lo_QALY,res$thr3$EACCI_lo_QALY,
                                   res$thr4$EACCI_lo_QALY, res$thr9$EACCI_lo_QALY),
                            cost=c(res$thr2$costNE, res$thr2$costEACCI_lo, res$thr3$costEACCI_lo, 
                                   res$thr4$costEACCI_lo, res$thr9$costEACCI_lo),
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

