set.seed(1234)
cohort<-data.frame(age=round(rnorm(n = 1000, mean = 65.9, sd = 15.8),0),
                      gender=rbinom(n = 1000, size = 1, prob = 0.583),
                      cci=sample(0:8, replace = TRUE, size=1000, 
                                 prob=c((5881/15953), (3684/15953), (2996/15953), (1801/15953),
                                        (1591/15953)*0.4,(1591/15953)*0.3,(1591/15953)*0.2,(1591/15953)*0.05,(1591/15953)*0.05)),
                      LYNE=NA,
                      LYEACCI_lo=NA,
                      LYEALL=NA,
                      costNE=NA,
                      costEACCI_lo=NA,
                      costEALL=NA,
                      QALYNE=NA,
                      QALYEACCI_lo=NA,
                      QALYEALL=NA) 
cohort$age<-ifelse(cohort.df$age<18, 18, cohort.df$age)

save(cohort, file="data/cohort.rdata")