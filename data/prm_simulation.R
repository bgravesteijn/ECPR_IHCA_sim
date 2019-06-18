ndrawz<-1000
set.seed(1234)
prm<-data.frame(  p.DNR_75_min        = rbeta(n=ndrawz, shape1 = 5, shape2=95),
                    #clinical insight
                    or.DNR_75_84        = rlnorm(n=ndrawz, meanlog = log(1.7), sdlog = (log(2.33)-log(1.7))/1.96),
                    or.DNR_85_plus       = rlnorm(n=ndrawz, meanlog=log(2.96), sdlog = (log(3.74)-log(2.96))/1.96),
                    #probability of DNR --> Cook, I., Kirkup, A. L., Langham, L. J., Malik, M. A., Marlow, G., & Sammy, I. (2017). End of Life Care and Do Not Resuscitate Orders: How Much Does Age Influence Decision Making? A Systematic Review and Meta-Analysis. Gerontology & geriatric medicine, 3, 2333721417713422. doi:10.1177/2333721417713422
                    p.Die_DNR           = 1,
                    p.Die_CPR           = rbeta(n=ndrawz, shape1=850, shape2=150),                
                    #Zhu A, Zhang J. Meta-analysis of the outcomes of the 2005 and 2010 cardiopulmunary resuscitation guidelines for adults with in-hospital cardiac arrest. AM J EMERG MED 2016;34:113-9
                    p.ECMO_c.ind        = rbeta(n=ndrawz, shape1=10, shape2=40), 
                    #Probability of contra-indication for ecmo: 
                    or.Die_c.ind_ECMO   = rlnorm(ndrawz, meanlog=log(2), sdlog=0.2),              
                    #OR for dying, having a contraindication vs not having a contraindication (e.g.: heart failure not bridgable to transplant)
                    p.rosc_20min        = rbeta(n=ndrawz, shape1=(10200+10500+7900+5200)*0.01,
                                                shape2=(3600+2600+1700+1200+800+600+300+200+100+50+20+20+10+5+1800+5000+7300+8050+6700+4950+3300+
                                                          2200+1500+1050+700+600+400+200+200+100+100+50+50+50+20+20+10)*0.01),
                    #Khan et al. Age, Sex, and Hospital factors are associated with the duration of cardiopulmonary resuscitation in hospitalized patients who do not experience sustained return of spontaneous circulation. JAHA 2018 July
                    rr.ECMO             = rlnorm(n = ndrawz, meanlog = log((205/1884)/(96/376)), sdlog = (log(3.45)-log(2.37))/1.96), 
                    #Wang et al: comparison of extracorporeal and conventional cardiopulmonary resuscitation: a meta-analysis of 2260 patients with cardiac arrest. World j emerg med 2017   
                    p.Compl             = rbeta(ndrawz, shape1=379*0.1, shape2=621*0.1),                  
                    #Pubmed ID: 22429669/26825953/20543669
                    p.Die_Compl         = rbeta(ndrawz,shape1 = 10,shape2 = 90),                  
                    #probability of dying with complications
                    b.Die_CPR_age_CCI   = log(rlnorm(n = ndrawz, meanlog = (log(0.62^-1)/5.5), sdlog=((log(0.83)-log(0.62))/1.96/5.5))),   
                    #Hilerkar al. comoridity and survival i out-of-hospital cardiac arrest.resuscitatio 
                    c.ECMO             = rnorm(ndrawz, 51997, 10767), 
                    #Lansink-Hartgring AO, Van Den Hengel B, Van Der Bij W, et al (2016) Hospital Costs of Extracorporeal Life Support Therapy. Crit Care Med. 
                    c.first_year       = triangle::rtriangle(n=ndrawz, a=374, b=40620, c=18629)*0.85,
                    c.after_firstyear  = triangle::rtriangle(n=ndrawz, a=(374-162)/(9/12), #the first year costs, without the extra costs of the first three months
                                                             b=(40620-28603)/(9/12), 
                                                             c=(18629-7741)/(9/12))*0.85,
                    #Readmission Rates and Long-Term Hospital Costs Among Survivors of In-Hospital Cardiac Arrest Paul S. Chan, M.D., M.Sc.  
                    utility.men         = triangle::rtriangle(n=1000, a=0.66, b=0.89, c=0.82),
                    utility.women       = triangle::rtriangle(n=1000, a=0.58, b=0.82, c=0.81)
                    # Israelsson, J., Bremer, A., Herlitz, J., Axelsson, ?. B., Cronberg, T., Dj?rv, T., Kristofferzon, M.,
                    # Larsson, I., Lilja, G., Sunnerhagen, K. S., Wallin, E., ?gren, S., ?kerman, E., ?restedt, K., (2017),
                    # Health status and psychological distress among in-hospital cardiac arrest survivors in relation to
                    # gender, Resuscitation, 114, 27-33. https://dx.doi.org/10.1016/j.resuscitation.2017.02.006
)

save(prm, file = "data/parameters.rdata")
