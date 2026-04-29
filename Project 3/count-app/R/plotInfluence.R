plotInfluence<-function(mod){
  ####################################
  # Create Data for Leverage Plot"
  ####################################
  d <- mod$model
  n <- nrow(d)
  p <- length(coef(mod))
  d <- d %>% mutate(obs = 1:n)
  ggdat <- d %>% mutate(h.values = hatvalues(mod))
  
  ####################################
  # Create Leverage Plot"
  ####################################
  p1<-ggplot(data=ggdat,aes(x=obs)) +
    geom_linerange(aes(ymin=0, ymax=h.values)) +
    geom_hline(yintercept=0)+
    theme_bw()+
    xlab("Observation Number")+
    ylab("Leverage")+
    geom_hline(yintercept =2*p/n, linetype="dotted", color="orange",size=.75)+
    geom_hline(yintercept =3*p/n, linetype="dotted", color="red",size=.75)
  
  ####################################
  # Create Data for Cook's D Plot"
  ####################################
  ggdat <- d %>% mutate(cook.d = cooks.distance(mod))
  ####################################
  # Create Cook's D Plot"
  ####################################
  p2<-ggplot(data=ggdat, aes(x=obs)) +
    geom_linerange(aes(ymin=0, ymax=cook.d)) +
    geom_hline(yintercept=0)+
    theme_bw()+
    xlab("Observation Number")+
    ylab("Cook's Distance")+
    geom_hline(yintercept = qf(p=0.10, df1=p, df2=n-p), linetype="dotted", color="orange",size=.75)+
    geom_hline(yintercept = qf(p=0.50, df1=p, df2=n-p), linetype="dotted", color="red",size=.75)
  
  ####################################
  # Create Data for DFFITS Plot"
  ####################################
  ggdat <- d %>% mutate(dffits=dffits(mod))
  ####################################
  # Create DFFITS Plot"
  ####################################
  p3<-ggplot(data=ggdat, aes(x=obs)) +
    geom_linerange(aes(ymin=0, ymax=dffits)) +
    geom_hline(yintercept=0)+
    theme_bw()+
    xlab("Observation Number")+
    ylab("DFFITs")+
    geom_hline(yintercept = c(-2*sqrt(p/n), 2*sqrt(p/n)),
               linetype="dotted",
               size=.75,
               color="orange")+
    geom_hline(yintercept = c(-3*sqrt(p/n), 3*sqrt(p/n)),
               linetype="dotted",
               size=.75,
               color="red")
  
  # ####################################
  # # Create Data for Residual Plot"
  # ####################################
  ggdat<-data.frame(obs=d$obs,
                    y=rstudent(mod))
  ggdat.out2<-ggdat %>% filter(abs(y)>2)
  ggdat.out3<-ggdat %>% filter(abs(y)>3)

  # ####################################
  # # Create Residual Plot"
  # ####################################
  p4<-ggplot(data=ggdat,aes(x=obs,y=y))+
    geom_point(shape=1)+
    geom_hline(yintercept = 0,color="red",linetype="dashed")+
    xlab("Observation Number")+
    ylab("Studentized Residual")+
    theme_bw()+
    geom_hline(yintercept = c(-3,3), color="red", linetype="dotted",size=0.75)+
    geom_hline(yintercept = c(-2,2), color="orange", linetype="dotted",size=0.75)+
    geom_point(data=ggdat.out2, aes(x=obs,y=y), fill="orange", shape=21)+
    geom_point(data=ggdat.out3, aes(x=obs,y=y), fill="red", shape=21)
  
  ####################################
  # Print Plots"
  ####################################
  (p1|p2)/(p3|p4)
}