source('groups/prev.R')

mu0.pred = numeric(nrow(job_binary))
mu1.pred = numeric(nrow(job_binary))
tau.pred = predict(lm(formula=as.formula(paste('(2*A-1)*ipw*Y ~ (',paste(Xall,collapse = ' + '),')')), data=job_binary,weights=job_binary$sw))
for (k in 1:K) {
  print(k);
  fmu0 = gbm(formula=as.formula(paste('Y ~ (',paste(Xall,collapse = ' + '),')')),
             data=job_binary[cvgroup!=k & job_binary$A==0,],
             weights=job_binary[cvgroup!=k & job_binary$A==0,'sw'],
             distribution='bernoulli',
             cv.folds=4)
  fmu1 = gbm(formula=as.formula(paste('Y ~ (',paste(Xall,collapse = ' + '),')')),
             data=job_binary[cvgroup!=k & job_binary$A==1,],
             weights=job_binary[cvgroup!=k & job_binary$A==1,'sw'],
             distribution='bernoulli',
             cv.folds=4)
  mu0.pred[cvgroup==k] = predict(fmu0, job_binary[cvgroup==k,],n.trees=gbm.perf(fmu0, method = "cv", plot.it = F),type='response')
  mu1.pred[cvgroup==k] = predict(fmu1, job_binary[cvgroup==k,],n.trees=gbm.perf(fmu1, method = "cv", plot.it = F),type='response')
}

job_binary = job_binary %>% mutate(
  tau = tau.pred,
  mu0 = mu0.pred,
  mu1 = mu1.pred
)

var0.pred = numeric(nrow(job_binary))
var1.pred = numeric(nrow(job_binary))
for (k in 1:K) {
  print(k);
  fvar = gbm(formula=as.formula(paste('(Y-mu0)^2 ~ (A + ',paste(Xall,collapse = ' + '),')')),
             data=job_binary[cvgroup!=k,],
             weights=job_binary[cvgroup!=k,'sw'],
             distribution='gaussian',
             cv.folds=4)
  var0.pred[cvgroup==k] = predict(fvar, job_binary[cvgroup==k,]%>%mutate(A=0),n.trees=gbm.perf(fvar, method = "cv", plot.it = F),type='response')
  var1.pred[cvgroup==k] = predict(fvar, job_binary[cvgroup==k,]%>%mutate(A=1),n.trees=gbm.perf(fvar, method = "cv", plot.it = F),type='response')
}

job_binary = job_binary %>% mutate(
  var0 = var0.pred*(var0.pred>0),
  var1 = var1.pred*(var1.pred>0)
)

correction1 = job_binary%>%summarise(mean(sw*(sqrt(var0)+sqrt(var1)))) %>% pull
Rsquared    = job_binary %>% group_by(A) %>% summarise(condvar = mean(sw*(Y-A*mu1-(1-A)*mu0)^2), marvar = mean(sw*Y))
correction2 = Rsquared %>% summarise(sum(sqrt(condvar))) %>% pull

wtdquantile = function(y,w,g) {
  if(g>=1) {
    max(y)
  } else {
    o = order(y)
    y[o[which(cumsum(w[o])>=sum(w)*g)[1]]]
  }
}

ps = seq(0.01, 1, 0.01)

zz = 1.64485

# CVaR(tau)
CVaR = foreach(p=ps, .combine=rbind) %do% {
  q = wtdquantile(job_binary$tau,job_binary$sw,p) 
  job_binary %>% mutate(IF = q + (mu1-mu0+(2*A-1)*ipw*(Y-A*mu1-(1-A)*mu0)-q)*(tau<=q)/p) %>% summarise(p=p, CVaR = mean(sw*IF,na.rm=T), CVaR.se = sd(sw*IF,na.rm=T)/sqrt(n()))
}
job_cvar = CVaR %>% mutate(CVaR=rearrangement(list(ps),CVaR,n=1000)) %>% ggplot + aes(x=p,y=CVaR,ymax=CVaR+zz*CVaR.se,ymin=CVaR-zz*CVaR.se)+ geom_line() + geom_point() + geom_ribbon(alpha=0.5) + ylab(TeX('${CVaR}_{\\alpha}$')) + xlab(TeX('$\\alpha$'))
job_cvar
# Produce Figure 1
#ggsave'job_cvar.pdf', plot=job_cvar, dpi = 300, height = 3.8, width = 6.3)  

job_cvar_notrearranged = CVaR %>% ggplot + aes(x=p,y=CVaR,ymax=CVaR+zz*CVaR.se,ymin=CVaR-zz*CVaR.se)+ geom_line() + geom_point() + geom_ribbon(alpha=0.5) + ylab(TeX('${CVaR}_{\\alpha}$')) + xlab(TeX('$\\alpha$'))
job_cvar_notrearranged
# Produce Figure EC.1
#ggsave'job_cvar_notrearranged.pdf', plot=job_cvar_notrearranged, dpi = 300, height = 3.8, width = 6.3)  


CVaR.plugin = foreach(p=ps, .combine=rbind) %do% {
  q = wtdquantile(job_binary$tau,job_binary$sw,p) 
  job_binary %>% mutate(IF = q + (tau-q)*(tau<=q)/p) %>% summarise(p=p, CVaR = mean(sw*IF,na.rm=T), CVaR.se = sd(sw*IF,na.rm=T)/sqrt(n()))
}
job_cvar.plugin = CVaR.plugin %>% ggplot + aes(x=p,y=CVaR,ymax=CVaR+zz*CVaR.se,ymin=CVaR-zz*CVaR.se)+ geom_line() + geom_point() + geom_ribbon(alpha=0.5) + ylab(TeX('${CVaR}_{\\alpha}$')) + xlab(TeX('$\\alpha$'))
job_cvar.plugin
# Produce Figure 3
#ggsave'job_cvar_plugin.pdf', plot=job_cvar.plugin, dpi = 300, height = 3.8, width = 6.3)


# CVaR(tau) with bad controls
Xbad = c('age','Paris_region','African','High_school_dropout')
tau.bad.pred = predict(lm(formula=as.formula(paste('(2*A-1)*ipw*Y ~ (',paste(Xbad,collapse = ' + '),')')), data=job_binary,weights=job_binary$sw))
job_binary = job_binary %>% mutate(
  tau.bad = tau.bad.pred,
)
CVaR.bad = foreach(p=ps, .combine=rbind) %do% {
  q = wtdquantile(job_binary$tau.bad,job_binary$sw,p) 
  job_binary %>% mutate(IF = q + (mu1-mu0+(2*A-1)*ipw*(Y-A*mu1-(1-A)*mu0)-q)*(tau.bad<=q)/p) %>% summarise(p=p, CVaR = mean(sw*IF,na.rm=T), CVaR.se = sd(sw*IF,na.rm=T)/sqrt(n()))
}
job_cvar.bad = CVaR.bad %>% mutate(CVaR=rearrangement(list(ps),CVaR,n=1000)) %>% ggplot + aes(x=p,y=CVaR,ymax=CVaR+zz*CVaR.se,ymin=CVaR-zz*CVaR.se)+ geom_line() + geom_point() + geom_ribbon(alpha=0.5) + ylab(TeX('${CVaR}_{\\alpha}$')) + xlab(TeX('$\\alpha$'))
job_cvar.bad
# Produce Figure 4
#ggsave'job_cvar_bad.pdf', plot=job_cvar.bad, dpi = 300, height = 3.8, width = 6.3)  

# CVaR(tau)-ATE
CVaRmATE = foreach(p=ps, .combine=rbind) %do% {
  q = wtdquantile(job_binary$tau,job_binary$sw,p) 
  job_binary %>% mutate(IF = q + (mu1-mu0+(2*A-1)*ipw*(Y-A*mu1-(1-A)*mu0))*((tau<=q)/p-1) - q*(tau<=q)/p) %>% summarise(p=p, CVaR = mean(sw*IF,na.rm=T), CVaR.se = sd(sw*IF,na.rm=T)/sqrt(n()))
}
job_cvarvsate = CVaRmATE %>% mutate(CVaR=rearrangement(list(ps),CVaR,n=1000)) %>% ggplot + aes(x=p,y=CVaR,ymax=CVaR+zz*CVaR.se,ymin=CVaR-zz*CVaR.se)+ geom_line() + geom_point() + geom_ribbon(alpha=0.5) + ylab(TeX('${CVaR}_{\\alpha}-\\bar{\\tau}$')) + xlab(TeX('$\\alpha$'))
job_cvarvsate
# Produce Figure 2
#ggsave'job_cvarvsate.pdf', plot=job_cvarvsate, dpi = 300, height = 3.8, width = 6.3)  
job_binary |> write_csv(here::here("data", "last_result.csv"))
