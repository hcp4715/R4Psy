## Adapted from Matteo Lisi, 2020, https://osf.io/sur3e
## Creative Commons Attribution 4.0 International Public License

library(optimx)
library(NORMT3)

psy_3par <- function(x, mu ,sigma, lambda){
  lambda + (1-2*lambda) * 0.5 * Re((1+erf((x-mu)/(sqrt(2)*sigma))))
}

lnorm_3par <- function(p,d){
    nL <- -sum(log(psy_3par(d$signed_coherence[d$rr==1], p[1] ,p[2] ,p[3]))) - 
      sum(log(1 - psy_3par(d$signed_coherence[d$rr==0], p[1] ,p[2], p[3])))
    nL = Re(nL)
  return(nL)
}
lnorm_nb_yl <- function(p,d){
    nL <- -sum(log(psy_3par(d$signed_coherence[d$rr==1], 0 ,p[1] ,p[2]))) - 
    sum(log(1 - psy_3par(d$signed_coherence[d$rr==0], 0 ,p[1], p[2])))
    nL = Re(nL)
  return(nL)
}

# wrapper functions (fit models and return parameters and AIC values)
# the functions are named according to:
# nb_nl: No bias - No lapse rate
# yb_nl: Yes bias - No lapse rate
# nb_yl: No bias - Yes lapse rate
# yb_yl: Yes bias - Yes lapse rate
fit_nb_nl <- function(d){
   #rr is response, signed_mu is intensity values signed (so for right vs left)
  m <- glm(rr~0+signed_coherence,family=binomial(link=probit), d)
  eta <- unname(1/coef(m))
  return(list(eta=eta, bias=0, lambda=0, aic=AIC(m)))
}

fit_yb_nl <- function(d){
  m <- glm(rr~signed_coherence,family=binomial(link=probit), d)
  eta <- unname(1/coef(m)[2])
  bias <- -unname(coef(m)[1] / coef(m)[2])
  return(list(eta=eta, bias=bias, lambda=0, aic=AIC(m)))
}

fit_nb_yl <- function(d){
  start_p <- c(mean(abs(d$signed_coherence),na.rm=T), 0)
  LB <- min(abs(d$signed_coherence))/2
  UB <- max(abs(d$signed_coherence))
  lpb <- c(LB, 0)
  upb <- c(UB, 0.5)
  m <- optimx::optimx(par = start_p, lnorm_nb_yl , d=d, method="L-BFGS-B", lower =lpb, upper =upb)#changed from bobyqa method which doesn't work on Linux for some reason
  cat(m$ierr)
  eta <- unlist(unname(m[1]))
  lambda <- unlist(unname(m[2]))
  aic <- 2*2 + 2*unlist(unname(m[3]))
  return(list(eta=eta, bias=0, lambda=lambda, aic=aic))
}

fit_yb_yl <- function(d){
  start_p <- c(0, mean(abs(d$signed_coherence)), 0)
  lpb <- c(-max(abs(d$signed_coherence)), min(abs(d$signed_coherence))/2, 0)
  upb <- c(max(abs(d$signed_coherence)), max(abs(d$signed_coherence)), 0.5)
  m <- optimx::optimx(par = start_p, lnorm_3par , d=d, method="L-BFGS-B", lower =lpb, upper =upb) #changed from bobyqa method which doesn't work on Linux for some reason
  cat(m$ierr)
  bias <- unlist(unname(m[1]))
  eta <- unlist(unname(m[2]))
  lambda <- unlist(unname(m[3]))
  aic <- 2*3 + 2*unlist(unname(m[4]))
  return(list(eta=eta, bias=bias, lambda=lambda, aic=aic))
}

# wrapper function that do the model averaging
estimateNoise <- function(d){
  
  nbnl_1 <- fit_nb_nl(d)
  ybnl_1 <- fit_yb_nl(d)
  nbyl_1 <- fit_nb_yl(d)
  ybyl_1 <- fit_yb_yl(d)
  deltas <- c(nbnl_1$aic, ybnl_1$aic, nbyl_1$aic, ybyl_1$aic) - min(c(nbnl_1$aic, ybnl_1$aic, nbyl_1$aic, ybyl_1$aic))
 
  weights <- exp(-0.5*deltas) / sum(exp(-0.5*deltas))
  eta_1 <- sum(weights * c(nbnl_1$eta, ybnl_1$eta, nbyl_1$eta, ybyl_1$eta))
  bias_1 <- sum(weights * c(nbnl_1$bias, ybnl_1$bias, nbyl_1$bias, ybyl_1$bias))
  lapse_1 <- sum(weights * c(nbnl_1$lambda, ybnl_1$lambda, nbyl_1$lambda, ybyl_1$lambda))
  
  outV <- c(eta_1, bias_1, lapse_1)
  names(outV) <- c("sigma", "bias", "lapse")
  return(outV)
}
