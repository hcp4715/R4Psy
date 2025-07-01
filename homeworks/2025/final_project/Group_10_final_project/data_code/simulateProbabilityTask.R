library(NORMT3)

simulateProbTask <- function(m1, m2, b, internalNoise, diffR, diffL, priorRs, reps, metanoise)
{
  reps = reps
  mode_m = mode_m
  internalNoise = internalNoise
  diffRight = diffR #in order: low, medium, high
  diffLeft = diffL #c(0.25, 0.5, 1)
  priorRs = priorRs
  sLength = length(diffRight)
  m1 = m1
  m2 = m2
  b = b
  stimL = -1
  stimR = 1
  if (missing(metanoise)){
    metanoise = 0
  }
  
  data <- data.frame(s = numeric(reps),
                     r = numeric(reps))
  
  for (i in 1:length(diffRight)){
    for (x in 1:length(priorRs)) {
    
      prior = priorRs[x]
      
      data$sign = rbinom(length(data$s), 1, prior) 
      data$sign[data$sign==0] = -1
      data$s = data$sign
      data$direction = data$s
      data$s[data$direction<0] = diffLeft[i]
      data$s[data$direction>0] = diffRight[i]
      
      data$r = rnorm(reps,data$s,internalNoise) 
      data$prior = prior
      
      priorSig = qnorm(prior)
      weightedPriorChoice = pnorm(priorSig, mean=0, sd=m1)
      weightedPriorConf = pnorm(priorSig, mean=0, sd=b*m2)
      
      data$weightedPriorChoice = weightedPriorChoice
      
      data$priorSig = priorSig
      data$confStim = pnorm(abs(data$r), mean=0, sd=1)
      
      crit = -(priorSig)/m1
      data$choseRightCrit = ifelse(data$r>crit,1,0)
        
      data$probTargetRight = Re((weightedPriorChoice*(1 + erf(data$r/(internalNoise*sqrt(2))))) / (1 + (2*weightedPriorChoice - 1)*(erf(data$r/(internalNoise*sqrt(2))))))
      data$probTargetLeft = 1 - data$probTargetRight
      
      data$rNoise = rnorm(reps,data$r, metanoise)
      
      if (mode_m==2){
        data$confTargetRight = Re((weightedPriorConf*(1 + erf(data$rNoise/(b*internalNoise*sqrt(2))))) / (1 + (2*weightedPriorConf - 1)*(erf(data$rNoise/(b*internalNoise*sqrt(2))))))
        data$confTargetLeft = 1 - data$confTargetRight
      }
      
      data$choseRight = (data$probTargetRight - data$probTargetLeft)>0
      data$correct = (data$direction>0)==data$choseRight 
      
      if (mode_m==2){
        data$confTarget = 0
        data$confTarget[which(data$choseRight==TRUE)] = data$confTargetRight[which(data$choseRight==TRUE)]
        data$confTarget[which(data$choseRight==FALSE)] = data$confTargetLeft[which(data$choseRight==FALSE)]
      }
      
      if (x==1 && i==1){
        data_full = data
      }else{
        data_full = rbind(data_full, data)
      }
      
    }
  }
  
  return(data_full)
  
}
