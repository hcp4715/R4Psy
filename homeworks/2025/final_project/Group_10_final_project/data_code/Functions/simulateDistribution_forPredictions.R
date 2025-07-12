library(NORMT3)

simulateDistribution_forPredictions <- function(m1, m2, b, internalNoise, diffPrior_all, diffTarget_all, reps)
{
  reps = reps
  mode_m = mode_m
  internalNoise = internalNoise
  diffPrior_all = diffPrior_all 
  diffTarget_all = diffTarget_all 
 
  sLength = length(diffPrior_all)
  m1 = m1
  m2 = m2
  b = b
  stimL = -1
  stimR = 1
  
  data <- data.frame(s = numeric(reps),
                     r1 = numeric(reps),
                     r2 = numeric(reps))
  
  for (i in 1:sLength){
    
    diffPrior = diffPrior_all[i]
    diffTarget = diffTarget_all[i]
    
    stimPrior = c(stimL, stimR)
    data$sign = sample(stimPrior, reps, replace=TRUE)
    data$s1 = data$sign
    data$prior_direction = data$s1
    data$s1[data$prior_direction<0] = -diffPrior
    data$s1[data$prior_direction>0] = diffPrior
    
    data$r1 = rnorm(reps,data$s1,internalNoise) 
    
    data$choseRight1 = data$r1>0 
    data$correct1 = (data$r1*data$prior_direction)>0
 
    data$priorRight = pnorm(abs(data$r1), mean=0, sd=m1*internalNoise)
    data$weightedPriorRight = pnorm(abs(data$r1), mean=0, sd=b*m2*internalNoise)
    
    data$s2 = 0
    data$s2[which(data$correct1 == 1)] = 1
    data$s2[which(data$correct1 == 0)] = -1
    data$target_direction = data$s2
    data$s2[data$target_direction<0] = -diffTarget
    data$s2[data$target_direction>0] = diffTarget
   
    data$r2 = rnorm(reps, data$s2, internalNoise)
    
    data$choseRightNoPrior = data$r2>0 
    
    data$probTargetRight = Re((data$priorRight*(1 + erf(data$r2/(internalNoise*sqrt(2))))) / (1 + (2*data$priorRight - 1)*(erf(data$r2/(internalNoise*sqrt(2))))))
    data$probTargetLeft = 1 - data$probTargetRight
    
    if (mode_m==2){
      data$confTargetRight = Re((data$weightedPriorRight*(1 + erf(data$r2/(b*internalNoise*sqrt(2))))) / (1 + (2*data$weightedPriorRight - 1)*(erf(data$r2/(b*internalNoise*sqrt(2))))))
      data$confTargetLeft = 1 - data$confTargetRight
    }
    
    data$choseRight2 = (data$probTargetRight - data$probTargetLeft)>0
    data$correct2 = data$correct1==data$choseRight2 
    
    if (mode_m==2){
      data$confTarget = 0
      data$confTarget[which(data$choseRight2==TRUE)] = data$confTargetRight[which(data$choseRight2==TRUE)]
      data$confTarget[which(data$choseRight2==FALSE)] = data$confTargetLeft[which(data$choseRight2==FALSE)]
      
    # For bounding confidence
     # data$confTarget[which(data$confTarget<0.5)] = 0.5
     # data$confTarget[which(data$confTarget>1)] = 1
    }
    
    if (i==1){
      data_full = data
    }else{
      data_full = rbind(data_full, data)
    }
    
  }
  
  data_full$condition = "prior"
  data_full$condition[which(abs(data_full$s1)<abs(data_full$s2))] = "target"
  return(data_full)
}
