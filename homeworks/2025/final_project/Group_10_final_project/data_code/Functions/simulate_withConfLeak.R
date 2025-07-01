library(NORMT3)

simulateDistribution_withConfLeak <- function(m1, m2, b, internalNoise, diffRight, diffLeft, reps, shift, metanoise, metaNpriorChoice, metaNpriorConf, metaNLikChoice, metaNpriorChoiceDouble,clFact)
{
  reps = reps
  mode_m = mode_m
  internalNoise = internalNoise
  diffRight = diffRight #in order: low, medium, high
  diffLeft = diffLeft 
  diffLevels = c(1,2,3)
  sLength = length(diffRight)
  m1 = m1
  m2 = m2
  b = b
  stimL = -1
  stimR = 1
  metanoise = metanoise
  if (missing(shift)){
    shift = FALSE
  }else{
    shift = FALSE
  }
  
  data <- data.frame(s = numeric(reps),
                     r1 = numeric(reps),
                     r2 = numeric(reps))
  
  for (i in 1:sLength){
    for (x in 1:sLength) {
      
      if (x==i) next
      
      diffPrior = diffLevels[i]
      diffTarget = diffLevels[x]
      
      
      stimPrior = c(stimL, stimR)
      data$sign = sample(stimPrior, reps, replace=TRUE)
      data$s1 = data$sign
      data$prior_direction = data$s1
      data$s1[data$prior_direction<0] = diffLeft[diffPrior]
      data$s1[data$prior_direction>0] = diffRight[diffPrior]
      
      data$priorLevel = diffPrior
      data$r1 = rnorm(reps,data$s1,internalNoise) # used to be sigmaPrior
      data$r1Noise = rnorm(reps,data$r1, metanoise)
      data$conf1 = pnorm(abs(data$r1Noise), mean=0, sd=b*internalNoise)
      data$s1Noise = rnorm(reps, data$s1, metanoise)
      
      data$choseRight1 = data$r1>0 
      data$correct1 = (data$r1*data$prior_direction)>0
      
      # Metanoise in internal conf even for adjusting decision criterion
      if (metaNpriorChoice==TRUE){
        if (metaNpriorChoiceDouble==TRUE){
          data$r1Noise2 = rnorm(reps,data$r1Noise, metanoise)
          data$priorRight = 0.5
          data$priorRight[data$choseRight1==1] = pnorm(data$r1Noise2[data$choseRight1==1], mean=0, sd=m1*internalNoise)
          data$priorRight[data$choseRight1==0] = 1 - pnorm(data$r1Noise2[data$choseRight1==0], mean=0, sd=m1*internalNoise)
        }else{
          data$priorRight = 0.5
          data$priorRight[data$choseRight1==1] = pnorm(data$r1Noise[data$choseRight1==1], mean=0, sd=m1*internalNoise)
          data$priorRight[data$choseRight1==0] = 1 - pnorm(data$r1Noise[data$choseRight1==0], mean=0, sd=m1*internalNoise)
        }
      }else{
        data$priorRight = pnorm(abs(data$r1), mean=0, sd=m1*internalNoise)
      }
      
      # Metanoise in internal conf for conf rating
      if (metaNpriorConf==TRUE){
        data$weightedPriorRight = 0.5
        data$weightedPriorRight[data$choseRight1==1] = pnorm(data$r1Noise[data$choseRight1==1], mean=0, sd=b*m2*internalNoise)
        data$weightedPriorRight[data$choseRight1==0] = 1 - pnorm(data$r1Noise[data$choseRight1==0], mean=0, sd=b*m2*internalNoise)
        
      }else{
        data$weightedPriorRight = pnorm(abs(data$r1), mean=0, sd=b*m2*internalNoise)
      }
      
      data$crit = -abs(data$r1)/m1
      
      data$s2 = 0
      data$s2[which(data$correct1 == 1)] = 1
      data$s2[which(data$correct1 == 0)] = -1
      data$target_direction = data$s2
      data$s2[data$target_direction<0] = diffLeft[diffTarget]
      data$s2[data$target_direction>0] = diffRight[diffTarget]
      data$targetLevel = diffTarget
      data$r2 = rnorm(reps, data$s2, internalNoise)
      
      data$r2Noise = rnorm(reps, data$r2, metanoise)
      data$s2Noise = rnorm(reps, data$s2, metanoise)
      
      data$choseRightNoPrior = data$r2>0 
  
      if (metaNLikChoice==TRUE){
        data$probTargetRight = Re((data$priorRight*(1 + erf(data$r2Noise/(internalNoise*sqrt(2))))) / (1 + (2*data$priorRight - 1)*(erf(data$r2Noise/(internalNoise*sqrt(2))))))
      }else{
        data$probTargetRight = Re((data$priorRight*(1 + erf(data$r2/(internalNoise*sqrt(2))))) / (1 + (2*data$priorRight - 1)*(erf(data$r2/(internalNoise*sqrt(2))))))
      }
      
      data$probTargetLeft = 1 - data$probTargetRight
      
      if (mode_m==2){
        data$confTargetRight = Re((data$weightedPriorRight*(1 + erf(data$r2Noise/(b*internalNoise*sqrt(2))))) / (1 + (2*data$weightedPriorRight - 1)*(erf(data$r2Noise/(b*internalNoise*sqrt(2))))))
        data$confTargetLeft = 1 - data$confTargetRight
      }
      
      data$choseRight2 = (data$probTargetRight - data$probTargetLeft)>0
      data$correct2 = data$correct1==data$choseRight2 
      
      if (mode_m==2){
        data$confTarget = 0
        data$confTarget[which(data$choseRight2==TRUE)] = data$confTargetRight[which(data$choseRight2==TRUE)]
        data$confTarget[which(data$choseRight2==FALSE)] = data$confTargetLeft[which(data$choseRight2==FALSE)]
        
        data$confTargetOld = data$confTarget
        data$confTarget = (data$confTarget*clFact + data$conf1)/(clFact+1)
        
        data$confTarget[which(data$confTarget<0.5)] = 0.5
        data$confTarget[which(data$confTarget>1)] = 1
      }
      
      if (x==2 && i==1){
        data_full = data
      }else{
        data_full = rbind(data_full, data)
      }
      
    }
  }
  
  data_full$prec = "L"
  data_full$prec[which(data_full$priorLevel+data_full$targetLevel==5)] = "H"
  data_full$prec[which(data_full$priorLevel+data_full$targetLevel==4)] = "M"
  
  
  data_full$condition = "prior"
  data_full$condition[which(data_full$priorLevel<data_full$targetLevel)] = "target"
  return(data_full)
  
}
