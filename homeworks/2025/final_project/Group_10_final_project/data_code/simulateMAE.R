library(tidyverse)
library(ggpubr)
require(here)

dataDir = here::here("Data/")
load(file=paste(dataDir, "expFitNoiseRaw.RData", sep=""))

af = 0.22
m1 = 1
m2 = 1
b = 1
experiment = exp_fitNoiseRaw

# Adjust their raw coherence values 
experiment$coherentDir1 = -sign(experiment$prior_direction-1)
experiment$signedCoh1 = experiment$prior_coherence*experiment$coherentDir1
experiment$adjCoh1 = (experiment$signedCoh1-experiment$fitBiasRaw)/experiment$fitNoiseRaw

d = data.frame(s1 = experiment$adjCoh1, s2 = experiment$target_coherence, posterior_level = experiment$posteriorFactor, condition = experiment$condition)

d$ir1 <- d$s1 + rnorm(length(d$s1),0,1) 
d$r1 <- (sign(d$ir1)+1)/2 
d$acc1 <- sign(d$ir1) * sign(d$s1)
d$s2 <- d$acc1 * abs(d$s2)
d$s2 <- (d$s2-experiment$fitBiasRaw)/experiment$fitNoiseRaw
d$theta2 <- -abs(d$ir1)/m1 
d$ir2_noAF <- d$s2 + rnorm(length(d$s2),0,1)

# add after-effect bias to target internal signal 
# with equal contribution of stimulus and internal response 
d$ir2 <- d$ir2_noAF - af/2*(d$s1 + d$ir1) # 
d$irtest <- d$ir2_noAF - 0/2*(d$s1 + d$ir1)

d$r2 <- (sign(d$ir2-d$theta2)+1)/2
d$acc2 <- sign(d$ir2 - d$theta2) * sign(d$s2)
d$acc1 <-  (d$acc1+1)/2
d$acc2 <-  (d$acc2+1)/2

# confidence
d$conf1 <- ifelse(d$r1==1, pnorm(d$ir1, sd=m2*b), 1-pnorm(d$ir1,sd=m2*b))
prob_r2 <- (pnorm(d$ir2,sd=b)*d$conf1)/(d$conf1*pnorm(d$ir2,sd=b) + (1-d$conf1)*(1-pnorm(d$ir2,sd=b)))
d$conf2 <- ifelse(d$r2==1, prob_r2, 1-prob_r2)

d$same = sign(d$s1)==sign(d$s2)
d$posterior_level = ordered(d$posterior_level, levels=c("L","M","H"))
d$condition = as.factor(d$condition) 
levels(d$condition) = c("Stronger-Lead", "Stronger-Target")

# decision
d %>%
  group_by(posterior_level,condition) %>%
  summarise(acc2 = mean(acc2)) %>%
  ggplot(aes(x=posterior_level, y=acc2, color=condition, group=condition)) +
  # facet_wrap(~same) +
  #scale_color_manual(values=c(rgb(210,210,0, maxColorValue=255),"red"), guide='none')+
  scale_color_manual(values=c("#F4CC08", "#C73030"), guide='none') +
  geom_line(size=1)+
  ylim(0.6,0.95) +
  labs(y="Target Accuracy", x="Posterior Information")+
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom") +
  geom_point(size=3) ->pl_d

# confidence
d %>%
  mutate(acc2 =ifelse(acc2==1,"Correct","Incorrect")) %>%
  group_by(posterior_level,condition,acc2) %>%
  summarise(conf2 = mean(conf2)) %>%
  ggplot(aes(x=posterior_level, y=conf2, color=condition, group=str_c(condition,acc2), shape=acc2)) +
  scale_shape_manual(values=c(17,19)) +
  scale_color_manual(values=c("#F4CC08", "#C73030")) +
  geom_line(size=1)+
  ylim(0.6,1) +
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), 
        legend.text = element_text(size=12), legend.title = element_text(size=14)) +
  labs(y="Mean Confidence", x="Posterior Information",shape="Response Accuracy",color="Condition") +
  geom_point(size=3) ->pl_c

ggarrange(pl_d, pl_c, ncol=2,nrow=1, widths=c(1,1.4))
