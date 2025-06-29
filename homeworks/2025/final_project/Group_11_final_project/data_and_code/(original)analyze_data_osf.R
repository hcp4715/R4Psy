library(sqldf)
library(doBy)
library(lme4) # for lmer
library(lmerTest) # for glmer.nb
library(emmeans)
library(MASS) # for glm.nb
library(crayon)
library(plyr)
library(ggplot2)
library(grid) # for creating panels of plots
library(gridExtra) # for creating panels of plots
library(cowplot) # for separating legend from plot

# location of the data files (directory)
dInput <- "~"
dFigures <- "~"


dfs2_general <- read.csv(paste(dInput,"data_general_osf.csv", sep=""), sep=";")
dfs2_daily <- read.csv(paste(dInput,"data_daily_osf.csv", sep=""), sep=";")


#####################################################
# 1. create functions
#####################################################

analyze_general <- function(df, dv, timepoints = 3){
  cat(blue("\n\nModel:\n"))
  cmd.lmer <- paste(dv,"~ T_Condition2*Time + (1|GooseChaseId_Fixed)",sep="")
  print(cmd.lmer)
  cat("\n")
  
  lmer.gen <- lmer(formula = as.formula(cmd.lmer), data = df)
  print(summary(lmer.gen))
  cat(blue("\nModel, CI's:\n"))
  print(confint(lmer.gen))

  # descriptive means  
  cat(blue("\nDescriptive means:\n"))
  cmd.means <- paste(dv,"~ T_Condition2 + Time",sep="")
  print(summaryBy(as.formula(cmd.means), data = df, FUN=c(mean,sd), na.rm=TRUE))
  
  # marginal means
  cat(blue("\n\nMarginal means:\n"))
  means.all <- emmeans(lmer.gen, specs = ~ Time*T_Condition2, type = "response")
  print(summary(means.all))

  # Post-hoc contrasts + adjust for multiple comparisons
  cat(blue("\n\nPost-hoc contrasts:\n"))
  lsmlist <- contrast(means.all, method = "pairwise", adjust = "none")
  # run only specific contrasts: 
  if(timepoints==3){
    # start vs. end and start vs. follow-up for each condition
    # treatment vs. control for each time
    lsmlist <- lsmlist[c(1,2,3,8,12,13,14)]
  } else if(timepoints==2.3){
    # two timepoints for control condition, three for experimental
    # control at end vs. experimental at start
    # experimental at start vs. end vs. fu
    lsmlist <- lsmlist[c(6,7,13,14)]
    
  } else { # timepoints==2
    # lsmlist <- lsmlist[c(1,6)]
    lsmlist <- lsmlist[c(1,2,5,6)]
  }
  mydiffs = update(lsmlist, pri.vars = "contrast", by.vars = NULL,adjust="mvt")
  print(mydiffs)

  cat(blue("\n\nPost-hoc contrasts, CI's:\n"))
  print(confint(mydiffs, adjust = "mvt"))
  
  return(means.all)
}


analyze_general_continuous <- function(df, dv){
  cat(blue("\n\nModel:\n"))
  cmd.lmer <- paste(dv,"~ T_Condition2*Timei + (1|GooseChaseId_Fixed)",sep="")
  print(cmd.lmer)
  cat("\n")
  
  lmer.gen <- lmer(formula = as.formula(cmd.lmer), data = df)
  print(summary(lmer.gen))
  
  # marginal means
  cat(blue("\n\nMarginal means:\n"))
  means.all <- emmeans(lmer.gen, specs = ~ Timei*T_Condition2, type = "response")
  print(summary(means.all))
  
  cat(blue("\nTrends:\n"))
  #source for emtrends: https://stats.idre.ucla.edu/r/seminars/interactions-r/#s4b
  print(test(emtrends(lmer.gen, ~ T_Condition2, var="Timei")))

  cat(blue("\nTrends: comparison\n"))
  print(emtrends(lmer.gen, pairwise ~ T_Condition2, var="Timei"))
}


analyze_daily <- function(df, dv){
  cat(blue("\n\nModel:\n"))
  cmd.lmer <- paste(dv,"~ RatingType*Day + (1|GooseChaseId_Fixed)",sep="")
  print(cmd.lmer)
  cat("\n")
  
  lmer.daily <- lmer(formula = as.formula(cmd.lmer), data = df)
  print(summary(lmer.daily))
  cat(blue("\nModel, CI's:\n"))
  print(confint(lmer.daily))
  
  # descriptive means  
  cat(blue("\nDescriptive means:\n"))
  cmd.means <- paste(dv,"~ Day + RatingType",sep="")
  print(summaryBy(as.formula(cmd.means), data = df, FUN=c(mean,sd), na.rm=TRUE))
  
  # marginal means
  cat(blue("\n\nMarginal means:\n"))
  means.all <- emmeans(lmer.daily, specs = ~ RatingType*Day, type = "response", pbkrtest.limit = 5000)
  print(summary(means.all))

  # Post-hoc contrasts
  # cat(blue("\n\nPost-hoc contrasts:\n"))
  # contr.all <- contrast(means.all, method = "pairwise", adjust = "none")
  # print(summary(contr.all))
  
  # look at trends over time for predictions and actuals separately
  # https://stats.idre.ucla.edu/r/seminars/interactions-r/#s4b
  cat(blue("\n\nPost-hoc trends:\n"))
  print(test(emtrends(lmer.daily, ~ RatingType, var="Day", pbkrtest.limit = 5000)))
  
  # cat(blue("\n\nPost-hoc trends, CI's:\n"))
  # emtrends(lmer.daily, ~ RatingType, var="Day", pbkrtest.limit = 5000)
  
  cat(blue("\nPost-hoc trends: comparison\n"))
  print(emtrends(lmer.daily, pairwise ~ RatingType, var="Day", pbkrtest.limit = 5000))
  
  return(means.all)
}


print_ttest_results <- function(t){
  if(t$p.value <= .05){
    cat(green(paste("t(",t$parameter,") = ", signif(t$statistic, digits=4),", p = ", signif(t$p.value, digits=4),", CI95[",signif(t$conf.int[1], digits=4),",",signif(t$conf.int[2], digits=4), "]","\n", sep="")))
  } else if(t$p.value <= .10){
    cat(yellow(paste("t(",t$parameter,") = ", signif(t$statistic, digits=4),", p = ", signif(t$p.value, digits=4),", CI95[",signif(t$conf.int[1], digits=4),",",signif(t$conf.int[2], digits=4), "]","\n", sep="")))
  } else {
    cat(red(paste("t(",t$parameter,") = ", signif(t$statistic, digits=4),", p = ", signif(t$p.value, digits=4),", CI95[",signif(t$conf.int[1], digits=4),",",signif(t$conf.int[2], digits=4), "]","\n", sep="")))
  }
}


my_ind_ttest <- function(df, dfname, testvar, groupvar, group1, group2, v){
  cat(paste("\n t-test comparing ",testvar," for ",groupvar,"=",group1," vs. ", group2,"\n", sep=""))

  cmd <- paste(testvar, " ~ ", groupvar, sep="")

  t <- t.test(as.formula(cmd), data = df, var.equal = TRUE)
  print_ttest_results(t)

  cmd1 <- paste("select ", testvar, " as testvar from ",dfname, " where ", groupvar, "=", group1)
  df1 <- sqldf(cmd1)
  cmd2 <- paste("select ", testvar, " as testvar from ",dfname, " where ", groupvar, "=", group2)
  df2 <- sqldf(cmd2)

  cat(paste(groupvar,"=",group1,": M=",signif(mean(df1$testvar, na.rm=T), digits=4),"; SD=",signif(sd(df1$testvar, na.rm=T), digits=4),"\n",  sep=""))
  cat(paste(groupvar,"=",group2,": M=",signif(mean(df2$testvar, na.rm=T), digits=4),"; SD=",signif(sd(df2$testvar, na.rm=T), digits=4),"\n",  sep=""))
  cat(paste("mean difference=",signif(mean(df1$testvar, na.rm=T)-mean(df2$testvar, na.rm=T), digits=4),"\n", sep=""))
  
  v <- append(v, t$p.value)

  return(v)
}


my_cor <- function(df, varname1, varname2){
  r <- cor.test(df[,varname1], df[,varname2], use="pairwise.complete.obs")
  # formula for calculating d from r (e.g., http://trendingsideways.com/index.php/cohens-d-formula/)
  d <- 2*r$estimate / (sqrt(1-r$estimate^2))

  if(r$p.value < .05){
    cat(green("Corr between ",varname1," and ",varname2, ": r(", r$parameter, ")=", signif(r$estimate, digits=4), ", p=", signif(r$p.value, digits=4), ", d=", signif(d,digits=4), "\n", sep=""))
  } else if(r$p.value < .10){
    cat(yellow("Corr between ",varname1," and ",varname2, ": r(", r$parameter, ")=", signif(r$estimate, digits=4), ", p=", signif(r$p.value, digits=4), ", d=", signif(d,digits=4), "\n", sep=""))
  } else {
    cat(red("Corr between ",varname1," and ",varname2, ": r(", r$parameter, ")=", signif(r$estimate, digits=4), ", p=", signif(r$p.value, digits=4), ", d=", signif(d,digits=4), "\n", sep=""))
  }
}

  
#####################################################
# 2. create dataframes for analyses (i.e., apply filters)
#####################################################

# exclusion criteria from pre-registration:
# - completed missions on at least 4 days
# - failed the "honesty" question (didn't actually complete at least one mission they said they did)
# - answer to "anything unusual" question was rated as having an extreme effect on mood
# NOTE: this filter was applied before this stage
dfs2_general_filtered <- sqldf("select * from dfs2_general 
             where T_Completed_NumDays>=4
             AND ES_Honesty_NoMission=0")
nrow(dfs2_general_filtered)

# after applying the filter, look for outliers on # of conversations last week
# at start of study
limit2sd <- mean(dfs2_general_filtered$SS_NumConvos_LastWk_Clean,na.rm=TRUE)+2*sd(dfs2_general_filtered$SS_NumConvos_LastWk_Clean,na.rm=TRUE)
limit25sd <- mean(dfs2_general_filtered$SS_NumConvos_LastWk_Clean,na.rm=TRUE)+2.5*sd(dfs2_general_filtered$SS_NumConvos_LastWk_Clean,na.rm=TRUE)
limit3sd <- mean(dfs2_general_filtered$SS_NumConvos_LastWk_Clean,na.rm=TRUE)+3*sd(dfs2_general_filtered$SS_NumConvos_LastWk_Clean,na.rm=TRUE)

dfs2_general_filtered$SS_NumConvos_LastWk_Clean_Trim2SD <- ifelse(dfs2_general_filtered$SS_NumConvos_LastWk_Clean<=limit2sd, dfs2_general_filtered$SS_NumConvos_LastWk_Clean, NA)
dfs2_general_filtered$SS_NumConvos_LastWk_Clean_Trim25SD <- ifelse(dfs2_general_filtered$SS_NumConvos_LastWk_Clean<=limit25sd, dfs2_general_filtered$SS_NumConvos_LastWk_Clean, NA)
dfs2_general_filtered$SS_NumConvos_LastWk_Clean_Trim3SD <- ifelse(dfs2_general_filtered$SS_NumConvos_LastWk_Clean<=limit3sd, dfs2_general_filtered$SS_NumConvos_LastWk_Clean, NA)

# at follow-up
limit2sd <- mean(dfs2_general_filtered$FU_NumConvos_Clean,na.rm=TRUE)+2*sd(dfs2_general_filtered$FU_NumConvos_Clean,na.rm=TRUE)
limit25sd <- mean(dfs2_general_filtered$FU_NumConvos_Clean,na.rm=TRUE)+2.5*sd(dfs2_general_filtered$FU_NumConvos_Clean,na.rm=TRUE)
limit3sd <- mean(dfs2_general_filtered$FU_NumConvos_Clean,na.rm=TRUE)+3*sd(dfs2_general_filtered$FU_NumConvos_Clean,na.rm=TRUE)

dfs2_general_filtered$FU_NumConvos_LastWk_Clean_Trim2SD <- ifelse(dfs2_general_filtered$FU_NumConvos_Clean<=limit2sd, dfs2_general_filtered$FU_NumConvos_Clean, NA)
dfs2_general_filtered$FU_NumConvos_LastWk_Clean_Trim25SD <- ifelse(dfs2_general_filtered$FU_NumConvos_Clean<=limit25sd, dfs2_general_filtered$FU_NumConvos_Clean, NA)
dfs2_general_filtered$FU_NumConvos_LastWk_Clean_Trim3SD <- ifelse(dfs2_general_filtered$FU_NumConvos_Clean<=limit3sd, dfs2_general_filtered$FU_NumConvos_Clean, NA)


# change wide to long
dfs2_general_filtered_long <- sqldf("
    select 1 as Time, GooseChaseId_Fixed, Uni,T_Condition2,T_Condition3,
      SS_Demog_Age,SS_Demog_SexFemale,
      SS_Trait_SocConn_Avg7 as SS_Trait_SocConn_Avg,
      SS_Trait_SHS_Avg7 as SS_Trait_SHS_Avg,
      SS_SocialCuriosity_Avg,SS_InteractionAnxiety_Avg,SS_Shy_Avg,SS_SelfEsteem_Avg,
      SS_Predict_NumToApp_Clean as NumApp,SS_Predict_Reject_Clean as Reject,SS_Predict_ConvoLen_Clean as ConvoLen,
      SS_Predict_Study_ValAroConn_Avg ValAroConn, 
      SS_General_Ability_Avg7 as Ability,
      SS_HardStart as HardStart,SS_HardStart_rev as AbleStart,
      SS_HardMaintain as HardMaintain,SS_HardMaintain_rev as AbleMaintain,
      SS_HardEnd as HardEnd,SS_HardEnd_rev as AbleEnd,
      SS_General_Awk_Avg7 as Awk_Avg,SS_General_Enj_Avg7 as Enj_Avg, 
      SS_General_PartnerPerception_Avg7 as PartnerPerception,
      SS_DV_Avg_z as DV_Avg_z,
      SS_Strangers_Trust as Strangers_Trust,SS_Strangers_FeelWarmly as Strangers_FeelWarmly,SS_Strangers_FeelConnected as Strangers_FeelConnected,SS_notice_opportunities as notice_opportunities,
      '' as Num_ContactInfo, '' as Num_Communicated
      , SS_NumConvos_LastWk_Clean_Trim2SD as NumConvos_LastWk_Clean_Trim2SD
      , SS_NumConvos_LastWk_Clean_Trim25SD as NumConvos_LastWk_Clean_Trim25SD
      , SS_NumConvos_LastWk_Clean_Trim3SD as NumConvos_LastWk_Clean_Trim3SD
    from dfs2_general_filtered
    union 
    select 2 as Time, GooseChaseId_Fixed, Uni,T_Condition2,T_Condition3,
      SS_Demog_Age,SS_Demog_SexFemale,SS_Trait_SocConn_Avg7,SS_Trait_SHS_Avg7,
      SS_SocialCuriosity_Avg,SS_InteractionAnxiety_Avg,SS_Shy_Avg,SS_SelfEsteem_Avg,
      ES_Predict_NumToApp_Clean,ES_Predict_Reject_Clean,ES_Predict_ConvoLen_Clean,
      ES_Predict_ValAroConn_Avg,
      ES_General_Ability_Avg7,
      ES_HardStart,ES_HardStart_rev,ES_HardMaintain,ES_HardMaintain_rev,ES_HardEnd,ES_HardEnd_rev,
      ES_General_Awk_Avg7,ES_General_Enj_Avg7, ES_General_PartnerPerception_Avg7,'',
      ES_Strangers_Trust,ES_Strangers_FeelWarmly,ES_Strangers_FeelConnected,ES_notice_opportunities,
      ES_Num_ContactInfo_Clean,ES_Num_Communicated_Clean
      ,'','',''
    from dfs2_general_filtered
    union 
      select 3 as Time, GooseChaseId_Fixed, Uni,T_Condition2,T_Condition3,
      SS_Demog_Age,SS_Demog_SexFemale,SS_Trait_SocConn_Avg7,SS_Trait_SHS_Avg7,
      SS_SocialCuriosity_Avg,SS_InteractionAnxiety_Avg,SS_Shy_Avg,SS_SelfEsteem_Avg,
      FU_Predict_NumToApp_Clean,FU_Predict_Reject_Clean,FU_Predict_ConvoLen_Clean,
      FU_Predict_ValAroConn_Avg,
      FU_General_Ability_Avg7,
      FU_HardStart,FU_HardStart_rev,FU_HardMaintain,FU_HardMaintain_rev,FU_HardEnd,FU_HardEnd_rev,
      FU_General_Awk_Avg7,FU_General_Enj_Avg7, FU_General_PartnerPerception_Avg7,'',
      FU_Strangers_Trust,FU_Strangers_FeelWarmly,FU_Strangers_FeelConnected,FU_notice_opportunities,
      '','',
      FU_NumConvos_LastWk_Clean_Trim2SD,FU_NumConvos_LastWk_Clean_Trim25SD,FU_NumConvos_LastWk_Clean_Trim3SD
    from dfs2_general_filtered")
nrow(dfs2_general_filtered_long)

dfs2_general_filtered_long$T_Condition2 <- as.factor(dfs2_general_filtered_long$T_Condition2)
dfs2_general_filtered_long$T_Condition2 <- revalue(dfs2_general_filtered_long$T_Condition2, c("0"="control","1"="treatment"))
dfs2_general_filtered_long$Timei <- as.numeric(dfs2_general_filtered_long$Time)
dfs2_general_filtered_long$Time <- as.factor(dfs2_general_filtered_long$Time)
dfs2_general_filtered_long$Time <- revalue(dfs2_general_filtered_long$Time, c("1"="start", "2"="end", "3"="followup"))


# for daily analyses
dfs2_daily_filtered <- sqldf("select * from dfs2_daily 
             where T_Pilot_0No1Yes=0 
             AND T_Completed_NumDays>=4
             AND ES_Honesty_NoMission=0 
             AND SS_Include_NoYes=1")
nrow(dfs2_daily_filtered)


dfs2_daily_filtered$RatingType <- as.factor(dfs2_daily_filtered$RatingType)
dfs2_daily_filtered$RatingType <- revalue(dfs2_daily_filtered$RatingType, c("0"="prediction", "1"="actual"))
dfs2_daily_filtered$Day <- as.numeric(dfs2_daily_filtered$Day)
dfs2_daily_filtered$Dayf <- as.factor(dfs2_daily_filtered$Day)
dfs2_daily_filtered$Dayf <- revalue(dfs2_daily_filtered$Dayf, c("1"="Monday", "2"="Tuesday", "3"="Wednesday", "4"="Thursday", "5"="Friday"))


# same as above, but including the dropouts
dfs2_daily_filtered_w_dropouts <- sqldf("select * from dfs2_daily 
             where T_Pilot_0No1Yes=0 
             AND ES_Honesty_NoMission=0 
             AND SS_Include_NoYes=1")
nrow(dfs2_daily_filtered_w_dropouts)


dfs2_daily_filtered_w_dropouts$RatingType <- as.factor(dfs2_daily_filtered_w_dropouts$RatingType)
dfs2_daily_filtered_w_dropouts$RatingType <- revalue(dfs2_daily_filtered_w_dropouts$RatingType, c("0"="prediction", "1"="actual"))
dfs2_daily_filtered_w_dropouts$Day <- as.numeric(dfs2_daily_filtered_w_dropouts$Day)
dfs2_daily_filtered_w_dropouts$Dayf <- as.factor(dfs2_daily_filtered_w_dropouts$Day)
dfs2_daily_filtered_w_dropouts$Dayf <- revalue(dfs2_daily_filtered_w_dropouts$Dayf, c("1"="Monday", "2"="Tuesday", "3"="Wednesday", "4"="Thursday", "5"="Friday"))




#####################################################
# Descriptives
#####################################################

# number of participants
nrow(dfs2_general_filtered)

# 0 = male, 1 = female, 2 = I do not identify with one of these labels, 3 = Prefer not to say
sqldf("select SS_Demog_SexFemale,count(*) from dfs2_general_filtered group by SS_Demog_SexFemale")

# age
ddply(dfs2_general_filtered, .(), summarize, M=mean(SS_Demog_Age, na.rm=TRUE), SD=sd(SS_Demog_Age, na.rm = TRUE))

# 1 = US, 2 = UK
sqldf("select Uni,count(*) from dfs2_general_filtered group by Uni")

# 0 = control, 1 = treatment
sqldf("select T_Condition2,count(*) from dfs2_general_filtered group by T_Condition2")

# how many conversations did we induce?
sqldf("select count(*) from dfs2_daily_filtered where T_Condition2=1 and RatingType='actual'")
# 1336



#####################################################
# General: Change in Fear of Rejection
#####################################################

# test for overdispersion; evidence of overdispersion means the data is a
# good candidate for negative binomial regression
# chisq <- pchisq(summary(mnb.general.reject)$AIC[4],summary(mnb.general.reject)$AIC[5],lower.tail=FALSE)
# cat(paste("\n test for dispersion: X2(",summary(mnb.general.reject)$AIC[5],") = ",summary(mnb.general.reject)$AIC[4],", p = ",signif(chisq[1], digits=4),"\n", sep=""))


mnb.general.reject <- glmer.nb(formula = Reject ~ T_Condition2*Time + (1|GooseChaseId_Fixed), data = dfs2_general_filtered_long)
# warnings are because negative binomial wants integers and we have some non-integer values 
# (e.g. 0.5) - results don't change if integers are forced
summary(mnb.general.reject)

# descriptive means
summaryBy(Reject ~ Time + T_Condition2, data = dfs2_general_filtered_long, FUN=c(mean,sd), na.rm=TRUE)

# marginal means
means.general.reject <- emmeans(mnb.general.reject, specs = ~ Time*T_Condition2, data=dfs2_general_filtered_long, type = "response")
means.general.reject

# paired contrasts
lsmlist <- contrast(means.general.reject, method = "pairwise", adjust = "none")
# run only specific contrasts: 
# start vs. end (13) and start vs. follow-up (14) for each condition
# (NOTE: end vs. follow-up for control (6), since not measured at start)
# treatment vs. control for each time (end: 8; follow-up:12)
# end, follow-up for control vs. start for treatment (7,10)
lsmlist <- lsmlist[c(6,7,8,10,12,13,14)]
mydiffs = update(lsmlist, pri.vars = "contrast", by.vars = NULL,adjust="mvt")
mydiffs

# confidence intervals on paired contrasts
confint(mydiffs, adjust = "mvt")


#####################################################
# General: other measures
#####################################################

# conversational ability
means.general.ability <- analyze_general(dfs2_general_filtered_long, "Ability")

# awkwardness
means.general.Awk <- analyze_general(dfs2_general_filtered_long, "Awk_Avg")

# enjoyment
means.general.Enj <- analyze_general(dfs2_general_filtered_long, "Enj_Avg")

# positive impression
means.general.posimp <- analyze_general(dfs2_general_filtered_long, "PartnerPerception")


# how many convos did they have w strangers?
x <- sqldf("Select * from dfs2_general_filtered_long where Time in ('start','followup')")
analyze_general(x, "NumConvos_LastWk_Clean_Trim3SD",2)

analyze_general(dfs2_general_filtered_long, "notice_opportunities")


# to compute effect sizes for start vs. end and start vs. follow-up, by condition
dfs2_general_filtered_c <- sqldf("select * from dfs2_general_filtered where T_Condition2=0")
cor(dfs2_general_filtered_c[c("SS_General_Ability_Avg7","ES_General_Ability_Avg7","FU_General_Ability_Avg7")] , use="complete.obs", method="pearson")
cor(dfs2_general_filtered_c[c("SS_General_Awk_Avg7","ES_General_Awk_Avg7","FU_General_Awk_Avg7")] , use="complete.obs", method="pearson")
cor(dfs2_general_filtered_c[c("SS_General_Enj_Avg7","ES_General_Enj_Avg7","FU_General_Enj_Avg7")] , use="complete.obs", method="pearson")
cor(dfs2_general_filtered_c[c("SS_General_PartnerPerception_Avg7","ES_General_PartnerPerception_Avg7","FU_General_PartnerPerception_Avg7")] , use="complete.obs", method="pearson")
cor(dfs2_general_filtered_c[c("SS_NumConvos_LastWk_Clean_Trim3SD","FU_NumConvos_LastWk_Clean_Trim3SD")] , use="complete.obs", method="pearson")
cor(dfs2_general_filtered_c[c("SS_notice_opportunities","ES_notice_opportunities","FU_notice_opportunities")] , use="complete.obs", method="pearson")

dfs2_general_filtered_e <- sqldf("select * from dfs2_general_filtered where T_Condition2=1")
cor(dfs2_general_filtered_e[c("SS_General_Ability_Avg7","ES_General_Ability_Avg7","FU_General_Ability_Avg7")] , use="complete.obs", method="pearson")
cor(dfs2_general_filtered_e[c("SS_General_Awk_Avg7","ES_General_Awk_Avg7","FU_General_Awk_Avg7")] , use="complete.obs", method="pearson")
cor(dfs2_general_filtered_e[c("SS_General_Enj_Avg7","ES_General_Enj_Avg7","FU_General_Enj_Avg7")] , use="complete.obs", method="pearson")
cor(dfs2_general_filtered_e[c("SS_General_PartnerPerception_Avg7","ES_General_PartnerPerception_Avg7","FU_General_PartnerPerception_Avg7")] , use="complete.obs", method="pearson")
cor(dfs2_general_filtered_e[c("SS_NumConvos_LastWk_Clean_Trim3SD","FU_NumConvos_LastWk_Clean_Trim3SD")] , use="complete.obs", method="pearson")
cor(dfs2_general_filtered_e[c("SS_notice_opportunities","ES_notice_opportunities","FU_notice_opportunities")] , use="complete.obs", method="pearson")



#####################################################
# Daily: Change in Fear of Rejection: Predictions vs. Experiences
#####################################################

mnb.daily.reject <- glmer.nb(formula = Reject_Clean ~ RatingType*Day + (1|GooseChaseId_Fixed), data = dfs2_daily_filtered)
summary(mnb.daily.reject)
confint(mnb.daily.reject)

summaryBy(Reject_Clean ~ Day + RatingType, data = dfs2_daily_filtered, FUN=c(mean,sd), na.rm=TRUE)
means.daily.reject <- emmeans(mnb.daily.reject, specs = ~ RatingType*Day, type = "response")
means.daily.reject

contr.all <- contrast(means.daily.reject, method = "pairwise", adjust = "none")
contr.all
confint(contr.all, adjust = "none")

#source for emtrends: https://stats.idre.ucla.edu/r/seminars/interactions-r/#s4b
test(emtrends(mnb.daily.reject, ~ RatingType, var="Day"))

#to get 95% CI
emtrends(mnb.daily.reject, ~ RatingType, var="Day")


# save emmeans with Day as a factor, for figures
mnb.daily.reject.categorical <- glmer.nb(formula = Reject_Clean ~ RatingType*Dayf + (1|GooseChaseId_Fixed), data = dfs2_daily_filtered)
means.daily.reject.categorical <- emmeans(mnb.daily.reject.categorical, specs = ~ RatingType*Dayf, type = "response")


# Descriptives re: rejection rate
sqldf("select Reject_Clean,count(*) from dfs2_daily_filtered where T_Condition2=1 and Day=1 and RatingType='prediction' group by Reject_Clean")
# 78/195=40%
sqldf("select Reject_Clean,count(*) from dfs2_daily_filtered where T_Condition2=1 and Day=1 and RatingType='actual' group by Reject_Clean")
# 260/(260+18+4+1)=92%




#####################################################
# Daily: Predictions vs. Experiences
# Change in Ability, PA, NA, positive impression
#####################################################

# ability
lm.daily.ability <- lmer(formula = Ability_Avg7 ~ RatingType*Day + (1|GooseChaseId_Fixed), data = dfs2_daily_filtered)
summary(lm.daily.ability)
confint(lm.daily.ability)

summaryBy(Ability_Avg7 ~ Day + RatingType, data = dfs2_daily_filtered, FUN=c(mean,sd), na.rm=TRUE)
means.daily.ability <- emmeans(lm.daily.ability, specs = ~ RatingType*Day, type = "response")
means.daily.ability

contr.all <- contrast(means.daily.ability, method = "pairwise", adjust = "none")
contr.all
confint(contr.all, adjust = "none")

#source for emtrends: https://stats.idre.ucla.edu/r/seminars/interactions-r/#s4b
test(emtrends(lm.daily.ability, ~ RatingType, var="Day"))

#to get 95% CI
emtrends(lm.daily.ability, ~ RatingType, var="Day")


# save emmeans with Day as a factor, for figures
lm.daily.ability.categorical <- lmer(formula = Ability_Avg7 ~ RatingType*Dayf + (1|GooseChaseId_Fixed), data = dfs2_daily_filtered)
means.daily.ability.categorical <- emmeans(lm.daily.ability.categorical, specs = ~ RatingType*Dayf, type = "response")



#####################################################
# awkwardness
lm.daily.Awk <- lmer(formula = Awk_Avg ~ RatingType*Day + (1|GooseChaseId_Fixed), data = dfs2_daily_filtered)
summary(lm.daily.Awk)
confint(lm.daily.Awk)

summaryBy(Awk_Avg ~ Day + RatingType, data = dfs2_daily_filtered, FUN=c(mean,sd), na.rm=TRUE)
means.daily.Awk <- emmeans(lm.daily.Awk, specs = ~ RatingType*Day, type = "response")
means.daily.Awk

contr.all <- contrast(means.daily.Awk, method = "pairwise", adjust = "none")
contr.all
confint(contr.all, adjust = "none")

#source for emtrends: https://stats.idre.ucla.edu/r/seminars/interactions-r/#s4b
test(emtrends(lm.daily.Awk, ~ RatingType, var="Day"))

#to get 95% CI
emtrends(lm.daily.Awk, ~ RatingType, var="Day")


# save emmeans with Day as a factor, for figures
lm.daily.Awk.categorical <- lmer(formula = Awk_Avg ~ RatingType*Dayf + (1|GooseChaseId_Fixed), data = dfs2_daily_filtered)
means.daily.Awk.categorical <- emmeans(lm.daily.Awk.categorical, specs = ~ RatingType*Dayf, type = "response")


#####################################################
# enjoyment
lm.daily.Enj <- lmer(formula = Enj_Avg ~ RatingType*Day + (1|GooseChaseId_Fixed), data = dfs2_daily_filtered)
summary(lm.daily.Enj)
confint(lm.daily.Enj)

summaryBy(Enj_Avg ~ Day + RatingType, data = dfs2_daily_filtered, FUN=c(mean,sd), na.rm=TRUE)
means.daily.Enj <- emmeans(lm.daily.Enj, specs = ~ RatingType*Day, type = "response")
means.daily.Enj

contr.all <- contrast(means.daily.Enj, method = "pairwise", adjust = "none")
contr.all
confint(contr.all, adjust = "none")

#source for emtrends: https://stats.idre.ucla.edu/r/seminars/interactions-r/#s4b
test(emtrends(lm.daily.Enj, ~ RatingType, var="Day"))

#to get 95% CI
emtrends(lm.daily.Enj, ~ RatingType, var="Day")


# save emmeans with Day as a factor, for figures
lm.daily.Enj.categorical <- lmer(formula = Enj_Avg ~ RatingType*Dayf + (1|GooseChaseId_Fixed), data = dfs2_daily_filtered)
means.daily.Enj.categorical <- emmeans(lm.daily.Enj.categorical, specs = ~ RatingType*Dayf, type = "response")



#####################################################
# positive impression
lm.daily.posimp <- lmer(formula = PartnerPerceptions_Avg ~ RatingType*Day + (1|GooseChaseId_Fixed), data = dfs2_daily_filtered)
summary(lm.daily.posimp)
confint(lm.daily.posimp)

summaryBy(PartnerPerceptions_Avg ~ Day + RatingType, data = dfs2_daily_filtered, FUN=c(mean,sd), na.rm=TRUE)
means.daily.posimp <- emmeans(lm.daily.posimp, specs = ~ RatingType*Day, type = "response")
means.daily.posimp

contr.all <- contrast(means.daily.posimp, method = "pairwise", adjust = "none")
contr.all
confint(contr.all, adjust = "none")

#source for emtrends: https://stats.idre.ucla.edu/r/seminars/interactions-r/#s4b
test(emtrends(lm.daily.posimp, ~ RatingType, var="Day"))

#to get 95% CI
emtrends(lm.daily.posimp, ~ RatingType, var="Day")


# save emmeans with Day as a factor, for figures
lm.daily.posimp.categorical <- lmer(formula = PartnerPerceptions_Avg ~ RatingType*Dayf + (1|GooseChaseId_Fixed), data = dfs2_daily_filtered)
means.daily.posimp.categorical <- emmeans(lm.daily.posimp.categorical, specs = ~ RatingType*Dayf, type = "response")





#####################################################
# General Discussion
#####################################################


# gamification (not reported)
# how many participants in each condition reported noticing their performance
# compared to others?
# sqldf("select T_Condition2,ES_Honesty_Ranking,count(*) from dfs2_general_filtered group by T_Condition2,ES_Honesty_Ranking")
# 26/(26+59+3) = 30%
# 51/(51+142+5) = 26%

# how many rejections were there
sqldf("select Reject_Clean,count(*) 
      from dfs2_daily_filtered 
      where T_Condition2=1 
      and RatingType='actual' 
      group by Reject_Clean")

# 87%
1164/1336

# how many people were in touch w someone they met during the study (details in som)
sqldf("select count(distinct GooseChaseId_Fixed)
      from  dfs2_general_filtered 
      where T_Condition2=1 
      and   ES_Num_Communicated_Clean > 0")

# 81

# 41%
81/198





#####################################################
# Figures
#####################################################
# Figure 3: 
# I'm having trouble getting the legend to have colours and linetypes when there are error bars, 
# so this simplified plot is just so I can save the legend
plot_legend_general <- get_legend(ggplot(as.data.frame(means.general.ability), aes(x = Time, y = emmean, group=T_Condition2)) + 
                            geom_line(aes(color = T_Condition2, linetype=T_Condition2)) + geom_point(aes(color = T_Condition2)) + 
                            scale_color_manual("Condition",values=c('#999999','#E69F00'),labels=c("Control","Treatment")) + 
                            scale_linetype_manual("Condition",values=c("dashed","solid"),labels=c("Control","Treatment")))

# Plot A: Rejection
plot_general_reject <- ggplot(as.data.frame(means.general.reject), aes(x = Time, y = response, group=T_Condition2)) + 
  geom_line(aes(color = T_Condition2, linetype=T_Condition2)) + geom_point(aes(color = T_Condition2)) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL, colour = T_Condition2), width=.1) + 
  scale_color_manual("Condition",values=c('#999999','#E69F00'),labels=c("Control","Treatment")) +
  scale_linetype_manual("Condition",values=c("dashed","solid"),labels=c("Control","Treatment")) +
  theme_classic(base_size = 15) + theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Perceived Likelihood of Rejection")+
  scale_x_discrete("Time",labels=c("start"="Start of\nStudy", "end"="End of\nStudy", "followup"="Follow-up\n(1 week)")) +
  scale_y_continuous(limits=c(0,1.6))


# Plot B: Conversational ability by study phase (start/end/follow-up talkers vs. observers)
plot_general_ability <- ggplot(as.data.frame(means.general.ability), aes(x = Time, y = emmean, group=T_Condition2)) + 
  geom_line(aes(color = T_Condition2, linetype=T_Condition2)) + geom_point(aes(color = T_Condition2)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, colour = T_Condition2), linetype="solid", width=.3,position=position_dodge(0.07)) +
  scale_color_manual("Condition",values=c('#999999','#E69F00'),labels=c("Control","Treatment")) + 
  scale_linetype_manual("Condition",values=c("dashed","solid"),labels=c("Control","Treatment")) +
  theme_classic(base_size = 15) + theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Perceived Conversational Ability")+
  scale_x_discrete(labels = c("start"="Start of\nStudy", "end"="End of\nStudy", "followup"="Follow-up\n(1 week)")) +
  scale_y_continuous(limits=c(3,5))


# g <- grid.arrange(plot_general_reject + theme(legend.position = "none"),
g <- arrangeGrob(plot_general_reject + theme(legend.position = "none",axis.title.x = element_blank()) + ylab("Predicted Number of Rejections"),
             plot_general_ability + theme(legend.position = "none",axis.title.x = element_blank()) + ylab("Ability"),
             plot_legend_general,
             widths = c(3,3,1), nrow = 1)

ggsave(paste(dFigures,"fig3.jpg", sep=""),g, dpi = 600,width=11, height=5)





#####################################################
# Figure 4: 
# Plot A: awkwardness
plot_general_Awk <- ggplot(as.data.frame(means.general.Awk), aes(x = Time, y = emmean, group=T_Condition2)) + 
  geom_line(aes(color = T_Condition2, linetype=T_Condition2)) + geom_point(aes(color = T_Condition2)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, colour = T_Condition2), width=.1) + 
  scale_color_manual("Condition",values=c('#999999','#E69F00'),labels=c("Control","Treatment")) +
  scale_linetype_manual("Condition",values=c("dashed","solid"),labels=c("Control","Treatment")) +
  theme_classic(base_size = 15) + theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Akwardness")+
  scale_x_discrete("Time",labels=c("start"="Start of\nStudy", "end"="End of\nStudy", "followup"="Follow-up\n(1 week)")) +
  scale_y_continuous(limits=c(3,5))


# Plot B: enjoyment
plot_general_Enj <- ggplot(as.data.frame(means.general.Enj), aes(x = Time, y = emmean, group=T_Condition2)) + 
  geom_line(aes(color = T_Condition2, linetype=T_Condition2)) + geom_point(aes(color = T_Condition2)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, colour = T_Condition2), 
                width=.3,position=position_dodge(0.07)) +
  scale_color_manual("Condition",values=c('#999999','#E69F00'),labels=c("Control","Treatment")) + theme(legend.position = "none") +
  scale_linetype_manual("Condition",values=c("dashed","solid"),labels=c("Control","Treatment")) +
  theme_classic(base_size = 15) + theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Enjoyment")+
  scale_x_discrete(labels = c("start"="Start of\nStudy", "end"="End of\nStudy", "followup"="Follow-up\n(1 week)")) +
  scale_y_continuous(limits=c(3,5))


# Plot C: Positive impression
plot_general_posimp <- ggplot(as.data.frame(means.general.posimp), aes(x = Time, y = emmean, group=T_Condition2)) + 
  geom_line(aes(color = T_Condition2, linetype=T_Condition2)) + geom_point(aes(color = T_Condition2)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, colour = T_Condition2), 
                width=.3,position=position_dodge(0.07)) +
  scale_color_manual("Condition",values=c('#999999','#E69F00'),labels=c("Control","Treatment")) + theme(legend.position = "none") +
  scale_linetype_manual("Condition",values=c("dashed","solid"),labels=c("Control","Treatment")) +
  theme_classic(base_size = 15) + theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Positive Impression")+
  scale_x_discrete(labels = c("start"="Start of\nStudy", "end"="End of\nStudy", "followup"="Follow-up\n(1 week)")) +
  scale_y_continuous(limits=c(3,5))


g <- arrangeGrob(plot_general_Awk + theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank()),
                 plot_general_Enj + theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank()),
                 plot_general_posimp + theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank()),
                 plot_legend_general,
                 widths = c(3,3,3,1.5), nrow=1)

ggsave(paste(dFigures,"fig4.jpg", sep=""),g, dpi = 600,width=11, height=5)





#####################################################
# Figure 5: 
# I'm having trouble getting the legend to have colours and linetypes when there are error bars, 
# so this simplified plot is just so I can save the legend
plot_legend_daily <- get_legend(ggplot(as.data.frame(means.daily.ability.categorical), aes(x = Dayf, y = emmean, group=RatingType)) +  
  geom_line(aes(color = RatingType, linetype=RatingType)) + geom_point(aes(color = RatingType)) + 
  scale_color_manual("Rating Type",values=c('dodgerblue4','#E69F00'),labels=c("Predicted","Actual")) +
  scale_linetype_manual("Rating Type",values=c("dashed","solid"),labels=c("Predicted","Actual")))

# Panel A: Rejection by day, talkers' daily predictions vs. experience
plot_daily_reject <- ggplot(as.data.frame(means.daily.reject.categorical), aes(x = Dayf, y = response, group=RatingType)) +  
  geom_line(aes(color = RatingType, linetype=RatingType)) + geom_point(aes(color = RatingType)) + 
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL, colour = RatingType), width=.1) +
  scale_color_manual("Rating Type",values=c('dodgerblue4','#E69F00'),labels=c("Predicted","Actual")) +
  scale_linetype_manual("Rating Type",values=c("dashed","solid"),labels=c("Predicted","Actual")) +
  theme_classic(base_size = 15) + theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Predicted and Actual Rejection")+
  scale_x_discrete("Time",labels=c("Monday"="Mon", "Tuesday"="Tues", "Wednesday"="Wed", "Thursday"="Thurs", "Friday"="Fri")) +
  scale_y_continuous(limits=c(0,1))


# Panel B: Ability by day, talkers' daily predictions vs. experience
plot_daily_ability <- ggplot(as.data.frame(means.daily.ability.categorical), aes(x = Dayf, y = emmean, group=RatingType)) +  
  geom_line(aes(color = RatingType, linetype=RatingType)) + geom_point(aes(color = RatingType)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, colour = RatingType), width=.1) +
  scale_color_manual("Rating Type",values=c('dodgerblue4','#E69F00'),labels=c("Predicted","Actual")) +
  scale_linetype_manual("Rating Type",values=c("dashed","solid"),labels=c("Predicted","Actual")) +
  theme_classic(base_size = 15) + theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Predicted and Actual Conversational Ability")+
  scale_x_discrete("Time",labels=c("Monday"="Mon", "Tuesday"="Tues", "Wednesday"="Wed", "Thursday"="Thurs", "Friday"="Fri")) +
  scale_y_continuous(limits=c(3,6))

g <- grid.arrange(plot_daily_reject + theme(legend.position = "none",axis.title.x = element_blank()) + ylab("Number of Rejections"),
             plot_daily_ability + theme(legend.position = "none",axis.title.x = element_blank()) + ylab("Ability"),
             plot_legend_daily,
             widths = c(3,3,1), nrow = 1)

ggsave(paste(dFigures,"fig5.jpg", sep=""),g, dpi = 600,width=11, height=5)





#####################################################
# Figure 6: 
# Plot A: awkwardness
plot_daily_Awk <- ggplot(as.data.frame(means.daily.Awk.categorical), aes(x = Dayf, y = emmean, group=RatingType)) +
  geom_line(aes(color = RatingType, linetype=RatingType)) + geom_point(aes(color = RatingType)) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, colour = RatingType), width=.1) +
  scale_color_manual(values=c('dodgerblue4','#E69F00'),labels=c("Predicted","Actual")) +
  scale_linetype_manual("Rating Type",values=c("dashed","solid"),labels=c("Predicted","Actual")) +
  theme_classic(base_size = 15) + theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Awkwardness")+labs(color="Rating Type") +
  scale_x_discrete("Time",labels=c("Monday"="Mon", "Tuesday"="Tues", "Wednesday"="Wed", "Thursday"="Thurs", "Friday"="Fri")) +
  scale_y_continuous(limits=c(2,4))

# Plot B: enjoyment
plot_daily_Enj <- ggplot(as.data.frame(means.daily.Enj.categorical), aes(x = Dayf, y = emmean, group=RatingType)) +
  geom_line(aes(color = RatingType, linetype=RatingType)) + geom_point(aes(color = RatingType)) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, colour = RatingType), width=.1) +
  scale_color_manual(values=c('dodgerblue4','#E69F00'),labels=c("Predicted","Actual")) +
  scale_linetype_manual("Rating Type",values=c("dashed","solid"),labels=c("Predicted","Actual")) +
  theme_classic(base_size = 15) + theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Enjoyment")+labs(color="Rating Type") +
  scale_x_discrete("Time",labels=c("Monday"="Mon", "Tuesday"="Tues", "Wednesday"="Wed", "Thursday"="Thurs", "Friday"="Fri")) +
  scale_y_continuous(limits=c(2,4))

# Plot C: Positive impression
plot_daily_posimps <- ggplot(as.data.frame(means.daily.posimp.categorical), aes(x = Dayf, y = emmean, group=RatingType)) +
  geom_line(aes(color = RatingType, linetype=RatingType)) + geom_point(aes(color = RatingType)) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, colour = RatingType), width=.1) +
  scale_color_manual(values=c('dodgerblue4','#E69F00'),labels=c("Predicted","Actual")) +
  scale_linetype_manual("Rating Type",values=c("dashed","solid"),labels=c("Predicted","Actual")) +
  theme_classic(base_size = 15) + theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Positive Impression")+labs(color="Rating Type") +
  scale_x_discrete("Time",labels=c("Monday"="Mon", "Tuesday"="Tues", "Wednesday"="Wed", "Thursday"="Thurs", "Friday"="Fri")) +
  scale_y_continuous(limits=c(2,4))

g2 <- grid.arrange(plot_daily_Awk + theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank()),
                  plot_daily_Enj + theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank()),
                  plot_daily_posimps + theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank()),
                  plot_legend_daily,
                  widths = c(3,3,3,1.5), nrow = 1)

ggsave(paste(dFigures,"fig6.jpg", sep=""),g2, dpi = 600,width=11, height=5)



#############################################
# Supplemental Material
#############################################


# SOM: Participants section
# correlations for sensitivity analysis
my_cor(dfs2_general_filtered,"SS_Predict_Reject_Clean","ES_Predict_Reject_Clean")
my_cor(dfs2_general_filtered,"SS_Predict_Reject_Clean","FU_Predict_Reject_Clean")
my_cor(dfs2_general_filtered,"ES_Predict_Reject_Clean","FU_Predict_Reject_Clean")

my_cor(dfs2_general_filtered,"SS_General_Ability_Avg7","ES_General_Ability_Avg7")
my_cor(dfs2_general_filtered,"SS_General_Ability_Avg7","FU_General_Ability_Avg7")
my_cor(dfs2_general_filtered,"ES_General_Ability_Avg7","FU_General_Ability_Avg7")



# SOM: Data Preparation
# test for differences at start of study between universities 
# Uni: U.S. = 1; U.K. = 2

# dummy vector
v <- vector()
my_ind_ttest(dfs2_general_filtered,"dfs2_general_filtered","SS_Predict_Reject_Clean","Uni",1,2,v)
my_ind_ttest(dfs2_general_filtered,"dfs2_general_filtered","SS_General_Ability_Avg7","Uni",1,2,v)
my_ind_ttest(dfs2_general_filtered,"dfs2_general_filtered","SS_General_Awk_Avg7","Uni",1,2,v)
my_ind_ttest(dfs2_general_filtered,"dfs2_general_filtered","SS_General_Enj_Avg7","Uni",1,2,v)
my_ind_ttest(dfs2_general_filtered,"dfs2_general_filtered","SS_General_PartnerPerception_Avg7","Uni",1,2,v)

# test for differences at start of study between tips (2) and no tips (1) conditions
dfs2_daily_filtered_treatmentonly_day1 <- sqldf("select * from dfs2_daily_filtered where T_Condition3 in(1,2) and RatingType = 'prediction' and Day = 1")
my_ind_ttest(dfs2_daily_filtered_treatmentonly_day1,"dfs2_daily_filtered_treatmentonly_day1","Reject_Clean","T_Condition3",1,2,v)
my_ind_ttest(dfs2_daily_filtered_treatmentonly_day1,"dfs2_daily_filtered_treatmentonly_day1","Ability_Avg7","T_Condition3",1,2,v)
my_ind_ttest(dfs2_daily_filtered_treatmentonly_day1,"dfs2_daily_filtered_treatmentonly_day1","Awk_Avg","T_Condition3",1,2,v)
my_ind_ttest(dfs2_daily_filtered_treatmentonly_day1,"dfs2_daily_filtered_treatmentonly_day1","Enj_Avg","T_Condition3",1,2,v)
my_ind_ttest(dfs2_daily_filtered_treatmentonly_day1,"dfs2_daily_filtered_treatmentonly_day1","PartnerPerceptions_Avg","T_Condition3",1,2,v)





#####################################################
# Analyses to examine drop-outs and exclusions
#####################################################

# for intent to treat analyses on general surveys
# change wide to long
dfs2_general_long <- sqldf("
    select 1 as Time, GooseChaseId_Fixed, Uni,T_Condition2,T_Condition3,SS_Demog_Age,SS_Demog_SexFemale,
      SS_Predict_Reject_Clean as Reject,
      SS_General_Ability_Avg7 as Ability,
      SS_General_Awk_Avg7 as Awk_Avg,SS_General_Enj_Avg7 as Enj_Avg, 
      SS_General_PartnerPerception_Avg7 as PartnerPerception
    from dfs2_general
    union 
    select 2 as Time, GooseChaseId_Fixed, Uni,T_Condition2,T_Condition3,SS_Demog_Age,SS_Demog_SexFemale,
      ES_Predict_Reject_Clean, ES_General_Ability_Avg7,
      ES_General_Awk_Avg7,ES_General_Enj_Avg7, ES_General_PartnerPerception_Avg7
    from dfs2_general
    union 
      select 3 as Time, GooseChaseId_Fixed, Uni,T_Condition2,T_Condition3,SS_Demog_Age,SS_Demog_SexFemale,
      FU_Predict_Reject_Clean,FU_General_Ability_Avg7,
      FU_General_Awk_Avg7,FU_General_Enj_Avg7, FU_General_PartnerPerception_Avg7
    from dfs2_general")
nrow(dfs2_general_long)

dfs2_general_long$T_Condition2 <- as.factor(dfs2_general_long$T_Condition2)
dfs2_general_long$T_Condition2 <- revalue(dfs2_general_long$T_Condition2, c("0"="control","1"="treatment"))
dfs2_general_long$Timei <- as.numeric(dfs2_general_long$Time)
dfs2_general_long$Time <- as.factor(dfs2_general_long$Time)
dfs2_general_long$Time <- revalue(dfs2_general_long$Time, c("1"="start", "2"="end", "3"="followup"))


#####################################################
# Descriptives
#####################################################
# number of participants
nrow(dfs2_general_filtered)

# 0 = male, 1 = female, 2 = I do not identify with one of these labels, 3 = Prefer not to say
sqldf("select SS_Demog_SexFemale,count(*) from dfs2_general group by SS_Demog_SexFemale")

# age
ddply(dfs2_general, .(), summarize, M=mean(SS_Demog_Age, na.rm=TRUE), SD=sd(SS_Demog_Age, na.rm = TRUE))

# 1 = US, 2 = UK
sqldf("select Uni,count(*) from dfs2_general group by Uni")

# 0 = control, 1 = treatment
sqldf("select T_Condition2,count(*) from dfs2_general group by T_Condition2")


#####################################################
# "Intent to treat": General: Change in Fear of Rejection
# (use dfs2_general instead of dfs2_general_filtered_long)
#####################################################

mnb.general.reject.intent <- glmer.nb(formula = Reject ~ T_Condition2*Time + (1|GooseChaseId_Fixed), data = dfs2_general_long)
# warnings are because negative binomial wants integers and we have some non-integer values 
# (e.g. 0.5) - results don't change if integers are forced
summary(mnb.general.reject.intent)

# descriptive means
summaryBy(Reject ~ Time + T_Condition2, data = dfs2_general_long, FUN=c(mean,sd), na.rm=TRUE)

# marginal means
means.general.reject.intent <- emmeans(mnb.general.reject.intent, specs = ~ Time*T_Condition2, data=dfs2_general_long, type = "response")
means.general.reject.intent

# paired contrasts
lsmlist <- contrast(means.general.reject.intent, method = "pairwise", adjust = "none")
# run only specific contrasts: 
# start vs. end (13) and start vs. follow-up (14) for each condition
# (NOTE: end vs. follow-up for control (6), since not measured at start)
# treatment vs. control for each time (end: 8; follow-up:12)
# end, follow-up for control vs. start for treatment (7,10)
lsmlist <- lsmlist[c(6,7,8,10,12,13,14)]
mydiffs = update(lsmlist, pri.vars = "contrast", by.vars = NULL,adjust="mvt")
mydiffs

# confidence intervals on paired contrasts
confint(mydiffs, adjust = "mvt")


#####################################################
# "Intent to treat": General: other measures
#####################################################

# conversational ability
means.general.ability <- analyze_general(dfs2_general_long, "Ability")

# awkwardness
means.general.Awk <- analyze_general(dfs2_general_long, "Awk_Avg")

# enjoyment
means.general.Enj <- analyze_general(dfs2_general_long, "Enj_Avg")

# positive impression
means.general.posimp <- analyze_general(dfs2_general_long, "PartnerPerception")



#####################################################
# Drop-outs vs. completions (treatment group)
#####################################################
# find the first day when the participant reported a post-conversation rating
dfFirstDay <- sqldf("select GooseChaseId_Fixed,Min(Day) as MinDay from dfs2_daily where RatingType=1 group by GooseChaseId_Fixed")
nrow(dfFirstDay)
sqldf("select MinDay,count(*) from dfFirstDay group by MinDay")

# select the responses on the first day, and average across them if >1
dfTreatFirstDay <- sqldf("
    select x.GooseChaseId_Fixed,x.T_Condition2,x.T_Completed_AtLeast4_No0Yes1,x.Day,
      avg(x.Reject_Clean) as Reject_Clean,
      avg(x.Ability_Avg7) as Ability_Avg,
      avg(x.Awk_Avg) as Awk_Avg,
      avg(x.Enj_Avg) as Enj_Avg,
      avg(x.PartnerPerceptions_Avg) as PartnerPerceptions_Avg,
      avg(x.DV_Avg_z) as DV_Avg_z
    from dfs2_daily x, dfFirstDay d
    where x.GooseChaseId_Fixed = d.GooseChaseId_Fixed
    and x.Day = d.MinDay
    and x.T_Condition2=1
    and x.RatingType=1
    group by x.GooseChaseId_Fixed")
nrow(dfTreatFirstDay)

sqldf("select T_Completed_AtLeast4_No0Yes1,count(*) from dfTreatFirstDay group by T_Completed_AtLeast4_No0Yes1")

# compare drop-outs to completions
# load a vector with the p-values so we can make a correction for multiple comparisons
v <- vector()
v <- my_ind_ttest(dfTreatFirstDay, "dfTreatFirstDay", "Reject_Clean", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatFirstDay, "dfTreatFirstDay", "Ability_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatFirstDay, "dfTreatFirstDay", "Awk_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatFirstDay, "dfTreatFirstDay", "Enj_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatFirstDay, "dfTreatFirstDay", "PartnerPerceptions_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
# test composite measures (average of the 5 DV's, standardized)
v <- my_ind_ttest(dfTreatFirstDay, "dfTreatFirstDay", "DV_Avg_z", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)







# join to start of study survey to get demogs, personality, initial predictions
dfTreatFirstDayPlus <- sqldf("
    select f.*, g.SS_Demog_Age, g.SS_Demog_SexFemale,
      g.SS_Trait_SocConn_Avg7 as SS_Trait_SocConn_Avg,
      g.SS_Trait_SHS_Avg7 as SS_Trait_SHS_Avg,
      g.SS_SocialCuriosity_Avg,g.SS_InteractionAnxiety_Avg,
      g.SS_Shy_Avg,g.SS_SelfEsteem_Avg,    
      g.SS_Predict_Reject_Clean, g.SS_General_Ability_Avg7,
      g.SS_General_Awk_Avg7 as SS_General_Awk_Avg, 
      g.SS_General_Enj_Avg7 as SS_General_Enj_Avg, 
      g.SS_General_PartnerPerception_Avg7 as SS_General_PartnerPerception_Avg,
      g.SS_DV_Avg_z
    from dfTreatFirstDay f, dfs2_general g
    where f.GooseChaseId_Fixed = g.GooseChaseId_Fixed")
nrow(dfTreatFirstDayPlus)

# compare drop-outs to completions
# demogs
sqldf("select T_Completed_AtLeast4_No0Yes1, SS_Demog_SexFemale,count(*) from dfTreatFirstDayPlus group by T_Completed_AtLeast4_No0Yes1, SS_Demog_SexFemale")
v <- my_ind_ttest(dfTreatFirstDayPlus, "dfTreatFirstDayPlus", "SS_Demog_Age", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)

# personality
v <- my_ind_ttest(dfTreatFirstDayPlus, "dfTreatFirstDayPlus", "SS_Trait_SocConn_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatFirstDayPlus, "dfTreatFirstDayPlus", "SS_Trait_SHS_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatFirstDayPlus, "dfTreatFirstDayPlus", "SS_SocialCuriosity_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatFirstDayPlus, "dfTreatFirstDayPlus", "SS_InteractionAnxiety_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatFirstDayPlus, "dfTreatFirstDayPlus", "SS_Shy_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatFirstDayPlus, "dfTreatFirstDayPlus", "SS_SelfEsteem_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)

# start of study predictions
v <- my_ind_ttest(dfTreatFirstDayPlus, "dfTreatFirstDayPlus", "SS_Predict_Reject_Clean", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatFirstDayPlus, "dfTreatFirstDayPlus", "SS_General_Ability_Avg7", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatFirstDayPlus, "dfTreatFirstDayPlus", "SS_General_Awk_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatFirstDayPlus, "dfTreatFirstDayPlus", "SS_General_Enj_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatFirstDayPlus, "dfTreatFirstDayPlus", "SS_General_PartnerPerception_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
# test composite measures (average of the 5 DV's, standardized)
v <- my_ind_ttest(dfTreatFirstDayPlus, "dfTreatFirstDayPlus", "SS_DV_Avg_z", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)

# correct the p values for multiple comparisons
v 
v <- sort(v, decreasing=FALSE)
p.adjust(v, method="holm",n=length(v))





##### DAY 2 #####
# find the second day when the participant reported a post-conversation rating
dfSecondDay <- sqldf("select GooseChaseId_Fixed,Min(Day) as MinDay 
                     from dfs2_daily d
                     where RatingType=1 
                     and Day > (select MinDay from dfFirstDay f where f.GooseChaseId_Fixed = d.GooseChaseId_Fixed)
                     group by GooseChaseId_Fixed")
nrow(dfSecondDay)
sqldf("select MinDay,count(*) from dfSecondDay group by MinDay")

# select the responses on the second day, and average across them if >1
dfTreatSecondDay <- sqldf("
    select x.GooseChaseId_Fixed,x.T_Condition2,x.T_Completed_AtLeast4_No0Yes1,
      avg(x.Reject_Clean) as Reject_Clean,
      avg(x.Ability_Avg7) as Ability_Avg,
      avg(x.Awk_Avg) as Awk_Avg,
      avg(x.Enj_Avg) as Enj_Avg,
      avg(x.PartnerPerceptions_Avg) as PartnerPerceptions_Avg,
      avg(x.DV_Avg_z) as DV_Avg_z
    from dfs2_daily x, dfSecondDay d
    where x.GooseChaseId_Fixed = d.GooseChaseId_Fixed
    and x.Day = d.MinDay
    and x.T_Condition2=1
    and x.RatingType=1
    group by x.GooseChaseId_Fixed")
nrow(dfTreatSecondDay)

sqldf("select T_Completed_AtLeast4_No0Yes1,count(*) from dfTreatSecondDay group by T_Completed_AtLeast4_No0Yes1")

# compare drop-outs to completions
# load a vector with the p-values so we can make a correction for multiple comparisons
v <- vector()
v <- my_ind_ttest(dfTreatSecondDay, "dfTreatSecondDay", "Reject_Clean", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatSecondDay, "dfTreatSecondDay", "Ability_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatSecondDay, "dfTreatSecondDay", "Awk_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatSecondDay, "dfTreatSecondDay", "Enj_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatSecondDay, "dfTreatSecondDay", "PartnerPerceptions_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatSecondDay, "dfTreatSecondDay", "DV_Avg_z", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)





##### DAY 3 #####
# find the third day when the participant reported a post-conversation rating
dfThirdDay <- sqldf("select GooseChaseId_Fixed,Min(Day) as MinDay 
                     from dfs2_daily d
                     where RatingType=1 
                     and Day > (select MinDay from dfSecondDay f where f.GooseChaseId_Fixed = d.GooseChaseId_Fixed)
                     group by GooseChaseId_Fixed")
nrow(dfThirdDay)
sqldf("select MinDay,count(*) from dfThirdDay group by MinDay")


# select the responses on the third day, and average across them if >1
dfTreatThirdDay <- sqldf("
    select x.GooseChaseId_Fixed,x.T_Condition2,x.T_Completed_AtLeast4_No0Yes1,
      avg(x.Reject_Clean) as Reject_Clean,
      avg(x.Ability_Avg7) as Ability_Avg,
      avg(x.Awk_Avg) as Awk_Avg,
      avg(x.Enj_Avg) as Enj_Avg,
      avg(x.PartnerPerceptions_Avg) as PartnerPerceptions_Avg,
      avg(x.DV_Avg_z) as DV_Avg_z
    from dfs2_daily x, dfThirdDay d
    where x.GooseChaseId_Fixed = d.GooseChaseId_Fixed
    and x.Day = d.MinDay
    and x.T_Condition2=1
    and x.RatingType=1
    group by x.GooseChaseId_Fixed")
nrow(dfTreatThirdDay)

sqldf("select T_Completed_AtLeast4_No0Yes1,count(*) from dfTreatThirdDay group by T_Completed_AtLeast4_No0Yes1")


# compare drop-outs to completions
# load a vector with the p-values so we can make a correction for multiple comparisons
v <- vector()
v <- my_ind_ttest(dfTreatThirdDay, "dfTreatThirdDay", "Reject_Clean", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatThirdDay, "dfTreatThirdDay", "Ability_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatThirdDay, "dfTreatThirdDay", "Awk_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatThirdDay, "dfTreatThirdDay", "Enj_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatThirdDay, "dfTreatThirdDay", "PartnerPerceptions_Avg", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)
v <- my_ind_ttest(dfTreatThirdDay, "dfTreatThirdDay", "DV_Avg_z", "T_Completed_AtLeast4_No0Yes1", 0, 1, v)





#####################################################
# Mentioning the scavhunt
#####################################################
# total number of daily records
sqldf("select count(*) from dfs2_daily_filtered where T_Condition2 = 1 and RatingType = 'actual'")
# total number of daily records where the participant answered the q about
# whether or not they mentioned the scavhunt
sqldf("select count(*) from dfs2_daily_filtered where T_Condition2 = 1 and RatingType = 'actual' and MentionedScavHunt is not null")
# 433/1336 = 32%

dfs2_daily_filtered_treatmentonly <- sqldf("select * from dfs2_daily_filtered where T_Condition2 = 1 and RatingType = 'actual' and MentionedScavHunt is not null")
sqldf("select count(distinct GooseChaseId_Fixed) from dfs2_daily_filtered_treatmentonly")
# 177 (out of 198 - the rest didn't answer the question)

# how many daily reports mentioned the scavhunt?
sqldf("select count(*) from dfs2_daily_filtered_treatmentonly")
sqldf("select count(*) from dfs2_daily_filtered_treatmentonly where MentionedScavHunt=1")
# 103/433 = 24%

# how many participants ever mentioned the scavhunt?
sqldf("select count(distinct GooseChaseId_Fixed) from dfs2_daily_filtered_treatmentonly")
sqldf("select count(distinct GooseChaseId_Fixed) from dfs2_daily_filtered_treatmentonly where MentionedScavHunt=1")
# 61/177 = 34%

# which days did people mention the scavhunt?
sqldf("select Day,count(*) from dfs2_daily_filtered_treatmentonly group by Day")
sqldf("select Day,count(*) from dfs2_daily_filtered_treatmentonly where MentionedScavHunt=1 group by Day")
# 1: 32/93 = 34%
# 2: 16/95 = 17%
# 3: 15/68 = 22%
# 4: 18/80 = 23%
# 5: 22/97 = 23%

# how many people have at least one mention and one no mention
sqldf("
  select count(distinct GooseChaseId_Fixed) 
  from dfs2_daily_filtered_treatmentonly t
  where exists(
    select null from dfs2_daily_filtered_treatmentonly t2
    where t2.GooseChaseId_Fixed = t.GooseChaseId_Fixed
      and t2.MentionedScavHunt = 1)
  and exists(
    select null from dfs2_daily_filtered_treatmentonly t2
    where t2.GooseChaseId_Fixed = t.GooseChaseId_Fixed
    and t2.MentionedScavHunt = 0)")
# 35

v2 <- vector()
my_ind_ttest(dfs2_daily_filtered_treatmentonly, "dfs2_daily_filtered_treatmentonly", "Ability_Avg7", "MentionedScavHunt", 0, 1, v2)
my_ind_ttest(dfs2_daily_filtered_treatmentonly, "dfs2_daily_filtered_treatmentonly", "Awk_Avg", "MentionedScavHunt", 0, 1, v2)
my_ind_ttest(dfs2_daily_filtered_treatmentonly, "dfs2_daily_filtered_treatmentonly", "Enj_Avg", "MentionedScavHunt", 0, 1, v2)
my_ind_ttest(dfs2_daily_filtered_treatmentonly, "dfs2_daily_filtered_treatmentonly", "PartnerPerceptions_Avg", "MentionedScavHunt", 0, 1, v2)

