#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#
#---------------------     Investigating how administrative burden and search costs        -----------------------------------#               
#--------------------                       affect social inequalities                     -----------------------------------#  
#---------------------      in early childcare access, a randomised controlled trial       -----------------------------------#
#                                                -------------
#--------------------                    Functions built for the analysis                  -----------------------------------# 
#--------------------                          Authors: XX & XX                            -----------------------------------#    
#--------------------                               Version 1                              -----------------------------------#  
#--------------------                               June 2024                              -----------------------------------#     
#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#


###### All functions used in the paper

#######################################################################################-
#######################################################################################-

##### glanceCustomFixest ####

#######################################################################################-
#######################################################################################-

## Function to get the mean of the DV in modelsummary with fixest
glance_custom.fixest <- function(x, ...) {
  dv <- insight::get_response(x)
  dv <- sprintf("%.2f", mean(dv, na.rm = TRUE))
  data.table::data.table(`Mean of DV` = dv)
}


#######################################################################################-
#######################################################################################-

#### check_controls ####

#######################################################################################-
#######################################################################################-

# useful for customised table to check if there are controls and indicate a checkmark in which case

check_controls <- function(variables, yes = "✓", no = "") {
  checkmate::assert_character(variables, min.len = 1)
  checkmate::assert_string(no)
  checkmate::assert_string(yes)
  reg <- paste0("^", paste(variables, collapse = "$|^"), "$")
  fun <- function(model) {
    est <- get_estimates(model)
    df <- if (all(variables %in% est$term)) yes else no
    df <- data.frame(Controls = df)
    return(df)
  }
  list("fun" = fun, "regex" = reg)
}






#######################################################################################-
#######################################################################################-

##### EstPostLasso #####

#######################################################################################-
#######################################################################################-

# Function requires package glmnet, fixest and estimatr



EstPostLasso <- function(Y="ECSApp",                 # Outcome as string
                         Z="Z",                      # Treatment variable as sting
                         Cluster= "SubSampleStrata", # Clusters 
                         FE = "SubSampleStrata"    , # Fixed effects
                         weights="WeightPS",         # Weights used in the ITT estimates
                         SubSample="T2-T1",          # Comparison group
                         DB=PostDB,                  # Database
                         X=X.c                       # X database (must be able to match with DB)
){  
  
  # get the database  
  DBUse <- {{DB}} %>% filter(SubSample=={{SubSample}}) 
  
  # Outcome
  Y <- DBUse[[{{Y}}]]  
  # treatment
  Z <- DBUse[[{{Z}}]]    
  # Cluster
  cluster <-  DBUse[[{{Cluster}}]]  
  # fixed effects
  FE <- DBUse[[{{Cluster}}]]  
  
  # Weights
  Weights <- DBUse[[{{weights}}]]  
  
  # New X matrix
  XT <- {{X}} %>% filter(Responded==1 & SubSample=={{SubSample}}) 
  
  
  ## We train a lasso to predict outcomes, so we want it to be able to pick the treated x covariate values that are high and so on.
  ## Therefore, the X matrix contains the treatment, and we want to interact the treatment with the rest.
  ## 
  
  # remove the variables we don't want, and add dummies for blocks and treatment to this matrix.
  # XT <- XT %>% select(-c("SubSampleStrata" ,"SubSample", "ResponseId","Responded")) %>% 
  #   bind_cols(model.matrix(~0+HighLowECECBaseline+Educ2+IntendUse+Which_wave,DBUse))
  # 
  # XT <- XT %>% select(-c("SubSampleStrata" ,"SubSample", "ResponseId","Responded")) %>% 
  #   bind_cols(DBUse %>% select(HighLowECECBaseline,Educ2,IntendUse,Which_wave))
  # 
  
  # remove the variables we don't want, and add dummies for blocks and treatment to this matrix.
  XT <- XT %>% select(-c("SubSampleStrata" ,"SubSample", "ResponseId","Responded")) %>% 
    bind_cols(model.matrix(~0+HighLowECECBaseline*Educ2*IntendUse*Which_wave+Z,DBUse))
  
  # put it as a matrix and allow every possible interactions (the number of columns gets crazy high)
  ModelMatrix <- model.matrix(~0+.*.,XT) 
  
  # We just don't want 
  ModelMatrix <- ModelMatrix[,-c(which(colnames(ModelMatrix)=="Z"))]
  
  # Remove near zero variable
  # ZeroVar <- XT %>% nearZeroVar(freqCut = 90/10) 
  # XT <- XT[,-c(ZeroVar)]
  # put it as a matrix and allow every possible interactions (the number of columns gets crazy high)
  #ModelMatrix <- model.matrix(~0+.*.,XT) 
 # ModelMatrix <- model.matrix(~0+(HighLowECECBaseline:Educ2:IntendUse)*.,XT) 
  
  # remove variables with less than 5% of distinct values (almost 0 variance variable which ends-up colinear)
  ZeroVar <- ModelMatrix %>% nearZeroVar(uniqueCut = 5) 
  ModelMatrix <- ModelMatrix[,-c(ZeroVar)]
  
  # We just don't want 
 # ModelMatrix <- ModelMatrix[,-c(which(colnames(ModelMatrix)=="Z"))]
  
  # Run the basic estimate
  # We use fixest::feols for the fixest effect. we put the variables in a database as this function requires a dataframe as input
  DB.FE.Reg <- as.data.frame(bind_cols(Y=Y,Z=Z,cluster=cluster,FE=FE,weights=Weights))
  basicModel <- feols(Y~Z|FE,data=DB.FE.Reg,cluster = ~cluster,weights = ~weights)
  
  ## now, let's run a cross validation 
  # Cross validation to identify the optimal lambda (may take some time to converge)
  cv_Lasso2 <- cv.glmnet(ModelMatrix,Y , alpha = 1,nfolds = 10)
  
  # get the best lambda
  bestlam <- cv_Lasso2$lambda.min
  
  # Now, we can run the lasso model to pick the right covariate using the optimal lambda
  Lasso_reg2 <- glmnet(ModelMatrix, Y, alpha = 1, lambda=bestlam)
  
  DB.X <- as.data.frame(ModelMatrix[,c(which(Lasso_reg2$beta>0|Lasso_reg2$beta<0))])
  DB.Reg <- bind_cols(DB.FE.Reg,DB.X) %>% janitor::clean_names() %>% rename(Z=z,FE=fe)
  DB.FE.Xc <- as.data.frame(bind_cols(Y=Y,Z=Z,cluster=cluster,FE=FE,weights=Weights))
  # Then, simply run the regression of the outcome on the treatment and the variables selected by lasso. 
  # lm_lin conviniently accept that and will even make sure that all variables are centred to ensure
  # a treatment effect interpretation.
 # post_lasso <- lm_robust(Y~Z+ModelMatrix[,c(which(Lasso_reg2$beta>0))],cluster=cluster,weights = Weights) 
  # Get all column names except Y, Z, and FE
  regressors <- setdiff(names(DB.Reg), c("y", "z", "FE","cluster","weights"))
  
  # do not get the design variable
  regressors <- regressors[str_detect(regressors,"educ2|intend_use|ecec_covering|wave")==FALSE]
  
  # Create the formula as a string
  formula_str <- paste("y ~ Z +", paste(regressors, collapse = " + "), "| FE")
  
  # Run the model
  post_lasso <- feols(as.formula(formula_str),data=DB.Reg,cluster = ~cluster,weights = ~weights)
  
  #post_lasso <- feols(Y~Z+.|FE,data=DB.Reg)
  #
  ITT_postLasso <- lm_lin(Y~Z,~ModelMatrix[,which((Lasso_reg2$beta>0 |Lasso_reg2$beta<0) & str_detect(rownames(Lasso_reg2$beta),":Z")==FALSE)],cluster=cluster,weights = Weights) 
  
  
  
  return(list("Basic Model"=basicModel,"Post Lasso"=post_lasso,"ITT Post lasso"=ITT_postLasso,"Lambda"=bestlam,"Lasso"=Lasso_reg2))
  
}

#test <- EstPostLasso()


EstPostLassolm_robust <- function(Y="ECSApp",                 # Outcome as string
                         Z="Z",                      # Treatment variable as sting
                         Cluster= "SubSampleStrata", # Clusters 
                         FE = "SubSampleStrata"    , # Fixed effects
                         weights="WeightPS",         # Weights used in the ITT estimates
                         SubSample="T2-T1",          # Comparison group
                         DB=PostDB,                  # Database
                         X=X.c                       # X database (must be able to match with DB)
){  
  
  # get the database  
  DBUse <- {{DB}} %>% filter(SubSample=={{SubSample}}) 
  
  # Outcome
  Y <- DBUse[[{{Y}}]]  
  # treatment
  Z <- DBUse[[{{Z}}]]    
  # Cluster
  cluster <-  DBUse[[{{Cluster}}]]  
  # fixed effects
  FE <- DBUse[[{{Cluster}}]]  
  
  # Weights
  Weights <- DBUse[[{{weights}}]]  
  
  # New X matrix
  XT <- {{X}} %>% filter(Responded==1 & SubSample=={{SubSample}}) 
  
  
  ## We train a lasso to predict outcomes, so we want it to be able to pick the treated x covariate values that are high and so on.
  ## Therefore, the X matrix contains the treatment, and we want to interact the treatment with the rest.
  ## 
  
  # remove the variables we don't want, and add dummies for blocks and treatment to this matrix.
  XT <- XT %>% select(-c("SubSampleStrata" ,"SubSample", "ResponseId","Responded")) %>% 
    bind_cols(model.matrix(~0+HighLowECECBaseline*Educ2*IntendUse*Which_wave,DBUse))
  
  # put it as a matrix and allow every possible interactions (the number of columns gets crazy high)
  ModelMatrix <- model.matrix(~0+.*.,XT) 
  
  # We just don't want 
  # ModelMatrix <- ModelMatrix[,-c(which(colnames(ModelMatrix)=="Z"))]
  
  # Run the basic estimate
  # We use fixest::feols for the fixest effect. we put the variables in a database as this function requires a dataframe as input
  DB.FE.Reg <- as.data.frame(bind_cols(Y=Y,Z=Z,cluster=cluster,FE=FE,weights=Weights))
  basicModel <- feols(Y~Z|FE,data=DB.FE.Reg,cluster = ~cluster,weights = ~weights)
  
  
  ## now, let's run a cross validation 
  # Cross validation to identify the optimal lambda (may take some time to converge)
  cv_Lasso2 <- cv.glmnet(ModelMatrix,Y , alpha = 1,nfolds = 5)
  
  # get the best lambda
  bestlam <- cv_Lasso2$lambda.min
  
  # Now, we can run the lasso model to pick the right covariate using the optimal lambda
  Lasso_reg2 <- glmnet(ModelMatrix, Y, alpha = 1, lambda=bestlam)
  
  
  # Then, simply run the regression of the outcome on the treatment and the variables selected by lasso. 
  # lm_lin conviniently accept that and will even make sure that all variables are centred to ensure
  # a treatment effect interpretation.
  post_lasso <- lm_robust(Y~Z+ModelMatrix[,c(which(Lasso_reg2$beta>0))],cluster=cluster,weights = Weights) 
  
  #
  ITT_postLasso <- lm_lin(Y~Z,~ModelMatrix[,which(Lasso_reg2$beta>0 & str_detect(rownames(Lasso_reg2$beta),":Z")==FALSE)],cluster=cluster,weights = Weights) 
  
  
  
  return(list("Basic Model"=basicModel,"Post Lasso"=post_lasso,"ITT Post lasso"=ITT_postLasso,"Lambda"=bestlam,"Lasso"=Lasso_reg2))
  
}


EstPostLassoOld <- function(Y="ECSApp",                 # Outcome as string
                         Z="Z",                      # Treatment variable as sting
                         Cluster= "SubSampleStrata", # Clusters 
                         FE = "SubSampleStrata"    , # Fixed effects
                         weights="WeightPS",         # Weights used in the ITT estimates
                         SubSample="T2-T1",          # Comparison group
                         DB=PostDB,                  # Database
                         X=X.c                       # X database (must be able to match with DB)
){  
  
  # get the database  
  DBUse <- {{DB}} %>% filter(SubSample=={{SubSample}}) 
  
  # Outcome
  Y <- DBUse[[{{Y}}]]  
  # treatment
  Z <- DBUse[[{{Z}}]]    
  # Cluster
  cluster <-  DBUse[[{{Cluster}}]]  
  # fixed effects
  FE <- DBUse[[{{Cluster}}]]  
  
  # Weights
  Weights <- DBUse[[{{weights}}]]  
  
  # New X matrix
  XT <- {{X}} %>% filter(Responded==1 & SubSample=={{SubSample}}) 
  
  
  ## We train a lasso to predict outcomes, so we want it to be able to pick the treated x covariate values that are high and so on.
  ## Therefore, the X matrix contains the treatment, and we want to interact the treatment with the rest.
  ## 
  
  # remove the variables we don't want, and add dummies for blocks and treatment to this matrix.
  XT <- XT %>% select(-c("SubSampleStrata" ,"SubSample", "ResponseId","Responded")) %>% 
    bind_cols(model.matrix(~0+HighLowECECBaseline*Educ2*IntendUse*Which_wave+Z,DBUse))
  
  # put it as a matrix and allow every possible interactions (the number of columns gets crazy high)
  ModelMatrix <- model.matrix(~0+.*.,XT) 
  
  # We just don't want 
  ModelMatrix <- ModelMatrix[,-c(which(colnames(ModelMatrix)=="Z"))]
  
  # Run the basic estimate
  # We use fixest::feols for the fixest effect. we put the variables in a database as this function requires a dataframe as input
  DB.FE.Reg <- as.data.frame(bind_cols(Y=Y,Z=Z,cluster=cluster,FE=FE,weights=Weights))
  basicModel <- feols(Y~Z|FE,data=DB.FE.Reg,cluster = ~cluster,weights = ~weights)
  
  
  ## now, let's run a cross validation 
  # Cross validation to identify the optimal lambda (may take some time to converge)
  cv_Lasso2 <- cv.glmnet(ModelMatrix,Y , alpha = 1)
  
  # get the best lambda
  bestlam <- cv_Lasso2$lambda.min
  
  # Now, we can run the lasso model to pick the right covariate using the optimal lambda
  Lasso_reg2 <- glmnet(ModelMatrix, Y, alpha = 1, lambda=bestlam)
  
  
  # Then, simply run the regression of the outcome on the treatment and the variables selected by lasso. 
  # lm_lin conviniently accept that and will even make sure that all variables are centred to ensure
  # a treatment effect interpretation.
  post_lasso <- lm_robust(Y~Z+ModelMatrix[,c(which(Lasso_reg2$beta>0|Lasso_reg2$beta<0))],cluster=cluster,weights = Weights) 
  
  #
  ITT_postLasso <- lm_lin(Y~Z,~ModelMatrix[,which(Lasso_reg2$beta>0|Lasso_reg2$beta<0 & str_detect(rownames(Lasso_reg2$beta),":Z")==FALSE)],cluster=cluster,weights = Weights) 
  
  
  
  return(list("Basic Model"=basicModel,"Post Lasso"=post_lasso,"ITT Post lasso"=ITT_postLasso,"Lambda"=bestlam,"Lasso"=Lasso_reg2))
  
}

#######################################################################################-
#######################################################################################-

##### ITTSimultaneous ####

#######################################################################################-
#######################################################################################-


# Function to compute average ITT with adjustment for multiple testing (choice of correction method as parameter)

ITTSimultaneous <- function(Y="UseCreche",
                            treat="Z",
                            DB=PostDB,
                            Correction="Westfall",
                            weights="WeightPS"
){
  #Correction methods available
  # c('single-step', 'Shaffer', 'Westfall', 'free', 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr', 'none')
  
  
  #Data
  DBInside <- DB 
  # outcome
  DBInside$Y <- DBInside[[{{Y}}]]  
  # treatment variable
  DBInside$Z <- DBInside[[{{treat}}]]
  
  # weights : initialized as 1 if no weights given
  if (weights==""){
    DBInside$w=1  
  }
  else{
    DBInside$w <- DBInside[[{{weights}}]]  
  }
  
  #model  
  model <- feols(Y~i(Z,SubSample,ref=0)|SubSampleStrata,DBInside,cluster = ~StrataWave,weights = ~w)
  
  #glht
  glht.model <- glht(model)
  
  #prepare results
  tidyglht <-   left_join(tidy(glht.model,test=adjusted(type={{Correction}})),
                          tidy(confint(glht.model,adjusted(type={{Correction}}))))
  
  tidy.final <- tidy({{model}}) %>% bind_cols(.,confint({{model}})[1],
                                              confint({{model}})[2]) %>% 
    rename("point.conf.low"="2.5 %","point.conf.high"="97.5 %") %>% left_join(.,tidyglht,by=c("term"="contrast","estimate","std.error","statistic")) %>% 
    separate(term,c("Z","term","Var"),sep="::") %>% mutate(term=Var,
                                                           Var=str_remove_all(Var,"MaternityWardBaseline|Educ|IntendUse|Which_wave|HighLowECECBaseline")
    )
  
  # Compute response rate in the comparison group (trick with OLS ;) )
  ControlMean <- feols(ZO*Y~i(ZO,SubSample,ref=0)|SubSampleStrata,DBInside %>% mutate(ZO=1-Z),cluster = ~StrataWave)
  
  # joint significance test
  Glht.ControlMean <- glht(ControlMean) 
  
  # get the results in a tidy data frame
  tidy.Glht.ControlMean <- left_join(tidy(Glht.ControlMean),tidy(confint(Glht.ControlMean)))
  
  # tidy the model 
  tidy.ControlMean <- tidy(ControlMean) %>% bind_cols(.,confint(ControlMean)[1],
                                                      confint(ControlMean)[2]) %>% 
    rename("point.conf.low"="2.5 %","point.conf.high"="97.5 %") %>% left_join(.,tidy.Glht.ControlMean,by=c("term"="contrast","estimate","std.error")) %>% 
    mutate(term=str_remove_all(term,"ZO::1:|\\(|\\)|SubSample::"))
  
  
  ### prepare for model summary
  
  ChisQTest <- glht(model) %>% summary(.,test=Chisqtest())
  
  fullModel <- list(tidy=tidy.final %>% 
                      bind_rows(.,tidy.ControlMean %>% filter(str_detect(term,"T2-C")) %>% 
                                  mutate(term="Control mean") %>% 
                                  mutate_at(vars(adj.p.value,p.value),~NA)), # list with tidy containing the dataframe with the estimates, # list with tidy containing the dataframe with the estimates
                    glance=get_gof(model) %>%  # statistics of the model
                      bind_cols(.,"Fixed effects"="X") %>% 
                      bind_cols(.,t(c("Chi 2"= ChisQTest$test$SSH,
                                      "P-value"=  ChisQTest$test$pvalue)
                      )))
  class(fullModel) <- "modelsummary_list"   # define the class
  
  
  return(list("Estimation"=model,"Tidy"=tidy.final,"Correction"={{Correction}},"ModelSummary"=fullModel))    
}

#######################################################################################-
#######################################################################################-

#### ITTDif ####

#######################################################################################-
#######################################################################################-


# this function is the former version of ITTSimultaneous and do not compute the mean of the control group inside.
# It is therefore useful if we want to trick it into computing control means for instance.
ITTDif <- function(Y="UseCreche",
                   DB=PostDB,
                   Correction="Westfall"
){
  #Data
  DBInside <- DB 
  DBInside$Y <- DBInside[[{{Y}}]]  
  
  #model  
  model <- feols(Y~i(Z,SubSample,ref=0)|SubSampleStrata,DBInside,cluster = ~StrataWave,weights = ~WeightPS)
  
  #glht
  glht.model <- glht(model)
  
  #prepare results
  tidyglht <-   left_join(tidy(glht.model,test=adjusted(type={{Correction}})),tidy(confint(glht.model,adjusted(type={{Correction}}))))
  
  tidy.final <- tidy({{model}}) %>% bind_cols(.,confint({{model}})[1],
                                              confint({{model}})[2]) %>% 
    rename("point.conf.low"="2.5 %","point.conf.high"="97.5 %") %>% left_join(.,tidyglht,by=c("term"="contrast","estimate","std.error","statistic")) %>% 
    separate(term,c("Z","term","Var"),sep="::") %>% mutate(term=str_remove_all(term,"SubSample"),
                                                           Var=str_remove_all(Var,"MaternityWardBaseline|Educ|IntendUse|Which_wave|HighLowECECBaseline")
    )
  
  return(list("Estimation"=model,"Tidy"=tidy.final,"Correction"={{Correction}}))    
}


#######################################################################################-
#######################################################################################-

#### LATESimultaneous ####

#######################################################################################-
#######################################################################################-

# this function is essentially the same as ITTSimultaneous, however it computes the LATE with TSLS instead.
# Note that it won't work for comparisons with T1 since we don't have the first stage.
#This function should therefore be used with PostDBT2
LATESimultaneous <- function(Y="UseCreche",
                             DB=PostDBT2,
                             Correction="Westfall",
                             weights="WeightPS"
){
  #Data
  DBInside <- DB 
  DBInside$Y <- DBInside[[{{Y}}]]  
  DBInside$w <- DBInside[[{{weights}}]]
  
  if (weights==""){
    DBInside$w=1  
  }
  
  #model  
  model <- feols(Y~1|SubSampleStrata|D:SubSample~Z.c:SubSample,DBInside,cluster = ~StrataWave,weights = ~w)
  
  #glht
  glht.model <- glht(model)
  
  #prepare results
  tidyglht <-   left_join(tidy(glht.model,test=adjusted(type={{Correction}})),tidy(confint(glht.model,adjusted(type={{Correction}}))))
  
  tidy.final <- tidy({{model}}) %>% bind_cols(.,confint({{model}})[1],
                                              confint({{model}})[2]) %>% 
    rename("point.conf.low"="2.5 %","point.conf.high"="97.5 %") %>% left_join(.,tidyglht,by=c("term"="contrast","estimate","std.error","statistic")) %>% 
    separate(term,c("D","term"),sep=":") %>% mutate(term=str_remove_all(term,"SubSample"))
  
  
  # Compute response rate in the comparison group
  # Compliers' missing potential outcome following Abadie 2003 and the trick with TSLS similar to the previous one
  Y0Compliers <- feols(D0*Y~1|SubSampleStrata|D0:SubSample~Z.c:SubSample,DBInside %>% mutate(ZO=1-Z,D0=1-D),cluster = ~StrataWave)
  
  
  # joint significance test
  Glht.ControlMean <- glht(Y0Compliers) 
  
  # get the results in a tidy data frame
  tidy.Glht.ControlMean <- left_join(tidy(Glht.ControlMean),tidy(confint(Glht.ControlMean)))
  
  # tidy the model 
  tidy.ControlMean <- tidy(Y0Compliers) %>% bind_cols(.,confint(Y0Compliers)[1],
                                                      confint(Y0Compliers)[2]) %>% 
    rename("point.conf.low"="2.5 %","point.conf.high"="97.5 %") %>% left_join(.,tidy.Glht.ControlMean,by=c("term"="contrast","estimate","std.error")) %>% 
    mutate(term=str_remove_all(term,"ZO::1:|\\(|\\)|SubSample"))
  
  
  ### prepare for model summary
  
  ChisQTest <- glht(model) %>% summary(.,test=Chisqtest())
  
  fullModel <- list(tidy=tidy.final %>% 
                      bind_rows(.,tidy.ControlMean %>% filter(str_detect(term,"T2-T1")) %>% 
                                  mutate(term="Avg. cfct.") %>% 
                                  mutate_at(vars(adj.p.value,p.value),~NA)), # list with tidy containing the dataframe with the estimates, # list with tidy containing the dataframe with the estimates
                    glance=get_gof(model) %>%  # statistics of the model
                      bind_cols(.,"Fixed effects"="X") %>% 
                      bind_cols(.,t(c("Chi 2"= ChisQTest$test$SSH,
                                      "P-value"=  ChisQTest$test$pvalue)
                      )))
  class(fullModel) <- "modelsummary_list"   # define the class
  
  
  
  
  
  return(list("Estimation"=model,"Tidy"=tidy.final,"Correction"={{Correction}},"ModelSummary"=fullModel))    
}


#######################################################################################-
#######################################################################################-

#### GroupHeterogeneityFn ####

#######################################################################################-
#######################################################################################-

# This function estimates either the conditional ITT or conditional LATE for a factor variable
# defined as the parameter Heterogeneity.  

GroupHeterogeneityFn <- function(DB = PostDBT2,             #Database
                                 Outcome = "UseCreche",    # Which outcome 
                                 Heterogeneity = "Educ2",  # Which heterogeneity variable
                                 ctrl.var = c("Act3","SingleMum","FrenchYNBaseline","Dep"), # NOT USED FOR NOW
                                 ITT = TRUE,               # ITT = TRUE -> OLS, ITT=FALSE -> TSLS Conditional LATE
                                 Weights = "WeightPS",     # Propensity score weights by default
                                 clusters = "StrataWave"   # Cluster variable
){
  
  DBInside <- {{DB}}
  
  #get the outcome
  DBInside$Y <- DBInside[[{{Outcome}}]]
  
  #get the weight
  DBInside$w <- DBInside[[{{Weights}}]]
  
  #Create heterogeneous variable
  DBInside$Het <- DBInside[[{{Heterogeneity}}]]
  
  # clusters
  DBInside$Clust <- DBInside[[{{clusters}}]]
  
  
  
  
  # Check if variable in the blocking variables
  
  InteractFE <- ({{Heterogeneity}} %in% c("HighLowECEC","Educ2","IntendUse","Which_wave"))
  
  if(InteractFE==TRUE){
    if({{ITT}}==TRUE){
      model <- feols(Y~Z.c:SubSample:Het|StrataWave^SubSample,DBInside,cluster=~Clust,weights=~w)  
    }
    else{
      model <- feols(Y~1|StrataWave^SubSample|D:SubSample:Het~Z.c:SubSample:Het,DBInside,cluster=~Clust,weights=~w)  
      # Get the F stat of the first stage
      Fstats <- fitstat(model,'ivf1') %>% unlist()
      Fstats <- Fstats[str_detect(names(Fstats),"stat")]
      names(Fstats) <- names(Fstats) %>% str_replace_all("ivf1::PhasesReg","F-stat ") %>% str_remove_all(.,":D1.stat")
      Fstats <- mean(Fstats)
      names(Fstats) <- "Mean F-stat 1st stage"
      
    }
  }
  
  if(InteractFE==FALSE){
    if({{ITT}}==TRUE){
      model <- feols(Y~Z.c:SubSample:Het|Het^StrataWave^SubSample,DBInside,cluster=~Clust,weights=~w)  
    }
    else{
      model <- feols(Y~1|Het^StrataWave^SubSample|D:SubSample:Het~Z.c:SubSample:Het,DBInside,cluster=~Clust,weights=~w)  
      
      Fstats <- fitstat(model,'ivf1') %>% unlist()
      Fstats <- Fstats[str_detect(names(Fstats),"stat")]
      names(Fstats) <- names(Fstats) %>% str_replace_all("ivf1::PhasesReg","F-stat ") %>% str_remove_all(.,":D1.stat")
      Fstats <- mean(Fstats)
      names(Fstats) <- "Mean F-stat 1st stage"
      
      
    }
  }
  
  # Tidy Database of the model
  
  M0 <- modelplot(model,draw=FALSE) %>% 
    separate(term,into=c("Treat","term","Group"),sep=":") %>% mutate(Model=ifelse(str_detect(Treat,"fit"),"TSLS","OLS")) %>% mutate(term=str_remove(term,"SubSample"),Group=str_remove(Group,"Het")) %>% 
    rename("point.conf.low"="conf.low","point.conf.high"="conf.high")
  
  
  ## Joint hypothesis testing for each value of the heterogeneity variable
  GLH <-c()
  temp <- c()
  HetVal <- unique(DBInside$Het) %>% as.character()
  
  
  for (value in HetVal){
    temp <- glht(model,
                 paste(paste("`",
                             names(model$coefficients)[
                               str_detect(names(model$coefficients),{{value}})],
                             "`= 0",sep="")))
    
    GLH <- bind_rows(GLH,left_join(tidy(temp),tidy(confint(temp))) %>% mutate(Het=value))
  } 
  
  # Clean names and labels
  GLH <- GLH %>% separate(contrast,into=c("Treat","term","Group"),sep=":") %>% mutate(Model=ifelse(str_detect(Treat,"fit"),"TSLS","OLS")) %>% mutate(term=str_remove(term,"SubSample"),Group=str_remove(Group,"Het"))
  
  # join with MO
  GLH <- left_join(GLH,M0 %>% select(-c(estimate,std.error)))
  
  #Prepare Gof for the model
  
  Gof <- get_gof(model)%>%  # statistics of the model
    bind_cols(.,"Fixed effects"="X")
  
  if ({{ITT}}==FALSE){
    Gof <- Gof %>% bind_cols(.,as.data.frame(t(Fstats)))
  }
  
  #%>% bind_cols(.,as.data.frame(t(Fstats)))
  
  
  ForModelSummary <- list(tidy=GLH,
                          glance=Gof
  )
  
  class(ForModelSummary) <- "modelsummary_list"   # define the class
  
  
  return(list("Estimation"=model,"Tidy"=GLH,"ModelSummary"=ForModelSummary))    
}


#######################################################################################-
#######################################################################################-

#### GroupHeterogeneityFnCTRL ####

#######################################################################################-
#######################################################################################-

# This function also compute the means in the contnrol group and adds it to the output 
# as both a model, and a list for modelsummary 

GroupHeterogeneityFnCTRL <- function(DB = PostDBT2,             #Database
                                     Outcome = "UseCreche",    # Which outcome 
                                     Heterogeneity = "Educ2",  # Which heterogeneity variable
                                     ITT = TRUE,               # ITT = TRUE -> OLS, ITT=FALSE -> TSLS Conditional LATE
                                     Weights = "WeightPS",     # Propensity score weights by default
                                     clusters = "StrataWave",   # Cluster variable
                                     Correction="Westfall"
){
  
  #Correction should be one of “single-step”, “Shaffer”, “Westfall”, “free”, “holm”, “hochberg”, “hommel”, “bonferroni”, “BH”, “BY”, “fdr”, “none”  
  DBInside <- {{DB}}
  
  #get the outcome
  DBInside$Y <- DBInside[[{{Outcome}}]]
  
  #get the weight
  DBInside$w <- DBInside[[{{Weights}}]]
  
  #Create heterogeneous variable
  DBInside$Het <- DBInside[[{{Heterogeneity}}]]
  
  # clusters
  DBInside$Clust <- DBInside[[{{clusters}}]]
  # Check if variable in the blocking variables
  
  InteractFE <- ({{Heterogeneity}} %in% c("HighLowECEC","Educ2","IntendUse","Which_wave"))
  
  if(InteractFE==TRUE){
    if({{ITT}}==TRUE){
      model <- feols(Y~Z.c:SubSample:Het|StrataWave^SubSample,DBInside,cluster=~Clust,weights=~w)
      model0 <- feols(Y~Z.c:SubSample:Het|StrataWave^SubSample,DBInside %>% mutate(Y=(1-Z)*Y,Z.c=psscore-Z)
                      ,cluster=~Clust,weights=~w)
    }
    else{
      model <- feols(Y~1|StrataWave^SubSample|D:SubSample:Het~Z.c:SubSample:Het,DBInside,cluster=~Clust,weights=~w)  
      model0 <- feols(Y~1|StrataWave^SubSample|D:SubSample:Het~Z.c:SubSample:Het,DBInside %>% mutate(Y=(1-D)*Y,D=1-D,Z.c=psscore-Z)
                      ,cluster=~Clust,weights=~w)
      # Get the F stat of the first stage
      Fstats <- fitstat(model,'ivf1') %>% unlist()
      Fstats <- Fstats[str_detect(names(Fstats),"stat")]
      names(Fstats) <- names(Fstats) %>% str_replace_all("ivf1::PhasesReg","F-stat ") %>% str_remove_all(.,":D1.stat")
      Fstats <- mean(Fstats)
      names(Fstats) <- "Mean F-stat 1st stage"
      
    }
  }
  
  if(InteractFE==FALSE){
    if({{ITT}}==TRUE){
      model <- feols(Y~Z.c:SubSample:Het|Het^StrataWave^SubSample,DBInside,cluster=~Clust,weights=~w)  
      model0 <- feols(Y~Z.c:SubSample:Het|Het^StrataWave^SubSample,DBInside %>% mutate(Y=(1-Z)*Y,Z.c=psscore-Z)
                      ,cluster=~Clust,weights=~w)
    }
    else{
      model <- feols(Y~1|Het^StrataWave^SubSample|D:SubSample:Het~Z.c:SubSample:Het,DBInside,cluster=~Clust,weights=~w)  
      model0 <- feols(Y~1|Het^StrataWave^SubSample|D:SubSample:Het~Z.c:SubSample:Het,DBInside %>% mutate(Y=(1-D)*Y,D=1-D,Z.c=psscore-Z)
                      ,cluster=~Clust,weights=~w)
      
      Fstats <- fitstat(model,'ivf1') %>% unlist()
      Fstats <- Fstats[str_detect(names(Fstats),"stat")]
      names(Fstats) <- names(Fstats) %>% str_replace_all("ivf1::PhasesReg","F-stat ") %>% str_remove_all(.,":D1.stat")
      Fstats <- mean(Fstats)
      names(Fstats) <- "Mean F-stat 1st stage"
      
      
    }
  }
  
  # Tidy Database of the model
  
  M0 <- modelplot(model,draw=FALSE) %>% 
    separate(term,into=c("Treat","term","Group"),sep=":") %>% mutate(Model=ifelse(str_detect(Treat,"fit"),"TSLS","OLS")) %>% mutate(term=str_remove(term,"SubSample"),Group=str_remove(Group,"Het")) %>% 
    rename("point.conf.low"="conf.low","point.conf.high"="conf.high")
  
  
  ## Joint hypothesis testing for each value of the heterogeneity variable
  GLH <-c()
  temp <- c()
  HetVal <- unique(DBInside$Het) %>% as.character()
  
  
  for (value in HetVal){
    temp <- glht(model,
                 paste(paste("`",
                             names(model$coefficients)[
                               str_detect(names(model$coefficients),{{value}})],
                             "`= 0",sep="")))
    sum.temp <- summary(temp,adjusted(type={{Correction}}))
    CI.temp <- confint(temp)
    # GLH <- bind_rows(GLH,left_join(tidy(temp),
    #                               tidy(confint(temp))) %>% mutate(Het=value))
    GLH <- bind_rows(GLH,left_join(tidy(sum.temp),
                                   tidy(CI.temp)) %>% mutate(Het=value))
    
  } 
  #adjusted(type={{Correction}})),tidy(confint(glht.model,adjusted(type={{Correction}}))))
  # Clean names and labels
  GLH <- GLH %>% separate(contrast,into=c("Treat","term","Group"),sep=":") %>% 
    mutate(Model=ifelse(str_detect(Treat,"fit"),"TSLS","OLS")) %>%
    mutate(term=str_remove(term,"SubSample"),
           Group=str_remove(Group,"Het"))
  
  # join with MO
  GLH <- left_join(GLH,M0 %>% select(-c(estimate,std.error)))
  
  #Prepare Gof for the model
  
  Gof <- get_gof(model)%>%  # statistics of the model
    bind_cols(.,"Fixed effects"="X")
  
  if ({{ITT}}==FALSE){
    Gof <- Gof %>% bind_cols(.,as.data.frame(t(Fstats)))
  }
  
  #### Same thing for the control outcomes :
  
  
  # Tidy Database of the model
  
  M0.ctrl <- modelplot(model0,draw=FALSE) %>% 
    separate(term,into=c("Treat","term","Group"),sep=":") %>% mutate(Model=ifelse(str_detect(Treat,"fit"),"TSLS","OLS")) %>% mutate(term=str_remove(term,"SubSample"),Group=str_remove(Group,"Het")) %>% 
    rename("point.conf.low"="conf.low","point.conf.high"="conf.high")
  
  
  ## Joint hypothesis testing for each value of the heterogeneity variable
  GLH0 <-c()
  temp <- c()
  
  for (value in HetVal){
    temp <- glht(model0,
                 paste(paste("`",
                             names(model0$coefficients)[
                               str_detect(names(model0$coefficients),{{value}})],
                             "`= 0",sep="")))
    
    sum.temp <- summary(temp,adjusted(type={{Correction}}))
    CI.temp <- confint(temp)
    # GLH <- bind_rows(GLH,left_join(tidy(temp),
    #                               tidy(confint(temp))) %>% mutate(Het=value))
    GLH0 <- bind_rows(GLH0,left_join(tidy(sum.temp),
                                     tidy(CI.temp)) %>% mutate(Het=value))
    
  } 
  
  # Clean names and labels
  GLH0 <- GLH0 %>% separate(contrast,into=c("Treat","term","Group"),sep=":") %>% mutate(Model=ifelse(str_detect(Treat,"fit"),"TSLS","OLS")) %>% mutate(term=str_remove(term,"SubSample"),Group=str_remove(Group,"Het"))
  
  # join with MO
  GLH0 <- left_join(GLH0,M0.ctrl %>% select(-c(estimate,std.error)))
  
  #Prepare Gof for the model
  
  Gof0 <- get_gof(model0)%>%  # statistics of the model
    bind_cols(.,"Fixed effects"="X")
  
  if ({{ITT}}==FALSE){
    Gof0 <- Gof0 %>% bind_cols(.,as.data.frame(t(Fstats)))
  }
  
  
  
  
  
  
  ForModelSummary0 <- list(tidy=GLH0,
                           glance=Gof0
  )
  
  class(ForModelSummary0) <- "modelsummary_list"   # define the class
   
  
  #Prepare Gof for the model
  
  Gof <- get_gof(model)%>%  # statistics of the model
    bind_cols(.,"Fixed effects"="X")
  
  if ({{ITT}}==FALSE){
    Gof <- Gof %>% bind_cols(.,as.data.frame(t(Fstats)))
  }
  
  #%>% bind_cols(.,as.data.frame(t(Fstats)))
  
  
  ForModelSummary <- list(tidy=GLH,
                          glance=Gof
  )
  
  class(ForModelSummary) <- "modelsummary_list"   # define the class
  
  
  
  
  return(list("Estimation"=model,"Tidy"=GLH,"ModelSummary"=ForModelSummary,"Model 0"=model0,"ModelSummary0"=ForModelSummary0))    
}

#######################################################################################-
#######################################################################################-

#### CompareCoef ####

#######################################################################################-
#######################################################################################-

# Following a model using the previous function such as
#Het.ATT.UseCreche <- GroupHeterogeneityFnCTRL(ITT=FALSE)

# The following function allows to test the equality of heterogeneous parameters
# it returns a list ready for model summary, and a full table with all the results

CompareCoef <- function(Model = Het.ATT.UseCreche,
                        Padjust.Method = "Westfall",
                        OutcomeLabel="daycare use",
                        
                        GroupLabel="baseline education"
                        ){
  

# Testing equal coefficients
MyList <- {{Model}}
m = MyList[["Estimation"]]

#names(m$coefficients)
# 4 coefficients. 
#Let's test T2-C:HetBac -T2-C:HetSup =0 and T2-T1:HetBac -T2-T1:HetSup =0 jointly

# get the names of the variables in labelnames and clean a bit for the table after
labelnames <- names(m$coefficients) %>% str_remove_all(.,"fit_|Z.c|Z|D|:SubSample")

# First row tests first coef - third coef = 0
# Second row tests second coef - fourth coef = 0
K <- rbind(c(1, 0, -1,0),
           c(0, 1, 0,-1))

#Let's put some names to the K matrix's row. 
rownames(K) <- c(paste(labelnames[which(K[1,]==1)],"-",labelnames[which(K[1,]==-1)],"=0"),
                 paste(labelnames[which(K[2,]==1)],"-",labelnames[which(K[2,]==-1)],"=0"))
# get the names of the coefficients for the columns
colnames(K) <- names(coef(m))

# See what this K matrix look like now that it has some nice names on it
#K

# Basic summary of testing jointly both rows of equality of coefficients (so difference = 0 against difference ≠0)
glht.effect <- glht(m,K) 

# How to get all these nice results in a modelsummary table:

# First, tidy the glht result and get the confidence interval too 
tidy.glht <- glht.effect %>% tidy(.,test=adjusted({{Padjust.Method}})) %>% left_join(.,
                                                                             confint(glht.effect,adjusted={{Padjust.Method}}) %>% tidy()) %>% 
  rename(term=contrast) %>% 
  left_join(.,tidy(m)) %>% 
  mutate(period=str_remove(term, ":.*"),
         Group=str_remove_all(term,"T2-C:|T2-T1:"),
         Group=str_replace_all(Group,"Het","TE "),
          term=period,
          model="GLHT"
)

# second, get the statistics of the initial model in gof
gof <- get_gof(m) %>% mutate("P adjust"={{Padjust.Method}})

#put them in a list

list.Glht <- list(tidy=tidy.glht,
                  glance=gof
)

# tell R it's a modelsummy type of list

class(list.Glht) <- "modelsummary_list"   # define the class


TheModels <-   list(
  MyList$ModelSummary0,
  MyList$ModelSummary,
  list.Glht)

TheTitle = paste("Average treatment effect by",{{GroupLabel}}, "on", {{OutcomeLabel}})

cm <- c('T2-C'    = 'Support + Information vs Control',
        'T2-T1' = 'Support vs Information')


names(TheModels) <- c(paste({{OutcomeLabel}},"Avg. cfct.",sep="_"),
                      paste({{OutcomeLabel}},"CATE",sep="_"),
                      paste({{OutcomeLabel}},"Test inference",sep="_"))

Summary.Model <- modelsummary(TheModels,
  shape = term + Group ~ model,
  coef_map = cm,
  fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
  estimate = '{estimate}{stars} ({std.error})',
  statistic = c("conf.int",
                "adj.p.val. = {adj.p.value}"),
  stars = c('*' = .1,'**' = .05, '***' = .01),
  gof_map = c(#"Mean of DV",
    "Covariates","Fixed effects","Mean F-stat 1st stage","P adjust",
    "nobs", "r.squared","adj.r.squared"),
  title=TheTitle,
  notes=paste("Sources:", SourcesStacked,
              "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference." 
  ),output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(3,4,5),width=2.7,unit = "cm")|>
  width(j=c(1,2),width=2.4,unit = "cm") %>% 
  hline(c(9,18),part="body")


return(list("CompareCoef" = list.Glht,"Summary.Model"=Summary.Model))

}

 #test <- CompareCoef(Het.LATE.UseCreche.Educ2C)
# 


#######################################################################################-
#######################################################################################-

#### CompareCoefDelta ####

#######################################################################################-
#######################################################################################-

# Following a model using the previous function such as
#Het.ATT.UseCreche <- GroupHeterogeneityFnCTRL(ITT=FALSE)

# The following function allows to test the equality of heterogeneous gaps in parameters
# it returns two lists ready for model summary, and a full table with all the results


CompareCoefDelta <- function(Model = Het.ATT.UseCreche,
                             Padjust.Method = "Westfall",
                             OutcomeLabel="daycare use",
                             
                             GroupLabel="baseline education"
){
  
  
  # Testing equal coefficients
  MyList <- {{Model}}
  m = MyList[["Estimation"]]
  m0 = MyList[["Model 0"]]
  
  
  # get the names of the variables in labelnames and clean a bit for the table after
  labelnames <- names(m$coefficients) %>% str_remove_all(.,"fit_|Z.c|Z|D|:SubSample")
  

  # First row tests first coef - third coef = 0
  # Second row tests second coef - fourth coef = 0
  K <- rbind(c(1, 0, -1,0),
             c(0, 1, 0,-1))
  
  #Let's put some names to the K matrix's row. 
  rownames(K) <- c(paste(labelnames[which(K[1,]==1)],"-",labelnames[which(K[1,]==-1)],"=0"),
                   paste(labelnames[which(K[2,]==1)],"-",labelnames[which(K[2,]==-1)],"=0"))
  # get the names of the coefficients for the columns
  colnames(K) <- names(coef(m))
  
  # See what this K matrix look like now that it has some nice names on it
  #K
  
  # Basic summary of testing jointly both rows of equality of coefficients (so difference = 0 against difference ≠0)
  glht.effect <- glht(m,K) 
  
  # How to get all these nice results in a modelsummary table:
  
  # First, tidy the glht result and get the confidence interval too 
  tidy.glht <- glht.effect %>% tidy(test=adjusted({{Padjust.Method}})) %>% left_join(.,
                                                                                       tidy(confint(glht.effect,test=adjusted({{Padjust.Method}}))) ) %>% 
    rename(term=contrast) %>% 
    left_join(.,tidy(m)) %>% 
    mutate(period=str_remove(term, ":.*"),
           Group=str_remove_all(term,"T2-C:|T2-T1:"),
           Group=str_replace_all(Group,"Het","TE "),
           term=period,
           term2="Conditional treatment effect"
    )
  
  
  # second, get the statistics of the initial model in gof
  gof <- get_gof(m) %>% mutate("P adjust"={{Padjust.Method}})
  
  #put them in a list
  
  list.Glht <- list(tidy=tidy.glht,
                    glance=gof
  )
  
  # tell R it's a modelsummy type of list
  
  class(list.Glht) <- "modelsummary_list"   # define the class
  
  
  
  
  ###### Difference in counterfactual
  # Basic summary of testing jointly both rows of equality of coefficients (so difference = 0 against difference ≠0)
  glht.cf <- glht(m0,K) 
  
  # How to get all these nice results in a modelsummary table:
  
  # First, tidy the glht result and get the confidence interval too 
  tidy.glht.cf <- glht.cf %>% tidy(test=adjusted({{Padjust.Method}})) %>% left_join(.,
                                                                                     tidy(confint(glht.cf,test=adjusted({{Padjust.Method}}))) ) %>% 
    rename(term=contrast) %>% 
    left_join(.,tidy(m0)) %>% 
    mutate(period=str_remove(term, ":.*"),
           Group=str_remove_all(term,"T2-C:|T2-T1:"),
           Group=str_replace_all(Group,"Het","TE "),
           term=period,
           term2="Conditional gap"
    )
  
  
  
  # second, get the statistics of the initial model in gof
  gof.m0 <- get_gof(m0) %>% mutate("P adjust"={{Padjust.Method}})
  
  #put them in a list
  
  list.Glht0 <- list(tidy=tidy.glht.cf,
                    glance=gof.m0
  )
  
  # tell R it's a modelsummy type of list
  
  class(list.Glht0) <- "modelsummary_list"   # define the class
  
  
  
  
  
   
  TheModels <-   list(
    MyList$ModelSummary0,
    MyList$ModelSummary,
    list.Glht0,
    list.Glht)
  
  TheTitle = paste("Average treatment effect by",{{GroupLabel}}, "on", {{OutcomeLabel}})
  
  isIV <- sum(names(m)=='iv')>0
  
  if (isIV){
  
  names(TheModels) <- c(paste({{OutcomeLabel}},"Avg. cfct.",sep="_"),
                        paste({{OutcomeLabel}},"Conditional LATE",sep="_"),
                        paste({{OutcomeLabel}},"Average gap in cft.",sep="_"),
                        paste({{OutcomeLabel}},"∆ Conditional LATE",sep="_"))
  }
  else{
    
    names(TheModels) <- c(paste({{OutcomeLabel}},"Avg. ctrl.",sep="_"),
                          paste({{OutcomeLabel}},"Conditional ITT",sep="_"),
                          paste({{OutcomeLabel}},"∆ ctrl.",sep="_"),
                          paste({{OutcomeLabel}},"∆ ITT",sep="_"))
  }
  
  
  cm <- c('T2-C'    = 'Information + Support vs Control',
          'T2-T1' = 'Support vs Information')
  
  Summary.Model <- modelsummary(TheModels,
                                shape =  Group ~ model,
                                coef_map = cm,
                                fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
                                estimate = '{estimate}{stars} ({std.error})',
                                statistic = c("conf.int",
                                              "adj.p.val. = {adj.p.value}"),
                                stars = c('*' = .1,'**' = .05, '***' = .01),
                                gof_map = c(#"Mean of DV",
                                  "Covariates","Fixed effects","Mean F-stat 1st stage","P adjust",
                                  "nobs", "r.squared","adj.r.squared"),
                                title=TheTitle,
                                notes=paste("Sources:", SourcesStacked,
                                            "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference." 
                                ),output = 'flextable') %>% 
    theme_booktabs()|>
    separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
    bold(i=1,  part = "header") %>%                # Variable labels bold
    merge_at(j=2,part="header")|>
    merge_at(j=1,part="header")|>
    merge_v(j=1,part="body")|>
    italic(i = c(1),  part = "header") %>% 
    italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
    align(part = "header", align = "center")|>                # center
    align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
    width(j=c(3,4,5),width=2.7,unit = "cm")|>
    width(j=c(1,2),width=2.4,unit = "cm") %>% 
    hline(c(9,18),part="body")
  
  Summary.Model <- modelsummary(TheModels,
                                shape =  Group ~ model,
                                coef_map = cm,
                                fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
                                estimate = '{estimate}{stars} ({std.error})',
                                statistic = c("conf.int",
                                              "adj.p.val. = {adj.p.value}"),
                                stars = c('*' = .1,'**' = .05, '***' = .01),
                                gof_map = c(#"Mean of DV",
                                  "Covariates","Fixed effects","Mean F-stat 1st stage","P adjust",
                                  "nobs", "r.squared","adj.r.squared"),
                                title=TheTitle,
                                notes=paste("Sources:", SourcesStacked,
                                            "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference." 
                                ),output = 'flextable') %>% 
    theme_booktabs()|>
    separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
    bold(i=1,  part = "header") %>%                # Variable labels bold
    merge_at(j=2,part="header")|>
    merge_at(j=1,part="header")|>
    merge_v(j=1,part="body")|>
    italic(i = c(1),  part = "header") %>% 
    italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
    align(part = "header", align = "center")|>                # center
    align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
    width(j=c(3,4,5),width=2.7,unit = "cm")|>
    width(j=c(1,2),width=2.4,unit = "cm") %>% 
    hline(c(9,18),part="body")
  
  
  return(list("CompareCoef" = list.Glht,"Compare.Y0"=list.Glht0,   "Summary.Model"=Summary.Model))
  
}

# test <- CompareCoefDelta(Het.LATE.UseCreche.Educ2C)









# test$Summary.Model
#              
#              ,
#              shape = term + Group ~ model,
#              fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
#              
#              )
#{Model}[["Estimation"]]
#tst <- GroupHeterogeneityFnCTRL(ITT=FALSE)



#########################################################################################

###########################################################################################

ITTSimNoControl <- function(Y="UseCreche",
                            treat="Z",
                            DB=PostDB,
                            Correction="Westfall",
                            weights="WeightPS"
){
  #Correction methods available
  # c('single-step', 'Shaffer', 'Westfall', 'free', 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr', 'none')
  
  
  #Data
  DBInside <- DB 
  # outcome
  DBInside$Y <- DBInside[[{{Y}}]]  
  # treatment variable
  DBInside$Z <- DBInside[[{{treat}}]]
  
  # weights : initialized as 1 if no weights given
  if (weights==""){
    DBInside$w=1  
  }else{
    DBInside$w <- DBInside[[{{weights}}]]  
  }
  
  #model  
  model <- feols(Y~i(Z,SubSample,ref=0)|SubSampleStrata,DBInside,cluster = ~StrataWave,weights = ~w)
  
  #glht
  glht.model <- glht(model)
  
  #prepare results
  tidyglht <-   left_join(tidy(glht.model,test=adjusted(type={{Correction}})),tidy(confint(glht.model,adjusted(type={{Correction}}))))
  
  tidy.final <- tidy({{model}}) %>% bind_cols(.,confint({{model}})[1],
                                              confint({{model}})[2]) %>% 
    rename("point.conf.low"="2.5 %","point.conf.high"="97.5 %") %>% left_join(.,tidyglht,by=c("term"="contrast","estimate","std.error","statistic")) %>% 
    separate(term,c("Z","term","Var"),sep="::") %>% mutate(term=Var,
                                                           Var=str_remove_all(Var,"MaternityWardBaseline|Educ|IntendUse|Which_wave|HighLowECECBaseline")
    )
  
  
  ### prepare for model summary
  
  ChisQTest <- glht(model) %>% summary(.,test=Chisqtest())
  
  fullModel <- list(tidy=tidy.final, # list with tidy containing the dataframe with the estimates, # list with tidy containing the dataframe with the estimates
                    glance=get_gof(model) %>%  # statistics of the model
                      bind_cols(.,"Fixed effects"="X") %>% 
                      bind_cols(.,t(c("Chi 2"= ChisQTest$test$SSH,
                                      "P-value"=  ChisQTest$test$pvalue)
                      )))
  class(fullModel) <- "modelsummary_list"   # define the class
  
  
  return(list("Estimation"=model,"Tidy"=tidy.final,"Correction"={{Correction}},"ModelSummary"=fullModel))    
}


#



#######################################################################################-
#######################################################################################-

#### Graphs Preferred Mode  ####

#######################################################################################-
#######################################################################################-

library(stringr)


# Function to count reasons by group
count_reasons_by_group <- function(data_column, group_column) {
  # Identify the unique levels
  group_levels <- unique(group_column[!is.na(group_column)])
  
  # Filter NAs
  valid_data <- data.frame(
    response = data_column,
    group = group_column
  ) %>% filter(!is.na(response))
  
  # Count the reasons
  count_reason <- function(data, patterns, group_value) {
    mentions <- unlist(strsplit(data$response[data$group == group_value], ","))
    if(length(mentions) == 0) return(0)
    sum(sapply(mentions, function(x) any(sapply(patterns, function(p) str_detect(x, fixed(p))))))
  }
  
  # List of patterns
  reasons_patterns <- list(
    "Too expensive" = c("Le mode de garde que je préférais coûtait trop cher", "Too expensive"),
    "No slot available" = c("Je n'ai pas eu de place dans le mode de garde que je préférais", "Rejected", "Still waiting/No Answer"),
    "Incompatible working hours" = c("Mes horaires de travail ne sont pas compatibles", "Incompatible working hours"),
    "Health issues" = c("Mon bébé n'allait pas bien / les médecins m'ont déconseillés", "Health issues"),
    "Lack of information" = c("Je pensais que je n'étais pas éligible", "Lack of information"),
    "Gave up" = "Je me suis découragée",
    "Applied too late" = "Applied too late",
    "Inadequate" = "Inadequate",
    "Father's disagreed" = "Father's disagreed",
    "Moved out" = "Moved out",
    "Paperwork too heavy" = "Paperwork too heavy",
    "Timing" = "Timing",
    "Too far away" = "Too far away"
  )
  
  # Count of reasons per group
  counts_list <- lapply(group_levels, function(level) {
    sapply(reasons_patterns, function(patterns) count_reason(valid_data, patterns, level))
  })
  
  # Create dataframe with results
  result_df <- data.frame(
    reason = rep(names(reasons_patterns), length(group_levels)),
    count = unlist(counts_list),
    group = rep(group_levels, each = length(reasons_patterns))
  )
  
  # Calculate significance using chi-square test for each reason
  significance_tests <- lapply(unique(result_df$reason), function(r) {
    contingency <- matrix(
      result_df$count[result_df$reason == r],
      nrow = 2,
      byrow = FALSE
    )
    test <- chisq.test(contingency)
    data.frame(
      reason = r,
      p_value = test$p.value,
      significance = case_when(
        test$p.value < 0.001 ~ "***",
        test$p.value < 0.01 ~ "**",
        test$p.value < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
  })
  
  significance_df <- do.call(rbind, significance_tests)
  result_df <- merge(result_df, significance_df, by = "reason")
  
  # Calculate totals and order
  totals <- aggregate(count ~ reason, result_df, sum)
  result_df$reason <- factor(result_df$reason, 
                             levels = totals$reason[order(totals$count)])
  
  return(result_df)
}

# Create plot function with significance stars
create_reasons_plot <- function(results_df, title = "Reasons by Group") {
  ggplot(results_df, aes(x = reason, y = count, fill = group)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#EE6677", "#CCBB44")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12)
    ) +
    labs(
      title = title,
      x = "Reason",
      y = "Count"
    ) +
    coord_flip() 
  # Add significance stars
  # +geom_text(aes(label = significance), 
  #           position = position_dodge(width = 0.9),
  #           vjust = -0.5, 
  #           size = 4)
}
