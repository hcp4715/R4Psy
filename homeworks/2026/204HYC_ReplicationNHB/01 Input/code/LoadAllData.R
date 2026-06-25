#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#
#---------------------     Investigating how administrative burden and search costs        -----------------------------------#               
#--------------------                       affect social inequalities                     -----------------------------------#  
# --------------------      in early childcare access, a randomised controlled trial       -----------------------------------#
#                                                   -------------
#--------------------                    Loading and cleaning all data                     -----------------------------------# 
#--------------------                          Authors: XX & XX                            -----------------------------------#    
#--------------------                               Version 1                              -----------------------------------#  
#--------------------                               June 2024                              -----------------------------------#     
#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#

##### Load Data and clean #####%
#here::iam()


#---- LoadSource ----
# Original baseline dataset with the number of refusals
BaselineFull <- read_csv(here("Data","BaselineWithRefusals.csv")) 
#read_csv("Data/BaselineWithRefusals.csv")
# Import the main database with both baseline and endline information for all participants
MainDB <- read_csv(here("Data","MainDatabase.csv")) %>%   mutate(
  NormsBaseline=factor(NormsBaseline,
                       levels=c("Ne sait pas","Aucune","Une minorité","Moitié moitié","La plupart","Toutes"), ordered = TRUE # how many mothers in their surroundings use childcare 
  ), 
  Dep = as.factor(Dep), 
  FmilyEarnLessThan2500 = as.factor(FmilyEarnLessThan2500), 
  NumberOfChildren3 = as.factor(NumberOfChildren3), 
  NormsOpposedYes = as.factor(NormsOpposedYes),
  DescriptiveNorms = ifelse(DescriptiveNorms == "Yes", "Majority", "Minority"),
  DescriptiveNorms = as.factor(DescriptiveNorms), 
  UsedECEC=as.factor(ifelse(UsedECEC == "Yes","Already used","Never used")), 
  InfoBaseline=as.factor(ifelse(LevelInfoSubExPost == "Aucun ou très bas","Low knowledge","High knowledge")),
  TrustCreche=ifelse(TrustCreche1or0 == "Yes","High","Low"),
  BelieveBenefits = as.factor(ifelse(LikertReturnHK1or0 == "Yes", "Believe in  benefits", "Don’t believe")),
  MigrationBackground = ifelse(
    FrenchYNBaseline == "Abroad", 
    "Yes", 
    "No"
  ),
  MigrationBackgroundParent2 = ifelse(
    (BirthPlace2 == "En France métropolitaine" | BirthPlace2 == "Dans un territoire français d’outre-mer"), 
    "No", 
    "Yes"
  ),
  MigrationBackgroundOneOfTheTwo = case_when(
    (MigrationBackground == "Yes" | MigrationBackgroundParent2 == "Yes") ~ "Yes", 
    TRUE ~ "No"
  ),
  MigrationBackground = as.factor(MigrationBackground), 
  MigrationBackgroundBoth = case_when(
    (MigrationBackground == "Yes" & is.na(MigrationBackgroundParent2)) ~ "Yes", 
      (MigrationBackground == "Yes" & MigrationBackgroundParent2 == "Yes") ~ "Yes", 
      TRUE ~ "No"
    ),
  GenderChild = as.factor(ifelse(BabyFemale == TRUE, "Girl", "Boy"))
)  %>% mutate_if(is.character, as.factor) 





#confession juive","pas en France ", "Hésitait", "idée qu'elle considère", 
#"pas le temps", "convenait pas finalement", "regroupement familial"
# Descrtiptive statistics 

## For descriptive stats of the coverage rates in 2021 we used data from https://data.caf.fr/explore/dataset/txcouv_pe_com/information/?disjunctive.annee&refine.annee=2021
## We import the data for the coverage rates in 2021 outside from the cities in the sample
### Import the files
txcouv_pe_com_EAJE_assmat <- read.csv(here("Data", "Spatial data","txcouv_pe_com_EAJE_assmat.csv"),sep=";")%>% filter(Date.référence == "2021") %>% mutate(NationalAvgTauxCouv2021 = round(mean(Taux.de.couv.global, na.rm = TRUE), 1)) 

# Import  shape data for the map at the city level

# Note: here they have data on PMI and EAJE in SSD https://data.iledefrance.fr/explore/?sort=modified&refine.theme=Logement+-+sant%C3%A9+-+social

# Import our map at the city level
gp_shape <- st_read(file.path(here("Data", "Spatial data", 
                                   "RECENSEMENT_COMMUNE_POPULATION.shp")))

#---- MainDBPrep ----


PredVars <- c(#"Age",
  #"SingleMum",
  #"Couple",
  #"CoupleCohabiting",
  #"PregMonth",
  #"Educ",
  #"Act3",
  #"FrenchYNBaseline",
  #"FmlyIncomeBaseline",
  #"IncomeBaseline",
  #"ECSUsed",
  #"ECSNeedBaseline",
  #"NumberChildren",
  #"Primipare",
  #"WorkBefore",
  #"WorkPlanTo",
  #"WorkPlanNotTo",
  #"WorkNoPlan",
  #  "ComputerYN",
  #"HighLowearly childcareBaseline",
  #"Dep",
  "Assignment",
  "StrataWave"
  # "KnowsCrecheOnly",
  #  "SmokePreg",
  #  "Primipare",
  #  "WorkPlanTo"#,
  # "LikertReturnHK1or0Discount501or0"
  #,
  #  "Assignment",
  # "StrataWave"
)



# Convert character variables to factor
char_vars <- lapply(MainDB, class) %in% c('character','logical')
MainDB[, char_vars] <- lapply(MainDB[, char_vars], as.factor)

DBResponse <- MainDB %>% select(Responded, one_of(PredVars))%>% as.data.frame()

ps_fit <- ps(Responded ~ . ,## Use all variables
             data=DBResponse, ## Select just the response and specified predictors
             estimand='ATE', ## ATE: Generalize to all those invited for the survey, not just responders
             verbose=FALSE
)


# get the weights
DBResponse$weight <- get.weights(ps_fit, stop.method="es.mean")
DBResponse$PsRX <- ps_fit$ps$es.mean.ATE

survey_trick <- rbind(
  data.frame(DBResponse, trt_trick=1, weight_trick=1) # full sample
  , data.frame(DBResponse, trt_trick=0, weight_trick=DBResponse$weight) %>%
    filter(Responded==1) # Responders, weighted
) # Trick dx.wts function to summarize this data set to compare if weighted responders are similar to the full sample (ATT since full sample has trt_trick=1 as trick treatment)



MainDB$WeightBalance <- DBResponse$weight

###### Compute propensity scores of treatment assignments
# I use a multinomial logit of assignment over blocks within each wave (i.e. the exact randomisation design)
# multinom is taken from the package glmnet
(fit_basic <- multinom(Assignment ~ StrataWave, data = MainDB))
#%>% invisible()


# Then retrieve the predicted probability

MainDB$predT1 <- predict(fit_basic,MainDB,"probs")[,"T1"]
MainDB$predT2 <- predict(fit_basic,MainDB,"probs")[,"T2"]

# From there, we generate centred treatment dummies

MainDB <- MainDB %>% mutate(Z1=ifelse(Assignment=="T1",1,0),
                            Z2=ifelse(Assignment=="T2",1,0),
                            Z1.c=Z1-predT1,
                            Z2.c=Z2-predT2
)




# Ultimately, we want to test all pairwise comparison of attrition rate across groups. So Thats how we do it
SampleT1C <- MainDB %>%filter( Assignment %in% c("Control","T1")) %>%
  mutate(SubSample="T1-C") %>% mutate(Z=ifelse(Assignment=="T1",1,0), 
                                      Z.c = Z-predT1, 
                                      WeightPS=Z/predT1+(1-Z)/(1-predT1))

SampleT2C <- MainDB %>%filter( Assignment %in% c("Control","T2"))%>% 
  mutate(SubSample="T2-C") %>% mutate(Z=ifelse(Assignment=="T2",1,0),
                                      Z.c=Z-predT2,
                                      WeightPS=Z/predT2+(1-Z)/(1-predT2))
SampleT2T1 <- MainDB %>%filter( Assignment %in% c("T2","T1"))%>% mutate(SubSample="T2-T1") %>% mutate(Z=ifelse(Assignment=="T2",1,0), 
                                                                                                      Z.c=Z-predT2,
                                                                                                      WeightPS=Z/predT2+(1-Z)/(1-predT2))                                                                          

# bind them

StackedDB <- bind_rows(SampleT1C,SampleT2C,SampleT2T1) %>% mutate(Treat=paste("Z=",Z,":",SubSample,sep=""),
                                                                  SubSampleStrata=paste(SubSample,StrataWave,sep=":")
)

# Propensity score for each pairwise comparison: simple probit
fit_basicStack <- feglm(Z ~ 1|SubSampleStrata, data = StackedDB,"probit",cluster = ~StrataWave)
#retrieve the prediction
StackedDB$psscore=predict(fit_basicStack,StackedDB)

StackedDB <- StackedDB %>% mutate(
  Z.c=Z-psscore,
  WeightPS=Z/psscore+(1-Z)/(1-psscore),
  ZT2C.c =Z.c*as.numeric(SubSample=="T2-C"),
  ZT1C.c =Z.c*as.numeric(SubSample=="T1-C"),
  ZT2T1.c=Z.c*as.numeric(SubSample=="T2-T1")
)



# Prepare all the covariates

#------ PrepCovariates ------
#Here, we build the "\dot{X}" matrix and the interaction with the treatment the we will use in all models for the Lasso
# The idea is to take each variable we consider, to generate dummies, and even centre them by block to mimic the fixed effect regression. We center every variable by block essentially.
# then, for the lasso, all covariates are centred, we can interact them with treatment and therefore keep and ATE interpretation of the coefficient.

## MaternityWardBaseline
matid <- StackedDB %>% select(ResponseId,SubSampleStrata,SubSample,MaternityWardBaseline) 
# everything as dummies
mat <- model.matrix(~0+MaternityWardBaseline,matid) %>% as.data.frame() 
# centred
mat.c <-  mat %>% bind_cols(.,matid %>% select(ResponseId,SubSampleStrata,SubSample)) %>% 
  group_by(SubSampleStrata) %>% mutate_at(all_of(names(mat)),~.x-mean(.))%>% ungroup()

## AgeChild1:AgeChild8,Primipare,Age, PregMonth
AgeChildenId <- StackedDB %>% select(ResponseId,SubSampleStrata,SubSample,Primipare,AgeChild1:AgeChild8,Age, PregMonth) %>% 
  mutate_at(vars(Primipare:AgeChild8,Age, PregMonth),~ifelse(is.na(.),0,.)) 
AgeChilden <-  model.matrix(~0+.,AgeChildenId %>% select(-c(ResponseId,SubSampleStrata,SubSample))) %>% as.data.frame() 


AgeChilden.c <- AgeChilden %>% bind_cols(.,AgeChildenId %>% select(ResponseId,SubSampleStrata,SubSample)) %>% 
  group_by(SubSampleStrata) %>% mutate_at(all_of(names(AgeChilden)),~.x-mean(.))%>% ungroup()

#KnowsCreche:KnowsNothing
#KnowsId <- StackedDB %>% select(ResponseId,SubSampleStrata,SubSample,KnowsCreche:KnowsCrecheOnly) 
KnowsId <- StackedDB %>% select(ResponseId,SubSampleStrata,SubSample,KnownNbTypeECS,KnowsCreche)
Knows <- model.matrix(~0+.,KnowsId %>% select(-c(ResponseId,SubSampleStrata,SubSample))) %>% as.data.frame() 
Knows.c <- Knows   %>% bind_cols(.,KnowsId %>% select(ResponseId,SubSampleStrata,SubSample)) %>% 
  group_by(SubSampleStrata)  %>% mutate_at(all_of(names(Knows)),~.x-mean(.))%>% ungroup()


# Relationship LanguageBaseline, OrderedEduc, Act3, FrenchYNBaseline

SocDemoId <- StackedDB %>% select(ResponseId,SubSampleStrata,SubSample,
                                  Relationship,LanguageBaseline, OrderedEduc, Act3, FrenchYNBaseline,Dep,
                                  "WorkBefore","WorkPlanTo" ,
                                  "WorkPlanNotTo","WorkNoPlan" 
) %>% mutate(Dep=as.factor(Dep))
SocDemo <-  model.matrix(~0+.,SocDemoId %>% select(-c(ResponseId,SubSampleStrata,SubSample))) %>% as.data.frame() 
SocDemo.c <- SocDemo %>% bind_cols(.,SocDemoId %>% select(ResponseId,SubSampleStrata,SubSample)) %>% 
  group_by(SubSampleStrata)  %>% mutate_at(all_of(names(SocDemo)),~.x-mean(.))%>% ungroup()

# FmlyIncome, Income at Baseline
# the authors corrected a typo between the initial submission and the second version of the manuscript

IncomesId <- StackedDB %>% select(ResponseId,SubSampleStrata,SubSample,
                                  FmlyIncome, Income) %>% 
  mutate_at(vars(FmlyIncome, Income),~as.character(.)) %>% 
  replace_na(list(FmlyIncome="NA", Income="NA"))

Incomes <-   model.matrix(~0+.,IncomesId %>% select(-c(ResponseId,SubSampleStrata,SubSample))) %>% as.data.frame()

Incomes.c <- Incomes %>% bind_cols(.,IncomesId %>%  select(ResponseId,SubSampleStrata,SubSample)) %>% 
  group_by(SubSampleStrata)  %>% mutate_at(all_of(names(Incomes)),~.x-mean(.))%>% ungroup()

SubjectiveId <-  StackedDB %>% select(ResponseId,SubSampleStrata,SubSample,
                                      "AccessEasyBaseline", "AccessHoursBaseline" ,"AccessInformalCareBaseline",
                                      "LikertReturnHKBaseline","LikertAccessInfoBaseline"  ,"LikertReturnHK1or0",
                                      "LikertAccessSoc","NormsBaseline" )

Subjective <- model.matrix(~0+.,SubjectiveId %>% select(-c(ResponseId,SubSampleStrata,SubSample))) %>% as.data.frame()

Subjective.c <- Subjective %>% bind_cols(.,SubjectiveId %>%  select(ResponseId,SubSampleStrata,SubSample)) %>% 
  group_by(SubSampleStrata)  %>% mutate_at(all_of(names(Subjective)),~.x-mean(.))%>% ungroup()


Depriveid <- StackedDB %>% select(ResponseId,SubSampleStrata,SubSample,
                                  "DeprivClothesIrrelevant" ,"DeprivFoodIrrelevant"  ,
                                  "DeprivBillsIrrelevant"   ,"DeprivHolidaysIrrelevant" ,
                                  "DeprivHousingIrrelevant" ,"DeprivClothes" ,
                                  "DeprivFood"              ,"DeprivBills"    ,
                                  "DeprivHolidays"          ,"DeprivHousing"  ,"LevelComprehensionExPost"  ) %>% mutate_at(vars(DeprivClothesIrrelevant:LevelComprehensionExPost),~ifelse(is.na(.),"NA",.))

Deprive <- model.matrix(~0+.,Depriveid %>% select(-c(ResponseId,SubSampleStrata,SubSample))) %>% as.data.frame()

Deprive.c <- Deprive %>% bind_cols(.,Depriveid %>%  select(ResponseId,SubSampleStrata,SubSample)) %>% 
  group_by(SubSampleStrata)  %>% mutate_at(all_of(names(Deprive)),~.x-mean(.)) %>% ungroup()


# Vector of outcome names
Outcomes <- c("ECSApp","ECSUseYes","UseCreche","AppCreche","AppAssMat","Suivi_administratif1_0")
# Vector of secondary outcomes' names
sec.Outcomes <- c("ECSIdealYes","UsePrivée","ECSAppTiming","KnowsAssMatEndline")

# Databases to use
PostDB <- StackedDB %>% filter(Responded==1) %>% ungroup()

# When we want to compare T2 with either T1 or C ; and we generate D for suivi administratif
PostDBT2 <- PostDB %>% filter(str_detect(SubSample,"T2")) %>% mutate(D=Suivi_administratif1_0) %>% ungroup()


# When we want to compare T2 with either T1 or C ; and we generate D for suivi administratif
PostDBT1C <- PostDB %>% filter(SubSample == "T1-C") %>% ungroup()

# When we want to compare T2 with either T1 or C ; and we generate D for suivi administratif
PostDBT2C <- PostDBT2 %>% filter(SubSample == "T2-C") %>% ungroup()
# Matrix of centred potential covariates
X.c <- left_join(mat.c,AgeChilden.c,by = join_by(ResponseId, SubSampleStrata, SubSample)) %>%
  left_join(.,Knows.c,by = join_by(ResponseId, SubSampleStrata, SubSample)) %>% 
  left_join(.,SocDemo.c,by = join_by(ResponseId, SubSampleStrata, SubSample)) %>% 
  left_join(.,Incomes.c,by = join_by(ResponseId, SubSampleStrata, SubSample)) %>% 
  left_join(.,Subjective.c,by = join_by(ResponseId, SubSampleStrata, SubSample)) %>% 
  left_join(.,Deprive.c,by = join_by(ResponseId, SubSampleStrata, SubSample)) %>% 
  left_join(.,StackedDB %>% select(ResponseId, SubSampleStrata, SubSample,Responded))# %>% mutate(Sub)


## For logit models


reg_MainDB <- MainDB %>%
  dplyr::select(ResponseId, ECSApp, ECSUseYes, AppCreche, 
                UseCreche, Assignment, StrataWave, Educ2, FrenchYNBaseline, HighLowECECBaseline, 
                InfoBaseline, Discount501or0,HigherThanMeadianISEIMother,HigherThanMeadianSESIndex, 
                HighLowCoverageGlobal, UsedECEC, Responded, ResponseStatus, T1, T2, High_SES,
                NoMigrationBackground, HighCoverageBaseline, High_knowledge, HighCoverage_total, PresentOrientated) %>% filter(Responded==1) %>% 
  mutate(StrataWave=as.factor(StrataWave)) 


# remove unecessary datasets
rm(SocDemo, SocDemo.c, SocDemoId, 
         Subjective, SubjectiveId, Subjective.c,
         AgeChilden, AgeChildenId, AgeChilden.c,
         Deprive, Depriveid, Deprive.c,
         Incomes, IncomesId, Incomes.c,
         Knows, KnowsId, Knows.c,
         mat, matid, mat.c,
         SampleT1C, SampleT2C, SampleT2T1,
         DBResponse, PredVars, fit_basic, fit_basicStack,
         survey_trick, ps_fit
         ) 

## Theme
#vis_theme <- theme(
#  # Agrandir tous les textes
#  text = element_text(size = 12),
#  # Titres de facettes plus grands
#  strip.text = element_text(size = 12, face = "bold"),
#  # Texte des axes plus grand
#  axis.title = element_text(size = 14, face = "bold"),
#  axis.text = element_text(size = 12),
#  # Légende plus visible
#  legend.title = element_text(size = 12, face = "bold"),
#  legend.text = element_text(size = 12),
#  # Notes de bas de page plus lisibles
#  plot.caption = element_text(size = 10, hjust = 0, margin = margin(t = 10)),
#  # Plus d'espace global
#  plot.margin = margin(15, 15, 15, 15)
#)

# Thème ajusté pour l'article avec meilleure gestion des longs labels
vis_theme <- theme(
  # Agrandir tous les textes mais rester raisonnable pour l'article
  text = element_text(size = 12),
  # Titres de facettes plus grands
  strip.text = element_text(size = 11, face = "bold"),
  # Texte des axes plus grand
  axis.title = element_text(size = 14, face = "bold"),
  axis.text = element_text(size = 12),
  # Marge additionnelle pour le texte d'axe y qui contient les longs labels
  axis.text.y = element_text(size = 12, margin = margin(r = 8)),
  # Légende plus visible
  legend.title = element_text(size = 12, face = "bold"),
  legend.text = element_text(size = 12),
  # Position de la légende ajustée pour libérer de l'espace
  legend.position = "bottom",
  # Réduire l'espacement des panneaux pour maximiser l'espace
  panel.spacing = unit(1, "lines"),
  # Notes de bas de page plus lisibles
  plot.caption = element_text(size = 10, hjust = 0, margin = margin(t = 10)),
  # Plus d'espace global mais plus conservateur en largeur
  plot.margin = margin(15, 10, 15, 15)
)


