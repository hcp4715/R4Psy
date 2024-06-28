#Author: Giacomo Bignardi
#Date: 2023-05-02
#Function to calculate Variance Component from an MLM
#(-adapted from Sutherland, Burton et al, 2020, PNAS; and Martinez et al. 2020, Behav Res Methods-) 
#input: lmer() intercept only model e.g. lmer(Rating ~ 1 + ((1|Sub) + (1 |Obj) + (1|Sub:Obj) + (1|Block) + (1|Block:Sub) + (1|Block:Obj)),data=yourData)
#if CI are left to T then use both bootMer and VCA_exposure_tidyCI to interpret output
VCA_exposure = function(mlm, ci = T) {
  #get a summary of the MLM to extract variances of random effects
  sum_mlm = summary(mlm)
  #calculate the total variance (including the residual term)
  total_variance =
    (sum_mlm$varcor$`Sub:Obj`[1])+
    (sum_mlm$varcor$`Sub`[1])+
    (sum_mlm$varcor$`Obj`[1])+ 
    #extend the VPC to the effect of exposure
    (sum_mlm$varcor$`Block`[1])+ 
    (sum_mlm$varcor$`Block:Obj`[1])+ 
    (sum_mlm$varcor$`Block:Sub`[1])+ 
    (sum_mlm$sigma^2)
  
  #calculate the repeatable variance (excluding the residual term)
  repeatable_variance =
    (sum_mlm$varcor$`Sub:Obj`[1])+
    (sum_mlm$varcor$`Sub`[1])+
    (sum_mlm$varcor$`Obj`[1])+ 
    #extend the VPC to the effect of exposure
    (sum_mlm$varcor$`Block`[1])+ 
    (sum_mlm$varcor$`Block:Obj`[1])+ 
    (sum_mlm$varcor$`Block:Sub`[1])
  
  
  #calculate variance components (over overall variance)
  VCSub1 = (sum_mlm$varcor$`Sub`[1]) / total_variance
  VCSubXObj1 = (sum_mlm$varcor$`Sub:Obj`[1]) / total_variance 
  VCObj1 = (sum_mlm$varcor$`Obj`[1]) / total_variance
  VCBlock1 = (sum_mlm$varcor$`Block`[1]) / total_variance 
  VCBlockXSub1 = (sum_mlm$varcor$`Block:Sub`[1]) /total_variance 
  VCBlockXObj1 = (sum_mlm$varcor$`Block:Obj`[1]) / total_variance 
  VCResidual1 = (sum_mlm$sigma^2) / total_variance
  
  #calculate variance components (over repeatable variance)
  VCSub2 = (sum_mlm$varcor$`Sub`[1]) / repeatable_variance
  VCSubXObj2 = (sum_mlm$varcor$`Sub:Obj`[1]) / repeatable_variance 
  VCObj2 = (sum_mlm$varcor$`Obj`[1]) / repeatable_variance
  VCBlock2 = (sum_mlm$varcor$`Block`[1]) / repeatable_variance 
  VCBlockXSub2 = (sum_mlm$varcor$`Block:Sub`[1]) /repeatable_variance 
  VCBlockXObj2 = (sum_mlm$varcor$`Block:Obj`[1]) / repeatable_variance 
  VCResidual2 = NA
  
  #honekopp index
  Vrepeat = 1 - VCResidual1
  mbi1 = (VCSub1 + VCSubXObj1 + VCBlockXSub1)
  mbi2 = (VCSub2 + VCSubXObj2 + VCBlockXSub2)
  Vshar1 = 1 - mbi1
  Vshar2 = 1 - mbi2
  if (ci == F){
  #spit the output (7X3 dataframe) values for both total [,2] or repeatable [,3] variance components
  data.frame(
    VPC = c("Stimulus", "Individual", "Exposure", "Stimulus*Individual", "Exposure*Individual", "Stimulus*Exposure", "Residual",
            "Repeatable",
            "Unique",
            "Shared"),
    total = c(VCObj1, VCSub1, VCBlock1, VCSubXObj1, VCBlockXSub1, VCBlockXObj1, VCResidual1, Vrepeat, mbi1, Vshar1),
    repeatable = c(VCObj2, VCSub2, VCBlock2, VCSubXObj2, VCBlockXSub2, VCBlockXObj2, VCResidual2, NA, mbi2, Vshar2)
  )
  }else{
    c(VCObj1, VCSub1, VCBlock1, VCSubXObj1, VCBlockXSub1, VCBlockXObj1, VCResidual1, #c("Stimulus", "Individual", "Exposure", "Stimulus*Individual", "Exposure*Individual", "Stimulus*Exposure", "Residual")
      VCObj2, VCSub2, VCBlock2, VCSubXObj2, VCBlockXSub2, VCBlockXObj2, # repeatable c("Stimulus", "Individual", "Exposure", "Stimulus*Individual", "Exposure*Individual", "Stimulus*Exposure")
      Vrepeat, #total repeatable variance
      mbi1, #modified Beholder index 
      mbi2, #modified Beholder index repeatable variance
      Vshar1, #shared variance
      Vshar2 #shared repeatable variance
      )
  }
}
