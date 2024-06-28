#Author: Giacomo Bignardi
#Date: 2023-02-13
#Function to calculate Variance Component from an MLM
#(adapted from Sutherland, Burton et al, 2020, PNAS; and Martinez et al. 2020, Behav Res Methods) 
#input: lmer() intercept only model e.g. lmer(Rating ~ 1 + ((1|Sub) + (1 |Obj) + (1|Sub:Obj) + (1|Block) + (1|Block:Sub) + (1|Block:Obj)),data=yourData)
rcVCA = function(mlm, null = T, nullmodel) {
  
  if (null == T){
  #get a summary of the MLM to extract variances of random effects
  sum_mlm = summary(mlm)
  #calculate the total variance (including the residual term)
  total_variance =
    (sum_mlm$varcor$`Sub:Obj`[1])+
    (sum_mlm$varcor$`Sub`[1])+
    (sum_mlm$varcor$`Obj`[1])+ 
    # #extend the VPC to the effect of exposure
    # (sum_mlm$varcor$`Block`[1])+ 
    # (sum_mlm$varcor$`Block:Obj`[1])+ 
    # (sum_mlm$varcor$`Block:Sub`[1])+ 
    (sum_mlm$sigma^2)
  
  #calculate the repeatable variance (excluding the residual term)
  repeatable_variance =
    (sum_mlm$varcor$`Sub:Obj`[1])+
    (sum_mlm$varcor$`Sub`[1])+
    (sum_mlm$varcor$`Obj`[1])
    # #extend the VPC to the effect of exposure
    # (sum_mlm$varcor$`Block`[1])+ 
    # (sum_mlm$varcor$`Block:Obj`[1])+ 
    # (sum_mlm$varcor$`Block:Sub`[1])
  
  
  #calculate variance components (over overall variance)
  VCSub1 = (sum_mlm$varcor$`Sub`[1]) / total_variance
  VCSubXObj1 = (sum_mlm$varcor$`Sub:Obj`[1]) / total_variance 
  VCObj1 = (sum_mlm$varcor$`Obj`[1]) / total_variance
  # VCBlock1 = (sum_mlm$varcor$`Block`[1]) / total_variance 
  # VCBlockXSub1 = (sum_mlm$varcor$`Block:Sub`[1]) /total_variance 
  # VCBlockXObj1 = (sum_mlm$varcor$`Block:Obj`[1]) / total_variance 
  VCResidual1 = (sum_mlm$sigma^2) / total_variance
  
  #calculate variance components (over repeatable variance)
  VCSub2 = (sum_mlm$varcor$`Sub`[1]) / repeatable_variance
  VCSubXObj2 = (sum_mlm$varcor$`Sub:Obj`[1]) / repeatable_variance 
  VCObj2 = (sum_mlm$varcor$`Obj`[1]) / repeatable_variance
  # VCBlock2 = (sum_mlm$varcor$`Block`[1]) / repeatable_variance 
  # VCBlockXSub2 = (sum_mlm$varcor$`Block:Sub`[1]) /repeatable_variance 
  # VCBlockXObj2 = (sum_mlm$varcor$`Block:Obj`[1]) / repeatable_variance 
  VCResidual2 = NA
  
  #spit the output (7X3 dataframe) values for both total [,2] or repeatable [,3] variance components
  data.frame(
    VPC = c("Stimulus", "Individual", "Stimulus*Individual", "Residual"),
    total = c(VCObj1, VCSub1, VCSubXObj1, VCResidual1),
    repeatable = c(VCObj2, VCSub2, VCSubXObj2, VCResidual2)
  )
  } else {
    sum_nullmlm = summary(nullmodel)
    sum_mlm = summary(mlm)
    #calculate the total variance (including the residual term)
    total_variance =
      (sum_nullmlm$varcor$`Sub:Obj`[1])+
      (sum_nullmlm$varcor$`Sub`[1])+
      (sum_nullmlm$varcor$`Obj`[1])+ 
      #extend the VPC to the effect of exposure
      # (sum_nullmlm$varcor$`Block`[1])+ 
      # (sum_nullmlm$varcor$`Block:Obj`[1])+ 
      # (sum_nullmlm$varcor$`Block:Sub`[1])+ 
      (sum_nullmlm$sigma^2)
    
    #calculate the repeatable variance (excluding the residual term)
    repeatable_variance =
      (sum_nullmlm$varcor$`Sub:Obj`[1])+
      (sum_nullmlm$varcor$`Sub`[1])+
      (sum_nullmlm$varcor$`Obj`[1])
      # #extend the VPC to the effect of exposure
      # (sum_nullmlm$varcor$`Block`[1])+ 
      # (sum_nullmlm$varcor$`Block:Obj`[1])+ 
      # (sum_nullmlm$varcor$`Block:Sub`[1])
    
    
    #calculate variance components (over overall variance)
    VCSub1 = (sum_mlm$varcor$`Sub`[1]) / total_variance
    VCSubXObj1 = (sum_mlm$varcor$`Sub:Obj`[1]) / total_variance 
    VCObj1 = (sum_mlm$varcor$`Obj`[1]) / total_variance
    # VCBlock1 = (sum_mlm$varcor$`Block`[1]) / total_variance 
    # VCBlockXSub1 = (sum_mlm$varcor$`Block:Sub`[1]) /total_variance 
    # VCBlockXObj1 = (sum_mlm$varcor$`Block:Obj`[1]) / total_variance 
    VCResidual1 = NA
    
    #calculate variance components (over repeatable variance)
    VCSub2 = (sum_mlm$varcor$`Sub`[1]) / repeatable_variance
    VCSubXObj2 = (sum_mlm$varcor$`Sub:Obj`[1]) / repeatable_variance 
    VCObj2 = (sum_mlm$varcor$`Obj`[1]) / repeatable_variance
    # VCBlock2 = (sum_mlm$varcor$`Block`[1]) / repeatable_variance 
    # VCBlockXSub2 = (sum_mlm$varcor$`Block:Sub`[1]) /repeatable_variance 
    # VCBlockXObj2 = (sum_mlm$varcor$`Block:Obj`[1]) / repeatable_variance 
    VCResidual2 = NA
    
    #spit the output (7X3 dataframe) values for both total [,2] or repeatable [,3] variance components
    data.frame(
      VPC = c("Stimulus", "Individual", "Stimulus*Individual", "Residual"),
      total = c(VCObj1, VCSub1, VCSubXObj1, VCResidual1),
      repeatable = c(VCObj2, VCSub2, VCSubXObj2, VCResidual2)
    )
  }
}
