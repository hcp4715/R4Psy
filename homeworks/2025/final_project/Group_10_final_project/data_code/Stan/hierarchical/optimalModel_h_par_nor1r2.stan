functions{  
  real partial_sum(array[] int trials, int start, int end, array[] real conf, vector model_conf){
      return normal_lpdf(conf[start:end] | model_conf[start:end], 0.0125); 
  }
}

data {
  int<lower=0> N; // total number of trials
  int<lower=1> L; // number of participants
  array[N] int<lower=0,upper=L> ll; // 更新语法
  array[24*L] int<lower=0,upper=L> intMap; // 更新语法
  array[N] real coh1; // adjusted signed prior coherence (更新语法)
  array[N] real coh2; // adjusted signed target coherence (更新语法)
  array[N] int<lower=0,upper=1> choice1; // participants' responses to decision 1 (更新语法)
  array[N] int<lower=0,upper=1> choice; // participants' responses to the targets (更新语法)
  array[N] real<lower=0.5,upper=1> conf; // participants' confidence ratings (更新语法)
  array[N] int trials; // 更新语法
  array[24*L] real coh1Int; // 更新语法
  array[24*L] real coh2Int; // 更新语法
  array[N] int levels; // 更新语法
  int<lower=1> grainsize;
}

parameters {
  real<lower=0> b_mu; // confidence bias
  real<lower=0> b_sd;

  // define subject-level parameters
  array[L] real<lower=0> b; // 更新语法
}

transformed parameters{
  vector[N] model_conf; 

    real<lower=0,upper=1> confPrior;
    real<lower=0,upper=1> confR;
    real<lower=0,upper=1> confL;
   
    real temp;

    for (i in 1:N){
        confPrior = Phi_approx(1*fabs(coh1Int[levels[i]])/(b[ll[i]]));

        temp = coh2Int[levels[i]]/(b[ll[i]]*sqrt(2));
        if (temp <= -4){
          temp = -4;
        }
 
        confR = (confPrior*(1+erf(temp)))/(1 + (2*confPrior-1)*(erf(temp)));
        confL = 1 - confR;

        if (choice[i]==1){
          model_conf[i] = confR;
        }else{
          model_conf[i] = confL;
        }
        //if (model_conf[i] < 0.5){
        //  model_conf[i] = 0.5;
        //}
    }
}

model {
    // priors 
    b_mu ~ lognormal(1,1); 
    b_sd ~ lognormal(1,1);

    b ~ normal(b_mu,b_sd); 
    
    target += reduce_sum(partial_sum, trials, grainsize, conf, model_conf);
}
