functions{

  real probR_density_R(real x,          // Function argument
                    real xc,         // Complement of function argument
                                     //  on the domain (defined later)
                    array[] real theta,    // parameters (更新语法)
                    array[] real x_r,      // data (real) (更新语法)
                    array[] int x_i ) {     // data (integer) (更新语法)
    
    real m = theta[1];
    int L = x_i[1];
    int max_val = 24*L;
    int min_val = 1;
    real temp = theta[2];
    int range = (max_val - min_val+1)/2; // We add 1 to make sure that truncation doesn't exclude a number
    int mid_pt = min_val + range;
    int out;
    real s1;
    real s2;

    while(range > 0) {
      if(temp == mid_pt){
        out = mid_pt;
        range = 0;
      } else {
        // figure out if range == 1
        range =  (range+1)/2; 
        mid_pt = temp > mid_pt ? mid_pt + range: mid_pt - range; 
        }
    }

    s1 = x_r[out];
    s2 = x_r[(24*L)+out];

    return erf((x/m + s2)/sqrt(2)) * exp(-0.5*((x-s1))^2);
  }

  real probR_density_L(real x,          // Function argument
                    real xc,         // Complement of function argument
                                     //  on the domain (defined later)
                    array[] real theta,    // parameters (更新语法)
                    array[] real x_r,      // data (real) (更新语法)
                    array[] int x_i ) {     // data (integer) (更新语法)
    real m = theta[1];
    int L = x_i[1];
    int max_val = 24*L;
    int min_val = 1;
    real temp = theta[2];
    int range = (max_val - min_val+1)/2; // We add 1 to make sure that truncation doesn't exclude a number
    int mid_pt = min_val + range;
    int out;
    real s1;
    real s2;

    while(range > 0) {
      if(temp == mid_pt){
        out = mid_pt;
        range = 0;
      } else {
        // figure out if range == 1
        range =  (range+1)/2; 
        mid_pt = temp > mid_pt ? mid_pt + range: mid_pt - range; 
        }
    }

    s1 = x_r[out];
    s2 = x_r[(24*L)+out];

    return erf((s2 - x/m)/sqrt(2)) * exp(-0.5*((x-s1))^2);
  }
   
  real partial_sum(array[] int trials, int start, int end, array[] int choice, array[] real conf, vector probR, vector model_conf){
      return bernoulli_lpmf(choice[start:end] | probR[start:end]) + normal_lpdf(conf[start:end] | model_conf[start:end], 0.0125); 
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

transformed data{
  array[1] int x_i = {L}; // 更新语法
  array[2*24*L] real x_r = append_array(coh1Int, coh2Int); // 更新语法
}

parameters {
  real<lower=0> m1_mu; // weighting of prior in decision 
  real<lower=0> m2_mu; // weighting of prior in confidence
  real<lower=0> b_mu; // confidence bias
  real<lower=0> m1_sd;
  real<lower=0> m2_sd;
  real<lower=0> b_sd;

  // define subject-level parameters
  array[L] real<lower=0> m1; // 更新语法
  array[L] real<lower=0> m2; // 更新语法
  array[L] real<lower=0> b; // 更新语法
}

transformed parameters{
  vector[N] probR;
  vector[N] model_conf; 

    vector[24*L] integrals_R;
    vector[24*L] integrals_L;
    real<lower=0,upper=1> confPrior;
    real<lower=0,upper=1> confR;
    real<lower=0,upper=1> confL;
    real<lower=0> m1_subj;
    real ind;
    real temp;

    for (i in 1:(24*L)){
      m1_subj = m1[intMap[i]];
      ind = i;
      integrals_R[i] = 0.5 + (1/(2*Phi(coh1Int[i])))   * (1/(sqrt(2*pi()))) *
          integrate_1d(probR_density_R, 0,positive_infinity(),{m1_subj, ind}, x_r , x_i, 1e-8); //
        
        integrals_L[i] = 0.5 + (1/(2*(1-Phi(coh1Int[i])))) * (1/(sqrt(2*pi()))) *
          integrate_1d(probR_density_L,
                                 negative_infinity(),
                                 0,
                                 {m1_subj, ind}, x_r, x_i, 1e-8); //
    }

    for (i in 1:N){
        if (choice1[i]==1){
          probR[i] = integrals_R[levels[i]];
        }else{
          probR[i] = integrals_L[levels[i]];
        }
        
        // confPrior = Phi_approx(fabs(coh1Int[levels[i]])/(b[ll[i]]*m2[ll[i]]));
        confPrior = Phi_approx(abs(coh1Int[levels[i]])/(b[ll[i]]*m2[ll[i]]));

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
    }
}

model {
    // priors 
    m1_mu ~ lognormal(0,0.5); 
    m2_mu ~ lognormal(0,0.5); 
    b_mu ~ lognormal(0,0.5); 

    m1_sd ~ lognormal(0,0.5); //lognormal(1,1); 
    m2_sd ~ lognormal(0,0.5); //lognormal(1,1); 
    b_sd ~ lognormal(0,0.5); //lognormal(1,1); 

    m1 ~ normal(m1_mu,m1_sd); 
    m2 ~ normal(m2_mu,m2_sd); 
    b ~ normal(b_mu,b_sd); 
    
    target += reduce_sum(partial_sum, trials, grainsize, choice, conf, probR, model_conf);
}
