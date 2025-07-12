functions{

  real probR_density_R(real x,          // Function argument
                    real xc,         // Complement of function argument
                                     //  on the domain (defined later)
                    array[] real theta,    // parameters (updated syntax)
                    array[] real x_r,      // data (real) (updated syntax)
                    array[] int x_i ) {     // data (integer) (updated syntax)
    
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
                    array[] real theta,    // parameters (updated syntax)
                    array[] real x_r,      // data (real) (updated syntax)
                    array[] int x_i ) {     // data (integer) (updated syntax)
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
  array[N] int<lower=0,upper=L> ll; // updated syntax
  array[24*L] int<lower=0,upper=L> intMap; // updated syntax
  array[N] real coh1; // adjusted signed prior coherence (updated syntax)
  array[N] real coh2; // adjusted signed target coherence (updated syntax)
  array[N] int<lower=0,upper=1> choice1; //participants' responses to decision 1 (updated syntax)
  array[N] int<lower=0,upper=1> choice; // participants' responses to the targets (updated syntax)
  array[N] real<lower=0.5,upper=1> conf; // participants' confidence ratings (updated syntax)
  array[N] int trials; // updated syntax
  array[24*L] real coh1Int; // updated syntax
  array[24*L] real coh2Int; // updated syntax
  array[N] int levels; // updated syntax
  int<lower=1> grainsize;
}

transformed data{
  array[1] int x_i = {L}; // updated syntax
  array[2*24*L] real x_r = append_array(coh1Int, coh2Int); // updated syntax
}

parameters {
  real<lower=0> m_mu; 
  real<lower=0> b_mu; // confidence bias
  real<lower=0> m_sd;
  real<lower=0> b_sd;

  // define subject-level parameters
  array[L] real<lower=0> m; // updated syntax
  array[L] real<lower=0> b; // updated syntax

 // vector[N] r1; // internal representation for stimulus 1
 // vector[N] r2; // internal representation for stimulus 2
}

transformed parameters{
  vector[N] probR;
  vector[N] model_conf; 

    vector[24*L] integrals_R;
    vector[24*L] integrals_L;
    real<lower=0,upper=1> confPrior;
    real<lower=0,upper=1> confR;
    real<lower=0,upper=1> confL;
    real<lower=0> m_subj;
    real ind;
    real temp;

    for (i in 1:(24*L)){
      m_subj = m[intMap[i]];
      ind = i;
      integrals_R[i] = 0.5 + (1/(2*Phi(coh1Int[i])))   * (1/(sqrt(2*pi()))) *
          integrate_1d(probR_density_R, 0,positive_infinity(),{m_subj, ind}, x_r , x_i, 1e-8); //
        
        integrals_L[i] = 0.5 + (1/(2*(1-Phi(coh1Int[i])))) * (1/(sqrt(2*pi()))) *
          integrate_1d(probR_density_L,
                                 negative_infinity(),
                                 0,
                                 {m_subj, ind}, x_r, x_i, 1e-8); //
    }

    for (i in 1:N){
        if (choice1[i]==1){
          probR[i] = integrals_R[levels[i]];
        }else{
          probR[i] = integrals_L[levels[i]];
        }
        
        confPrior = Phi_approx(1*fabs(coh1Int[levels[i]])/(b[ll[i]]*m[ll[i]]));

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
    m_mu ~ lognormal(0,0.5); 
    b_mu ~ lognormal(0,0.5); 

    //r1 ~ normal(coh1, 1);  
    //r2 ~ normal(coh2, 1);

    m_sd ~ normal(0,0.5); //lognormal(1,1); //
    b_sd ~ normal(0,0.5); //lognormal(1,1);

    m ~ normal(m_mu,m_sd); 
    b ~ normal(b_mu,b_sd); 
    
    target += reduce_sum(partial_sum, trials, grainsize, choice, conf, probR, model_conf);
}

generated quantities {
   // vector[N] log_lik_choice;
   // vector[N] log_lik_conf;
   // vector[N] log_lik;
   // vector[L] log_lik_summed;
   // vector[L] log_lik_mean;
   // int subj;
   // real trialcount;
   // real log_lik_temp;

   // subj = 1;
   // trialcount = 0;
   // log_lik_temp = 0;
   // for (i in 1:(N-1)) {
   //   trialcount = trialcount + 1;
   //   log_lik_choice[i] = bernoulli_lpmf(choice[i] | probR[i]);
   //   log_lik_conf[i] = normal_lpdf(conf[i] | model_conf[i], 0.025);
   //   log_lik[i] = log_lik_choice[i] + log_lik_conf[i];
   //   log_lik_temp = log_lik_temp + log_lik_choice[i] + log_lik_conf[i];

    //  if (ll[i+1]>subj){
    //    log_lik_summed[subj] = log_lik_temp;
    //    log_lik_mean[subj] = log_lik_temp/trialcount;
    //    log_lik_temp = 0;
    //    trialcount = 0;
    //    subj = subj+1;
    //  }
    //}
    //trialcount = trialcount + 1;
    //log_lik_choice[N] = bernoulli_lpmf(choice[N] | probR[N]);
    //log_lik_conf[N] = normal_lpdf(conf[N] | model_conf[N], 0.025);
    //log_lik[N] = log_lik_choice[N] + log_lik_conf[N];
    //log_lik_temp = log_lik_temp + log_lik_choice[N] + log_lik_conf[N];
    //log_lik_summed[L] = log_lik_temp;
    //log_lik_mean[L] = log_lik_temp/trialcount;

}
