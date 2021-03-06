functions {
  /**
  * Return the log probability of a proper conditional autoregressive (CAR) prior
  * with a sparse representation for the adjacency matrix
  *
  * @param phi Vector containing the parameters with a CAR prior
  * @param tau Precision parameter for the CAR prior (real)
  * @param alpha Dependence (usually spatial) parameter for the CAR prior (real)
  * @param W_sparse Sparse representation of adjacency matrix (int array)
  * @param n Length of phi (int)
  * @param W_n Number of adjacent pairs (int)
  * @param D_sparse Number of neighbors for each location (vector)
  * @param lambda Eigenvalues of D^{-1/2}*W*D^{-1/2} (vector)
  *
  * @return Log probability density of CAR prior up to additive constant
  * FORM https://github.com/mbjoseph/CARstan/blob/master/stan/car_sparse.stan
  */
  real sparse_car_lpdf(vector phi, real tau, real alpha,
    int[,] W_sparse, vector D_sparse, vector lambda, int n, int W_n) {
      row_vector[n] phit_D; // phi' * D
      row_vector[n] phit_W; // phi' * W
      vector[n] ldet_terms;

      phit_D = (phi .* D_sparse)';
      phit_W = rep_row_vector(0, n);
      for (i in 1:W_n) {
        phit_W[W_sparse[i, 1]] = phit_W[W_sparse[i, 1]] + phi[W_sparse[i, 2]];
        phit_W[W_sparse[i, 2]] = phit_W[W_sparse[i, 2]] + phi[W_sparse[i, 1]];
      }

      for (i in 1:n) ldet_terms[i] = log1m(alpha * lambda[i]);
      return 0.5 * (n * log(tau)
                    + sum(ldet_terms)
                    - tau * (phit_D * phi - alpha * (phit_W * phi)));
  }
}



data{
  int<lower=1> nSampledCells; //Number of cells that have been sampled
  int<lower=1> sampledId [nSampledCells]; //Id of sampled cells in complete raster
  int<lower=0> nNotSampled; //Number of cells that have not been sampled
  int<lower=1> notSampledId[nNotSampled]; // Id of not sampled cells in complete raster
  int<lower=1> n;      //Number of no NA cells
  int<lower=1> W_n; // Number of adjacent pairs
  int<lower=1> W_sparse[W_n, 2];   // adjacency pairs
  vector<lower=1>[n] D_sparse;     // diagonal of D (number of neigbors for each site)
  vector[n] lambda;       // eigenvalues of invsqrtD * W * invsqrtD
  int<lower=1> N[nSampledCells];   //Number of sampling events
  int<lower=0> y[nSampledCells];   //Number of detections
  real<lower=0,upper=1> minP; //Minimum value for true detectability

}

transformed data{

}

parameters{
  vector <lower=0, upper=1> [nSampledCells] psi_Sampled; // Likelihood of occupancy in sampled cell
  vector <lower=0, upper=1> [nNotSampled] psi_NotSampled; // Likelihood of occupancy in notSampled cell
  real <lower=0> tau; // Autocorrelation dispersion
  real <lower=0, upper=1> alpha;/// Strength of Autocorrelation
  ordered [2] odds;

}

transformed parameters {

  vector <lower=0, upper=1> [n] psi_i; // Likelihood of occupancy in cells
  real<lower=0,upper=1> q; // Rate of false positives
  real <lower=minP, upper=1> p; // Detectability
  real <lower=0, upper= 1> qRate; // Rate of fase to true detections
  vector [nSampledCells] lLh_cell; // Likelihood of presence given sampling pattern

  psi_i[sampledId] = psi_Sampled;
  psi_i[notSampledId] = psi_NotSampled;
  q = inv_logit(odds[1]);
  p = inv_logit(odds[2]);
  qRate = q/p;

    for (cell in 1:nSampledCells){

   lLh_cell[cell]   = log_mix(psi_Sampled[cell],binomial_lpmf(y[cell] | N[cell],p),
                              binomial_lpmf(y[cell] | N[cell] , q)

                           );

    }



}

model
  {
// Priors
    target += normal_lpdf(qRate | 0,0.05);
    target += beta_lpdf(psi_i | 0.5, 0.5);
    target += gamma_lpdf(tau | 2, 2);

// Model log probabilities
    target += lLh_cell;
// Autocorrelation log probabilities
    target += sparse_car_lpdf(psi_i | tau, alpha, W_sparse, D_sparse, lambda, n, W_n);

  }

generated quantities
  {


int<lower=0> sim_y[nSampledCells]; //Simulated Sampling
int<lower=0> sim_true_y[nSampledCells]; //Simulated True Detections
int<lower=0> sim_false_y[nSampledCells]; //Simulated False Detections
int<lower=1> cell;

real<lower=0, upper=1> psi; //Global Occupancy
real<lower=0, upper=1> cellpres_i[n];
real<lower=0, upper=1> pCorr[nSampledCells];
vector <lower=0, upper=1> [n] pp; //Probability of presence
vector [nSampledCells] expRec; //
real chi_sq;
real npars;
real lLh;
real AIC;
real AICc;
real bAIC;

npars = nSampledCells + nNotSampled + 1 + 1 + 2;


lLh = sum(lLh_cell);
AIC = 2 * npars - 2 * lLh;
AICc = AIC + ((2*npars*(npars+1))/(nSampledCells-npars-1));
bAIC = log(nSampledCells) * npars - 2 * lLh;

expRec = (psi_Sampled .* to_vector(N)) * p  + ((1-psi_Sampled) .* to_vector(N)) * q;
chi_sq = sum(((expRec - to_vector(y)) .* (expRec - to_vector(y))) ./ expRec);


for (ncell in 1:nSampledCells ){

    cell = sampledId[ncell];
    pp[cell] = exp(
    log(psi_i[cell])+binomial_lpmf(y[ncell] | N[ncell],p) -
    log_mix(psi_i[cell],binomial_lpmf(y[ncell] | N[ncell],p),
                              binomial_lpmf(y[ncell] | N[ncell] , q))
                              );  // Probability of presence

      if(bernoulli_rng(pp[cell])){
         cellpres_i[cell] = 1;
         pCorr[ncell] = p;
         sim_true_y[ncell]=binomial_rng(N[ncell],p);
         sim_false_y[ncell]=0;

      }else{
         cellpres_i[cell] = 0;
         pCorr[ncell] = 0;
         sim_true_y[ncell]=0;
         sim_false_y[ncell]=binomial_rng(N[ncell],q);
      }

  sim_y[ncell] = sim_true_y[ncell]+sim_false_y[ncell];

  }

 pp[notSampledId] = psi_i[notSampledId];

 for (ncell in 1:nNotSampled){
   cell = notSampledId[ncell];
   cellpres_i[cell] = bernoulli_rng(pp[cell]);

 }

 psi = sum(cellpres_i)/n;


}
