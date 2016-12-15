#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]

double loglikelihood(double sig, double prior_a, double prior_b,NumericVector y, NumericVector x) {
  NumericVector pred = prior_a + prior_b*x ;
  double loglik = 0;
 for (int i=0; i < y.length(); i++) {
   NumericVector l = dnorm(y, pred[i],sig,true);
   loglik = loglik + l[i] ;
  }
 return loglik;
}

// [[Rcpp::export]]
double logprior(NumericVector prior_a, NumericVector prior_b, NumericVector sig){
  
  NumericVector aprior = dnorm(prior_a,0,170,true);
  NumericVector bprior = dnorm(prior_b,10,true);
  NumericVector sdprior = dunif(sig,0,30,true);
  return(aprior[0]+bprior[0]+sdprior[0]);
}

//[[Rcpp::export]]

double logposterior(double sig, double prior_a, double prior_b,NumericVector y, NumericVector x, 
                    NumericVector prior_a2, NumericVector prior_b2, NumericVector sig2) {
 return(loglikelihood(sig,prior_a,prior_b,y,x)+logprior(prior_a2,prior_b2,sig2));
}


//[[Rcpp::export]]
NumericVector run_mcmc(
    int n_sim,
    double start_va,
    double start_vb,
    double start_vsig,
    double jump,
    double sig, 
    double prior_a, 
    double prior_b,
    NumericVector x,
    NumericVector y,
    NumericVector prior_a2,
    NumericVector prior_b2, 
    NumericVector sig2) {
  
  NumericVector sima(n_sim + 1); // aqui voy a guardar las simulaciones
  NumericVector simb(n_sim + 1);
  NumericVector simsig(n_sim + 1);
  sima[0] = start_va;
  simb[0] = start_vb;
  simsig[0] = start_vsig;
  double U, eta_a, eta_b, eta_sig;
  bool accepted;
  for (int i=0; i < n_sim; i++) {
    // do while hasta que acepte el candidato
    do {
      eta_a = (rnorm(1, sima[i], jump))[0]; // genera el candidato
      eta_b = (rnorm(1, simb[i], jump))[0];
      eta_sig = (runif(1, simsig[i], jump))[0];
      U = (runif(1))[0];
      if (eta_sig < 0) {
        accepted = false;
      } else {
        accepted = (log(U) <= logposterior(eta_sig,eta_a, eta_b,y,x,eta_a,eta_b, eta_sig) -
          logposterior(simsig[i],sima[i], simb[i],y,x,sima[i],simb[i], simsig[i]));
      }
    } while (!accepted);
    simsig[i + 1] = eta_sig;
    sima[i + 1] = eta_a;
    simb[i+1] = eta_b;
  }
  return sima;
}
