data {
  int<lower=1> n; // length of dependent variable
  int<lower=2> m; // number of variables
  int<lower=1> k; // number of slope flexes
  int<lower=1> h; // forecast horizon
  //intercept
  real a;
  real<lower=0> a_sd;
  real b;
  real<lower=0> b_sd;
  vector[n] y;
  matrix[n,m] X; // model matrix
  matrix[h,m] X_fc; // forecat model matrix
  vector[k] params; // param guesses for slope flexes
  vector<lower=0>[k] params_sd; // sd of params for slope flex
}
transformed data {
  vector[n] yy;

  yy = y - y[1];

}
parameters {
  vector[m] B;
  real<lower=0> sig;
}
model {
  B[1] ~ normal(a, a_sd);
  B[2] ~ normal(b, b_sd);
  B[3:m] ~ normal(params, params_sd);
  sig ~ exponential(1);
  yy ~ normal(X*B, sig);
}
generated quantities {
  vector[n] yhat;
  vector[n+h] yhat_fc;
  yhat = (X*B) + y[1];
  yhat_fc = (append_row(X,X_fc)*B) + y[1];
}
