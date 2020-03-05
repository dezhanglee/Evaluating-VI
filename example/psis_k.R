
#code to generate the average PSIS k values, used in chapter 6 
if ("rstan" %in% rownames(installed.packages())  == FALSE) {
  install.packages("rstan")
}

library(rstan)
options(mc.cores = parallel::detectCores())
Sys.setenv(LOCAL_CPPFLAGS = '-march=native') #gcc compiler flag
rstan_options(auto_write = TRUE)


#copy the stan code as a string here. 
stan_code1 = "
data {
  int<lower=0> N;
  int<lower=0> p;
  matrix[N,p] X;
  int<lower=0,upper=1> y[N]; 
}
parameters {
  real alpha;
  vector[p] beta;
}
model {
  y ~ bernoulli_logit(alpha + X*beta);
}
"

#do not modify this. this is to pass the stan_code into RStan 
m=stan_model(model_code = stan_code1)

#replace NULL with the dataset you are working with

#first, read in the dataset
mydata = readRDS("data_logistic.rds")
mydata = as.data.frame(mydata)
#pass in the dataset + all the relevant parameters needed by stan. 
data = list(N=nrow(mydata), p=2, X=mydata[,1:2], y=mydata$agg_i)



#to get the average psis k diagnostic value


nTrials = 50 #number of times to fit the model
k_vals = numeric(nTrials)


for (qq in 1:nTrials) {
  
  #change this accordingly based on your VI fit specifications
  fit_vb=vb(m, data=data, iter=1e6,
            output_samples=20000,tol_rel_obj=0.003,eta=0.05,
            adapt_engaged=T)
  
  k_vals[qq] = fit_vb@sim$diagnostics$psis$pareto_k
  
}

print("======================================")
print(sprintf("The average PSIS k value across %d iterations is",
              nTrials))
print(mean(k_vals))
print("======================================")
print(sprintf("The standard deviation across %d iterations is",
              nTrials))
print(sd(k_vals))