#we can get the priors from the stan code


#code to generate the VSBC diagnostic histogram
#and p values, used in chapter 6 
if ("rstan" %in% rownames(installed.packages())  == FALSE) {
  install.packages("rstan")
}
if ("lawstat" %in% rownames(installed.packages())  == FALSE) {
  install.packages("lawstat")
}

library(rstan)
options(mc.cores = parallel::detectCores())
Sys.setenv(LOCAL_CPPFLAGS = '-march=native') #gcc compiler flag
rstan_options(auto_write = TRUE)


#copy the stan code as a string here. 
stan_code2="
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
alpha~normal(0,0.5);
beta~normal(1,1.5);
y ~ bernoulli_logit(alpha + X*beta);
}
"


#do not modify this. this is to pass the stan_code into RStan 
m=stan_model(model_code = stan_code2)

#first, read in the dataset
mydata = readRDS("data_logistic.rds")
mydata = as.data.frame(mydata)
#pass in the dataset + all the relevant parameters needed by stan. 
data = list(N=nrow(mydata), p=2, X=mydata[,1:2], y=mydata$agg_i)


nTrials = 10 #in practice will definitely need more. 
generated_dat = NULL;
probs.vsbc = NULL

for (qq in 1:nTrials) {
  #change the following line accordingly
  fit_vb=vb(m, data=data, iter=1e6,
            output_samples=20000,tol_rel_obj=0.007,eta=0.05,
            adapt_engaged=T)
  
  vb.out.1 = rstan::extract(fit_vb)
  #select the parameters you are interested in
  #For example
  vb.out.1 = cbind(vb.out.1$alpha, vb.out.1$beta[,1], vb.out.1$beta[,2])
  
  if (is.null(generated_dat)) {
    generated_dat = vb.out.1
  }
  else {
    generated_dat = rbind(generated_dat, vb.out.1)
  }

  #Generate from the priors, and 
  #concatinate them into the vector ab. For example
  a=rnorm(1,0,.5)
  b=rnorm(2,1,1.5)
  ab = c(a,b)
  
  resp = (ab<vb.out.1)
  prbs = apply(resp,2,mean)
  if (is.null(probs.vsbc))
    probs.vsbc = prbs
  else
    probs.vsbc = rbind(probs.vsbc,prbs)
}


#now, each column in probs.vsbc are the VSBC p values
#for each parameter of interest

#to plot the histogram
for (i in 1:ncol(probs.vsbc)) {
  hist(probs.vsbc[,i]) 
  #modify this with the usual histogram function arguments
}

#to test for symmetry
library(lawstat)
#for each parameter, prints p values in this order
#1. overall test of symmetry
#2. test if data is left skewed
#3. test if data is right skewed
for (i in 1:ncol(probs.vsbc)) {
  print(symmetry.test(probs.vsbc[,i])$p.value)
  print(symmetry.test(probs.vsbc[,i],side="left")$p.value)
  print(symmetry.test(probs.vsbc[,i],side="right")$p.value)
  print("==========================")
}