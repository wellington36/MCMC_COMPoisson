real logFunction(real loglamb, real nu, int n) {
  return (n - 1) * loglamb - nu * lgamma(n);
}

array[] real sequential(real loglamb, real nu, int MAX_ITERS) {
  vector[MAX_ITERS] storeVal;
  real logZ;
  int i = 1;
  
  storeVal[1] = - nu * lgamma(1);
  
  while (i < MAX_ITERS) {
    i+=1;
    storeVal[i] = logFunction(loglamb, nu, i);
  }
  
  logZ = log_sum_exp(sort_asc(storeVal[:i]));
  return {logZ, 1. * i};
}
