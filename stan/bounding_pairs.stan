real logFunction(real loglamb, real nu, int n) {
  return (n - 1) * loglamb - nu * lgamma(n);
}

array[] real infiniteBoundingPairs(real loglamb, real nu, real epsilon, int MAX_ITERS) {
  vector[MAX_ITERS] storeVal;
  real leps = log(epsilon);
  real logZ;
  int i = 1;
  
  storeVal[i] = logFunction(loglamb, nu, i);
  i+=1;
  storeVal[i] = logFunction(loglamb, nu, i);
  
  while ((storeVal[i] >= storeVal[i-1] || (storeVal[i] - log(-expm1(storeVal[i] - storeVal[i-1])) >= leps)) && (i < MAX_ITERS)) {
    i+=1;
    storeVal[i] = logFunction(loglamb, nu, i);
  }
  
  logZ = log_sum_exp(sort_asc(storeVal[:i]));
  return {logZ, 1. * i};
}
