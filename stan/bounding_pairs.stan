real log_kernel(real mu, real phi, int n) {
  if (n == 0)
    return 0;

  return -n
         + n * log(n)
         - lgamma(n + 1)
         + phi * n
         + phi * n * log(mu)
         - phi * n * log(n);
}

array[] real infiniteBoundingPairs(real mu, real phi, real epsilon, int MAX_ITERS) {
  vector[MAX_ITERS] storeVal;
  real leps = log(epsilon);
  real logZ;
  int i = 1;
  
  storeVal[1] = log_kernel(mu, phi, 0);
  i+=1;
  storeVal[i] = log_kernel(mu, phi, i-1);
  
  while ((storeVal[i] >= storeVal[i-1] || (storeVal[i] - log(-expm1(storeVal[i] - storeVal[i-1])) >= leps)) && (i < MAX_ITERS)) {
    i+=1;
    storeVal[i] = log_kernel(mu, phi, i-1);
  }
  
  logZ = log_sum_exp(sort_asc(storeVal[:i]));
  return {logZ, 1. * i};
}
