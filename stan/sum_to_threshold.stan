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


array[] real infiniteSumToThreshold(real mu, real phi, real epsilon, int MAX_ITERS) {
  vector[MAX_ITERS] storeVal;
  real leps = log(epsilon);
  real logZ;
  int i = 1;
  
  storeVal[1] = 0;
  
  while (storeVal[i] > leps && i < MAX_ITERS) {
    i+=1;
    storeVal[i] = log_kernel(mu, phi, i-1);
  }
  
  logZ = log_sum_exp(sort_asc(storeVal[:i]));
  return {logZ, 1. * i};
}
