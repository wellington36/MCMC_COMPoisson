array[] real infiniteSumToThreshold(real loglamb, real nu, real epsilon, int MAX_ITERS) {
  vector[MAX_ITERS + 1] storeVal;
  real leps = log(epsilon);
  real logZ;
  int i = 1;
  
  storeVal[1] = - nu * lgamma(1);
  
  while (storeVal[i] > leps && i < MAX_ITERS) {
    storeVal[i+1] = i * loglamb - nu * lgamma(i + 1);
    i+=1;
  }
  
  logZ = log_sum_exp(sort_asc(storeVal[:i]));
  return {logZ, 1. * i};
}
