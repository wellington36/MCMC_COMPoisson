array[] real infiniteBoundingPairs(real loglamb, real nu, real epsilon, int MAX_ITERS) {
  vector[MAX_ITERS + 1] storeVal;
  real leps = log(epsilon);
  real logZ;
  int i = 1;
  
  storeVal[1] = 0 * loglamb - nu * lgamma(0 + 1);
  storeVal[2] = 1 * loglamb - nu * lgamma(1 + 1);
  i+=1;
  
  while (storeVal[i] - log(-expm1(storeVal[i] - storeVal[i-1])) > leps + log2() && i < MAX_ITERS + 1) {
    storeVal[i+1] = i * loglamb - nu * lgamma(i + 1);
    i+=1;
  }
  
  logZ = log_sum_exp(sort_asc(storeVal[:i]));
  
  real bound1 = storeVal[i] + log(0) - log(1);
  real bound2 = storeVal[i] - log(-expm1(storeVal[i] - storeVal[i-1]));
  return {log_sum_exp([logZ, bound1 - log2(), bound2 - log2()]), 1. * i};
}
