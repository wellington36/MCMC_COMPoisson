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

array[] real infiniteSumToThreshold(real loglamb, real nu, real L,
                                    real epsilon, int M, int initial_k) {
  vector[M] storeVal;          // at most M terms will ever be stored
  real leps = log(epsilon);
  real M_bound = (L + 1) / 2;
  real log_M_bound = log(M_bound);
  real threshold = leps + log1m(M_bound) - log(M_bound);
  real logZ;
  int idx = 1;
  int k = initial_k;
  int ratio_ok = 1;

  if (log_kernel(loglamb, nu, M) > threshold) {
    reject("It is not possible to reach the stopping criterion with the given M.");
  }

  storeVal[1] = log_kernel(loglamb, nu, initial_k);

  // The stopping criterion (below threshold) is only valid when the ratio
  // between consecutive terms doesn't exceed M_bound. Track both, and only
  // stop the loop once BOTH conditions hold.
  while ((storeVal[idx] > threshold || ratio_ok == 0) && k < M - 1 + initial_k) {
    k += 1;
    idx += 1;
    storeVal[idx] = log_kernel(loglamb, nu, k);
    ratio_ok = (storeVal[idx] - storeVal[idx - 1]) <= log_M_bound ? 1 : 0;
  }

  logZ = log_sum_exp(sort_asc(storeVal[:idx]));
  return {logZ, 1. * (k - initial_k)};
}