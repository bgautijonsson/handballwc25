/**
 * Bradley-Terry model for maximum likelihood estimation (i.e., no prior).
 */
data {
  int<lower=0> K; // teams
  int<lower=0> N; // games
  array[N] int<lower=1, upper=K> team1; // team 1 for game n
  array[N] int<lower=1, upper=K> team2; // team 2 for game n
  array[N] int<lower=0, upper=1> y; // winner for game n
}

parameters {
  sum_to_zero_vector[K] alpha; // ability for teams
}


model {
  alpha ~ std_normal();
  y ~ bernoulli_logit(alpha[team2] - alpha[team1]);
}

generated quantities {
  array[K] int<lower=1, upper=K> ranked = sort_indices_desc(alpha);
}
