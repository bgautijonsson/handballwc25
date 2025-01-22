/**
 * Bradley-Terry model for maximum likelihood estimation (i.e., no prior).
 */
data {
  int<lower=0> K; // teams
  int<lower=0> N; // games
  array[N] int<lower=1, upper=K> team1; // team 1 for game n
  array[N] int<lower=1, upper=K> team2; // team 2 for game n
  array[N] int<lower=0> goals_team1; // team 1 for game n
  array[N] int<lower=0> goals_team2; // team 2 for game n
  array[N] int<lower=0, upper=1> winner; // winner for game n
  int<lower=1, upper=K> pred_team1;
  int<lower=1, upper=K> pred_team2;
}

parameters {
  sum_to_zero_vector[K] z_alpha; // ability for teams
  vector[K] offense;
  vector[K] defense;
  real<lower = 0> mean_goals;
  real<lower = 0> sd_goals;
  vector[N] z_game_goals;

  real<lower = 0> beta_offense;
  real<lower = 0> beta_defense;
}

transformed parameters {
  vector[N] game_goals = mean_goals + sd_goals * z_game_goals;
  vector[K] alpha = z_alpha + beta_offense * offense - beta_defense * defense;
}


model {
  alpha ~ std_normal();
  offense ~ std_normal();
  defense ~ std_normal();
  mean_goals ~ normal(0, 2);
  sd_goals ~ normal(0, 2);
  z_game_goals ~ std_normal();
  beta_offense ~ std_normal();
  beta_defense ~ std_normal();
  z_alpha ~ std_normal();
  goals_team1 ~ poisson_log(game_goals + offense[team1] - defense[team2]);
  goals_team2 ~ poisson_log(game_goals + offense[team2] - defense[team1]);
  winner ~ bernoulli_logit(alpha[team2] - alpha[team1]);
}

generated quantities {
  array[K] int<lower=1, upper=K> ranked = sort_indices_desc(alpha);
  int pred_outcome = bernoulli_logit_rng(alpha[pred_team2] - alpha[pred_team1]);
}
