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
  array[N] int<lower=0> shots_team1; // team 1 for game n
  array[N] int<lower=0> shots_team2; // team 2 for game n
  vector[N] goal_diff;
  int<lower=1, upper=K> pred_team1;
  int<lower=1, upper=K> pred_team2;

  array[K] int<lower=0> gk_saves;
  array[K] int<lower=0> gk_shots;

  real<lower = 0> df;
}

transformed data {
  vector[N] log_shots_team1 = log(to_vector(shots_team1));
  vector[N] log_shots_team2 = log(to_vector(shots_team2));
}

parameters {
  sum_to_zero_vector[K] z_alpha; // ability for teams
  real<lower = 0> sd_z_alpha;
  
  // Replace individual offense/defense with matrix
  matrix[K, 2] z_abilities;  // standardized abilities (offense, defense)
  cholesky_factor_corr[2] L_Omega;  // Cholesky decomp of correlation matrix
  vector<lower=0>[2] sigma;  // standard deviations for offense/defense

  real mean_efficiency;
  real mean_gk_saves;

  real beta_n_shots;

  real<lower = 0> sigma_goal_diff;

}

transformed parameters {
  matrix[K, 2] abilities;  // actual abilities (offense, defense)
  vector[K] offense;
  vector[K] defense;
  vector[K] alpha;

  // Transform z_abilities to correlated abilities
  abilities = z_abilities * diag_pre_multiply(sigma, L_Omega)';
  
  // Extract offense and defense from abilities matrix
  offense = abilities[, 1];
  defense = abilities[, 2];
  
  // Compute overall ability (now without beta parameters)
  alpha = (1.0 * K) / (K - 1.0) * sd_z_alpha * z_alpha + offense + defense;
}

model {
  // Remove individual offense/defense priors
  to_vector(z_abilities) ~ std_normal();  // prior for standardized abilities
  L_Omega ~ lkj_corr_cholesky(2.0);  // prior for correlation matrix
  sigma ~ exponential(1);  // prior for standard deviations
  
  z_alpha ~ std_normal();
  sd_z_alpha ~ exponential(1);
  mean_efficiency ~ std_normal();
  mean_gk_saves ~ std_normal();

  beta_n_shots ~ std_normal();
  gk_saves ~ binomial_logit(gk_shots, mean_gk_saves + defense);
  sigma_goal_diff ~ exponential(1);
  goals_team1 ~ binomial_logit(shots_team1, mean_efficiency + offense[team1] - defense[team2]);
  goals_team2 ~ binomial_logit(shots_team2, mean_efficiency + offense[team2] - defense[team1]);
  goal_diff ~ student_t(df, alpha[team2] - alpha[team1], sigma_goal_diff);
}

generated quantities {
  array[K] int<lower=1, upper=K> ranked = sort_indices_desc(alpha);
  real goal_diff_pred = exp(student_t_rng(df, alpha[pred_team2] - alpha[pred_team1], sigma_goal_diff)) - 1;
  int pred_win = goal_diff_pred > 0 ? 1 : 0;
  corr_matrix[2] Omega = multiply_lower_tri_self_transpose(L_Omega);
  array[N] real goal_diff_rep = student_t_rng(df, alpha[team2] - alpha[team1], sigma_goal_diff);
}
