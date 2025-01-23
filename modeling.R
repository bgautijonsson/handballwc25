library(tidyverse)
library(cmdstanr)
library(posterior)
library(metill)
library(bayesplot)
theme_set(theme_metill())


d <- read_csv("data.csv")


teams <- unique(c(d$team1, d$team2))

teams <- tibble(
  team = teams
) |>
  arrange(team) |>
  mutate(
    team_nr = row_number()
  )

gk <- read_csv("gk.csv") |>
  inner_join(
    teams,
    by = join_by(team)
  ) |>
  arrange(team_nr)


model_d <- d |>
  filter(winner != 0) |>
  bind_rows(
    d |>
      filter(
        winner == 0
      ) |>
      crossing(
        wnr = c(1, 2)
      ) |>
      select(-winner) |>
      rename(winner = wnr)
  ) |>
  inner_join(
    teams |>
      rename(
        team_nr1 = team_nr
      ),
    by = join_by(team1 == team)
  ) |>
  inner_join(
    teams |>
      rename(
        team_nr2 = team_nr
      ),
    by = join_by(team2 == team)
  ) |>
  select(
    team_nr1,
    team_nr2,
    winner,
    goals_team1 = goals1,
    goals_team2 = goals2,
    shots_team1 = shots1,
    shots_team2 = shots2
  ) |>
  mutate(
    winner = winner - 1
  )


K <- length(unique(teams$team))
N <- nrow(model_d)
team1 <- model_d$team_nr1
team2 <- model_d$team_nr2
winner <- model_d$winner
goals_team1 <- model_d$goals_team1
goals_team2 <- model_d$goals_team2
shots_team1 <- model_d$shots_team1
shots_team2 <- model_d$shots_team2

pred_team2 <- teams |>
  filter(team == "Iceland") |>
  pull(team_nr)

pred_team1 <- teams |>
  filter(team == "Croatia") |>
  pull(team_nr)

stan_data <- list(
  K = K,
  N = N,
  team1 = team1,
  team2 = team2,
  goals_team1 = goals_team1,
  goals_team2 = goals_team2,
  shots_team1 = shots_team1,
  shots_team2 = shots_team2,
  winner = winner,
  pred_team2 = pred_team2,
  pred_team1 = pred_team1,
  gk_saves = gk$saves,
  gk_shots = gk$shots
)

model <- cmdstan_model("model.stan")

results <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000
)

results$summary(c("Omega[2,1]", "sigma", "sd_z_alpha", "beta_n_shots"))

results$summary(c("pred_outcome", "prob_win"))


alpha <- results$summary("alpha")


alpha |>
  mutate(
    team_nr = parse_number(variable)
  ) |>
  inner_join(
    teams,
    by = join_by(team_nr)
  ) |>
  mutate(
    team = fct_reorder(team, median)
  ) |>
  ggplot(aes(median, team)) +
  geom_point(
    size = 3
  ) +
  geom_segment(
    aes(x = q5, xend = q95, y = team),
    linewidth = 0.4
  ) +
  scale_x_continuous(
    limits = c(-2, 2),
    guide = ggh4x::guide_axis_truncated(),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_y_discrete(
    guide = ggh4x::guide_axis_truncated()
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Mat á 'getu' landsliða eftir frammistöðu á HM hingað til"
  )

ggsave(
  "Figures/ability.png",
  width = 8,
  height = 0.8 * 8,
  scale = 1.4
)

results$draws("ranked") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-.chain, -.draw, -.iteration)) |>
  mutate(
    placement = parse_number(name)
  ) |>
  filter(
    placement == 1
  ) |>
  count(team_nr = value) |>
  mutate(
    p = n / sum(n)
  ) |>
  inner_join(
    teams
  ) |>
  mutate(
    team = fct_reorder(team, p)
  ) |>
  ggplot(aes(p, team)) +
  geom_segment(
    aes(xend = 0, yend = team),
    linewidth = 0.1
  ) +
  geom_point(
    size = 2
  ) +
  scale_x_continuous(
    limits = c(0, NA),
    labels = label_percent(),
    guide = ggh4x::guide_axis_truncated(),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_y_discrete(
    guide = ggh4x::guide_axis_truncated()
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Líkur á að vera 'besta' liðið á HM",
    subtitle = "Ekki líkur á að vinna HM, því það fer líka eftir því hvaða liðum þú mætir osf"
  )

ggsave(
  "Figures/best_team.png",
  width = 8,
  height = 0.8 * 8,
  scale = 1.4
)

offense <- results$summary("offense") |>
  mutate(
    type = "offense"
  ) |>
  mutate(
    team_nr = parse_number(variable)
  ) |>
  inner_join(
    teams,
    by = join_by(team_nr)
  ) |>
  mutate(
    team = fct_reorder(team, median)
  )

defense <- results$summary("defense") |>
  mutate(
    type = "defense"
  ) |>
  mutate(
    team_nr = parse_number(variable)
  ) |>
  inner_join(
    teams,
    by = join_by(team_nr)
  ) |>
  mutate(
    team = fct_reorder(team, median)
  )

defense |>
  ggplot(aes(median, team)) +
  geom_hline(
    yintercept = seq(1, K, 2),
    linewidth = 8.5,
    alpha = 0.05
  ) +
  geom_pointrange(
    aes(xmin = q5, xmax = q95, y = team, color = "Vörn"),
    linewidth = 0.5,
    size = 1,
    shape = 15,
    position = position_nudge(y = 0.2)
  ) +
  geom_pointrange(
    data = offense,
    aes(xmin = q5, xmax = q95, y = team, color = "Sókn"),
    linewidth = 0.4,
    size = 1,
    position = position_nudge(y = -0.2)
  ) +
  scale_x_continuous(
    guide = ggh4x::guide_axis_truncated(),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_y_discrete(
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_colour_brewer(
    palette = "Set1"
  ) +
  theme(
    legend.position = "top",
    plot.margin = margin(5, 15, 5, 15)
  ) +
  labs(
    x = NULL,
    y = NULL,
    colour = NULL,
    title = "Mat á sókn og vörn landsliða eftir frammistöðu á HM hingað til"
  )


ggsave(
  "Figures/defense_offense.png",
  width = 8,
  height = 0.8 * 8,
  scale = 1.4
)

offense |>
  ggplot(aes(median, team)) +
  geom_hline(
    yintercept = seq(1, K, 2),
    linewidth = 8.5,
    alpha = 0.05
  ) +
  geom_pointrange(
    aes(xmin = q5, xmax = q95, y = team, color = "Sókn"),
    linewidth = 0.5,
    size = 1,
    shape = 15,
    position = position_nudge(y = 0.2)
  ) +
  geom_pointrange(
    data = defense,
    aes(xmin = q5, xmax = q95, y = team, color = "Vörn"),
    linewidth = 0.4,
    size = 1,
    position = position_nudge(y = -0.2)
  ) +
  scale_x_continuous(
    guide = ggh4x::guide_axis_truncated(),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_y_discrete(
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_colour_brewer(
    palette = "Set1"
  ) +
  theme(
    legend.position = "top",
    plot.margin = margin(5, 15, 5, 15)
  ) +
  labs(
    x = NULL,
    y = NULL,
    colour = NULL,
    title = "Mat á sókn og vörn landsliða eftir frammistöðu á HM hingað til"
  )

ggsave(
  "Figures/offense_defense.png",
  width = 8,
  height = 0.8 * 8,
  scale = 1.4
)

results$summary(c("mean_efficiency", "mean_gk_saves")) |>
  mutate_at(
    vars(median, q5, q95),
    \(x) 1 / (1 + exp(-x))
  ) |>
  select(
    variable,
    median,
    q5,
    q95
  )

results$summary("z_alpha") |>
  mutate(
    team_nr = parse_number(variable)
  ) |>
  inner_join(
    teams,
    by = join_by(team_nr)
  ) |>
  mutate(
    team = fct_reorder(team, median)
  ) |>
  ggplot(aes(median, team)) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.5,
    alpha = 0.5,
    lty = 2
  ) +
  geom_point(
    size = 3
  ) +
  geom_segment(
    aes(xend = 0, yend = team),
    linewidth = 0.5
  ) +
  scale_x_continuous(
    guide = ggh4x::guide_axis_truncated(),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_y_discrete(
    guide = ggh4x::guide_axis_truncated()
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Íslenska liðið er með hæsta X-Factorinn"
  )

ggsave(
  "Figures/x_factor.png",
  width = 8,
  height = 0.8 * 8,
  scale = 1.4
)
