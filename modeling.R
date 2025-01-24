library(tidyverse)
library(cmdstanr)
library(posterior)
library(metill)
library(bayesplot)
library(patchwork)
library(glue)
theme_set(theme_metill())


d <- read_csv("data.csv") |>
  mutate(
    match = glue("{team1} - {team2}"),
    game_nr = row_number()
  )


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
    winner = winner - 1,
    game_nr = row_number()
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
  gk_shots = gk$shots,
  goal_diff = log(goals_team2) - log(goals_team1),
  df = 7
)

model <- cmdstan_model("model_goaldiff.stan")

results <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000
)

results$summary(c("Omega[2,1]", "sigma", "sd_z_alpha", "sigma_goal_diff"))

results$summary(c("pred_win", "goal_diff_pred"))

results$draws("goal_diff_pred") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-.chain, -.draw, -.iteration)) |>
  mutate(
    value = round(exp(value) * 25 - 25)
  ) |>
  count(value) |>
  mutate(
    p = n / sum(n)
  ) |>
  ggplot(aes(value, p)) +
  geom_col() +
  scale_x_continuous(
    breaks = breaks_pretty(n = 8)
  )


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
    # limits = c(-2, 2),
    guide = guide_none(),
    expand = expansion(mult = c(0.01, 0.05)),
    breaks = breaks_pretty(n = 8)
  ) +
  scale_y_discrete(
    guide = ggh4x::guide_axis_truncated()
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Mat á 'getu' landsliða eftir frammistöðu á HM hingað til",
    subtitle = "Reiknað með því að bera saman sigur- og taplið hvers leiks"
  )

ggsave(
  "Figures/ability.png",
  width = 8,
  height = 0.8 * 8,
  scale = 1.2
)

results$draws("ranked") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-.chain, -.draw, -.iteration)) |>
  mutate(
    placement = parse_number(name)
  ) |>
  summarise(
    n = sum(placement == 1),
    .by = value
  ) |>
  rename(
    team_nr = value
  ) |>
  mutate(
    p = n / sum(n)
  ) |>
  inner_join(
    teams
  ) |>
  mutate(
    team = fct_reorder(team, p)
  ) |>
  filter(p > 0) |>
  ggplot(aes(p, team)) +
  geom_segment(
    aes(xend = 0, yend = team),
    linewidth = 0.1
  ) +
  geom_point(
    size = 2
  ) +
  scale_x_continuous(
    limits = c(0, 1),
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
  height = 0.4 * 8,
  scale = 1.1
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

p1 <- results$draws("defense") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-.chain, -.draw, -.iteration)) |>
  mutate(
    team_nr = parse_number(name)
  ) |>
  reframe(
    median = median(value),
    coverage = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
    lower = quantile(value, 0.5 - coverage / 2),
    upper = quantile(value, 0.5 + coverage / 2),
    .by = c(team_nr)
  ) |>
  inner_join(
    teams,
    by = join_by(team_nr)
  ) |>
  mutate(
    team = fct_reorder(team, median)
  ) |>
  ggplot(aes(median, team)) +
  geom_hline(
    yintercept = seq(1, K, 2),
    linewidth = 5.4,
    alpha = 0.03
  ) +
  geom_point(
    shape = "|",
    color = "#e41a1c",
    size = 4,
    position = position_nudge(y = 0.04)
  ) +
  geom_segment(
    aes(
      x = lower, xend = upper,
      yend = team,
      alpha = -coverage
    ),
    linewidth = 1,
    color = "#e41a1c"
  ) +
  scale_alpha_continuous(
    range = c(0, 0.3),
    guide = guide_none()
  ) +
  scale_x_continuous(
    guide = guide_none(),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_y_discrete(
    guide = ggh4x::guide_axis_truncated()
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  ) +
  labs(
    x = NULL,
    y = NULL,
    colour = NULL,
    subtitle = "Vörn"
  )

p2 <- results$draws("offense") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-.chain, -.draw, -.iteration)) |>
  mutate(
    team_nr = parse_number(name)
  ) |>
  reframe(
    median = median(value),
    coverage = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
    lower = quantile(value, 0.5 - coverage / 2),
    upper = quantile(value, 0.5 + coverage / 2),
    .by = c(team_nr)
  ) |>
  inner_join(
    teams,
    by = join_by(team_nr)
  ) |>
  mutate(
    team = fct_reorder(team, median)
  ) |>
  ggplot(aes(median, team)) +
  geom_hline(
    yintercept = seq(1, K, 2),
    linewidth = 5.4,
    alpha = 0.03
  ) +
  geom_point(
    shape = "|",
    color = "#377eb8",
    size = 4,
    position = position_nudge(y = 0.04)
  ) +
  geom_segment(
    aes(
      x = lower, xend = upper,
      yend = team,
      alpha = -coverage
    ),
    linewidth = 1,
    color = "#377eb8"
  ) +
  scale_alpha_continuous(
    range = c(0, 0.3),
    guide = guide_none()
  ) +
  scale_x_continuous(
    guide = guide_none(),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_y_discrete(
    guide = ggh4x::guide_axis_truncated()
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  ) +
  labs(
    x = NULL,
    y = NULL,
    colour = NULL,
    subtitle = "Sókn"
  )


p1 + p2 +
  plot_annotation(
    title = "Mat á sókn og vörn landsliða eftir frammistöðu á HM hingað til",
    subtitle = "Byggt á skotnýtingu liða og andstæðinga þeirra í hverjum leik ásamt markvörslu á mótinu | Myndin sýnir miðgildi og 90% spábil"
  )

ggsave(
  "Figures/offense_and_defense.png",
  width = 8,
  height = 0.5 * 8,
  scale = 1.6
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
    legend.position = c(0.2, 0.986),
    legend.direction = "horizontal",
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 15)
  ) +
  labs(
    x = NULL,
    y = NULL,
    colour = NULL,
    title = "Mat á sókn og vörn landsliða eftir frammistöðu á HM hingað til",
    subtitle = "Byggt á skotnýtingu liða og andstæðinga þeirra í hverjum leik ásamt markvörslu"
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
    title = "Hvaða lið er með hæsta X-Factorinn"
  )

ggsave(
  "Figures/x_factor.png",
  width = 8,
  height = 0.8 * 8,
  scale = 1.4
)

plot_dat <- results$summary("goal_diff_rep") |>
  mutate(
    team1 = teams$team[team1],
    team2 = teams$team[team2],
    goal_diff = log(goals_team2) - log(goals_team1)
  )


plot_dat |>
  select(team1, team2, goal_diff, median, q5, q95) |>
  mutate(
    match = glue("{team1} - {team2}")
  ) |>
  inner_join(
    d |>
      select(
        match,
        game_nr
      ),
    by = join_by(match)
  ) |>
  mutate(
    match = fct_reorder(match, -game_nr)
  ) |>
  ggplot(aes(goal_diff, match)) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.5,
    alpha = 0.5,
    lty = 2
  ) +
  geom_point(
    size = 3,
    col = "red"
  ) +
  geom_pointrange(
    aes(xmin = q5, xmax = q95, x = median),
    size = 0.5
  ) +
  scale_x_continuous(
    guide = ggh4x::guide_axis_truncated(),
    expand = expansion(mult = c(0.01, 0.05)),
    labels = \(x) percent(exp(x) - 1)
  ) +
  labs(
    x = "Hlutfallslegur munur á markafjölda\n<0 = lið 1 vann | >0 = lið 2 vann",
    y = NULL,
    title = "Hversu vel passar líkanið við fyrri leiki?",
    subtitle = "Rauðir punktar eru raunverulegar niðurstöður, svartir punktar og línur eru miðgildisspá og 90% spábil"
  )

ggsave(
  "Figures/goal_diff_fit.png",
  width = 8,
  height = 1 * 8,
  scale = 1.4
)
