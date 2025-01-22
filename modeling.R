library(tidyverse)
library(googlesheets4)
library(cmdstanr)
library(posterior)
library(metill)
theme_set(theme_metill())


d <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1ctStWrpf3JSe--qObOrej1qPlHLPYeTbw9FoixESMZ0/edit?gid=0#gid=0"
) |>
  janitor::clean_names()

d |>
  write_csv("data.csv")


teams <- unique(c(d$team1, d$team2))

teams <- tibble(
  team = teams
) |>
  arrange(team) |>
  mutate(
    team_nr = row_number()
  )



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
    goals_team2 = goals2
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

pred_team2 <- teams |>
  filter(team == "Iceland") |>
  pull(team_nr)

pred_team1 <- teams |>
  filter(team == "Egypt") |>
  pull(team_nr)

stan_data <- list(
  K = K,
  N = N,
  team1 = team1,
  team2 = team2,
  goals_team1 = goals_team1,
  goals_team2 = goals_team2,
  winner = winner,
  pred_team2 = pred_team2,
  pred_team1 = pred_team1
)

model <- cmdstan_model("model.stan")

results <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4
)

results$summary("pred_outcome")

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
    limits = c(-3, 3),
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
    limits = c(0, 0.2),
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

offense |>
  ggplot(aes(median, team)) +
  geom_pointrange(
    aes(xmin = q5, xmax = q95, y = team, color = "Offense"),
    linewidth = 0.4,
    size = 1
  ) +
  geom_pointrange(
    data = defense,
    aes(xmin = q5, xmax = q95, y = team, color = "Defense"),
    linewidth = 0.4,
    size = 1
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
    title = "Mat á sókn og vörn landsliða eftir frammistöðu á HM hingað til"
  )

results$summary("game_goals")
results$summary(c("mean_goals", "sd_goals"))

results$summary(c("beta_offense", "beta_defense"))
