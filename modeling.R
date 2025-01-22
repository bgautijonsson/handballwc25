library(tidyverse)
library(googlesheets4)
library(cmdstanr)
library(posterior)
library(metill)
theme_set(theme_metill())


d <- read_sheet("https://docs.google.com/spreadsheets/d/1ctStWrpf3JSe--qObOrej1qPlHLPYeTbw9FoixESMZ0/edit?gid=0#gid=0") |> 
  janitor::clean_names()



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
    team_nr1, team_nr2, winner
  ) |> 
  mutate(
    winner = winner - 1
  )


K <- length(unique(teams$team))
N <- nrow(model_d)
team1 <- model_d$team_nr1
team2 <- model_d$team_nr2
y <- model_d$winner

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
  y = y,
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
  geom_point() +
  geom_segment(
    aes(x = q5, xend = q95, y = team)
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
  geom_point()

