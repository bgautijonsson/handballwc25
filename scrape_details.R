library(tidyverse)
library(rvest)
library(glue)
library(purrr)
#### Preliminary Round

preliminary_round <- "https://www.ihf.info/competitions/men/308/29th-ihf-mens-world-championship-2025/177649/stage-matches/178659"

links <- read_html(preliminary_round) |>
  html_elements("a") |>
  html_attr("href")

links <- na.omit(links[str_detect(links, "match-center")])

data_preliminary <- tribble(
  ~team1, ~team2, ~shots1, ~shots2, ~score
)

for (i in seq_along(links)) {
  Sys.sleep(0.1)
  game_url <- paste0("https://www.ihf.info", links[i])

  match_node <- str_match(game_url, "match-center/(.*)$")[, 2]

  d <- read_html(game_url)

  team1 <- d |>
    html_element(
      xpath = glue(
        '//*[@id="ihf-competitions-match-results-match-node-{match_node}"]/div[2]/div[1]/div/div[1]/span'
      )
    ) |>
    html_text()

  shots1 <- d |>
    html_element(xpath = '//*[@id="all-shots-a"]') |>
    html_text()


  team2 <- d |>
    html_element(
      xpath = glue(
        '//*[@id="ihf-competitions-match-results-match-node-{match_node}"]/div[2]/div[1]/div/div[3]/span'
      )
    ) |>
    html_text()

  shots2 <- d |>
    html_element(xpath = '//*[@id="all-shots-b"]') |>
    html_text()



  score <- d |>
    html_element(xpath = '//*[@id="result-a-b"]') |>
    html_text()

  data_preliminary <- data_preliminary |>
    add_row(
      team1 = team1,
      team2 = team2,
      shots1 = shots1,
      shots2 = shots2,
      score = score
    )

  if ((shots1 == 0) & (shots2 == 0)) {
    break
  }
}

#### Main Round ####

main_round <- "https://www.ihf.info/competitions/men/308/29th-ihf-mens-world-championship-2025/177649/stage-matches/200623"

links <- read_html(main_round) |>
  html_elements("a") |>
  html_attr("href")

links <- na.omit(links[str_detect(links, "match-center")])

data_main <- tribble(
  ~team1, ~team2, ~shots1, ~shots2, ~score
)

for (i in seq_along(links)) {
  Sys.sleep(0.1)
  game_url <- paste0("https://www.ihf.info", links[i])

  match_node <- str_match(game_url, "match-center/(.*)$")[, 2]

  d <- read_html(game_url)

  team1 <- d |>
    html_element(
      xpath = glue(
        '//*[@id="ihf-competitions-match-results-match-node-{match_node}"]/div[2]/div[1]/div/div[1]/span'
      )
    ) |>
    html_text()

  shots1 <- d |>
    html_element(xpath = '//*[@id="all-shots-a"]') |>
    html_text()


  team2 <- d |>
    html_element(
      xpath = glue(
        '//*[@id="ihf-competitions-match-results-match-node-{match_node}"]/div[2]/div[1]/div/div[3]/span'
      )
    ) |>
    html_text()

  shots2 <- d |>
    html_element(xpath = '//*[@id="all-shots-b"]') |>
    html_text()

  score <- d |>
    html_element(xpath = '//*[@id="result-a-b"]') |>
    html_text()

  data_main <- data_main |>
    add_row(
      team1 = team1,
      team2 = team2,
      shots1 = shots1,
      shots2 = shots2,
      score = score
    )

  if ((shots1 == 0) & (shots2 == 0)) {
    break
  }
}



#### President's Cup ####

president_cup <- "https://www.ihf.info/competitions/men/308/29th-ihf-mens-world-championship-2025/177649/stage-matches/200625"

links <- read_html(president_cup) |>
  html_elements("a") |>
  html_attr("href")

links <- na.omit(links[str_detect(links, "match-center")])

data_president <- tribble(
  ~team1, ~team2, ~shots1, ~shots2, ~score
)

for (i in seq_along(links)) {
  Sys.sleep(0.1)
  game_url <- paste0("https://www.ihf.info", links[i])

  match_node <- str_match(game_url, "match-center/(.*)$")[, 2]

  d <- read_html(game_url)

  team1 <- d |>
    html_element(
      xpath = glue(
        '//*[@id="ihf-competitions-match-results-match-node-{match_node}"]/div[2]/div[1]/div/div[1]/span'
      )
    ) |>
    html_text()

  shots1 <- d |>
    html_element(xpath = '//*[@id="all-shots-a"]') |>
    html_text()


  team2 <- d |>
    html_element(
      xpath = glue(
        '//*[@id="ihf-competitions-match-results-match-node-{match_node}"]/div[2]/div[1]/div/div[3]/span'
      )
    ) |>
    html_text()

  shots2 <- d |>
    html_element(xpath = '//*[@id="all-shots-b"]') |>
    html_text()



  score <- d |>
    html_element(xpath = '//*[@id="result-a-b"]') |>
    html_text()

  data_president <- data_president |>
    add_row(
      team1 = team1,
      team2 = team2,
      shots1 = shots1,
      shots2 = shots2,
      score = score
    )

  if ((shots1 == 0) & (shots2 == 0)) {
    break
  }
}

d <- bind_rows(
  data_preliminary,
  data_main,
  data_president
) |>
  separate(
    score,
    into = c("goals1", "goals2"),
    sep = ":"
  ) |>
  mutate_at(
    vars(shots1:goals2),
    parse_number
  ) |>
  mutate(
    winner = case_when(
      goals2 > goals1 ~ 2,
      goals2 == goals1 ~ 0,
      TRUE ~ 1
    )
  ) |>
  filter(goals1 > 0, goals2 > 0)

d |>
  write_csv("data.csv")


#### Goalkeeper ####

url <- "https://www.ihf.info/competitions/men/308/29th-ihf-mens-world-championship-2025/177649/statistics/top-gk-shots"
url <- paste0(
  url,
  "?q=/competitions/men/308/29th-ihf-mens-world-championship-2025/177649/statistics/top-gk-shots"
)

links <- paste0(
  url,
  c("", paste("&page", 1:3, sep = "="))
)

tables <- links |>
  map(read_html) |>
  map(html_table)


gk <- tables |>
  bind_rows() |>
  select(Team, Saves, Shots) |>
  janitor::clean_names() |>
  summarise(
    saves = sum(saves),
    shots = sum(shots),
    .by = team
  )

gk |> write_csv("gk.csv")
