library(dplyr)
require(httr)

headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/passing/?Season=2019-20&SeasonType=Regular%20Season&PerMode=Totals',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

url <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=Totals&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=2019-20&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

res <- httr::GET(url = url, httr::add_headers(.headers=headers))
json_resp <- jsonlite::fromJSON(content(res, "text"))
df <- data.frame(json_resp$resultSets$rowSet[1])
colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]

df$Name = df$PLAYER_NAME

scoring = read.csv("players1920.csv", header = T, sep = ",", dec = ".", stringsAsFactors = F)
turnovers = read.csv("turnovers1920.csv", header = T, sep = ",", dec = ".", stringsAsFactors = F)
ftsource = read.csv("ftsource1920.csv", header = T, sep = ",", dec = ".", stringsAsFactors = F)

scousage = left_join(scoring, ftsource, by = "Name")
turnusage = left_join(scoring, turnovers, by = "Name")

scousage = scousage %>% 
  mutate(scoringusage = 100*(FG2A + FG3A + FTA*0.44)/OffPoss)

turnusage = turnusage %>% 
  mutate(turnoverusage = 100*Turnovers/OffPoss)

pmakeusage = left_join(df, scoring, by = "Name")
pmakeusage$POTENTIAL_AST = as.numeric(as.character(pmakeusage$POTENTIAL_AST))
pmakeusage$FT_AST = as.numeric(as.character(pmakeusage$FT_AST))
pmakeusage = pmakeusage %>% 
  mutate(pmausage = 100*(POTENTIAL_AST + FT_AST)/OffPoss)

usage1 = left_join(scousage, pmakeusage, by = "Name")
usageall = left_join(usage1, turnusage, by = "Name")

library(ggplot2)
library(ggrepel)
library(tidyverse)
library(teamcolors)
library(gt)
library(xlsx)

usageall %>% 
  select(Name, TeamAbbreviation.x.x, Minutes.x.x, OffPoss.x, Points.x, scoringusage, pmausage, turnoverusage) %>% 
  rename(Team = TeamAbbreviation.x.x, Min = Minutes.x.x, Poss = OffPoss.x, Pts = Points.x) %>% 
  mutate(TotUsage = scoringusage + pmausage + turnoverusage) %>% 
  group_by(Team) %>% 
  filter(Min > 500) %>% 
  slice_max(order_by = TotUsage, n = 2) %>% 
  mutate(Ranks = seq_along(Name)) %>% 
  select(Name, Team, scoringusage, pmausage, TotUsage, Ranks)
  
usageall %>% 
  filter(Minutes.x.x > 300) %>% 
  mutate(totalusage = scoringusage + pmausage + turnoverusage) %>% 
  mutate(Nameee = str_split(Name, " ", simplify = T)[,2]) %>% 
  ggplot(aes(scoringusage, pmausage)) +
  geom_point(alpha = 0.4) +
  geom_text_repel(data = . %>% group_by(TeamAbbreviation.x.x) %>% filter(scoringusage == max(scoringusage)), 
                  aes(label = Nameee)) +
  theme_bw() +
  #facet_wrap(~TeamAbbreviation.x.x) +
  theme_bw() +
  scale_x_continuous(breaks = seq(10, 40, by = 5)) +
  scale_y_continuous(breaks = seq(5, 30, by = 5)) +
  labs(title = "Usage Breakdown of NBA Teams", subtitle = "2019-2020 Regular Season, min. 300 minutes",
       caption = "graph: @burakcankoc\n data: NBA.com & pbpstats.com",
       x = "Scoring Usage", y = "Playmaking Usage")


usageall %>% 
  filter(Minutes.x.x > 300) %>% 
  filter(TeamAbbreviation.x.x == "HOU" | TeamAbbreviation.x.x == "NOP") %>% 
  mutate(totalusage = scoringusage + pmausage + turnoverusage) %>% 
  mutate(Nameee = str_split(Name, " ", simplify = T)[,2]) %>% 
  ggplot(aes(scoringusage, pmausage)) +
  geom_point(data = . %>% filter(TeamAbbreviation.x.x == "HOU" | TeamAbbreviation.x.x == "NOP"), 
                           alpha = 0.2) +
  geom_text(data = . %>% filter(TeamAbbreviation.x.x == "HOU" | TeamAbbreviation.x.x == "NOP"), 
                  aes(label = Nameee)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(10, 40, by = 5)) +
  scale_y_continuous(breaks = seq(5, 30, by = 5)) +
  labs(title = "Usage Breakdown of NBA Teams", subtitle = "2019-2020 Regular Season, min. 300 minutes",
       caption = "graph: @burakcankoc\n data: NBA.com & pbpstats.com",
       x = "Scoring Usage", y = "Playmaking Usage") +
  facet_wrap(~TeamAbbreviation.x.x) +
  theme(legend.position="none")

usageall %>% 
  mutate(totalusage = scoringusage + pmausage + turnoverusage) %>%
  filter(Minutes.x.x > 300 & scoringusage > 30) %>% 
  select(Name, TeamAbbreviation, FG2A, FG3A, FTA, scoringusage) %>% 
  arrange(desc(scoringusage)) %>% 
  rename(Player = Name, Team = TeamAbbreviation, ScoringUsage = scoringusage) %>% 
  kable(. , "pipe", digits = 3)

usageall %>% 
  mutate(totalusage = scoringusage + pmausage + turnoverusage) %>%
  filter(Minutes.x.x > 300) %>% 
  select(Name, TeamAbbreviation, POTENTIAL_AST, FT_AST, pmausage) %>% 
  arrange(desc(pmausage)) %>% 
  rename(Player = Name, Team = TeamAbbreviation, PlaymakingUsage = pmausage) %>% 
  kable(. , "pipe", digits = 3)


usageall %>% 
  mutate(totalusage = scoringusage + pmausage + turnoverusage) %>%
  filter(Minutes.x.x > 300) %>% 
  arrange(desc(POTENTIAL_AST)) %>% 
  mutate(Rank = order(POTENTIAL_AST, decreasing = T)) %>% 
  select(Rank, Name, TeamAbbreviation, POTENTIAL_AST) %>% 
  rename(Player = Name, Team = TeamAbbreviation) %>% 
  kable(. , "pipe", digits = 3)

usageall %>% 
  mutate(totalusage = scoringusage + pmausage + turnoverusage) %>%
  filter(Minutes.x.x > 300) %>% 
  arrange(desc(FT_AST)) %>% 
  mutate(Rank = order(FT_AST, decreasing = T)) %>% 
  select(Rank, Name, TeamAbbreviation, FT_AST) %>% 
  rename(Player = Name, Team = TeamAbbreviation) %>% 
  kable(. , "pipe", digits = 3)

usageall %>% 
  filter(Minutes.x.x > 300) %>% 
  filter(TeamAbbreviation %in% c("HOU", "LAL", "LAC", "MIL", "ATL", "DAL", "MIA")) %>% 
  mutate(totalusage = scoringusage + pmausage + turnoverusage) %>% 
  # mutate(Nameee = str_split(Name, " ", simplify = T)[,2]) %>% 
  arrange_(~ desc(scoringusage)) %>%
  ggplot(aes(scoringusage, pmausage, col = TeamAbbreviation)) +
  geom_point(alpha = 0.4) +
  geom_text_repel(data = . %>% group_by(TeamAbbreviation) %>% filter(scoringusage == head(scoringusage, n = 2)),
                  aes(label = Name)) +
  theme_bw() +
  ## facet_wrap(~TeamAbbreviation.x.x) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(10, 40, by = 5)) +
  scale_y_continuous(breaks = seq(5, 30, by = 5)) +
  labs(title = "Usage Breakdown of ATL, DAL, LAC, LAL, MIA, MIL", subtitle = "2019-2020 Regular Season, min. 300 minutes",
       caption = "graph: @burakcankoc\n data: NBA.com & pbpstats.com",
       x = "Scoring Usage", y = "Playmaking Usage") +
  scale_color_manual(breaks = c("ATL", "DAL", "HOU", "LAC", "LAL", "MIA", "MIL"),
                     values = c("#e13a3e", "#007dc5", "#ce1141", "#ed174c", "#fdb927", "#98002e",
                                "#00471b"))
