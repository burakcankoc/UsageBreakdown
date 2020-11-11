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

url1314 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=Totals&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=2013-14&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

res <- httr::GET(url = url1314, httr::add_headers(.headers=headers))
json_resp <- jsonlite::fromJSON(content(res, "text"))
df1314 <- data.frame(json_resp$resultSets$rowSet[1])
colnames(df1314) <- json_resp[["resultSets"]][["headers"]][[1]]
df1314$Season = "2014"

url1415 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=Totals&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=2014-15&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

res <- httr::GET(url = url1415, httr::add_headers(.headers=headers))
json_resp <- jsonlite::fromJSON(content(res, "text"))
df1415 <- data.frame(json_resp$resultSets$rowSet[1])
colnames(df1415) <- json_resp[["resultSets"]][["headers"]][[1]]
df1415$Season = "2015"

url1516 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=Totals&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=2015-16&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

res <- httr::GET(url = url1516, httr::add_headers(.headers=headers))
json_resp <- jsonlite::fromJSON(content(res, "text"))
df1516 <- data.frame(json_resp$resultSets$rowSet[1])
colnames(df1516) <- json_resp[["resultSets"]][["headers"]][[1]]
df1516$Season = "2016"

url1617 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=Totals&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

res <- httr::GET(url = url1617, httr::add_headers(.headers=headers))
json_resp <- jsonlite::fromJSON(content(res, "text"))
df1617 <- data.frame(json_resp$resultSets$rowSet[1])
colnames(df1617) <- json_resp[["resultSets"]][["headers"]][[1]]
df1617$Season = "2017"

url1718 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=Totals&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=2017-18&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

res <- httr::GET(url = url1718, httr::add_headers(.headers=headers))
json_resp <- jsonlite::fromJSON(content(res, "text"))
df1718 <- data.frame(json_resp$resultSets$rowSet[1])
colnames(df1718) <- json_resp[["resultSets"]][["headers"]][[1]]
df1718$Season = "2018"

url1819 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=Totals&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=2018-19&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

res <- httr::GET(url = url1819, httr::add_headers(.headers=headers))
json_resp <- jsonlite::fromJSON(content(res, "text"))
df1819 <- data.frame(json_resp$resultSets$rowSet[1])
colnames(df1819) <- json_resp[["resultSets"]][["headers"]][[1]]
df1819$Season = "2019"

nbacomdat = rbind(df1314, df1415, df1516, df1617, df1718, df1819) %>% 
  select(Season, PLAYER_ID, PLAYER_NAME, TEAM_ABBREVIATION, FT_AST, POTENTIAL_AST)

scoring1314 = read.csv("1314scoring.csv", header = T, sep = ",", dec = ".", stringsAsFactors = F)
scoring1314$Season = "2014"

scoring1415 = read.csv("1415scoring.csv", header = T, sep = ",", dec = ".", stringsAsFactors = F)
scoring1415$Season = "2015"

scoring1516 = read.csv("1516scoring.csv", header = T, sep = ",", dec = ".", stringsAsFactors = F)
scoring1516$Season = "2016"

scoring1617 = read.csv("1617scoring.csv", header = T, sep = ",", dec = ".", stringsAsFactors = F)
scoring1617$Season = "2017"

scoring1718 = read.csv("1718scoring.csv", header = T, sep = ",", dec = ".", stringsAsFactors = F)
scoring1718$Season = "2018"

scoring1819 = read.csv("1819scoring.csv", header = T, sep = ",", dec = ".", stringsAsFactors = F)
scoring1819$Season = "2019"

chances = rbind(scoring1314, scoring1415, scoring1516, scoring1617, scoring1718, scoring1819) %>% 
  select(Season, Name, Minutes, OffPoss)

playersta = nbastatR::bref_players_stats(seasons = 2014:2019, tables = "totals")%>% 
  select(yearSeason, idPlayerNBA, namePlayer, fgaTotals, ftaTotals, tovTotals)

deneme = merge(chances, playersta, by.x=c("Season", "Name"), by.y=c("yearSeason", "namePlayer"))
usageall = merge(deneme, nbacomdat, by.x=c("Season", "idPlayerNBA"), by.y = c("Season", "PLAYER_ID"))

usageall$POTENTIAL_AST = as.numeric(as.character(usageall$POTENTIAL_AST))
usageall$FT_AST = as.numeric(as.character(usageall$FT_AST))

asdf = usageall %>% 
  filter(Minutes > 300) %>% 
  mutate(ScoringUsage = 100 * (fgaTotals + ftaTotals*0.44) / OffPoss) %>% 
  mutate(TurnoverUsage = 100 * (tovTotals / OffPoss)) %>% 
  mutate(PlaymakingUsage = 100 * (POTENTIAL_AST + FT_AST) / OffPoss) %>% 
  mutate(TotalUsage = ScoringUsage + PlaymakingUsage + TurnoverUsage) %>% 
  mutate(Name = str_split(Name, " ", simplify = T)[,2]) %>% 
  mutate(Season = as.numeric(as.character(Season)))
  
ggplot(asdf, aes(ScoringUsage, PlaymakingUsage)) +
  geom_point(alpha = 0.4) +
  geom_text_repel(data = . %>% group_by(Season, TEAM_ABBREVIATION) %>% filter(TotalUsage == max(TotalUsage)), 
                  aes(label = Name), nudge_y = 2) +
  theme_bw() +
  facet_wrap(~TEAM_ABBREVIATION) +
  scale_x_continuous(breaks = seq(5, 40, by = 10)) +
  scale_y_continuous(breaks = seq(5, 35, by = 10)) +
  transition_states(
    Season,
    transition_length = 1,
    state_length = 3
  ) +
  ease_aes("linear") +
  labs(title = "Usage Breakdown of NBA Teams, {closest_state}", subtitle = "Regular Season, min. 300 minutes",
       caption = "graph: @burakcankoc\n data: NBA.com & pbpstats.com",
       x = "Scoring Usage", y = "Playmaking Usage") +
  anim_save("usageyears.gif")