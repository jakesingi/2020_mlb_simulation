# This function simulates the 2020 baseball season
one_simulation_20 <- function(arg = "arg") {
  require(dplyr)
  require(baseballr)
  make_schedule = function(teams, k) {
    n_teams = length(teams)
    Home = rep(rep(teams, each = n_teams), k)
    Visitor = rep(rep(teams, n_teams), k)
    schedule = tibble(Home = Home, Visitor = Visitor) %>%
      filter(Home != Visitor)
    return(schedule)
  }
  
  NLWest = c("LAD", "SFG", "COL", "ARZ", "SDP")
  NLCentral = c("CHC", "STL", "MIL", "CIN", "PIT")
  NLEast = c("ATL", "MIA", "NYM", "PHI", "WSH")
  ALWest = c("OAK", "HOU", "TEX", "SEA", "LAA")
  ALCentral = c("MIN", "CWS", "CLE", "KCR", "DET")
  ALEast = c("NYY", "BOS", "TOR", "TBR", "BAL")
  
  teams = c(NLWest, NLCentral, NLEast, ALWest, ALCentral, ALEast)
  league = c(rep(1, 15), rep(2, 15))
  div = c(rep(3, 5), rep(4, 5), rep(5, 5), rep(6, 5), rep(7, 5), rep(8,5))
  
  # Each team plays its divisional foes 10 times, half at home and half away
  schedule = bind_rows(make_schedule(NLWest, 5),
                       make_schedule(NLCentral, 5),
                       make_schedule(NLEast, 5),
                       make_schedule(ALWest, 5),
                       make_schedule(ALCentral, 5),
                       make_schedule(ALEast, 5))
  # Add in interleague matchups
  schedule = bind_rows(schedule,
                       make_schedule(c("ARZ", "TEX"), 2),  # D-Backs
                       make_schedule(c("ARZ", "HOU"), 3),
                       make_schedule(c("ARZ", "OAK"), 2),
                       c(Home = "ARZ", Visitor = "SEA"),
                       c(Home = "ARZ", Visitor = "SEA"),
                       c(Home = "ARZ", Visitor = "SEA"),
                       c(Home = "LAA", Visitor = "ARZ"),
                       c(Home = "LAA", Visitor = "ARZ"),
                       c(Home = "LAA", Visitor = "ARZ"),
                       make_schedule(c("NYY", "ATL"), 2),  # Braves
                       make_schedule(c("ATL", "BOS"), 3),
                       c(Home = "BAL", Visitor = "ATL"),
                       c(Home = "BAL", Visitor = "ATL"),
                       c(Home = "BAL", Visitor = "ATL"),
                       c(Home = "ATL", Visitor = "TOR"),
                       c(Home = "ATL", Visitor = "TOR"),
                       c(Home = "ATL", Visitor = "TOR"),
                       make_schedule(c("ATL", "TBR"), 2),
                       make_schedule(c("BAL", "NYM"), 2),  # Orioles
                       make_schedule(c("BAL", "MIA"), 2),
                       c(Home = "PHI", Visitor = "BAL"),
                       c(Home = "PHI", Visitor = "BAL"),
                       c(Home = "PHI", Visitor = "BAL"),
                       make_schedule(c("BAL", "WSH"), 3),
                       make_schedule(c("BOS", "NYM"), 2),  # Red Sox
                       c(Home = "MIA", Visitor = "BOS"),
                       c(Home = "MIA", Visitor = "BOS"),
                       c(Home = "MIA", Visitor = "BOS"),
                       c(Home = "BOS", Visitor = "WSH"),
                       c(Home = "BOS", Visitor = "WSH"),
                       c(Home = "BOS", Visitor = "WSH"),
                       make_schedule(c("BOS", "PHI"), 2),
                       make_schedule(c("CHC", "KCR"), 2),  # Cubs
                       c(Home = "CHC", Visitor = "MIN"),
                       c(Home = "CHC", Visitor = "MIN"),
                       c(Home = "CHC", Visitor = "MIN"),
                       make_schedule(c("CHC", "CLE"), 2),
                       c(Home = "DET", Visitor = "CHC"),
                       c(Home = "DET", Visitor = "CHC"),
                       c(Home = "DET", Visitor = "CHC"),
                       make_schedule(c("CHC", "CWS"), 3),
                       make_schedule(c("CWS", "PIT"), 2),  # White Sox
                       c(Home = "CWS", Visitor = "STL"),
                       c(Home = "CWS", Visitor = "STL"),
                       c(Home = "CWS", Visitor = "STL"),
                       make_schedule(c("CWS", "MIL"), 2),
                       c(Home = "CIN", Visitor = "CWS"),
                       c(Home = "CIN", Visitor = "CWS"),
                       c(Home = "CIN", Visitor = "CWS"),
                       c(Home = "MIN", Visitor = "CIN"),  # Reds
                       c(Home = "MIN", Visitor = "CIN"),
                       c(Home = "MIN", Visitor = "CIN"),
                       make_schedule(c("CIN", "CLE"), 2),
                       make_schedule(c("CIN", "KCR"), 2),
                       make_schedule(c("CIN", "DET"), 3),
                       c(Home = "CLE", Visitor = "MIL"),  # Indians
                       c(Home = "CLE", Visitor = "MIL"),
                       c(Home = "CLE", Visitor = "MIL"),
                       make_schedule(c("CLE", "PIT"), 3),
                       c(Home = "STL", Visitor = "CLE"),
                       c(Home = "STL", Visitor = "CLE"),
                       c(Home = "STL", Visitor = "CLE"),
                       make_schedule(c("COL", "OAK"), 2),  # Rockies
                       make_schedule(c("COL", "HOU"), 2),
                       make_schedule(c("COL", "TEX"), 3),
                       c(Home = "COL", Visitor = "LAA"),
                       c(Home = "COL", Visitor = "LAA"),
                       c(Home = "COL", Visitor = "LAA"),
                       c(Home = "SEA", Visitor = "COL"),
                       c(Home = "SEA", Visitor = "COL"),
                       c(Home = "SEA", Visitor = "COL"),
                       #c(Home = "STL", Visitor = "DET"),  # Tigers... games against STL not played due to COVID
                       #c(Home = "STL", Visitor = "DET"),
                       make_schedule(c("DET", "MIL"), 2),
                       c(Home = "PIT", Visitor = "DET"),
                       c(Home = "PIT", Visitor = "DET"),
                       c(Home = "PIT", Visitor = "DET"),
                       make_schedule(c("LAD", "HOU"), 2), # Astros
                       c(Home = "SDP", Visitor = "HOU"),
                       c(Home = "SDP", Visitor = "HOU"),
                       c(Home = "SDP", Visitor = "HOU"),
                       c(Home = "HOU", Visitor = "SFG"),
                       c(Home = "HOU", Visitor = "SFG"),
                       c(Home = "HOU", Visitor = "SFG"),
                       make_schedule(c("KCR", "STL"), 3),  # Royals
                       c(Home = "MIL", Visitor = "KCR"),
                       c(Home = "MIL", Visitor = "KCR"),
                       c(Home = "MIL", Visitor = "KCR"),
                       c(Home = "KCR", Visitor = "PIT"),
                       c(Home = "KCR", Visitor = "PIT"),
                       c(Home = "KCR", Visitor = "PIT"),
                       make_schedule(c("LAD", "LAA"), 3),  # Angels
                       make_schedule(c("SDP", "LAA"), 2),
                       make_schedule(c("LAA", "SFG"), 2),
                       c(Home = "TEX", Visitor = "LAD"),  # Dodgers
                       c(Home = "TEX", Visitor = "LAD"),
                       c(Home = "TEX", Visitor = "LAD"),
                       c(Home = "LAD", Visitor = "OAK"),
                       c(Home = "LAD", Visitor = "OAK"),
                       c(Home = "LAD", Visitor = "OAK"),
                       make_schedule(c("LAD", "SEA"), 2),
                       c(Home = "NYY", Visitor = "MIA"),  # Marlins
                       c(Home = "NYY", Visitor = "MIA"),
                       c(Home = "NYY", Visitor = "MIA"),
                       make_schedule(c("MIA", "TBR"), 3),
                       make_schedule(c("MIA", "TOR"), 2),
                       make_schedule(c("MIL", "MIN"), 3),  # Brewers
                       make_schedule(c("MIN", "STL"), 2),  # Twins
                       make_schedule(c("MIN", "PIT"), 2),
                       make_schedule(c("NYY", "NYM"), 3),  # Mets
                       c(Home = "NYM", Visitor = "TBR"),
                       c(Home = "NYM", Visitor = "TBR"),
                       c(Home = "NYM", Visitor = "TBR"),
                       c(Home = "TOR", Visitor = "NYM"),
                       c(Home = "TOR", Visitor = "NYM"),
                       c(Home = "TOR", Visitor = "NYM"),
                       make_schedule(c("PHI", "NYY"), 2),  # Yankees
                       c(Home = "WSH", Visitor = "NYY"),
                       c(Home = "WSH", Visitor = "NYY"),
                       c(Home = "WSH", Visitor = "NYY"),
                       c(Home = "OAK", Visitor = "SDP"),  # A's
                       c(Home = "OAK", Visitor = "SDP"),
                       c(Home = "OAK", Visitor = "SDP"),
                       make_schedule(c("OAK", "SFG"), 3),
                       c(Home = "TBR", Visitor = "PHI"),  # Phillies
                       c(Home = "TBR", Visitor = "PHI"),
                       c(Home = "TBR", Visitor = "PHI"),
                       make_schedule(c("PHI", "TOR"), 3),  # Pirates nothing to be done
                       make_schedule(c("SEA", "SDP"), 3),  # Padres
                       make_schedule(c("SDP", "TEX"), 2),
                       make_schedule(c("SFG", "SEA"), 2),  # Giants
                       c(Home = "SFG", Visitor = "TEX"),  
                       c(Home = "SFG", Visitor = "TEX"),
                       c(Home = "SFG", Visitor = "TEX"),  # Mariners and Cardinals nothing to be done
                       make_schedule(c("WSH", "TBR"), 2),  # Rays; Rangers nothing to be done
                       make_schedule(c("WSH", "TOR"), 2)  # Blue Jays; Nationals nothing to be done
  )
  
  # Step 2: Simulate Team Talents and Win Probabilities
  # Use Pythagorean Win%... scrape the standings:
  standings_NL = standings_on_date_bref(date = "2020-09-28", division = "NL Overall")
  standings_AL = standings_on_date_bref(date = "2020-09-28", division = "AL Overall")
  
  # NL
  NLWest_talents = c(0.712, 0.503, 0.388, 0.458, 0.633)
  NLCentral_talents = c(0.545, 0.521, 0.470, 0.5, 0.363)
  NLEast_talents = c(0.586, 0.434, 0.466, 0.493, 0.488)
  NL_talents = c(NLWest_talents, NLCentral_talents, NLEast_talents)
  
  # AL
  ALWest_talents = c(0.576, 0.507, 0.353, 0.420, 0.460)
  ALCentral_talents = c(0.601, 0.599, 0.578, 0.458, 0.390)
  ALEast_talents = c(0.570, 0.417, 0.485, 0.605, 0.468)
  AL_talents = c(ALWest_talents, ALCentral_talents, ALEast_talents)
  
  # Aggregate all talents
  all_talents = c(NL_talents, AL_talents)
  names(all_talents) = teams
  
  # Standardize the talents
  standardized_talents = (all_talents - mean(all_talents)) / sd(all_talents)
  
  # Put into data frame
  TAL = tibble(Team = teams, League = league, Division = div, Talent = standardized_talents)
  SCH = schedule %>%
    inner_join(TAL, by = c("Home" = "Team")) %>%
    rename(Talent.Home = Talent) %>%
    inner_join(TAL, by = c("Visitor" = "Team")) %>%
    rename(Talent.Visitor = Talent)
  
  # Win probabilities according to Bradley-Terry Model
  SCH = SCH %>%
    mutate(prob_Home = exp(Talent.Home) / (exp(Talent.Home) + exp(Talent.Visitor)))
  
  # Play out a whole season
  SCH %>%
    mutate(outcome = rbinom(nrow(.), 1, prob_Home),
           winner = ifelse(outcome, Home, Visitor)) -> SCH
  
  # compute number of games won for all teams
  SCH %>% 
    group_by(winner) %>%
    summarize(Wins = n()) %>%
    inner_join(TAL, by = c("winner" = "Team")) -> RESULTS
  
  win_or_runnerup_division <- function(RR) {
    out = RR %>%
      mutate(Winner.Div = 0,
             RunnerUp.Div = 0,
             prob = exp(Talent),
             outcome = sample(nrow(.), prob = prob)) %>%
      arrange(Division, desc(Wins), outcome) %>%
      select(-outcome)
    out[1 + c(0, 5, 10, 15, 20, 25), "Winner.Div"] = 1
    out[1 + c(1, 6, 11, 16, 21, 26), "RunnerUp.Div"] = 1
    out
  }
  
  wild_card_teams = function(out) {
    WC_teams = out %>%
      filter(Winner.Div == 0 & RunnerUp.Div == 0) %>%
      group_by(League) %>%
      arrange(desc(Wins)) %>%
      slice(1:2) %>%
      pull(winner)
    out = out %>%
      mutate(WC = ifelse(winner %in% WC_teams, 1, 0))
    out
  }
  
  RESULTS = win_or_runnerup_division(RESULTS)
  RESULTS = wild_card_teams(RESULTS)
  
  #### HERE
  
  # Now that we have the playoff teams, need to simulate the playoffs themselves
  # Wild Card Round (best of 3). First get WC matchups
  playoff_teams = RESULTS %>% 
    filter(Winner.Div == 1 | RunnerUp.Div == 1 | WC == 1) %>% 
    arrange(League, desc(Winner.Div), desc(RunnerUp.Div), desc(WC), desc(Wins)) %>% 
    pull(winner)
  NL_playoff_teams = playoff_teams[1:8]
  AL_playoff_teams = playoff_teams[9:16]
  NL_1_vs_8 = rbind(NL_playoff_teams[1], NL_playoff_teams[8])
  NL_2_vs_7 = rbind(NL_playoff_teams[2], NL_playoff_teams[7])
  NL_3_vs_6 = rbind(NL_playoff_teams[3], NL_playoff_teams[6])
  NL_4_vs_5 = rbind(NL_playoff_teams[4], NL_playoff_teams[5])
  
  AL_1_vs_8 = rbind(AL_playoff_teams[1], AL_playoff_teams[8])
  AL_2_vs_7 = rbind(AL_playoff_teams[2], AL_playoff_teams[7])
  AL_3_vs_6 = rbind(AL_playoff_teams[3], AL_playoff_teams[6])
  AL_4_vs_5 = rbind(AL_playoff_teams[4], AL_playoff_teams[5])
  
  # Stack into df
  wc_matchups = as_tibble(rbind(NL_1_vs_8, NL_2_vs_7, NL_3_vs_6, NL_4_vs_5,
                                AL_1_vs_8, AL_2_vs_7, AL_3_vs_6, AL_4_vs_5)) %>%
    rename(Team = V1) %>%
    mutate(Seed = rep(c(1, 8, 2, 7, 3, 6, 4, 5), 2))
  
  # Merge with playoff_teams
  wc_matchups = wc_matchups %>% 
    inner_join(RESULTS, by = c("Team" = "winner"))
  
  # Simulate wild card round
  wc_results = wc_matchups %>%
    mutate(Group = rep(1:(n()/2), each = 2)) %>%
    group_by(Group) %>%
    mutate(outcome = rmultinom(1, 3, prob), Winner.WC = ifelse(outcome > 1, 1, 0)) %>%
    #filter(outcome > 1) %>%
    ungroup() %>%
    select(-Group)#, -outcome)
  
  # Simulate division series
  # Arrange rows such that we get the correct division series matchups
  div_matchups = (wc_results %>%
                    filter(outcome > 1))[c(1, 4, 2, 3, 5, 8, 6, 7), ]
  
  # Simulate DS series
  div_series_results = div_matchups %>%
    mutate(Group = rep(1:(n()/2), each = 2)) %>%
    group_by(Group) %>%
    mutate(outcome = rmultinom(1, 5, prob), Winner.DS = ifelse(outcome > 2, 1, 0)) %>%
    #filter(outcome > 2) %>%
    ungroup() %>%
    select(-Group) #, -outcome)
  
  # Simulate championship series
  # Get appropriate matchups as above
  champ_matchups = div_series_results %>%
    filter(outcome > 2)
  
  championship_series_results = champ_matchups %>%
    mutate(Group = rep(1:(n()/2), each = 2)) %>%
    group_by(Group) %>%
    mutate(outcome = rmultinom(1, 7, prob), Winner.CS = ifelse(outcome > 3, 1, 0)) %>%
    #filter(outcome > 3) %>%
    ungroup() %>%
    select(-Group) #, -outcome)
  
  # Simulate world series
  # Get appropriate matchups as above
  ws_matchup = championship_series_results %>%
    filter(outcome > 3)
  
  ws_results <- ws_matchup %>%
    mutate(outcome = rmultinom(1, 7, prob),
           Winner.WS = ifelse(outcome > 3, 1, 0)) #%>%
  #filter(outcome > 3) %>%
  #select(Team, Winner.WS)
  
  champion = ws_results %>%
    filter(Winner.WS == 1) %>%
    select(Team, Winner.WS)
  
  # data frame has teams, division, talent, wins, and diff playoff results. Gets returned
  final = RESULTS %>%
    left_join(wc_results, by = c("winner" = "Team")) %>%
    left_join(div_series_results, by = c("winner" = "Team")) %>%
    left_join(championship_series_results, by = c("winner" = "Team")) %>%
    left_join(ws_results, by = c("winner" = "Team")) %>%
    select_at(vars(-ends_with(".y"))) %>% 
    select_at(vars(-ends_with(".x.x"))) %>% 
    select(-Wins, -League, -Division, -Winner.Div, -RunnerUp.Div, -WC, -Winner.DS, -Talent, -prob, -prob.x) %>% 
    select_at(vars(-starts_with("outcome")))
  final[is.na(final)] = 0
  return(final)
}