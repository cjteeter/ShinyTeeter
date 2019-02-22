##### Helper File for Shiny Masters - Tables Creation ####

#### Table Set 1 - Historical Leaderboards
historical_tables <- function(score_data, rds_data, years, table, min_trns = NULL, min_rds = NULL) {
        
        trns <- score_data %>%
                        filter(between(Year, years[1], years[2])) %>%
                        group_by(Player_FullName) %>%
                        summarise(Tournaments = n())
                        
        if(table == 'Tournaments Entered') {
                header <- c("Player" = "Player_FullName", "Tournaments" = "Tournaments", "First Tournament" = "First_Year",
                            "Last Tournament" = "Last_Year", "Tournament Span (years)" = "Tournament_span")
                tbl_oi <- score_data %>%
                                filter(between(Year, years[1], years[2])) %>%
                                group_by(Player_FullName) %>%
                                summarise(Tournaments = n(),
                                          First_Year = min(Year),
                                          Last_Year = max(Year)) %>%
                                mutate(Tournament_span = Last_Year - First_Year) %>%
                                filter(if(!is.null(min_trns)) { Tournaments >= min_trns } else { Tournaments > 0 }) %>%
                                arrange(desc(Tournaments)) %>%
                                rename(!!header)
        } else if(table == 'Cuts Made') {
                if(years[1] < 1957) { years[1] <- 1957 } else { years[1] <- years[1] }
                header <- c("Player" = "Player_FullName", "Cuts Made" = "Cuts", "Tournaments" = "Tournaments",
                            "Cuts Made %" = "Cuts_perc", "First Cut Made" = "First_Cut",
                            "Last Cut Made" = "Last_Cut", "Made Cuts Span (years)" = "Cut_span")
                tbl_oi <- score_data %>%
                                filter(between(Year, years[1], years[2]),
                                       !Finish_Group_6 %in% c('Missed Cut', 'Withdrew', 'Disqualified')) %>%
                                group_by(Player_FullName) %>%
                                summarise(Cuts = n(),
                                          First_Cut = min(Year),
                                          Last_Cut = max(Year)) %>%
                                mutate(Cut_span = Last_Cut - First_Cut) %>%
                                left_join(trns, by = "Player_FullName") %>%
                                filter(if(!is.null(min_trns)) { Tournaments >= min_trns } else { Tournaments > 0 }) %>%
                                mutate(Cuts_perc = round((Cuts/Tournaments)*100, 2)) %>%
                                select(Player_FullName, Cuts, Tournaments, Cuts_perc, First_Cut, Last_Cut, Cut_span) %>%
                                arrange(desc(Cuts)) %>%
                                rename(!!header)
        } else if(table == 'Wins') {
                header <- c("Player" = "Player_FullName", "Wins" = "Wins", "Years" = "Years")
                tbl_oi <- score_data %>%
                                filter(between(Year, years[1], years[2]),
                                       Finish_Group_6 == 'Winner') %>%
                                group_by(Player_FullName) %>%
                                summarise(Wins = n(),
                                          Years = list(Year)) %>%
                                mutate(Years = map_chr(Years, ~ str_c(sort(unlist(.x)), collapse = ", "))) %>%
                                arrange(desc(Wins)) %>%
                                rename(!!header)
        } else if(table == 'Runners-up') {
                header <- c("Player" = "Player_FullName", "2nd Place Finishes" = "Snd_place", "Years" = "Years")
                tbl_oi <- score_data %>%
                                filter(between(Year, years[1], years[2]),
                                       Finish == 2) %>%
                                group_by(Player_FullName) %>%
                                summarise(Snd_place = n(),
                                          Years = list(Year)) %>%
                                mutate(Years = map_chr(Years, ~ str_c(sort(unlist(.x)), collapse = ", "))) %>%
                                arrange(desc(Snd_place)) %>%
                                rename(!!header)
        } else if(table == 'Top 5s') {
                header <- c("Player" = "Player_FullName", "Top 5s" = "Top5s", "Tournaments" = "Tournaments", 
                            "Top 5 %" = "Percent_Top5s", "Years" = "Years")
                tbl_oi <- score_data %>%
                                filter(between(Year, years[1], years[2]),
                                       Finish <= 5) %>%
                                group_by(Player_FullName) %>%
                                summarise(Top5s = n(),
                                          Years = list(Year)) %>%
                                mutate(Years = map_chr(Years, ~ str_c(sort(unlist(.x)), collapse = ", "))) %>%
                                left_join(trns, by = "Player_FullName") %>%
                                filter(if(!is.null(min_trns)) { Tournaments >= min_trns } else { Tournaments > 0 }) %>%
                                mutate(Percent_Top5s = round((Top5s/Tournaments)*100, 2)) %>%
                                select(Player_FullName, Top5s, Tournaments, Percent_Top5s, Years) %>%
                                arrange(desc(Top5s)) %>%
                                rename(!!header)
        } else if(table == 'Top 10s') {
                header <- c("Player" = "Player_FullName", "Top 10s" = "Top10s", "Tournaments" = "Tournaments", 
                            "Top 10 %" = "Percent_Top10s", "Years" = "Years")
                tbl_oi <- score_data %>%
                                filter(between(Year, years[1], years[2]),
                                       Finish <= 10) %>%
                                group_by(Player_FullName) %>%
                                summarise(Top10s = n(),
                                          Years = list(Year)) %>%
                                mutate(Years = map_chr(Years, ~ str_c(sort(unlist(.x)), collapse = ", "))) %>%
                                left_join(trns, by = "Player_FullName") %>%
                                filter(if(!is.null(min_trns)) { Tournaments >= min_trns } else { Tournaments > 0 }) %>%
                                mutate(Percent_Top10s = round((Top10s/Tournaments)*100, 2)) %>%
                                select(Player_FullName, Top10s, Tournaments, Percent_Top10s, Years) %>%
                                arrange(desc(Top10s)) %>%
                                rename(!!header)
        } else if(table == 'Top 25s') {
                header <- c("Player" = "Player_FullName", "Top 25s" = "Top25s", "Tournaments" = "Tournaments", 
                            "Top 25 %" = "Percent_Top25s", "First Top 25" = "First_Top25",
                            "Last Top 25" = "Last_Top25", "Top 25 Span (years)" = "Top25_span")
                tbl_oi <- score_data %>%
                                filter(between(Year, years[1], years[2]),
                                       Finish <= 25) %>%
                                group_by(Player_FullName) %>%
                                summarise(Top25s = sum(Finish <= 25),
                                          First_Top25 = min(Year),
                                          Last_Top25 = max(Year)) %>%
                                mutate(Top25_span = Last_Top25 - First_Top25) %>%
                                left_join(trns, by = "Player_FullName") %>%
                                filter(if(!is.null(min_trns)) { Tournaments >= min_trns } else { Tournaments > 0 }) %>%
                                mutate(Percent_Top25s = round((Top25s/Tournaments)*100, 2)) %>%
                                select(Player_FullName, Top25s, Tournaments, Percent_Top25s, First_Top25, Last_Top25, Top25_span) %>%
                                arrange(desc(Top25s)) %>%
                                rename(!!header)
        } else if(table == 'Sub-par Rounds') {
                header <- c("Player" = "Player_FullName", "Sub-par Rounds" = "SubPar_Rds", "Rounds Played" = "Rounds", 
                            "Sub-par %" = "Percent_SubPar", "Lowest Round" = "Low_Rd")
                tbl_oi <- score_data %>%
                                select(Year, Player_FullName, R1, R2, R3, R4) %>%
                                gather(Round, Score, -Year, -Player_FullName) %>%
                                filter(between(Year, years[1], years[2]),
                                       !is.na(Score)) %>%
                                group_by(Player_FullName) %>%
                                summarise(Rounds = n(),
                                          SubPar_Rds = sum(Score < 72),
                                          Percent_SubPar = round(SubPar_Rds/Rounds, 4)*100,
                                          Low_Rd = min(Score)) %>%
                                filter(if(!is.null(min_rds)) { Rounds >= min_rds } else { Rounds > 0 }) %>%
                                arrange(desc(SubPar_Rds)) %>%
                                select(Player_FullName, SubPar_Rds, Rounds, everything()) %>%
                                rename(!!header)
        } else if(table == 'Rounds in the 60s') {
                header <- c("Player" = "Player_FullName", "60s Rounds" = "Rds60s", "Rounds Played" = "Rounds", 
                            "60s Rounds %" = "Percent_Rds60s", "Lowest Round" = "Low_Rd")
                tbl_oi <- score_data %>%
                                select(Year, Player_FullName, R1, R2, R3, R4) %>%
                                gather(Round, Score, -Year, -Player_FullName) %>%
                                filter(between(Year, years[1], years[2]),
                                       !is.na(Score)) %>%
                                group_by(Player_FullName) %>%
                                summarise(Rounds = n(),
                                          Rds60s = sum(Score < 70),
                                          Percent_Rds60s = round(Rds60s/Rounds, 4)*100,
                                          Low_Rd = min(Score)) %>%
                                filter(if(!is.null(min_rds)) { Rounds >= min_rds } else { Rounds > 0 }) %>%
                                arrange(desc(Rds60s)) %>%
                                select(Player_FullName, Rds60s, Rounds, everything()) %>%
                                rename(!!header)
        } else if(table == 'Lowest Rounds') {
                header <- c("Player" = "Player_FullName", "Score" = "Score", "Year" = "Year", "Round" = "Round",
                            "Tournament Finish" = "Finish")
                tbl_oi <- rds_data %>%
                                filter(between(Year, years[1], years[2]),
                                       if(!is.null(min_rds)) { Career_Rds >= min_rds } else { Career_Rds > 0 }) %>%
                                arrange(Score, Finish, desc(Year)) %>%
                                select(Player_FullName, Score, Year, Round, Finish) %>%
                                top_n(-100, Score) %>%
                                rename(!!header)
        } else { NA }
        
        return(tbl_oi)
}

#### Table Set 2 - Yearly Tournament Leaderboards
yearly_leaderboard <- function(score_data, year) {
        
        header <- c("Pos" = "Finish", "Player" = "Player_FullName", "R1" = "R1", "R2" = "R2", "R3" = "R3", "R4" = "R4",
                    "Score" = "Total_Score", "Par" = "Finish_Par")
        tbl_oi <- score_data %>%
                        filter(Year == year, Rds_Complete == 4) %>%
                        arrange(Finish, Last_Name) %>%
                        select(Finish, Player_FullName, R1, R2, R3, R4, Total_Score, Finish_Par) %>%
                        rename(!!header)
        
        return(tbl_oi)
}

#### Table Set 3 - Scoring Averages Leaderboard
scoring_leaderboard <- function(score_data, years, career_rounds) {
        
        header <- c("Player" = "Player_FullName", "Rounds Played" = "Career_Rds", 
                    "Median Score" = "Career_Median", "Average Score" = "Career_Avg")
        tbl_oi <- score_data %>%
                        filter(between(Year, years[1], years[2])) %>%
                        select(Year, Player_FullName, R1, R2, R3, R4) %>%
                        gather(Round, Score, -Year, -Player_FullName) %>%
                        filter(!is.na(Score)) %>%
                        group_by(Player_FullName) %>% 
                        summarise(Career_Rds = n(), 
                                  Career_Median = round(median(Score, na.rm = T),2),
                                  Career_Avg = round(mean(Score, na.rm = T), 2)) %>%
                        filter(Career_Rds >= career_rounds) %>%
                        arrange(Career_Median, Career_Avg) %>%
                        rename(!!header)
        
        return(tbl_oi)
}

#### Table Set 4 - Player Page Tournaments
player_tournaments <- function(score_data, player) {
        
        header <- c("Year" = "Year", "R1" = "R1", "R2" = "R2", "R3" = "R3", "R4" = "R4", "Score" = "Total_Score", 
                    "Par" = "Finish_Par", "Pos" = "Finish_upd", "Group" = "Finish_Group_6")
        tbl_oi <- score_data %>%
                        filter(Player_FullName == player) %>%
                        mutate(Finish_upd = map_dbl(Finish, ~ if(.x >= 997) { NA } else { .x })) %>%
                        arrange(Year) %>%
                        select(Year, R1, R2, R3, R4, Total_Score, Finish_Par, Finish_upd, Finish_Group_6) %>%
                rename(!!header)
        
        return(tbl_oi)
}