#### Helper File - Data Tables Creation ####

# Create Data Table used for Figure Creation
figdata_tbl <- function(master_df, year, team, roll_num) {
        
        team_data <- master_df %>% filter(Year == year, Full.Name == team)
        
        fig_data <- data.frame(Year = year,
                               Team = team_data %>% distinct(Tm) %>% pull(),
                               Game = as.numeric(seq(roll_num, nrow(team_data), 1)),
                               RS = round(rollmean(team_data$R, roll_num), 2),
                               RA = round(rollmean(team_data$RA, roll_num), 2)) %>%
                        mutate(R_Diff = round(RS - RA, 2),
                               Gms_Avgd = map_chr(Game, ~ paste0((.x - (roll_num - 1)), "-", .x))) %>%
                        select(1:3, 7, 4:6) %>%
                        rename("RS/G" = "RS", "RA/G" = "RA")
        
        return(fig_data)
}

# Create Standings-like table for Selected Year
standings_tbl <- function(master_df, year) {
        
        standings_data <- master_df %>%
                        filter(Year == year) %>%
                        group_by(Tm) %>%
                        summarise(G = max(Gm_num, na.rm = T),
                                  W = sum(substr(Gm_Result, 1, 1) == "W"),
                                  L = sum(substr(Gm_Result, 1, 1) == "L"),
                                  RSperG = round(mean(R, na.rm = T), 2),
                                  RAperG = round(mean(RA, na.rm = T), 2),
                                  StDev_RSperG = round(sd(R, na.rm = T), 2),
                                  StDev_RAperG = round(sd(RA, na.rm = T), 2),
                                  RS = sum(R, na.rm = T),
                                  RA = sum(RA, na.rm = T),
                                  Lg = ifelse(year > 2012, League, League_pre2013),
                                  Div = ifelse(year > 2012, Division, Division_pre2013)) %>%
                        mutate(Year = year,
                               Rdiff = RS - RA,
                               Wpct = round(W/(W+L), 3),
                               pythW = round(((RS^1.83)/((RS^1.83) + (RA^1.83)))*G, 0),
                               pythL = G - pythW,
                               pythWpct = round(pythW/(pythW+pythL), 3),
                               Luck = W - pythW,
                               RS_CV = round(StDev_RSperG/RSperG, 3),
                               RA_CV = round(StDev_RAperG/RAperG, 3)) %>%
                        select(Year, Tm, Lg, Div, 
                               G, W, L, Wpct, 
                               RS, RA, Rdiff,
                               pythWpct, Luck,
                               RSperG, StDev_RSperG, RS_CV,
                               RAperG, StDev_RAperG, RA_CV) %>%
                        arrange(desc(Wpct)) %>%
                        rename("Team" = "Tm", "League" = "Lg", "Division" = "Div", "Win%" = "Wpct", "pythWin%" = "pythWpct", "RS/G" = "RSperG",
                               "RA/G" = "RAperG", "RS_StDev" = "StDev_RSperG", "RA_StDev" = "StDev_RAperG")
        
        return(standings_data)
}