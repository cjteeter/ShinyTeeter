#### Helper File - Data Tables Creation ####

# Create Data Table used for Figure Creation
figdata_tbl <- function(m_df, year, team_code, roll_num) {
        team_data <- m_df %>% filter(Year == year, Tm_Current == team_code)
        
        fig_data <- data.frame("Year" = year,
                               "Team" = team_data$Tm[1],
                               "Game" = seq(roll_num, nrow(team_data), 1),
                               "RS" = round(rollmean(team_data$R, roll_num), 2),
                               "RA" = round(rollmean(team_data$RA, roll_num),2))
        
        fig_data$R_Diff <- round(fig_data$RS - fig_data$RA, 2)
        for (r in 1:nrow(fig_data)) { 
                fig_data$Gms_Avgd[r] <- paste((fig_data$Game[r] - (roll_num - 1)), "-", fig_data$Game[r]) 
        }
        
        fig_data <- fig_data[, c(1:3, 7, 4:6)]
        
        return(fig_data)
}

# Create Standings-like table for Selected Year
standings_tbl <- function(m_df, tm_df, year) {
        standings_data <- data.frame("Year" = integer(), "Team" = character(), "League" = character(),
                                     "Division" = character(), "G" = integer(), "W" = integer(), 
                                     "L" = integer(), "R" = integer(), "RA" = integer(), 
                                     "RperG" = numeric(), "RAperG" = numeric(), 
                                     "sd_RperG" = numeric(), "sd_RAperG" = numeric(),
                                     stringsAsFactors = F)
        
        for (t in 1:length(tm_df$Team.Code)) {
                team_data <- m_df %>%
                                filter(Year == year, Tm_Current == tm_df$Team.Code[t])
                
                next_row <- data.frame("Year" = year,
                              "Team" = team_data$Tm[1],
                              "League" = ifelse(year > 2012, tm_df$League_Current[tm_df$Team.Code == tm_df$Team.Code[t]], tm_df$League_pre2013[tm_df$Team.Code == tm_df$Team.Code[t]]),
                              "Division" = ifelse(year > 2012, tm_df$Division_Current[tm_df$Team.Code == tm_df$Team.Code[t]], tm_df$Division_pre2013[tm_df$Team.Code == tm_df$Team.Code[t]]),
                              "G" = nrow(team_data), 
                              "W" = sum(substr(team_data$Gm_result, 1, 1) == "W"),
                              "L" = sum(substr(team_data$Gm_result, 1, 1) == "L"),
                              "RS" = sum(team_data$R),
                              "RA" = sum(team_data$RA),
                              "RSperG" = round(mean(team_data$R, na.rm = T), 2),
                              "RAperG" = round(mean(team_data$RA, na.rm = T), 2),
                              "StDev_RSperG" = round(sd(team_data$R, na.rm = T), 2),
                              "StDev_RAperG" = round(sd(team_data$RA, na.rm = T), 2))
                
                standings_data <- rbind(standings_data, next_row)
        }
        
        standings_data$Rdiff <- with(standings_data, (RS-RA))
        standings_data$Wpct <- with(standings_data, round(W/(W+L), 3))
        standings_data$pythW <- with(standings_data, round(((RS^1.83)/((RS^1.83)+(RA^1.83)))*G, 0))
        standings_data$pythL <- with(standings_data, (G - pythW))
        standings_data$pythWpct <- with(standings_data, round((RS^1.83)/((RS^1.83)+(RA^1.83)), 3))
        standings_data$CV_RS <- with(standings_data, round(StDev_RSperG/RSperG, 3))
        standings_data$CV_RA <- with(standings_data, round(StDev_RAperG/RAperG, 3))
        
        standings_data <- standings_data[, c(1:7, 15, 8:9, 14, 16:18, 10, 12, 19, 11, 13, 20)]
        
        return(standings_data)
}