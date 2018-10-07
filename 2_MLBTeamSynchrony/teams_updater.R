#### Helper File - Teams List ####

update_teams <- function(tm_df, year) {
        if (year == "") { return(sort(tm_df$Full.Name))
        } else if (year <= 2004) {
                tm_df$Full.Name[13] <- 'Anaheim Angels'
                tm_df$Full.Name[15] <- 'Florida Marlins'
                tm_df$Full.Name[27] <- 'Tampa Bay Devil Rays'
                tm_df$Full.Name[30] <- 'Montreal Expos'
                return(sort(tm_df$Full.Name))
        } else if (year > 2004 & year <= 2007) {
                tm_df$Full.Name[15] <- 'Florida Marlins'
                tm_df$Full.Name[27] <- 'Tampa Bay Devil Rays'
                return(sort(tm_df$Full.Name))
        } else if (year > 2007 & year <= 2011) {
                tm_df$Full.Name[15] <- 'Florida Marlins'
                return(sort(tm_df$Full.Name))
        } else { return(sort(tm_df$Full.Name))
        }
}

team_flip <- function(name) {
        case_when(
                name == 'Anaheim Angels' ~ 'Los Angeles Angels',
                name == 'Florida Marlins' ~ 'Miami Marlins',
                name == 'Tampa Bay Devil Rays' ~ 'Tampa Bay Rays',
                name == 'Montreal Expos' ~ 'Washington Nationals',
                TRUE ~ name)
}

teamcode_for_link <- function(name) {
        case_when(
                name == 'Anaheim Angels' ~ 'ANA',
                name == 'Florida Marlins' ~ 'FLA',
                name == 'Tampa Bay Devil Rays' ~ 'TBD',
                name == 'Montreal Expos' ~ 'MON',
                TRUE ~ name)
}