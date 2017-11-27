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
        if (name == 'Anaheim Angels') { return('Los Angeles Angels') 
        } else if (name == 'Florida Marlins') { return('Miami Marlins')
        } else if (name == 'Tampa Bay Devil Rays') { return('Tampa Bay Rays')
        } else if (name == 'Montreal Expos') { return('Washington Nationals')
        } else { return(name)
        }
}

teamcode_for_link <- function(name) {
        if (name == 'Anaheim Angels') { return('ANA') 
        } else if (name == 'Florida Marlins') { return('FLA')
        } else if (name == 'Tampa Bay Devil Rays') { return('TBD')
        } else if (name == 'Montreal Expos') { return('MON')
        } else { return(name)
        }
}