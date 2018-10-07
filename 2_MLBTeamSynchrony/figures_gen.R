##### Helper File - Figure Creation ####

curSeason <- 2018

### Runs Scored and Runs Allowed Plot
rs_ra_roll_plot <- function(m_df, year, team_code, roll_num, team_name) {
        team_data <- m_df %>% filter(Year == year, Tm_Current == team_code)

        team_rolling <- data.frame(Year = year,
                                   Team = team_code,
                                   Game = seq(roll_num, nrow(team_data), 1),
                                   RS = rollmean(team_data$R, roll_num),
                                   RA = rollmean(team_data$RA, roll_num))
        
        team_rolling <- team_rolling %>% mutate(r_diff = RS - RA,
                                                diff_valence = ifelse(r_diff >= 0, 'pos', 'neg'),
                                                ymin = map2_dbl(RS, RA, min),
                                                ymax = map2_dbl(RS, RA, max),
                                                RSmin = ifelse(diff_valence == 'pos', ymin, NA),
                                                RSmax = ifelse(diff_valence == 'pos', ymax, NA),
                                                RAmin = ifelse(diff_valence == 'neg', ymin, NA),
                                                RAmax = ifelse(diff_valence == 'neg', ymax, NA))
        
        hi_wtr <- round(max(max(team_rolling$RS), max(team_rolling$RA)), 0)
        ymax_fig <- ifelse(hi_wtr %% 2 == 0, (hi_wtr + 2), (hi_wtr + 3))
        xmax_fig <- if (year == curSeason & max(team_rolling$Game) < 136) { plyr::round_any(max(team_rolling$Game)*1.1, 5, f = ceiling)} else { 162 }
        xscale <- if (year == curSeason & max(team_rolling$Game) <= 100) { seq(10, 100, 10) } else { seq(25, 150, 25) }
        
        avg_RS <- round(mean(team_data$R), 2)
        avg_RA <- round(mean(team_data$RA), 2)
        
        wins <- sum(str_sub(team_data$Gm_result, 1, 1) == "W")
        losses <- sum(str_sub(team_data$Gm_result, 1, 1) == "L")
        
        figure <- ggplot(team_rolling, aes(x = Game)) +
                        {if(sum(team_rolling$diff_valence == 'neg') > 0) geom_ribbon(aes(ymin = RAmin, ymax = RAmax), fill = 'firebrick3', alpha = 0.45, show.legend = F)} +
                        {if(sum(team_rolling$diff_valence == 'pos') > 0) geom_ribbon(aes(ymin = RSmin, ymax = RSmax), fill = 'dodgerblue3', alpha = 0.45, show.legend = F)} +
                        geom_line(aes(y = RA, colour = 'firebrick3'), size = 1.35) +
                        geom_line(aes(y = RS, colour = 'dodgerblue3'), size = 1.35) +
                        scale_colour_manual("", values = c("dodgerblue3", "firebrick3"), labels = c("Runs Scored", "Runs Allowed")) +
                        ggtitle(paste(year, team_name)) +
                        xlab("Game Number") +
                        ylab(paste(roll_num, "-game Rolling Average of Runs", sep="")) +
                        scale_y_continuous(breaks=seq(0.0, ymax_fig, 2.0), limits=c(0.0, ymax_fig)) +
                        scale_x_continuous(breaks=c(1, xscale, xmax_fig), limits = c(1, xmax_fig), expand = c(0.025, 0)) +
                        labs(caption = "cteeter.ca") +
                        theme_bw() +
                        theme(plot.title = element_text(face="bold", size=28, vjust=0.5, hjust=0.5),
                                axis.title.x = element_text(face="bold", size=16, vjust=0.5), 
                                axis.title.y = element_text(face="bold", size=16, vjust=0.5),
                                axis.text.x = element_text(size=12, colour="black", face="bold"),
                                axis.text.y = element_text(size=12, colour="black", face="bold"),
                                legend.text = element_text(size=12, colour="black", face="bold"),
                                legend.background = element_rect(fill = "transparent"),
                                legend.key = element_rect(fill = "transparent"),
                                legend.justification = c(1, 0), legend.position = c(0.99, 0.01),
                                plot.margin = margin(5, 5, 10, 5),
                                plot.caption = element_text(size=12, colour="gray75", face="italic", hjust = 1, vjust = 1),
                                panel.grid.minor=element_blank(), panel.grid.major=element_blank())
        
        figure <- figure + 
                        annotate("text", x = 1, y = 0.9, label = paste("Record:", wins, "-", losses), hjust = 0, vjust = 0.5, size = 3, fontface = 'italic') +
                        annotate("text", x = 1, y = 0.5, label = paste("Avg RS/G:", avg_RS), hjust = 0, vjust = 0.5, size = 3, fontface = 'italic') +
                        annotate("text", x = 1, y = 0.1, label = paste("Avg RA/G:", avg_RA), hjust = 0, vjust = 0.5, size = 3, fontface = 'italic')
        
        return(figure)
}

### Run Differential Plot
rdiff_roll_plot <- function(m_df, year, team_code, roll_num, team_name) {
        team_data <- m_df %>% filter(Year == year, Tm_Current == team_code)
        
        team_rolling <- data.frame(Year = year,
                                   Team = team_code,
                                   Game = seq(roll_num, nrow(team_data), 1),
                                   RS = rollmean(team_data$R, roll_num),
                                   RA = rollmean(team_data$RA, roll_num))
        
        team_rolling <- team_rolling %>% mutate(r_diff = RS - RA,
                                                diff_valence = ifelse(r_diff >= 0, 'pos', 'neg'))
        
        hi_diff <- round(max(abs(team_rolling$r_diff)), 0)
        ymax_fig <- ifelse(hi_diff %% 2 == 0, (hi_diff + 2), (hi_diff + 3))
        xmax_fig <- if (year == curSeason & max(team_rolling$Game) < 136) { plyr::round_any(max(team_rolling$Game)*1.1, 5, f = ceiling)} else { 162 }
        xscale <- if (year == curSeason & max(team_rolling$Game) <= 100) { seq(10, 100, 10) } else { seq(25, 150, 25) }
        
        avg_RS <- round(mean(team_data$R), 2)
        avg_RA <- round(mean(team_data$RA), 2)
        avg_R_diff <- round(avg_RS - avg_RA, 2)
        
        wins <- sum(str_sub(team_data$Gm_result, 1, 1) == "W")
        losses <- sum(str_sub(team_data$Gm_result, 1, 1) == "L")
        
        figure <- ggplot(team_rolling, aes(x = Game, y = r_diff, fill = diff_valence, colour = diff_valence)) +
                geom_bar(stat = 'identity') +
                { if(sum(team_rolling$diff_valence == 'pos') == 0) { scale_colour_manual(values = "firebrick3",
                                                                                       labels = "Negative",
                                                                                       guide = F) }
                        else if (sum(team_rolling$diff_valence == 'neg') == 0) { scale_colour_manual(values = "dodgerblue3",
                                                                                                   labels = "Positive",
                                                                                                   guide = F) }
                        else { scale_colour_manual(values = c("firebrick3", "dodgerblue3"),
                                                 breaks = c("neg", "pos"),
                                                 labels = c("Negative", "Positive"),
                                                 guide = F) } } +
                { if(sum(team_rolling$diff_valence == 'pos') == 0) { scale_fill_manual(values = alpha("firebrick3", 0.45),
                                                                                        labels = "Negative") }
                        else if (sum(team_rolling$diff_valence == 'neg') == 0) { scale_fill_manual(values = alpha("dodgerblue3", 0.45),
                                                                                                       labels = "Positive") }
                        else { scale_fill_manual(values = alpha(c("firebrick3", "dodgerblue3"), 0.45),
                                  breaks = c("neg", "pos"),
                                  labels = c("Negative", "Positive")) } } +
                ggtitle(paste(year, team_name)) +
                xlab("Game Number") +
                ylab(paste(roll_num, "-game Rolling Average Run Differential", sep="")) +
                scale_y_continuous(breaks=seq(-ymax_fig, ymax_fig, 2.0), limits=c(-ymax_fig, ymax_fig)) +
                scale_x_continuous(breaks=c(1, xscale, xmax_fig), limits = c(1, xmax_fig), expand = c(0.025, 0)) +
                labs(caption = "cteeter.ca") +
                theme_bw() +
                guides(fill = guide_legend(reverse = T)) +
                theme(plot.title = element_text(face="bold", size=28, vjust=0.5, hjust=0.5),
                      axis.title.x = element_text(face="bold", size=16, vjust=0.5), 
                      axis.title.y = element_text(face="bold", size=16, vjust=0.5),
                      axis.text.x = element_text(size=12, colour="black", face="bold"),
                      axis.text.y = element_text(size=12, colour="black", face="bold"),
                      legend.title=element_blank(),
                      legend.text = element_text(size=12, colour="black", face="bold"),
                      legend.background = element_rect(fill = "transparent"),
                      legend.key = element_rect(fill = "transparent"),
                      legend.justification = c(1, 0), legend.position = c(0.99, 0.01),
                      plot.margin = margin(5, 5, 10, 5),
                      plot.caption = element_text(size=12, colour="gray75", face="italic", hjust = 1, vjust = 1),
                      panel.grid.minor=element_blank(), panel.grid.major=element_blank())
        
        figure <- figure + 
                        annotate("text", x = 1, y = -ymax_fig+0.9, label = paste("Record:", wins, "-", losses), hjust = 0, vjust = 0.5, size = 3, fontface = 'italic') +
                        annotate("text", x = 1, y = -ymax_fig+0.5, label = paste("Avg R_Diff:", avg_R_diff), hjust = 0, vjust = 0.5, size = 3, fontface = 'italic')
        
        return(figure)
}