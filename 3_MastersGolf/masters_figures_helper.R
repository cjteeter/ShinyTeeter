##### Helper File for Shiny Masters - Figure Creation ####

#### Figure 1 - Par by Year
fig1_par <- function(score_data, cut_data, years, cut_line = T, finish_groups, player_highlight = NULL) {
        
        # Set up possible highlighting of selected players
        players <- if(!is.null(player_highlight)) { player_highlight } else { NA }
        player_cols <- c('#b10026', '#225ea8', '#fc6f0a')
        player_cols <- if(!is.null(player_highlight)) { player_cols[1:length(players)] } else { player_cols }
        names(player_cols) <- players
        
        pl_par_df <- score_data %>%
                        mutate(Finish_Group_6 = factor(Finish_Group_6, 
                                             levels = c('Missed Cut', 'Others', 'Top 10', 'Winner'),
                                             labels = c('Missed Cut', 'Others', 'Top 10', 'Winner'),
                                             ordered = T)) %>%
                        filter(!is.na(Finish_Par),
                               !Finish_Group_6 %in% c('Withdrew', 'Disqualified'),
                               between(Year, years[1], years[2]),
                               if(!is.null(finish_groups)) { Finish_Group_6 %in% finish_groups } else { is.na(Finish_Group_6) },
                               if(!is.null(player_highlight)) { Player_FullName %in% players } else { is.na(Player_FullName) }) %>%
                        arrange(Year, desc(Finish))
        
        # Filter cut line data frame
        cut_df <- cut_data %>%
                        filter(between(Year, years[1], years[2]))
        
        # Filter and Organize primary data frame
        
        par_df <- score_data %>%
                        filter(!is.na(Finish_Par),
                               !Finish_Group_6 %in% c('Withdrew', 'Disqualified'),
                               between(Year, years[1], years[2]),
                               Finish_Group_6 %in% finish_groups,
                               if(!is.null(player_highlight)) { !Player_FullName %in% players } else { !is.na(Player_FullName) }) %>%
                        mutate(Finish_Group_6 = factor(Finish_Group_6, 
                                             levels = c('Missed Cut', 'Others', 'Top 10', 'Winner'),
                                             labels = c('Missed Cut', 'Others', 'Top 10', 'Winner'),
                                             ordered = T)) %>%
                        arrange(Year, desc(Finish)) 
        
        # Create plot
        x_text_size <- case_when(
                        (years[2] - years[1]) >= 76 ~ 10,
                        (years[2] - years[1]) >= 46 & (years[2] - years[1]) < 76 ~ 12,
                        (years[2] - years[1]) < 46 ~ 14,
                        TRUE ~ NA_real_)
        
        par_fig <- ggplot(par_df) +
                        geom_hline(yintercept = 0, size = 0.5) +
                        geom_point(aes(x = Year_jit, y = Finish_Par, fill = Finish_Group_6, group = 1), size = 3.25, pch = 21, colour = "black", stroke = 1.35) +
                        { if(!is.null(player_highlight)) { geom_point(data = pl_par_df, aes(x = Year_jit, y = Finish_Par, fill = Player_FullName), size = 4.5, pch = 21, stroke = 1.8, inherit.aes = F) } } +
                        { if(!is.null(player_highlight)) { scale_fill_manual(breaks = c('Winner', 'Top 10', 'Others', 'Missed Cut', sort(players)), 
                                                                             values = c(player_cols, 'Winner' = '#076652', 'Top 10' = 'yellow3', 'Others' = 'gray60', 'Missed Cut' = 'gray20'),
                                                                             guide = guide_legend(reverse = TRUE)) }
                                else { scale_fill_manual(breaks = c('Winner', 'Top 10', 'Others', 'Missed Cut'), 
                                                         values = c('Winner' = '#076652', 'Top 10' = 'yellow3', 'Others' = 'gray60', 'Missed Cut' = 'gray20'),
                                                         guide = guide_legend(reverse = TRUE)) } } +
                        { if(cut_line) { geom_line(data = cut_df, aes(x = Year, y = Cut), size = 2, color = "red3", linetype = 'solid') } } +
                        scale_x_continuous(breaks = seq(min(par_df$Year), max(par_df$Year), 1), limits = c(min(par_df$Year)-1, max(par_df$Year)+1), expand = expand_scale(add = 0.5)) +
                        scale_y_continuous(breaks = seq(-20, 60, 5), limits = c(-20, 60),
                                           labels = map_chr(seq(-20, 60, 5), ~ if(.x > 0) { paste0("+", .x) } else if(.x == 0) { "E" } else { as.character(.x) } )) +
                        labs(x = 'Year', y = 'Total Score Relative to Par', caption = 'cteeter.ca') +
                        theme_teeter() +
                        theme(axis.text.x = element_text(size = x_text_size, angle = -90, hjust = 0, vjust = 0.5)) +
                        { if(years[1] <= 1943) { annotate("rect", xmin = 1943, xmax = 1945, ymin = -5, ymax = 5, fill = 'white') } } +
                        { if(years[1] <= 1943) { annotate("text", x = 1944, y = -15, label = "[ Tournament not held from 1943 - 1945, due to World War II ]", angle = 90, hjust = 0, vjust = 0.35) } } +
                        { if(cut_line & years[1] <= 1956) { annotate("text", x = years[1] + 1, y = 57, label = "{ No 36-hole cut from 1934 - 1956 }", hjust = 0, vjust = 0.5, color = "red3", fontface = "bold") } }
        
        return(par_fig)
}

#### Figure 2 - Scoring Distributions
fig2_scrdist <- function(score_data, years, career_rounds, num_players, col_blind) {
        
        # Create, Organize and Filter Scoring Averages Data Frame
        rounds_df <- score_data %>%
                        filter(between(Year, years[1], years[2])) %>%
                        select(Year, Player_FullName, R1, R2, R3, R4) %>%
                        gather(Round, Score, -Year, -Player_FullName) %>%
                        arrange(Year, Player_FullName) %>%
                        left_join(x =., y = filter(., !is.na(Score)) %>% 
                                                group_by(Player_FullName) %>% 
                                                summarise(Career_Rds = n(), 
                                                          Career_Avg = round(mean(Score, na.rm = T), 2),
                                                          Career_Median = round(median(Score, na.rm = T),2)),
                                  by = 'Player_FullName')
        
        # Generate list of the Top-n players (based on Career Average score) and Add Rank for Plotting
        players_to_plot <- rounds_df %>% 
                                filter(!is.na(Score),
                                       between(Year, years[1], years[2]),
                                       Career_Rds >= career_rounds) %>% 
                                select(Player_FullName, Career_Median, Career_Avg) %>% 
                                distinct(Player_FullName, .keep_all = T) %>% 
                                arrange(Career_Median, Career_Avg) %>%
                                slice(1:num_players) %>%
                                mutate(Rank = row_number())
        
        # Filter Primary Data Frame to only include players on the players_to_plot list
        rounds_df_fig <- rounds_df %>% 
                                filter(!is.na(Score),
                                between(Year, years[1], years[2]),
                                Career_Rds >= career_rounds,
                                Player_FullName %in% (players_to_plot %>% pull(Player_FullName))) %>%
                        left_join(players_to_plot %>% select(Player_FullName, Rank), by = "Player_FullName")
        
        # Create Plot
        scrdist_fig <- ggplot(rounds_df_fig, aes(x = Score, y = reorder(Player_FullName, Rank), fill = ..x..)) +
                stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = T,
                                    quantile_lines = T, quantiles = 2, alpha = 0.90, scale = 1.25, size = 1.05) +
                scale_fill_gradient2(limits = c(62, 82), 
                                     low = ifelse(col_blind, "#d8b365", "red"), 
                                     mid = "white", 
                                     high = ifelse(col_blind, "#5ab4ac", "springgreen4"), 
                                     midpoint = 72, guide = F) +
                scale_y_discrete(expand = expand_scale(add = c(1, 1.75))) +
                scale_x_continuous(breaks = seq(62, 82, 2), limits = c(62, 82), oob = rescale_none) +
                labs(x = 'Score', y = "", 
                     title = paste(num_players, "Lowest Career Scorers"), 
                     subtitle = paste0("Among players with at least ", career_rounds, " rounds played between ", years[1],
                                      " and ", years[2], ".\n"),
                     caption = 'cteeter.ca') +
                theme_teeter()
        
        return(scrdist_fig)
}

#### Figure 3 & 4 - Player Page Par by Year
fig3and4_plyr <- function(score_data, player, col_blind) {
        
        # Filter and Organize primary data frame
        
        par_df <- score_data %>%
                filter(Player_FullName == player,
                       !is.na(Finish_Par),
                       !Finish_Group_6 %in% c('Withdrew', 'Disqualified')) %>%
                mutate(Finish_Group_6 = factor(Finish_Group_6, 
                                               levels = c('Missed Cut', 'Others', 'Top 10', 'Winner'),
                                               labels = c('Missed Cut', 'Others', 'Top 10', 'Winner'),
                                               ordered = T)) %>%
                arrange(Year, desc(Finish)) 
        
        ymin <- min(-5, plyr::round_any(min(par_df$Finish_Par), 5, floor))
        ymax <- max(5, plyr::round_any(max(par_df$Finish_Par), 5, ceiling))
        
        par_fig <- ggplot(par_df) +
                geom_hline(yintercept = 0, size = 0.5) +
                geom_point(aes(x = Year, y = Finish_Par, fill = Finish_Group_6, group = 1), size = 4.25, pch = 21, colour = "black", stroke = 1.35) +
                scale_fill_manual(breaks = c('Winner', 'Top 10', 'Others', 'Missed Cut'), 
                                  values = c('Winner' = '#076652', 'Top 10' = 'yellow3', 'Others' = 'gray60', 'Missed Cut' = 'gray20'),
                                  guide = guide_legend(reverse = TRUE)) +
                scale_x_continuous(breaks = seq(min(par_df$Year), max(par_df$Year), 1), limits = c(min(par_df$Year)-1, max(par_df$Year)+1), expand = expand_scale(add = 0.5)) +
                scale_y_continuous(breaks = seq(ymin, ymax, 5), limits = c(ymin, ymax),
                                   labels = map_chr(seq(ymin, ymax, 5), ~ if(.x > 0) { paste0("+", .x) } else if(.x == 0) { "E" } else { as.character(.x) } )) +
                labs(x = 'Year', y = 'Total Score Relative to Par') +
                theme_teeter() +
                theme(axis.text.x = element_text(size = 10, angle = -90, hjust = 0, vjust = 0.5))
        
        # Create, Organize and Filter Scoring Averages Data Frame
        rounds_df <- score_data %>%
                filter(Player_FullName == player) %>%
                select(Year, Player_FullName, R1, R2, R3, R4) %>%
                gather(Round, Score, -Year, -Player_FullName) %>%
                filter(!is.na(Score)) %>%
                arrange(Year, Player_FullName)
        
        plyr_stats <- rounds_df %>%
                filter(!is.na(Score)) %>% 
                summarise(Career_Rds = n(), 
                          Career_Avg = round(mean(Score, na.rm = T), 2),
                          Career_Median = round(median(Score, na.rm = T),2))
        
        xmin <- min(60, plyr::round_any(min(rounds_df$Score), 2, floor))
        xmax <- max(82, plyr::round_any(max(rounds_df$Score), 2, ceiling))
        
        # Create Plot
        scrdist_fig <- ggplot(rounds_df, aes(x = Score, y = Player_FullName, fill = ..x..)) +
                stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = T,
                                    quantile_lines = T, quantiles = 2, alpha = 0.90, scale = 5, size = 1.05) +
                scale_fill_gradient2(limits = c(xmin, xmax), 
                                     low = ifelse(col_blind, "#d8b365", "red"), 
                                     mid = "white", 
                                     high = ifelse(col_blind, "#5ab4ac", "springgreen4"),
                                     midpoint = 72, guide = F) +
                scale_y_discrete(expand = expand_scale(add = c(0.1, 1.05))) +
                scale_x_continuous(breaks = seq(xmin, xmax, 2), limits = c(xmin, xmax), oob = rescale_none) +
                labs(x = 'Score', y = "", 
                     #title = "Career Scoring Distribution",
                     caption = 'cteeter.ca') +
                theme_teeter() +
                theme(axis.text.y = element_blank(),
                      axis.ticks.y = element_blank())
        
        scrdist_fig <- scrdist_fig +
                        annotate("text", x = xmax, y = 1.95, 
                                 label = paste0("Median Score: ", plyr_stats$Career_Median), 
                                 hjust = 1, vjust = 0.5, size = 4, fontface = 'italic') +
                        annotate("text", x = xmax, y = 1.90, 
                                 label = paste0("Average Score: ", plyr_stats$Career_Avg), 
                                 hjust = 1, vjust = 0.5, size = 4, fontface = 'italic')
        
        final_fig <- grid.arrange(par_fig, scrdist_fig)
        
        return(final_fig)
}