##### ggplot Custom Theme #####

theme_teeter <- function(baseSize = 12) {
        
        theme_bw(base_size = baseSize, base_family = "Arial Narrow") %+replace%
                
                theme(plot.margin = unit(rep(0.5, 4), "cm"),
                      panel.border = element_blank(),
                      panel.grid.minor = element_blank(), 
                      panel.grid.major = element_blank(),
                      plot.title.position = "plot",
                      plot.title = element_text(face = "bold", size = round(baseSize * 1.3, 0), hjust = 0, margin = margin(b = 7.5)),
                      plot.subtitle = element_text(face = "italic", size = round(baseSize * 1.17, 0), hjust = 0, margin = margin(b = 15)),
                      axis.line = element_line(colour = "black", linewidth = 1.15),
                      axis.ticks = element_line(),
                      axis.ticks.length = unit(4, "pt"),
                      axis.title.x = element_text(face = "bold", size = round(baseSize * 1.083, 0), vjust = 0.5, hjust = 0.5, margin = margin(t = 8)), 
                      axis.title.y = element_text(face = "bold", size = round(baseSize * 1.083, 0), angle = 90, margin = margin(r = 8)),
                      axis.text.x = element_text(colour = "black", face = "bold", size = baseSize, margin = margin(t = 2)),
                      axis.text.y = element_text(colour = "black", face = "bold", size = baseSize, margin = margin(r = 2)),
                      legend.title = element_blank(),
                      legend.text = element_text(colour = "black", face = "bold", size = round(baseSize * 1.083, 0)),
                      legend.background = element_rect(fill = "transparent", colour = NA),
                      legend.key = element_rect(fill = "transparent", colour = NA),
                      legend.key.size = unit(2, "lines"),
                      strip.background = element_rect(linewidth = 1.15, colour = "black"),
                      strip.text = element_text(face = "bold", size = round(baseSize * 1.167, 0),
                                                hjust = 0.5, vjust = 0.5, margin = margin(t = 2, b = 2)),
                      plot.caption = element_text(size = round(baseSize * 0.83, 0), colour = "gray75", face = "italic", hjust = 1, margin = margin(t = 10))
                )
}