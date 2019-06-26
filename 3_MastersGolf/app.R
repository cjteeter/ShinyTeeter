##### Shiny App for Exploring The Masters Tournament Golf Data #####

# Load required packages -------------------------------
require(shiny)
require(shinythemes)
require(DT)
require(tidyverse)
require(scales)
require(ggridges)
require(gridExtra)
require(extrafont)

# Load Helper files -------------------------------
source("masters_figures_helper.R")
source("masters_tables_helper.R")
source("teeter_ggplot-theme.R")

# Load data -------------------------------
masters <- read.csv('data/CT_Masters_playerscores_1934-2019.csv', stringsAsFactors = F)
cutline <- read.csv('data/CT_Masters_cutline_1934-2019.csv', stringsAsFactors = F)
low_rounds <- read.csv('data/CT_Masters_lowrounds_1934-2019.csv', stringsAsFactors = F)

# Create Variables --------------------------------------------------------
latest_trn <- 2019

# Create user interface -------------------------------
ui <- navbarPage(
        #theme = shinytheme('flatly'),
        theme = "flatlyST_bootstrap_CTedit.css",
        inverse = F, 
        id = "masters_golf",
        # App Title
        tags$div(tags$img(src='masters_logo_3.png', width = 108, height = 108, style="float:left; margin-left: 5px; margin-right: 5px; margin-top: -15px")), 
        
        # Yearly Tournaments ------------------------------------------------------
        tabPanel("Tournament Results",
                 fluidRow(
                         column(9,
                                wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 720px;",
                                          plotOutput("par_yr", height = 680, click = "plot_click"))),
                         column(3,
                                fluidRow(column(12,
                                        # Error Message Appearance
                                        tags$head(tags$style(HTML(".shiny-output-error-validation { font-style: italic; font-size: 125%; }"))),
                                         wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 250px;",
                                                   p(tags$em('Click on a point on the figure to get details.', style = "font-size: 70%; font-family:Helvetica; color:#4c4c4c"),
                                                     style = 'text-align:left; margin-bottom: 15px;'),
                                                   htmlOutput('par_plr_click')))),
                                fluidRow(column(12, 
                                         wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 450px;",
                                                   p(tags$b("Tournament Years:", style = "font-size: 102%")),
                                                   fluidRow(column(5, align = 'center',
                                                                   selectInput(inputId = 'par_years_start',
                                                                               label = NULL,
                                                                               choices = seq(1934, latest_trn, 1), 
                                                                               selected = 1934)),
                                                            column(2, style = 'margin-top: 7px', align = 'center', p("to")),
                                                            column(5, align = 'center',
                                                                   selectInput(inputId = 'par_years_end',
                                                                               label = NULL,
                                                                               choices = seq(1934, latest_trn, 1), 
                                                                               selected = latest_trn))),
                                                   p(tags$b("Groupings to plot:", style = "font-size: 102%")),
                                                   fluidRow(style = "margin-top: -5px;",
                                                           column(5, offset = 1, align = 'left',
                                                                   checkboxGroupInput(inputId = "par_groups_L",
                                                                                      label = NULL,
                                                                                      choices = list("Winner" = 'Winner',
                                                                                                     "Others" = 'Others'),
                                                                                      inline = F,
                                                                                      selected = c("Winner", "Others"))),
                                                            column(6, align = 'left',
                                                                   checkboxGroupInput(inputId = "par_groups_R",
                                                                                      label = NULL,
                                                                                      choices = list("Top 10" = 'Top 10',
                                                                                                     "Missed Cut" = 'Missed Cut'),
                                                                                      inline = F,
                                                                                      selected = c("Top 10", "Missed Cut")))),
                                                   checkboxInput("par_cutline", label = "Show the 36-hole cut line?", value = F),
                                                   selectizeInput(inputId = "par_players", label = "Select (up to three) players to highlight:",
                                                                  choices = sort(unique(masters$Player_FullName)),
                                                                  multiple = T,
                                                                  options = list(placeholder = "All",
                                                                                 maxOptions = 1500,
                                                                                 maxItems = 3,
                                                                                 onInitialize = I('function() { this.setValue("Select player(s)"); }'))),
                                                   br(),
                                                   hr(),
                                                   fluidRow(column(12, align = "right",
                                                                   actionButton(inputId = 'par_reset', label = 'Reset', icon = icon("refresh"), class = 'btn btn-primary btn-sm'))))))))),
        # Leaderboards and Historical Tables -------------------------------------------------------
        tabPanel("Leaderboards",
                 fluidRow(
                         column(5,
                                wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 775px;",
                                          fluidRow(style = "margin-top: 25px;",
                                                  column(12, 
                                                         p(tags$b('Yearly Tournament Leaderboard', style = "font-size: 150%; font-family:Helvetica; color:#4c4c4c; text-align:left;")))),
                                          hr(),
                                          fluidRow(column(4,
                                                          selectInput(inputId = 'lb_yearly_year',
                                                                      label = NULL,
                                                                      choices = sort(unique(masters$Year), decreasing = T), 
                                                                      selected = latest_trn))),
                                          DTOutput("lb_yearly_table"))),
                         column(7,
                                wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 775px;",
                                          fluidRow(style = "margin-top: 25px; margin-bottom: -10px;",
                                                   column(5, 
                                                          p(tags$b('Historical Records', style = "font-size: 150%; font-family:Helvetica; color:#4c4c4c; text-align:left;"))),
                                                   column(2, style = 'margin-top: 7px;', align = 'right', p("From:")),
                                                   column(2, align = 'left',
                                                          selectInput(inputId = 'lb_tbl_yr_start',
                                                                      label = NULL,
                                                                      choices = seq(1934, latest_trn, 1), 
                                                                      selected = 1934)),
                                                   column(1, style = 'margin-top: 7px;', align = 'center', p("to")),
                                                   column(2, align = 'left',
                                                          selectInput(inputId = 'lb_tbl_yr_end',
                                                                      label = NULL,
                                                                      choices = seq(1934, latest_trn, 1), 
                                                                      selected = latest_trn))),
                                          hr(),
                                          fluidRow(column(5,
                                                          selectizeInput(inputId = "lb_tbl_choice", 
                                                                         label = NULL,
                                                                         choices = c("Tournaments Entered", "Cuts Made", "Wins", "Runners-up",
                                                                                     "Top 5s", "Top 10s", "Top 25s", "Sub-par Rounds", 
                                                                                     "Rounds in the 60s", "Lowest Rounds"), 
                                                                         selected = "Tournaments Entered",
                                                                         width = '85%')),
                                                   conditionalPanel(condition = "input.lb_tbl_choice == 'Tournaments Entered' || input.lb_tbl_choice == 'Cuts Made' || input.lb_tbl_choice == 'Top 5s' || input.lb_tbl_choice == 'Top 10s' || input.lb_tbl_choice == 'Top 25s'",
                                                                    column(4, style = 'margin-top: 7px;', align = 'right', p("Minimum tournaments entered:")),
                                                                    column(2, align = 'center',
                                                                           tags$style(HTML('#min_trns{height: 35px}')),
                                                                           numericInput(inputId = "min_trns", 
                                                                                        label = NULL, 
                                                                                        value = 1,
                                                                                        min = 1,
                                                                                        max = 50,
                                                                                        width = '90%'))),
                                                   conditionalPanel(condition = "input.lb_tbl_choice == 'Sub-par Rounds' || input.lb_tbl_choice == 'Rounds in the 60s' || input.lb_tbl_choice == 'Lowest Rounds'",
                                                                    column(4, style = 'margin-top: 7px;', align = 'right', p("Minimum rounds played:")),
                                                                    column(2, align = 'center',
                                                                           tags$style(HTML('#min_rds{height: 30px}')),
                                                                           numericInput(inputId = "min_rds", 
                                                                                        label = NULL, 
                                                                                        value = 1,
                                                                                        min = 1,
                                                                                        max = 150,
                                                                                        width = '90%'))),
                                                   uiOutput("tbl_reset_button")),
                                          br(),
                                          DTOutput("lb_historical_table"))))),
        # Scoring Averages --------------------------------------------------------
        tabPanel("Scoring Averages",
                 fluidRow(
                         column(5,
                                fluidRow(column(12,
                                                # Error Message Appearance
                                                tags$head(tags$style(HTML(".shiny-output-error-validation { font-style: italic; font-size: 125%; }"))),
                                                wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 215px;",
                                                          fluidRow(style = 'margin-top: 18px',
                                                                  column(4, style = 'margin-top: 7px', align = 'right', p("For years:")),
                                                                  column(3, align = 'center',
                                                                          selectInput(inputId = 'scr_years_start',
                                                                                      label = NULL,
                                                                                      choices = seq(1934, latest_trn, 1), 
                                                                                      selected = 1934)),
                                                                   column(1, style = 'margin-top: 7px', align = 'center', p("to")),
                                                                   column(3, align = 'center',
                                                                          selectInput(inputId = 'scr_years_end',
                                                                                      label = NULL,
                                                                                      choices = seq(1934, latest_trn, 1), 
                                                                                      selected = latest_trn))),
                                                          fluidRow(
                                                                   column(4, style = 'margin-top: 15px', align = 'right', p("Min. rounds played:")),
                                                                   column(7, align = 'left',
                                                                          tags$style(HTML('#scr_min_rds{height: 30px}')),
                                                                          tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
                                                                          sliderInput(inputId = "scr_min_rds", 
                                                                                      label = NULL, 
                                                                                      min = 4, 
                                                                                      max = 100, 
                                                                                      value = 20))),
                                                          fluidRow(
                                                                   column(4, align = 'right', p("Plot lowest:")),
                                                                   column(6, align = 'left',
                                                                          radioButtons(inputId = "scr_num_plyrs", 
                                                                                       label = NULL,
                                                                                       choices = list('25' = 25, '20' = 20, '15' = 15, '10' = 10, '5' = 5), 
                                                                                       selected = 10,
                                                                                       inline = T)),
                                                                   column(2, align = 'right',
                                                                          tags$style(HTML('#tbl_reset{height: 30px}')),
                                                                          actionButton(inputId = 'scr_reset', label = NULL, icon = icon("refresh"), class = 'btn btn-primary btn-sm')))))),
                                fluidRow(column(12, 
                                                wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 485px;",
                                                          DTOutput("lb_scoring"))))),
                         column(7,
                                wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 720px;",
                                          plotOutput("scr_avg", height = 680))))),
        # Player Pages --------------------------------------------------------
        tabPanel("Player Pages",
                 fluidRow(
                         column(6,
                                fluidRow(column(12,
                                                wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 220px;",
                                                          fluidRow(style = 'margin-top: 18px',
                                                                   column(4, style = 'margin-top: 7px', align = 'right', p("Select a player:")),
                                                                   column(8, align = "left",
                                                                          selectizeInput(inputId = "plyr_pg_player", label = NULL,
                                                                                         choices = sort(unique(masters$Player_FullName)),
                                                                                         selected = sample(c("Arnold Palmer", "Jack Nicklaus", "Tiger Woods", "Jordan Spieth", "Padraig Harrington",
                                                                                                             "Payne Stewart", "Greg Norman", "Fred Couples", "Rory McIlroy", "Rickie Fowler"), 1),
                                                                                         multiple = F,
                                                                                         options = list(maxOptions = 1500)))),
                                                          fluidRow(column(12, 
                                                                          p(tags$b('Career at The Masters:', style = "font-size: 105%; font-family:Helvetica;"), style = 'text-align:left; margin-bottom: 5px;'))),
                                                          fluidRow(column(5, offset = 1, align = 'center',
                                                                          htmlOutput('plyr_career_L')),
                                                                   column(6, align = 'center',
                                                                          htmlOutput('plyr_career_R')))))),
                                fluidRow(column(12, 
                                                wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 480px;",
                                                          DTOutput("plyr_pg_tournaments"))))),
                         column(6,
                                # Error Message Appearance
                                tags$head(tags$style(HTML(".shiny-output-error-validation { font-style: italic; font-size: 125%; }"))),
                                wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 720px;",
                                fluidRow(column(12,
                                                plotOutput("plyr_fig", height = 695))))))),
        # About Tab ------------------------------------------------
        tabPanel("About", icon = icon("bars"),
                 fluidRow(
                         column(12,
                                wellPanel(style = "background-color: #fff; border-color: #2c3e50;",
                                          includeHTML("about_masters.html"))))),
        # Footer -------------------------------
        hr(style = "border-color: #cbcbcb;"),
        fluidRow(
                column(9,
                       p('All of the data used to generate this app were obtained from ', tags$a(href = "http://www.masters.com/en_US/tournament/index.html", 'Masters.com', target = '_blank'), '.', style = "font-size: 85%"),
                       p("App created by ", tags$a(href = "https://www.cteeter.ca", 'Chris Teeter', target = '_blank'), " in January 2019", HTML("&bull;"),
                         "Find the code on Github:", tags$a(href = "https://github.com/cjteeter/ShinyTeeter/tree/master/3_MastersGolf", tags$i(class = 'fa fa-github', style = 'color:#5000a5'), target = '_blank'), style = "font-size: 85%"),
                       p("Have a question? Spot an error? Send an email ", tags$a(href = "mailto:christopher.teeter@gmail.com", tags$i(class = 'fa fa-envelope', style = 'color:#990000'), target = '_blank'), style = "font-size: 85%"),
                       p(tags$em("Last updated: April 2019"), style = 'font-size:75%')),
                column(3, align = "right",
                       conditionalPanel(
                               condition = "input.masters_golf == 'Scoring Averages' | input.masters_golf == 'Player Pages'",
                               p(tags$em('Check the box below to change the colour scheme of the distributions to one that is more easily read by those with color blindness.', style = "font-size: 70%; font-family:Helvetica")),
                               checkboxInput("col_blind", label = "Colourblind?", value = F)))),
        windowTitle = "The Masters Data Viz"
)

# Server logic -----------------------------------
        
server <- function(input, output, session) {
        
        # Dynamically render the reset button on the Leaderboards page
        output$tbl_reset_button <- renderUI({
                
                if(input$lb_tbl_choice %in% c('Wins', 'Runners-up', 'Lowest Rounds')) {
                        column(7,
                               align = 'right',
                               tags$style(HTML('#tbl_reset{height: 30px}')),
                               actionButton(inputId = 'tbl_reset', label = NULL, icon = icon("refresh"), class = 'btn btn-primary btn-sm'))
                } else { column(1,
                               align = 'right',
                               tags$style(HTML('#tbl_reset{height: 30px}')),
                               actionButton(inputId = 'tbl_reset', label = NULL, icon = icon("refresh"), class = 'btn btn-primary btn-sm')) }
                })
        
        ### Tournaments Tab -----------------------------------
        # Update player list based on filters
        observe({ 
                par_players <- masters %>% filter(between(Year, as.numeric(input$par_years_start), as.numeric(input$par_years_end))) %>% select(Player_FullName) %>% distinct() %>% arrange(Player_FullName) %>% pull()
                
                updateSelectizeInput(session, inputId = "par_players", choices =  par_players)
        })
        
        # If Reset button pressed, reset all filters to initial settings
        observeEvent(input$par_reset, {
                        updateSelectInput(session, "par_years_start", selected = 1934)
                        updateSelectInput(session, "par_years_end", selected = latest_trn)
                        updateCheckboxGroupInput(session, "par_groups_L", selected = c("Winner", "Others"))
                        updateCheckboxGroupInput(session, "par_groups_R", selected = c("Top 10", "Missed Cut"))
                        updateCheckboxInput(session, "par_cutline", value = F)
                        updateSelectizeInput(session, "par_players", selected = "All") })
        
        # Figure DL Size Parameters
        par_w_DL <- 14
        par_h_DL <- 10
        
        # Error Messages
        par_err_msg1 <- "\n You have filtered out all of the available data. \n \n To restore the figure, please adjust your selections."
        par_err_msg2 <- ""
        
        # Update the year filters
        observeEvent(input$par_years_start, { updateSelectInput(session, "par_years_end", label = NULL, 
                                                                choices = seq(ifelse(input$par_years_start == latest_trn, latest_trn, as.numeric(input$par_years_start)), latest_trn, 1),
                                                                selected = input$par_years_end) })
        observeEvent(input$par_years_end, { updateSelectInput(session, "par_years_start", label = NULL, 
                                                                choices = seq(1934, ifelse(input$par_years_end == 1934, 1934, as.numeric(input$par_years_end)), 1),
                                                                selected = input$par_years_start) })
        
        # Add jitter column to primary data frame
        jit_amnt <- reactive({ round((-0.0027*(as.numeric(input$par_years_end) - as.numeric(input$par_years_start)))+0.28, 2) })
        masters_update <- reactive({ masters %>% 
                                        arrange(desc(Year), Finish, Last_Name) %>%
                                        filter(!is.na(Finish_Par)) %>%
                                        group_by(Year, Finish_Par) %>%
                                        mutate(Year_jit = pmap_dbl(list(Year, Finish_Group_6, n()), 
                                                                   ~ ifelse(..2 == 'Winner', ..1, 
                                                                            round(..1 + (runif(..3, min = -jit_amnt(), max = jit_amnt()) * (..3 > 1)), 6)))) %>%
                                        ungroup() })
        
        # Plot the Tournaments Figure
        output$par_yr <- renderPlot({ 
                
                validate(need(nrow(masters_update() %>% filter(Finish_Group_6 %in% c(input$par_groups_L, input$par_groups_R))) != 0, par_err_msg1))
                fig1_par(masters_update(), 
                         cutline, 
                         years = c(as.numeric(input$par_years_start), as.numeric(input$par_years_end)), 
                         cut_line = input$par_cutline, 
                         finish_groups = c(input$par_groups_L, input$par_groups_R), 
                         player_highlight = input$par_players) })
        
        # Player information for click
        output$par_plr_click <- renderPrint({
                
                validate(need(nrow(masters_update() %>% filter(Finish_Group_6 %in% c(input$par_groups_L, input$par_groups_R))) != 0, par_err_msg2))
                plr_info_str <- function(click_df) {
                        
                        details <- if(nrow(click_df) == 0) {
                                        p(tags$b("Year: ", style = "font-size: 108%; font-family:Helvetica; color:#000000"), br(),
                                          tags$b("Player: ", style = "font-size: 108%; font-family:Helvetica; color:#000000"), br(),
                                          tags$b("Total Par: ", style = "font-size: 108%; font-family:Helvetica; color:#000000"), br(),
                                          tags$b("Finish Position: ", style = "font-size: 108%; font-family:Helvetica; color:#000000"), br(),
                                          tags$b("Grouping: ", style = "font-size: 108%; font-family:Helvetica; color:#000000"), br(), br(),
                                          tags$b("Cut: ", style = "font-size: 85%; font-family:Helvetica; color:#000000")) }
                                        else if(nrow(click_df) >= 1) {
                                                p(tags$b("Year: ", style = "font-size: 108%; font-family:Helvetica; color:#000000"), tags$em(click_df$Year), br(),
                                                  tags$b("Player: ", style = "font-size: 108%; font-family:Helvetica; color:#000000"), tags$em(click_df$Player_FullName), br(),
                                                  tags$b("Total Par: ", style = "font-size: 108%; font-family:Helvetica; color:#000000"), { if(click_df$Finish_Par > 0) { tags$em(paste0("+", click_df$Finish_Par)) } else if(click_df$Finish_Par == 0) { tags$em("E") } else { tags$em(click_df$Finish_Par) }}, br(),
                                                  tags$b("Finish Position: ", style = "font-size: 108%; font-family:Helvetica; color:#000000"), { if(click_df$Finish == 999) { tags$em("MC") } else { tags$em(click_df$Finish) }}, br(),
                                                  tags$b("Grouping: ", style = "font-size: 115%; font-family:Helvetica; color:#000000"), tags$em(click_df$Finish_Group_6), br(), br(),
                                                  tags$b("Cut: ", style = "font-size: 85%; font-family:Helvetica; color:#000000"), { if(click_df$Year < 1957) { tags$em("No cut") } else { tags$em("+", cutline$Cut[cutline$Year == click_df$Year]) }}) }
                                        else { NA }
                                                
                        return(details)
                }
                
                plr_info_str(nearPoints(masters_update() %>% 
                                                filter(between(Year, as.numeric(input$par_years_start), as.numeric(input$par_years_end)),
                                                       Finish_Group_6 %in% c(input$par_groups_L, input$par_groups_R)),
                                        coordinfo = input$plot_click,
                                        xvar = 'Year_jit',
                                        yvar = 'Finish_Par',
                                        threshold = 3,
                                        maxpoints = 3,
                                        addDist = T) %>%
                                     arrange(dist_) %>%
                                     slice(1) %>%
                                     select(Year, Player_FullName, Finish_Par, Finish, Finish_Group_6))
                })

        ### Leaderboards Tab -----------------------------------
        # If Reset button pressed, reset all filters to initial settings
        observeEvent(input$tbl_reset, {
                updateSelectInput(session, "lb_tbl_yr_start", selected = 1934)
                updateSelectInput(session, "lb_tbl_yr_end", selected = latest_trn)
                updateNumericInput(session, "min_trns", value = 1)
                updateNumericInput(session, "min_rds", value = 1) })
        
        # Update the year filters
        observeEvent(input$lb_tbl_yr_start, { updateSelectInput(session, "lb_tbl_yr_end", label = NULL, 
                                                                choices = seq(ifelse(input$lb_tbl_yr_start == latest_trn, latest_trn, as.numeric(input$lb_tbl_yr_start)), latest_trn, 1),
                                                                selected = input$lb_tbl_yr_end) })
        observeEvent(input$lb_tbl_yr_end, { updateSelectInput(session, "lb_tbl_yr_start", label = NULL, 
                                                                choices = seq(1934, ifelse(input$lb_tbl_yr_end == 1934, 1934, as.numeric(input$lb_tbl_yr_end)), 1),
                                                                selected = input$lb_tbl_yr_start) })
        
        # Present the Yearly Tournament Leaderboard Table
        tbl_lb_yearly <- reactive({ yearly_leaderboard(masters,
                                                       year = as.numeric(input$lb_yearly_year)) })
        
        output$lb_yearly_table <- renderDT({ tbl_lb_yearly() }, 
                                               options = list(info = F,
                                                              paging = F,
                                                              searching = T,
                                                              stripeClasses = F, 
                                                              lengthChange = F,
                                                              scrollY = '505px',
                                                              scrollCollapse = T),
                                               rownames = F)
        
        # Present the Selected Historical Leaderboard Table
        tbl_lb_historical <- reactive({ historical_tables(masters,
                                                          low_rounds,
                                                          years = c(as.numeric(input$lb_tbl_yr_start), as.numeric(input$lb_tbl_yr_end)), 
                                                          table = input$lb_tbl_choice, 
                                                          min_trns = input$min_trns,
                                                          min_rds = input$min_rds) })
        
        output$lb_historical_table <- renderDT({ datatable(tbl_lb_historical(), 
                                                        options = list(info = F,
                                                                       paging = F,
                                                                       searching = T,
                                                                       stripeClasses = F, 
                                                                       lengthChange = F,
                                                                       orderMulti = T,
                                                                       scrollY = '445px',
                                                                       scrollCollapse = T),
                                                        rownames = F) %>%
                                                { if(input$lb_tbl_choice %in% c('Cuts Made', 'Top 5s', 'Top 10s', 'Top 25s', 'Sub-par Rounds', 'Rounds in the 60s')) { 
                                                        formatRound(table = ., columns = 4, digits = 2) } else { formatRound(table = ., columns = 2, digits = 0) } }})
                                                        
        
        ### Scoring Averages Tab -----------------------------------
        # If Reset button pressed, reset all filters to initial settings
        observeEvent(input$scr_reset, {
                updateSelectInput(session, "scr_years_start", selected = 1934)
                updateSelectInput(session, "scr_years_end", selected = latest_trn)
                updateSliderInput(session, "scr_min_rds", value = 20)
                updateRadioButtons(session, "scr_num_plyrs", selected = 10) })
        
        # Figure DL Size Parameters
        scr_w_DL <- 14
        scr_h_DL <- 10
        
        # Error Messages
        scr_err_msg1 <- "\n You have filtered out all of the available data. \n \n To restore the figure, please adjust your selections."
        
        # Update the year filters
        observeEvent(input$scr_years_start, { updateSelectInput(session, "scr_years_end", label = NULL, 
                                                                choices = seq(ifelse(input$scr_years_start == latest_trn, latest_trn, as.numeric(input$scr_years_start)), latest_trn, 1),
                                                                selected = input$scr_years_end) })
        observeEvent(input$scr_years_end, { updateSelectInput(session, "scr_years_start", label = NULL, 
                                                              choices = seq(1934, ifelse(input$scr_years_end == 1934, 1934, as.numeric(input$scr_years_end)), 1),
                                                              selected = input$scr_years_start) })
        
        # Plot the Scoring Averages Ridges Figure
        output$scr_avg <- renderPlot({ 
                
                #validate(need(nrow(masters_update() %>% filter(Finish_Group_6 %in% c(input$par_groups_L, input$par_groups_R))) != 0, scr_err_msg1))
                
                fig2_scrdist(masters, 
                             years = c(as.numeric(input$scr_years_start), as.numeric(input$scr_years_end)),
                             career_rounds = as.numeric(input$scr_min_rds),
                             num_players = as.numeric(input$scr_num_plyrs),
                             col_blind = input$col_blind) })
        
        # Present the Scoring Averages Leaderboard Table
        tbl_lb_scoring <- reactive({ scoring_leaderboard(masters,
                                                         years = c(as.numeric(input$scr_years_start), as.numeric(input$scr_years_end)), 
                                                         career_rounds = input$scr_min_rds) })
        
        output$lb_scoring <- renderDT({ datatable(tbl_lb_scoring(), 
                                                  options = list(info = F,
                                                              paging = F,
                                                              searching = T,
                                                              stripeClasses = F, 
                                                              lengthChange = F,
                                                              scrollY = '360px',
                                                              scrollCollapse = T),
                                                  rownames = F) %>%
                                                formatRound(columns = c(3,4), digits = c(1,2)) })
        
        ### Player Pages Tab -----------------------------------
        
        # Error Messages
        plyr_err_msg1 <- "Waiting for player..."
        plyr_err_msg2 <- ""
        
        # Present the Player's Tournaments Table
        plyr_trns <- reactive({ player_tournaments(masters,
                                                   player = input$plyr_pg_player) })
        
        output$plyr_pg_tournaments <- renderDT({ datatable(plyr_trns(), 
                                                  options = list(info = F,
                                                                 paging = F,
                                                                 searching = F,
                                                                 stripeClasses = F, 
                                                                 lengthChange = F,
                                                                 scrollY = '360px',
                                                                 scrollCollapse = T),
                                                  rownames = F) })
        # Player career information
        output$plyr_career_L <- renderPrint({ 
                
                validate(need(input$plyr_pg_player, plyr_err_msg2))
                plyr_career_str_L <- function(score_data, rounds_data) {
                        
                        details <- if(nrow(score_data) == 0) {
                                p(tags$b("Tournaments: ", style = "font-size: 101%; font-family:Helvetica; color:#000000"), br(),
                                  tags$b("Cuts: ", style = "font-size: 101%; font-family:Helvetica; color:#000000"), br(),
                                  tags$b("Rounds: ", style = "font-size: 101%; font-family:Helvetica; color:#000000"), br(),
                                  tags$b("Low Round: ", style = "font-size: 101%; font-family:Helvetica; color:#000000"), br()) }
                        else if(nrow(score_data) >= 1) {
                                p(tags$b("Tournaments: ", style = "font-size: 101%; font-family:Helvetica; color:#000000"), tags$em(nrow(score_data)), br(),
                                  tags$b("Cuts: ", style = "font-size: 101%; font-family:Helvetica; color:#000000"), tags$em(nrow(score_data %>% filter(!Finish_Group_6 %in% c('Missed Cut', 'Withdrew', 'Disqualified')))), br(),
                                  tags$b("Rounds: ", style = "font-size: 101%; font-family:Helvetica; color:#000000"), tags$em(nrow(rounds_data)), br(),
                                  tags$b("Low Round: ", style = "font-size: 101%; font-family:Helvetica; color:#000000"), tags$em(min(rounds_data$Score)), br()) }
                        else { NA }
                        
                        return(details)
                }
                
                plyr_career_str_L(score_data = masters %>% filter(Player_FullName == input$plyr_pg_player), 
                                  rounds_data = low_rounds %>% filter(Player_FullName == input$plyr_pg_player))
                })
        
        output$plyr_career_R <- renderPrint({ 
                
                validate(need(input$plyr_pg_player, plyr_err_msg2))
                plyr_career_str_R <- function(score_data, rounds_data) {
                        
                        details <- if(nrow(score_data) == 0) {
                                p(tags$b("Wins: ", style = "font-size: 101%; font-family:Helvetica; color:#000000"), br(),
                                  tags$b("Runner-ups: ", style = "font-size: 101%; font-family:Helvetica; color:#000000"), br(),
                                  tags$b("Top 5s: ", style = "font-size: 101%; font-family:Helvetica; color:#000000"), br(),
                                  tags$b("Top 10s: ", style = "font-size: 101%; font-family:Helvetica; color:#000000"), br()) }
                        else if(nrow(score_data) >= 1) {
                                p(tags$b("Wins: ", style = "font-size: 101%; font-family:Helvetica; color:#000000"), tags$em(nrow(score_data %>% filter(Finish_Group_6 == 'Winner'))), br(),
                                  tags$b("Runner-ups: ", style = "font-size: 101%; font-family:Helvetica; color:#000000"), tags$em(nrow(score_data %>% filter(Finish == 2))), br(),
                                  tags$b("Top 5s: ", style = "font-size: 101%; font-family:Helvetica; color:#000000"), tags$em(nrow(score_data %>% filter(Finish <= 5))), br(),
                                  tags$b("Top 10s: ", style = "font-size: 101%; font-family:Helvetica; color:#000000"), tags$em(nrow(score_data %>% filter(Finish <= 10))), br()) }
                        else { NA }
                        
                        return(details)
                }
                
                plyr_career_str_R(score_data = masters %>% filter(Player_FullName == input$plyr_pg_player), 
                                  rounds_data = low_rounds %>% filter(Player_FullName == input$plyr_pg_player))
                
                })
        
        # Plot the Combined Figure
        output$plyr_fig <- renderPlot({
                
                validate(need(input$plyr_pg_player, plyr_err_msg1))
                fig3and4_plyr(masters, 
                              player = input$plyr_pg_player,
                              col_blind = input$col_blind) })
}

# Run app -------------------------------
shinyApp(ui = ui, server = server)