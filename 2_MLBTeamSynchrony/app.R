##### Shiny App for Viewing the Synchrony of MLB Teams 1998-Present #####

# Load required packages -------------------------------
require(shiny)
require(shinythemes)
require(DT)
require(tidyverse)
require(curl)
require(zoo)

# Load Helper files -------------------------------
source("figures_gen.R")
source("datatables_gen.R")

# Load data -------------------------------
teams_df <- read.csv("data/MLB_teamCodes.csv", stringsAsFactors = F)
master_data <- read.csv(curl(sprintf("https://docs.google.com/uc?id=%s&export=download", "1z0kyo87FzMeW0-B6urITWHAgEWhelbBZ")), stringsAsFactors = F, na.strings = "") %>%
                left_join(teams_df, by = c('Tm' = 'Team.Code')) %>%
                select(Year, Full.Name, everything())

# Set some variable values -------------------------------
curSeason <- 2019
minGames <- min(master_data %>% filter(Year == curSeason) %>% group_by(Tm) %>% summarise(Games = max(as.numeric(Gm_num))) %>% pull(Games), na.rm = T)
sl_max <- ifelse(minGames <= 50, minGames - 1, 50)

# Create user interface -------------------------------
ui <- function(request) {
        fluidPage(theme = shinytheme('spacelab'),
                  id = 'synchrony',
                  # App Title
                  titlePanel(tags$b("MLB Team Synchrony", style = "font-size: 110%, font-family:Helvetica; color:#010151"), windowTitle = "MLB Team Synchrony"),
                  hr(),
                  # App Description
                  p("Teams' run scoring and run prevention performance varies throughout the season. Teams that are", tags$em('in-sync'), "have both components performing well (or poorly) at the same time, while teams that are", 
                    tags$em('out-of-sync'), "have one component performing well and the other struggling. This app helps demonstrate the variability and synchrony (or lack thereof) of teams' run scoring and prevention over a season.", 
                    style = "font-size: 90%"), 
                  p("Given a season, a team, and a number of games to be averaged over, the app will", tags$em('Generate'), "four things: (1) a plot of the team's average runs scored and allowed that season, (2) a complementary 
                    plot of the average run differential, (3) the data used to create the plots, and (4) a standings-like table for that season.
                    Data are available for the ", min(master_data$Year), " to ", curSeason, "seasons. The", tags$em('Reset'), "button will reload the page.", style = "font-size: 90%"),
                  p(tags$em('Note: during the season the data are updated on a nightly basis starting on April 20th.'), style = "font-size: 80%"), 
                  hr(),
        # User Input Section -------------------------------
        sidebarLayout(
                sidebarPanel(
                        selectizeInput(inputId = "season_choice", 
                                       label = tags$h4("Select a Season:"),
                                       choices = seq(curSeason, min(master_data$Year), -1),
                                       selected = NULL,
                                       multiple = T,
                                       options = list(placeholder = '----------', maxItems = 1)),
                        hr(),
                        selectizeInput(inputId = "team_choice", 
                                       label = tags$h4("Select a Team:"),
                                       choices = master_data %>% filter(Year == curSeason) %>% select(Year, Full.Name) %>% arrange(Full.Name) %>% distinct(Full.Name) %>% pull(Full.Name),
                                       selected = NULL,
                                       multiple = T,
                                       options = list(placeholder = '----------', maxItems = 1)),
                        hr(),
                        uiOutput("rolling_choice_slider"),
                        br(),
                        hr(),
                        br(),
                        fluidRow(column(9,
                                        actionButton(inputId = 'generate', label = 'Generate', icon = icon("bolt"), class = 'btn-primary'),
                                        actionButton(inputId = 'reset', label = 'Reset', icon = icon("refresh"), class = 'btn-warning')),
                                 column(3, align = 'right',
                                        bookmarkButton(label = 'Share', icon = icon('share-alt')))),
                        br()),
                # Output of Plot, Data, and Summary -------------------------------
                mainPanel(
                        tabsetPanel(id = 'main_tabs',
                                tabPanel("RS and RA Plot", icon = icon("area-chart"), 
                                         tags$br(), plotOutput("figure_rsra", height = 625),
                                         tags$style(type='text/css', "#fig_link_rsra {
                                                    font-family: 'Arial';
                                                    font-size: 65%;}"),
                                         conditionalPanel(condition = "output.figure_rsra",
                                                                hr(), uiOutput(outputId = 'fig_link_rsra'), tags$br(),
                                                                downloadButton(outputId = 'dl_fig_rsra', 'Download plot as .png', class = 'btn-default btn-sm'))),
                                tabPanel("Run Diff Plot", icon = icon("bar-chart"), 
                                         tags$br(), plotOutput("figure_rdiff", height = 625),
                                         tags$style(type='text/css', "#fig_link_rdiff {
                                                    font-family: 'Arial';
                                                    font-size: 65%;}"),
                                         conditionalPanel(condition = "output.figure_rdiff",
                                                                hr(), uiOutput(outputId = 'fig_link_rdiff'), tags$br(),
                                                                downloadButton(outputId = 'dl_fig_rdiff', 'Download plot as .png', class = 'btn-default btn-sm'))),
                                tabPanel("Data Table", icon = icon("table"),
                                         tags$br(), 
                                         conditionalPanel(condition = "output.figure_table",
                                                                radioButtons(inputId = "table_widelong", label = 'Table Format:', choices = list("Wide" = 'wide', "Long" = 'long'),
                                                                inline = T, selected = 'wide')),
                                         dataTableOutput("figure_table"),
                                         tags$style(type='text/css', "#fig_tbl_link {
                                                    font-family: 'Arial';
                                                    font-size: 65%;}"),
                                         conditionalPanel(condition = "output.figure_table",
                                                                tags$br(), uiOutput(outputId = 'fig_tbl_link'), tags$br(),
                                                                downloadButton(outputId = 'dl_tbl1', 'Download table as .csv', class = 'btn-default btn-sm'))), 
                                tabPanel("Season Standings Table", icon = icon("table"),
                                         tags$br(),
                                         conditionalPanel(condition = "output.standings_table",
                                                          fluidRow(
                                                                column(4,
                                                                        selectizeInput(inputId = "league", label = "League:",
                                                                                       choices = c("Both", "AL", "NL"), selected = "Both")),
                                                                column(4,
                                                                        selectizeInput(inputId = "division", label = "Division:",
                                                                                       choices = c("All", "East", "Central", "West"), selected = "All")))),
                                         dataTableOutput("standings_table"),
                                         tags$style(type='text/css', "#stnd_link {
                                                    font-family: 'Arial';
                                                    font-size: 65%;}"),
                                         conditionalPanel(condition = "output.standings_table", tags$br(),
                                                          p(tags$small(tags$em('Pythagorean expectation numbers use 1.83 as the exponent'))),
                                                          p(tags$small('CV is the', tags$a(href = 'https://en.wikipedia.org/wiki/Coefficient_of_variation', 'Coefficient of Variation', target = '_blank'))),
                                                          tags$br(), uiOutput(outputId = 'stnd_link'), tags$br(),
                                                          downloadButton(outputId = 'dl_tbl2', 'Download table as .csv', class = 'btn-default btn-sm')))
                        )
                )
        ),
        # Footer -------------------------------
        tags$br(),
        hr(),
        p('All of the data used to generate the figures and tables were obtained from the incredible resource that is ', tags$a(href = "https://www.baseball-reference.com/", 'Baseball-Reference.com', target = '_blank'), '.', style = "font-size: 85%"),
        p("App created by ", tags$a(href = "https://www.cteeter.ca", 'Chris Teeter', target = '_blank'), " in November 2017", HTML("&bull;"), "Follow Chris on Twitter:", tags$a(href = "https://twitter.com/c_mcgeets", tags$i(class = 'fa fa-twitter'), target = '_blank'),
          HTML("&bull;"), "Find the code on Github:", tags$a(href = "https://github.com/cjteeter/ShinyTeeter/tree/master/2_MLBTeamSynchrony", tags$i(class = 'fa fa-github', style = 'color:#5000a5'), target = '_blank'), style = "font-size: 85%"),
        p("Have a question? Send an email ", tags$a(href = "mailto:christopher.teeter@gmail.com", tags$i(class = 'fa fa-envelope', style = 'color:#990000'), target = '_blank'), style = "font-size: 85%"),
        p(tags$em("Last updated: April 2019"), style = 'font-size:75%')
)
}

# Server logic -----------------------------------
server <- function(input, output, session) {
        
        values <- reactiveValues(season = NULL,
                                 team = NULL,
                                 games = NULL,
                                 launch = 0)
        
        observeEvent(input$season_choice, { values$season <- input$season_choice })
        observeEvent(input$team_choice, { values$team <- input$team_choice })
        observeEvent(input$rolling_choice, { values$games <- input$rolling_choice })
        
        # Bookmarking ----------------------------------

        onBookmark(function(state) {
                        state$values$curSeason <- curSeason
                        state$values$sl_max <- sl_max
                        state$values$season <- values$season
                        state$values$team <- values$team
                        state$values$games <- values$games
                        state$values$launch <- 0
        })
        
        onRestored(function(state) {
                        curSeason <- state$values$curSeason
                        sl_max <- state$values$sl_max
                        values$season <- state$values$season
                        values$team <- state$values$team
                        values$games <- state$values$games
                        updateSelectizeInput(session, inputId = "season_choice", selected = values$season)
                        updateSelectizeInput(session, inputId = "team_choice", choices = master_data %>% 
                                                                                                filter(Year == values$season) %>% 
                                                                                                select(Year, Full.Name) %>% arrange(Full.Name) %>% 
                                                                                                distinct(Full.Name) %>% 
                                                                                                pull(Full.Name), selected = values$team)
                        updateSliderInput(session, inputId = "rolling_choice", value = values$games)
                        values$launch <- state$values$launch + 1
                 })
        
        # Need to exclude the buttons from themselves being bookmarked
        setBookmarkExclude(c("bookmarker", "figure_table_cell_clicked", "figure_table_rows_all", "figure_table_rows_current", "figure_table_rows_selected",
                             "figure_table_search", "figure_table_state", "standings_table_cell_clicked", "standings_table_rows_all", "standings_table_rows_current", 
                             "standings_table_rows_selected", "standings_table_search", "standings_table_state",
                             "reset", "division-selectized", "league-selectized", "season_choice-selectized", "team_choice-selectized",
                             "rolling_choice", "season_choice", "team_choice", "generate"))
        
        # Dynamically render the slider
        output$rolling_choice_slider <- renderUI({
                if(!is.null(values$season) && values$season == 2019 && sl_max < 50) {
                        fluidRow(column(12,
                                        # Remove the minor ticks on the slider
                                        tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
                                        sliderInput(inputId = "rolling_choice", label = tags$h4("Select number of games for rolling averages of RS and RA:"),
                                                    min = 5, max = sl_max, value = 5, step = 1)))
                } else {
                        fluidRow(column(12,
                                        # Remove the minor ticks on the slider
                                        tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
                                        sliderInput(inputId = "rolling_choice", label = tags$h4("Select number of games for rolling averages of RS and RA:"),
                                                    min = 5, max = 50, value = 5, step = 1))) } })
        
        # Check for events and create outputs -------------------------------
        observeEvent(input$reset, { 
                        updateSelectizeInput(session, inputId = "season_choice", 
                                             choices = seq(curSeason, min(master_data$Year), -1),
                                             selected = '----------')
                        updateSelectizeInput(session, inputId = "team_choice", 
                                             choices = master_data %>% filter(Year == curSeason) %>% select(Year, Full.Name) %>% arrange(Full.Name) %>% distinct(Full.Name) %>% pull(Full.Name),
                                             selected = '----------')
                        updateSliderInput(session, inputId = "rolling_choice", value = 5)
                        values$season <- NULL
                        values$team <- NULL
                        values$games <- NULL
                        values$launch <- 0 })
        
        observeEvent(input$season_choice, { updateSelectizeInput(session, inputId = "team_choice", 
                                                                 choices = master_data %>% 
                                                                                filter(Year == values$season) %>% 
                                                                                select(Year, Full.Name) %>% arrange(Full.Name) %>% 
                                                                                distinct(Full.Name) %>% 
                                                                                pull(Full.Name), 
                                                                 selected = ifelse(!is.null(values$team), values$team, '----------')) }, ignoreInit = T)
        
        # Create variables for commonly used items -------------------------------
        tm_code <- eventReactive(input$generate + values$launch, { 
                                        master_data %>% 
                                                filter(Year == values$season, Full.Name == values$team) %>%
                                                distinct(Tm) %>%
                                                pull(Tm) })
        
        # First Tab - RS_RA Figure -----------------------------------
        eventPlot_rsra <- eventReactive(input$generate + values$launch, {
                                        rs_ra_roll_plot(master_data, values$season, values$team, values$games, curSeason) })
        
        output$figure_rsra <- renderPlot({ eventPlot_rsra() })
        
        link1 <- eventReactive(input$generate + values$launch, { as.character({ paste0('https://www.baseball-reference.com/teams/', tm_code(),
                                                                       '/', values$season, '-schedule-scores.shtml') }) })
        eventLink1 <- eventReactive(input$generate + values$launch, {
                                    as.character({ paste(values$season, values$team, 'schedule and results on Baseball-Reference.com') }) })
        
        output$fig_link_rsra <- renderUI({ tags$a(href = link1(), eventLink1(), target = '_blank') })
        
        eventPlot_rsra_dl <- function(){ rs_ra_roll_plot(master_data, values$season, values$team, values$games, curSeason) }
        
        output$dl_fig_rsra <- downloadHandler(filename = function() { paste0('MLBsyncrony_', values$season, '_', tm_code(), '_RS-RA_plot_dl-', Sys.Date(), '.png') },
                                              content = function(file) { ggsave(file, width = 9, height = 6, plot = eventPlot_rsra_dl(), device = "png") })
        
        
        # Second Tab - RDiff Figure -----------------------------------
        eventPlot_rdiff <- eventReactive(input$generate + values$launch, {
                                        rdiff_roll_plot(master_data, values$season, values$team, values$games, curSeason) })
        
        output$figure_rdiff <- renderPlot({ eventPlot_rdiff() })
        
        output$fig_link_rdiff <- renderUI({ tags$a(href = link1(), eventLink1(), target = '_blank') })
        
        eventPlot_rdiff_dl <- function(){ rdiff_roll_plot(master_data, values$season, values$team, values$games, curSeason) }
        
        output$dl_fig_rdiff <- downloadHandler(filename = function() { paste0('MLBsyncrony_', values$season, '_', tm_code(), '_RDiff_plot_dl-', Sys.Date(), '.png') },
                                               content = function(file) { ggsave(file, width = 9, height = 6, plot = eventPlot_rdiff_dl(), device = "png") })
        
        # Third Tab - Data Table for Figure -----------------------------------
        eventTable1 <- eventReactive(input$generate + values$launch, {
                                        figdata_tbl(master_data, values$season, values$team, values$games) })
        
        output$figure_table <- renderDT({ 
                                        datatable(
                                                { if (input$table_widelong == 'wide') { eventTable1() }
                                                        else if (input$table_widelong == 'long') { 
                                                                eventTable1_long <- gather(eventTable1(), Measure, Value, `RS/G`:R_Diff)
                                                                eventTable1_long }}, 
                                                options = list(info = F,
                                                               searching = F,
                                                               paging = F,
                                                               scrollY = '600px',
                                                               scrollCollapse = T,
                                                               columnDefs = list(list(orderable = F, targets = c(0,1)), list(class = 'dt-center', targets = '_all')),
                                                               order = list(list(2, 'asc'))), 
                                                rownames = F) %>%
                        formatRound(columns = c(5:7), digits = 2) })
        
        output$fig_tbl_link <- renderUI({ tags$a(href = link1(), eventLink1(), target = '_blank') })
        
        eventTable1_dl <- function(){ figdata_tbl(master_data, values$season, values$team, values$games) }
        
        output$dl_tbl1 <- downloadHandler(filename = function() { paste0('MLBsyncrony_', values$season, '_', tm_code(), '_DataTable-', {if (input$table_widelong == 'wide') { 'wide' } else { 'long' }}, '_dl-', Sys.Date(), '.csv') },
                                          content = function(file) {
                                                  write.csv(
                                                          { if (input$table_widelong == 'wide') { eventTable1_dl() }
                                                                  else if (input$table_widelong == 'long') { 
                                                                          eventTable1_long <- gather(eventTable1_dl(), Measure, Value, `RS/G`:R_Diff)
                                                                          eventTable1_long }},
                                                          file, row.names = F) })
        
        # Fourth Tab - Standings Table -----------------------------------
        eventTable2 <- eventReactive(input$generate + values$launch, { standings_tbl(master_data, values$season) })
        
        output$standings_table <- renderDT({ 
                                        datatable({ 
                                                eventTable2 <- eventTable2()
                                                if (input$league != "Both") { eventTable2 <- eventTable2 %>% filter(League == input$league) }
                                                if (input$division != "All") { eventTable2<- eventTable2 %>% filter(Division == input$division) }
                                                eventTable2 },
                                                extensions = 'FixedColumns',
                                                options = list(info = F, 
                                                               searching = F,
                                                               paging = F,
                                                               scrollX = T,
                                                               scrollY = '600px',
                                                               scrollCollapse = T,
                                                               columnDefs = list(list(orderable = F, targets = c(0,1)), list(class = 'dt-center', targets = '_all')),
                                                               order = list(list(7, 'desc')),
                                                               fixedColumns = list(leftColumns = 2)),
                                                rownames = F) %>%
                        formatRound(columns = c(8, 12, 14:19), digits = c(3, 3, rep(2, 6))) })
        
        
        eventTable2_dl <- function(){ standings_tbl(master_data, values$season) }
        
        link2 <- eventReactive(input$generate + values$launch, { as.character({ paste0('https://www.baseball-reference.com/leagues/MLB/', values$season, '-standings.shtml') }) })
        
        eventLink2 <- eventReactive(input$generate + values$launch, {
                                        as.character({ paste(values$season, 'Standings on Baseball-Reference.com') }) })
        
        output$stnd_link <- renderUI({ tags$a(href = link2(), eventLink2(), target = '_blank') })

        output$dl_tbl2 <- downloadHandler(filename = function() { paste0('MLBsyncrony_', values$season, '_Standings-plus_dl-', Sys.Date(), '.csv') },
                                          content = function(file) { write.csv(eventTable2_dl(), file, row.names = F) })
        
        # Reset Launch value
        observeEvent(input$generate, { values$launch <- 0 } )
}

# Run app -------------------------------
shinyApp(ui = ui, server = server, enableBookmarking = "url")