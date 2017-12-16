##### Shiny App for Viewing the Synchrony of MLB Teams 1998-2017 #####

# Load required packages -------------------------------
require(shiny)
require(shinythemes)
require(DT)
require(plyr)
require(dplyr)
require(tidyr)
require(ggplot2)
require(zoo)

# Load Helper files -------------------------------
source("figures_gen.R")
source("datatables_gen.R")
source("teams_updater.R")

# Load data -------------------------------
master_data <- read.csv("data/MLB_teamSchedulesResults_1998-2017.csv", stringsAsFactors = F)
teams_df <- read.csv("data/MLB_teamCodes-Lg-Div_1998-2017.csv", stringsAsFactors = F)

# Create user interface -------------------------------
ui <- fluidPage(theme = shinytheme('spacelab'),
        
        # App Title
        titlePanel(tags$b("MLB Team Synchrony", style = "font-size: 110%, font-family:Helvetica; color:#010151"), windowTitle = "MLB Team Synchrony"),
        hr(),
        # App Description
        p("Over the course of a season, teams' run scoring and run prevention performance varies. Team that are", tags$em('in-sync'), "have both components performing well (or poorly) at the same time, while ", 
          tags$em('out-of-sync'), "teams have one component performing well and one struggling. This app helps demonstrate the variability in- and synchrony (or lack thereof) between a teams' run scoring and prevention over a season.", style = "font-size: 90%"),
        p("Given a season, team, and the number of games to be averaged over, the app will", tags$em('Generate'), "four things: (1) a plot of the team's
          average runs scored and allowed that season, (2) a complementary plot of the average run differential, (3) the data used to create the plots, and (4) a standings-like table for that season.
          Data are from the 1998 to 2017 seasons. The", tags$em('Reset'), "button will reload the page.", style = "font-size: 90%"),
        hr(),
        # User Input Section -------------------------------
        sidebarLayout(
                sidebarPanel(
                        selectizeInput(inputId = "season_choice", label = tags$h4("Select Season:"),
                                    choices = seq(2017, 1998, -1),
                                    options = list(placeholder = 'Select a season', onInitialize = I('function() { this.setValue(""); }'))),
                        hr(),
                        selectizeInput(inputId = "team_choice", label = tags$h4("Select Team:"),
                                    choices = teams_df$Full.Name,
                                    options = list(placeholder = 'Select a team', onInitialize = I('function() { this.setValue(""); }'))),
                        hr(),
                        # Remove the minor ticks on the slider
                        tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
                        sliderInput(inputId = "rolling_choice", label = tags$h4("Select number of games for rolling average"),
                                     min = 5, max = 50, value = 0, step = 1),
                        hr(),
                        actionButton(inputId = 'generate', label = 'Generate', icon = icon("bolt"), class = 'btn-primary'),
                        actionButton(inputId = 'reset', label = 'Reset', icon = icon("refresh"), class = 'btn-warning')),
                # Output of Plot, Data, and Summary -------------------------------
                mainPanel(
                        tabsetPanel(
                                tabPanel("RS and RA Plot", icon = icon("area-chart"), 
                                         tags$br(), plotOutput("figure_rsra", height = 550),
                                         tags$style(type='text/css', "#fig_link_rsra {
                                                    font-family: 'Arial';
                                                    font-size: 65%;}"),
                                         conditionalPanel(condition = "input.generate",
                                                                hr(), uiOutput(outputId = 'fig_link_rsra'), tags$br(),
                                                                downloadButton(outputId = 'dl_fig_rsra', 'Download plot as .png', class = 'btn-default btn-sm'))),
                                tabPanel("Run Diff Plot", icon = icon("bar-chart"), 
                                         tags$br(), plotOutput("figure_rdiff", height = 550),
                                         tags$style(type='text/css', "#fig_link_rdiff {
                                                    font-family: 'Arial';
                                                    font-size: 65%;}"),
                                         conditionalPanel(condition = "input.generate",
                                                                hr(), uiOutput(outputId = 'fig_link_rdiff'), tags$br(),
                                                                downloadButton(outputId = 'dl_fig_rdiff', 'Download plot as .png', class = 'btn-default btn-sm'))),
                                tabPanel("Data Table", icon = icon("table"),
                                         tags$br(), 
                                         conditionalPanel(condition = "input.generate",
                                                                radioButtons(inputId = "table_widelong", label = 'Table Format:', choices = list("Wide" = 'wide', "Long" = 'long'),
                                                                inline = T, selected = 'wide')),
                                         dataTableOutput("figure_table"),
                                         tags$style(type='text/css', "#fig_tbl_link {
                                                    font-family: 'Arial';
                                                    font-size: 65%;}"),
                                         conditionalPanel(condition = "input.generate",
                                                                tags$br(), uiOutput(outputId = 'fig_tbl_link'), tags$br(),
                                                                downloadButton(outputId = 'dl_tbl1', 'Download table as .csv', class = 'btn-default btn-sm'))), 
                                tabPanel("Season Standings Table", icon = icon("table"),
                                         tags$br(),
                                         conditionalPanel(condition = "input.generate",
                                                          fluidRow(
                                                                column(4,
                                                                        selectizeInput(inputId = "league", label = "League:",
                                                                                       choices = c("Both", "AL", "NL"), selected = "Both")),
                                                                column(4,
                                                                        selectizeInput(inputId = "division", label = "Division:",
                                                                                       choices = c("All", "East", "Central", "West"), selected = "All"))
                                                        )),
                                         dataTableOutput("standings_table"),
                                         tags$style(type='text/css', "#stnd_link {
                                                    font-family: 'Arial';
                                                    font-size: 65%;}"),
                                         conditionalPanel(condition = "input.generate", tags$br(),
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
        p("App created by ", tags$a(href = "http://www.cteeter.ca", 'Chris Teeter', target = '_blank'), " in November 2017", HTML("&bull;"), "Follow Chris on Twitter:", tags$a(href = "https://twitter.com/c_mcgeets", tags$i(class = 'fa fa-twitter'), target = '_blank'),
          HTML("&bull;"), "Find the code on Github:", tags$a(href = "https://github.com/cjteeter/ShinyTeeter/tree/master/2_MLBTeamSynchrony", tags$i(class = 'fa fa-github', style = 'color:#5000a5'), target = '_blank'), style = "font-size: 85%"),
        p("Have a question? Send an email ", tags$a(href = "mailto:christopher.teeter@gmail.com", tags$i(class = 'fa fa-envelope', style = 'color:#990000'), target = '_blank'), style = "font-size: 85%"),
        p(tags$em("Last updated: November, 2017"), style = 'font-size:75%')
)

# Server logic -----------------------------------
server <- function(input, output, session) {
        
        # Check for events and create outputs -------------------------------
        observeEvent(input$reset, { session$reload() } )
        
        observeEvent(input$season_choice, { updateSelectizeInput(session, inputId = "team_choice", choices = update_teams(teams_df, isolate(input$season_choice))) })
        
        # First Tab - RS_RA Figure -----------------------------------
        eventPlot_rsra <- eventReactive(input$generate, isolate({
                                        rs_ra_roll_plot(master_data, input$season_choice, teams_df$Team.Code[teams_df$Full.Name == team_flip(input$team_choice)], input$rolling_choice, input$team_choice)
        }))
        
        output$figure_rsra <- renderPlot(eventPlot_rsra())
        
        link1 <- eventReactive(input$generate, as.character({ paste('https://www.baseball-reference.com/teams/',
                                                                    { if (isolate(input$team_choice) %in% c("Anaheim Angels", "Florida Marlins", "Montreal Expos", "Tampa Bay Devil Rays")) {
                                                                            teamcode_for_link(isolate(input$team_choice)) } else { teams_df$Team.Code[teams_df$Full.Name == team_flip(isolate(input$team_choice))] }},
                                                                    '/', isolate(input$season_choice), '-schedule-scores.shtml', sep = "") }))
        eventLink1 <- eventReactive(input$generate, 
                                    as.character({ paste(isolate(input$season_choice), isolate(input$team_choice), 'schedule and results on Baseball-Reference.com') }))
        output$fig_link_rsra <- renderUI({ tags$a(href = link1(), eventLink1(), target = '_blank') })
        
        eventPlot_rsra_dl <- function(){ isolate(rs_ra_roll_plot(master_data, input$season_choice, teams_df$Team.Code[teams_df$Full.Name == team_flip(input$team_choice)], input$rolling_choice, input$team_choice))}
        
        output$dl_fig_rsra <- downloadHandler(filename = function() { paste('MLBsyncrony_', isolate(input$season_choice), '_', teams_df$Team.Code[teams_df$Full.Name == team_flip(isolate(input$team_choice))], '_RS-RA_plot_dl-', Sys.Date(), '.png', sep="") },
                                         content = function(file) {
                                                 ggsave(file, width = 9, height = 6, plot = eventPlot_rsra_dl(), device = "png")
                                         })
        
        # Second Tab - RDiff Figure -----------------------------------
        eventPlot_rdiff <- eventReactive(input$generate, isolate({
                                        rdiff_roll_plot(master_data, input$season_choice, teams_df$Team.Code[teams_df$Full.Name == team_flip(input$team_choice)], input$rolling_choice, input$team_choice)
        }))
        
        output$figure_rdiff <- renderPlot(eventPlot_rdiff())
        
        output$fig_link_rdiff <- renderUI({ tags$a(href = link1(), eventLink1(), target = '_blank') })
        
        eventPlot_rdiff_dl <- function(){ isolate(rdiff_roll_plot(master_data, input$season_choice, teams_df$Team.Code[teams_df$Full.Name == team_flip(input$team_choice)], input$rolling_choice, input$team_choice))}
        
        output$dl_fig_rdiff <- downloadHandler(filename = function() { paste('MLBsyncrony_', isolate(input$season_choice), '_', teams_df$Team.Code[teams_df$Full.Name == team_flip(isolate(input$team_choice))], '_RDiff_plot_dl-', Sys.Date(), '.png', sep="") },
                                         content = function(file) {
                                                 ggsave(file, width = 9, height = 6, plot = eventPlot_rdiff_dl(), device = "png")
                                         })
        
        # Third Tab - Data Table for Figure -----------------------------------
        eventTable1 <- eventReactive(input$generate, isolate({
                figdata_tbl(master_data, input$season_choice, teams_df$Team.Code[teams_df$Full.Name == team_flip(input$team_choice)], input$rolling_choice)
        }))
        
        output$figure_table <- renderDataTable({if (input$table_widelong == 'wide') { eventTable1() }
                else if (input$table_widelong == 'long') { 
                        eventTable1_long <- gather(eventTable1(), Measure, Value, RS:R_Diff)
                        eventTable1_long }}, 
                options = list(info = F,
                               searching = F,
                               paging = F,
                               scrollY = '600px',
                               scrollCollapse = T,
                               #pageLength = 40,
                               #lengthMenu = list(c(40, 80, 120, -1), c('40', '80', '120', 'All')),
                               columnDefs = list(list(orderable = F, targets = c(0,1)), list(class = 'dt-center', targets = '_all')),
                               order = list(list(2, 'asc'))),
                rownames = F)
        
        output$fig_tbl_link <- renderUI({ tags$a(href = link1(), eventLink1(), target = '_blank') })
        
        eventTable1_dl <- function(){ isolate(figdata_tbl(master_data, input$season_choice, teams_df$Team.Code[teams_df$Full.Name == team_flip(input$team_choice)], input$rolling_choice))
        }
        
        output$dl_tbl1 <- downloadHandler(filename = function() { paste('MLBsyncrony_', isolate(input$season_choice), '_', teams_df$Team.Code[teams_df$Full.Name == team_flip(isolate(input$team_choice))], '_DataTable-', {if (input$table_widelong == 'wide') { 'wide' } else { 'long' }}, '_dl-', Sys.Date(), '.csv', sep="") },
                                                content = function(file) {
                                                 write.csv({if (input$table_widelong == 'wide') { eventTable1_dl() }
                                                                else if (input$table_widelong == 'long') { 
                                                                 eventTable1_long <- gather(eventTable1_dl(), Measure, Value, RS:R_Diff)
                                                                 eventTable1_long }},
                                                           file, row.names = F) })
        
        # Fourth Tab - Standings Table -----------------------------------
        eventTable2 <- eventReactive(input$generate, isolate({ standings_tbl(master_data, teams_df, input$season_choice)
        }))
        
        output$standings_table <- renderDataTable({ eventTable2 <- eventTable2()
                                                   if (input$league != "Both") { eventTable2 <- eventTable2 %>% filter(League == input$league) }
                                                   if (input$division != "All") { eventTable2<- eventTable2 %>% filter(Division == input$division) }
                                                  eventTable2 }, 
                                                  extensions = 'FixedColumns',
                                                  options = list(info = F, 
                                                       searching = F,
                                                       paging = F,
                                                       scrollX = '325px',
                                                       scrollY = '600px',
                                                       scrollCollapse = T,
                                                       columnDefs = list(list(orderable = F, targets = c(0,1)), list(class = 'dt-center', targets = '_all')),
                                                       order = list(list(7, 'desc')),
                                                       fixedColumns = list(leftColumns = 2)),
                                        rownames = F)
        
        
        eventTable2_dl <- function(){ isolate(standings_tbl(master_data, teams_df, input$season_choice)) }
        
        link2 <- eventReactive(input$generate, as.character({ paste('https://www.baseball-reference.com/leagues/MLB/', isolate(input$season_choice), '-standings.shtml', sep = "") }))
        eventLink2 <- eventReactive(input$generate, 
                            as.character({ paste(isolate(input$season_choice), 'Standings on Baseball-Reference.com') }))
        output$stnd_link <- renderUI({ tags$a(href = link2(), eventLink2(), target = '_blank') })

        output$dl_tbl2 <- downloadHandler(filename = function() { paste('MLBsyncrony_', isolate(input$season_choice), '_Standings-plus_dl-', Sys.Date(), '.csv', sep="") },
                                  content = function(file) {
                                          write.csv(eventTable2_dl(), file, row.names = F) })
        
}

# Run app -------------------------------
shinyApp(ui = ui, server = server)