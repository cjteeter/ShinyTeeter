##### Shiny App for Making a Decision by Random Choice #####

# Load required packages -------------------------------
require(shiny)
require(shinythemes)

# Create user interface -------------------------------
ui <- fluidPage(theme = shinytheme('spacelab'),
        
        # App Title
        titlePanel("A Simple Random-Decision Maker"),
        hr(),
        # App Description
        p("This app is intended to help limit the time and effort you spend making decisions. We are confronted with many decisions everyday and often take for granted their cumulative effect.",
                tags$em('What should I have for dinner? Should I go out? Where should I go? What will I order? Should I just order in? What should I order?'), "Making these seemingly simple decisions
                can lead to", tags$a(href = 'https://en.wikipedia.org/wiki/Decision_fatigue', 'decision fatigue,', target = '_blank'),
                "which can manifest as physical fatigue and lead to undesirable behaviour. To avoid this issue it is a good idea to find strategies that limit the number of decisions you need to make in a day. These include
                automating the decision-making processes, limiting the number of options under consideration, and/or leaving the decision up to chance -- there is research that shows",  
                tags$a(href = 'https://www.nber.org/papers/w22487.pdf#', 'flipping a coin', target = '_blank'), "can be an effective way to make difficult decisions and may even increase your happiness with the outcome. 
                This strategy of leaving things up to chance, like a coin-flip, is where this app is useful."),
        hr(),
        p("There are two ways to use the app:", tags$em('Simple'), "and", tags$em('Advanced'), ". The irony of starting things with a decision is not lost on me."),
        p(tags$b('Simple'), ": Use a slider to indicate the number of options under consideration. The decision-robot will return a number. You need to know how the number maps onto your choices."),
        p(tags$b('Advanced'), ": Type the options under consideration into a text-box (e.g., Futurama, Mad Men, Seinfeld). The decision-robot will return one of the options."),
        p("When you are ready, click the", tags$em('Make Decision'), "button to have the decision-robot conjure its magic (read:", tags$code('randomly select'), "an option) and let you
          move forward with your day."),
        p("If you want to start anew, the", tags$em('Reset'), "button will reload the page."),
        hr(),
        # User Input Section -------------------------------
        sidebarLayout(
                sidebarPanel(
                        radioButtons(inputId = 'smp_adv',
                                     label = 'How do you want to go about deciding?',
                                     choices = list("Simple" = 1, "Advanced" = 2),
                                     selected = 0),
                        hr(),
                        # Remove the minor ticks on the slider
                        tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
                        conditionalPanel(condition = "input.smp_adv == '1'",
                                         sliderInput(inputId = 'num_options',
                                                     label = 'Number of options under consideration:',
                                                     min = 2,
                                                     max = 10,
                                                     value = 0,
                                                     step = 1),
                                         hr(),
                                         actionButton(inputId = 's_generate', label = 'Make Decision', icon = icon("gavel")),
                                         actionButton(inputId = 's_reset', label = 'Reset', icon = icon("refresh"))),
                        conditionalPanel(condition = "input.smp_adv == '2'",
                                         textInput(inputId = 'option_list',
                                                   label = 'Enter the name of an option and click the Add button'),
                                         actionButton(inputId = 'add_item', label = 'Add'),
                                         hr(),
                                         verbatimTextOutput(outputId = 'user_list'),
                                         hr(),
                                         actionButton(inputId = 'a_generate', label = 'Make Decision', icon = icon("gavel")),
                                         actionButton(inputId = 'a_reset', label = 'Reset', icon = icon("refresh"))),
                        hr()),
                # Output of Plot, Data, and Summary -------------------------------
                mainPanel(tags$style(type='text/css', "#s_winner {
                                        font-family: 'Arial';
                                        font-size: 350%;
                                        font-weight: bold;
                                        text-align: center;
                                        color: #1e00dd;}"),
                          tags$style(type='text/css', "#a_winner {
                                        font-family: 'Arial';
                                     font-size: 350%;
                                     font-weight: bold;
                                     text-align: center;
                                     color: #1e00dd;}"),
                          
                          conditionalPanel(condition = "output.s_winner > '0'",
                                        tags$h4('After much deliberating, the winning option is:'),
                                        hr(),
                                        verbatimTextOutput(outputId = 's_winner')),
                          conditionalPanel(condition = "output.a_winner > '0'",
                                           tags$h4('After much deliberating, the winning option is:'),
                                           hr(),
                                           verbatimTextOutput(outputId = 'a_winner'))
                )
        ),
        # Footer -------------------------------
        tags$br(),
        hr(),
        p("App created by ", tags$a(href = "http://www.cteeter.ca", 'Chris Teeter', target = '_blank'), " in October 2017", HTML("&bull;"), "Follow Chris on Twitter:", tags$a(href = "https://twitter.com/c_mcgeets", tags$i(class = 'fa fa-twitter'), target = '_blank'),
          HTML("&bull;"), "Find the code on Github:", tags$a(href = "https://github.com/cjteeter/ShinyTeeter/tree/master/1_DecisionMaker", tags$i(class = 'fa fa-github', style = 'color:#5000a5'), target = '_blank'), style = "font-size: 85%"),
        p("Have a question? Send an email ", tags$a(href = "mailto:christopher.teeter@gmail.com", tags$i(class = 'fa fa-envelope', style = 'color:#990000'), target = '_blank'), style = "font-size: 85%"),
        p(tags$em("Last updated: May 2018"), style = 'font-size:75%')
)

# Server logic -----------------------------------
server <- function(input, output, session) {
        
        # Output for Simple Option -----------------------------------
        observeEvent(input$s_reset, { session$reload() } )
        output$s_winner <- eventReactive(input$s_generate, { sample(1:input$num_options, 1) } )
        
        # Output for Advanced Option -----------------------------------
        observeEvent(input$a_reset, { session$reload() } )
        user_options <- reactiveValues()
        observe({
                if(input$add_item > 0) {
                        user_options$oList <- c(isolate(user_options$oList), isolate(input$option_list))
                        updateTextInput(session, 'option_list',
                                        label = 'Enter the name of an option and click the Add button',
                                        NA)
                }
        })
        output$user_list <- renderText({ HTML(paste0(user_options$oList, collapse='\n')) })
        output$a_winner <- eventReactive(input$a_generate, {
                                a_winner_num <- sample(1:(length(as.vector(isolate(user_options$oList)))), 1)
                                a_winner_value <- as.vector(isolate(user_options$oList))[a_winner_num]
                        })
        outputOptions(output, 's_winner', suspendWhenHidden=FALSE)
        outputOptions(output, 'a_winner', suspendWhenHidden=FALSE)
}

# Run app -------------------------------
shinyApp(ui = ui, server = server)