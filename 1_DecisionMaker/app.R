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
        p("This app is intended to help you limit the time and effort you spend making decisions. Even on the most standard of days we make a large number of decisions.",
                tags$em('What should I have for dinner? Should I go out? Where should I go? What will I order? Should I just order in? What should I order?'), "While many 
                (even most) of the decisions we make over the course of a day are fairly mundane, we often fail to account for their cumulative effect. They can lead to", 
                tags$a(href = 'https://en.wikipedia.org/wiki/Decision_fatigue', 'decision fatigue,', target = '_blank'),
                "which can manifest as physical fatigue and even bring about undesirable behaviour. So it is a good idea to find strategies that limit the mental 
                effort you put into making decisions. Options include automating the decision-making processes, limiting the number of options under consideration, 
                accepting the option you'll be", tags$a(href = 'https://www.nytimes.com/2018/06/04/smarter-living/how-to-finally-just-make-a-decision.html', tags$em('fine'), 'with,', target = '_blank'),
                "and/or leaving the decision to chance.", "For example,", tags$a(href = 'https://www.nber.org/papers/w22487.pdf#', 'flipping a coin', target = '_blank'), "can be an
                effective way to make a decision and doing so may even increase your satisfaction with the outcome. This strategy of leaving things to chance, like the 
                flip of a coin, is where this application is useful."),
        hr(),
        p("There are two ways to use the app:", tags$em('Simple'), "and", tags$em('Advanced'), "."),
        p(tags$b('Simple'), ": Use a slider to indicate the number of options under consideration. The decision-robot will return a number. You need to know how the number maps onto your options."),
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
                          
                          conditionalPanel(condition = "input.smp_adv == '1' && output.s_winner > '0'",
                                        tags$h4('After much deliberating, the winning option is:'),
                                        hr(),
                                        verbatimTextOutput(outputId = 's_winner')),
                          conditionalPanel(condition = "input.smp_adv == '2' && output.a_winner > '0'",
                                           tags$h4('After much deliberating, the winning option is:'),
                                           hr(),
                                           verbatimTextOutput(outputId = 'a_winner'))
                )
        ),
        # Footer -------------------------------
        tags$br(),
        hr(),
        p("App created by ", tags$a(href = "https://www.cteeter.ca", 'Chris Teeter', target = '_blank'), " in October 2017", HTML("&bull;"), "Follow Chris on Twitter:", tags$a(href = "https://twitter.com/c_mcgeets", tags$i(class = 'fa fa-twitter'), target = '_blank'),
          HTML("&bull;"), "Find the code on Github:", tags$a(href = "https://github.com/cjteeter/ShinyTeeter/tree/master/1_DecisionMaker", tags$i(class = 'fa fa-github', style = 'color:#5000a5'), target = '_blank'), style = "font-size: 85%"),
        p("Have a question? Send an email ", tags$a(href = "mailto:christopher.teeter@gmail.com", tags$i(class = 'fa fa-envelope', style = 'color:#990000'), target = '_blank'), style = "font-size: 85%"),
        p(tags$em("Last updated: August 2018"), style = 'font-size:75%')
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