library(shiny)

source("./simulate-cio-data.R")
source("./GGbars.R")
source("./GGbar.R")

options(RCHART_LIB = 'NVD3')

ui <- fluidPage(
    
    tabsetPanel(title = "Hallo dies ist ein Titel",
                
                tabPanel(title = "IT-Funktionen gruppiert",
                         h1("Darstellung der Umsetzungsgrade innerhalb einer Funktionsgruppe"),
                         
                         selectInput(inputId = "it_function_groups", 
                                     label = "Bitte wählen Sie die Gruppe der IT-Funktionen:", 
                                     choices = list("Klinische Dokumentationsfunktionen" = "KD1", 
                                                    "Leistungsanforderung und Befundrückmeldung" = "LB1", 
                                                    "Entscheidungsunterstützung" = "EN1",
                                                    "Patientensicherheit" = "P1",
                                                    "Versorgungsfunktionen" = "S1",
                                                    "Schnittstellenfunktionen" = "VS1",
                                                    "Telemedizin und -monitoring" = "Tel"),
                                     selected = "KD1",
                                     width = 400),
                         
                         radioButtons(inputId = "color", 
                                      label = "Bitte wählen Sie eine Farbe:", 
                                      choices = list("Grün" = "Greens", 
                                                     "Orange" = "Oranges", 
                                                     "Grau" = "Greys", 
                                                     "Rot" = "Reds"),
                                      selected = "Grün"),
                         
                         plotOutput("implementation_bars", width = "100%")
                         
                ),
                
                tabPanel(title = "IT-Funktionen einzeln",
                         
                         h1("Darstellung der Umsetzungsgrade jeder IT-Funktion"),
                         
                         selectInput(inputId = "it_function_groups_tigger", 
                                     label = "Bitte wählen Sie die Gruppe der IT-Funktionen:", 
                                     choices = list("Klinische Dokumentationsfunktionen" = "KD1", 
                                                    "Leistungsanforderung und Befundrückmeldung" = "LB1", 
                                                    "Entscheidungsunterstützung" = "EN1",
                                                    "Patientensicherheit" = "P1",
                                                    "Versorgungsfunktionen" = "S1",
                                                    "Schnittstellenfunktionen" = "VS1",
                                                    "Telemedizin und -monitoring" = "Tel"),
                                     selected = "KD1",
                                     width = 400),
                         
                         selectInput(inputId = "it_function_subgroup",
                                     label = "Bitte wählen Sie eine IT-Funktion aus:",
                                     choices = c("label 1" = "option1",
                                                 "label 2" = "option2")),
                         
                         showOutput(outputId = "rChart_implementation_bars", lib = 'nvd3')
                         
                         # plotOutput("implementation_bar", width = "75%%")
                )
    )
    
)

server <- function(input, output, session) {
    
    df <- reactive({
        data %>% filter(question_internal_id == input$it_function_groups) %>% droplevels()
    })
    
    plot_title <- reactive({
        data %>% 
            filter(question_internal_id == input$it_function_groups) %>% 
            select(question_text)
    })
    
    observeEvent(input$it_function_groups, {
        print(input$it_function_groups)
        print(input$color)
        
    })
    
    rv <- reactiveValues(gg_tilte = NULL)
    
    observeEvent(input$it_function_groups, {
        rv$gg_title <- data %>% 
            filter(question_internal_id == input$it_function_groups) %>% 
            filter(row_number()==1) %>% 
            select(question_text) %>% as.character()
    })
    
    observeEvent(input$it_function_groups_tigger, {
        s_options <- filter_subcategory(data = subquestion, category = input$it_function_groups_tigger)
        updateSelectInput(session, "it_function_subgroup",
                          choices = s_options)
        print(s_options)
    })
    
    # generate stacked barplot
    output$implementation_bars <- renderPlot({
        GGbars(data = df(), X = "subquestion_text", value = "response_label", plot_color = input$color, plot_title = rv$gg_title)
    })
    
    # generate dodged barplots
    output$implementation_bar <- renderPlot({
        GGbar(data = data, X = "response_labels", value = input$it_function_subgroup)
    })
    
    output$rChart_implementation_bars <- renderChart2(
        rChart_bar(data = data, X = "response_labels", value = input$it_function_subgroup)
    )

}

shinyApp(ui = ui, server = server)


