library(shiny)
library(ggplot2)
library(tidyverse)
library(markdown)

test_data <- read.csv('data-for-app.csv') |> 
  rename(Valence = mean_valence,
         Arousal = mean_arousal,
         Concreteness = mean_conc,
         Humor = mean_humor) |> 
  mutate(Valence = round(Valence, 2),
         Arousal = round(Arousal, 2),
         Concreteness = round(Concreteness, 2),
         Humor = round(Humor, 2))

ui <- navbarPage("Singlish norms",
           
            ### ABOUT ----
  tabPanel('About', 
           fluidRow(
             column(6,
                    includeMarkdown("about.md")
          )
        )
      ),
           ### TABLE ### 
            tabPanel("Table",
                    titlePanel("Lexical-semantic norms for 283 Singlish Concepts"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("man",
                       "Item:",
                       c("All",
                         unique(as.character(test_data$Word))))
    ),
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table"),
  downloadButton("downloadData", "Download Data")
                    
           ),
  
  ### VALENCE ---- 
  tabPanel("Valence",
               
               # Give the page a title
  titlePanel("Valence norms"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      # input some words 
      tags$h4('Select up to 20 items from the list.'),
      selectizeInput(
        'fooV', label = '', choices = c("", test_data$Word), multiple = TRUE, 
        options = list(maxItems = 20, placeholder = 'search for a Singlish word!')
      ),
      br(),
      # filter range 
      sliderInput( 
        "sliderV", "", 
        min = 1, max = 9, 
        value = c(1, 9), step = 0.5 
      ), 
      hr(),
      helpText("Valence refers to the pleasantness of the emotion elicited by a particular word or concept. It ranges from highly pleasant (e.g., joke and kitten) to unpleasant (e.g., jail and kill). The Singlish items were rated on a 9 point Likert scale, with 1 = most unpleasant (negative) and 9 = most pleasant (positive).")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      fluidRow(
        column(9,
      plotOutput("distPlotValence", height = 900)  
    )
    )
    )
  )
  ),
  
  ### AROUSAL ---- 
  tabPanel("Arousal",
           
           # Give the page a title
           titlePanel("Arousal norms"),
           
           # Generate a row with a sidebar
           sidebarLayout(      
             
             # Define the sidebar with one input
             sidebarPanel(
               # input some words 
               tags$h4('Select up to 20 items from the list.'),
               selectizeInput(
                 'fooA', label = '', choices = c("", test_data$Word), multiple = TRUE, 
                 options = list(maxItems = 20, placeholder = 'search for a Singlish word!')
               ),
               br(),
               # filter range 
               sliderInput( 
                 "sliderA", "", 
                 min = 1, max = 9, 
                 value = c(1, 9), step = 0.5 
               ), 
               hr(),
               helpText("Arousal refers to the degree of arousal invoked by a particular word or concept. It ranges from highly arousing (e.g., sex and thrill) to unexciting (e.g., statue and bored). The Singlish items were rated on a 9 point Likert scale, with 1 = least arousing (boring) and 9 = highly arousing (exciting).")
             ),
             
             # Create a spot for the barplot
             mainPanel(
               fluidRow(
                 column(9,
                        plotOutput("distPlotArousal", height = 900)  
                 )
               )
             )
           )
  ),
  
  ### CONCRETENESS ----
  tabPanel("Concreteness",
           
           # Give the page a title
           titlePanel("Concreteness norms"),
           
           # Generate a row with a sidebar
           sidebarLayout(      
             
             # Define the sidebar with one input
             sidebarPanel(
               # input some words 
               tags$h4('Select up to 20 items from the list.'),
               selectizeInput(
                 'fooC', label = '', choices = c("", test_data$Word), multiple = TRUE, 
                 options = list(maxItems = 20, placeholder = 'search for a Singlish word!')
               ),
               br(),
               # filter range 
               sliderInput( 
                 "sliderC", "", 
                 min = 1, max = 5, 
                 value = c(1, 5), step = 0.5 
               ), 
               hr(),
               helpText("Concreteness refers to the extent to which the concept represented by a word (or short phrase) refers to a perceptible entity. It ranges from highly concrete, whereby an entity exists in the real world and elicits immediate experiences of that concept through one’s senses, to highly abstract, whereby an entity cannot be directly experienced through one’s senses. The Singlish items were rated on a 5 point Likert scale, with 1 = most abstract and 5 = most concrete.")
             ),
             
             # Create a spot for the barplot
             mainPanel(
               fluidRow(
                 column(9,
                        plotOutput("distPlotConcreteness", height = 900)  
                 )
               )
             )
           )
  ),
  
  ### HUMOR ----
  tabPanel("Humor",
           
           # Give the page a title
           titlePanel("Humor norms"),
           
           # Generate a row with a sidebar
           sidebarLayout(      
             
             # Define the sidebar with one input
             sidebarPanel(
               # input some words 
               tags$h4('Select up to 20 items from the list.'),
               selectizeInput(
                 'fooH', label = '', choices = c("", test_data$Word), multiple = TRUE, 
                 options = list(maxItems = 20, placeholder = 'search for a Singlish word!')
               ),
               br(),
               # filter range 
               sliderInput( 
                 "sliderH", "", 
                 min = 1, max = 5, 
                 value = c(1, 5), step = 0.5 
               ), 
               hr(),
               helpText("Humor ratings capture the extent to which a word or concept elicited feelings of amusement or humorous thought. It ranges from highly humorous (e.g., booty and waddle) to unhumorous (e.g., nightmare and pain). The Singlish items were rated on a 5 point Likert scale, with 1 = most humorless and 5 = most humorous.")
             ),
             
             # Create a spot for the barplot
             mainPanel(
               fluidRow(
                 column(9,
                        plotOutput("distPlotHumor", height = 900)  
                 )
               )
             )
           )
  )

)

server <- function(input, output, session) {
  
# Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- test_data
    if (input$man != "All") {
      data <- data[data$Word == input$man,]
    }
    data
  }))
  
  # Prepare data for download
  output$downloadData <- downloadHandler(
    filename = "sge-norm-data.csv",
    content = function(file) {
      write.csv(test_data, file, row.names = F)
    }
  )

  ### VALENCE PLOT ----
    output$distPlotValence <- renderPlot({
      
sample_data_valence <- reactive({
        sample_data <- test_data |> filter(Valence > input$sliderV[1],
                                      Valence < input$sliderV[2],
                                      Word %in% input$fooV) |> 
          mutate(word_type = if_else(Valence < 5, 'negative', 'positive'),
                                     Valence = Valence - 5)
      })

      ggplot(sample_data_valence(), aes(x = reorder(Word, Valence), y = Valence)) +
          geom_point(
            stat = 'identity', aes(col=word_type), size = 6
            ) + 
          scale_color_manual(
            name = 'Word Type', 
      #      labels = c('negative', 'positive'),
            values = c('negative' = 'red', 'positive' = 'blue')
            ) + 
          geom_segment(
            aes(y = 0, x = Word, yend = Valence, xend = Word, color = 'grey')
            ) +
          geom_hline(yintercept = 0, color = 1, lwd = 0.2) +
          geom_text(aes(label = paste0(Word, ' ', round(5 + Valence, 2)), # Text with groups
                hjust = ifelse(Valence < 0, 1.5, -1),
                vjust = 0.5), size = 5) +
          xlab("Valence") +
          ylab("Negative -- Neutral -- Positive") +
          scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
          coord_flip() +
          theme_minimal() +
          theme(axis.text.y = element_blank(),  # Remove Y-axis texts
                axis.ticks.y = element_blank(), # Remove Y-axis ticks
                axis.text.x = element_blank(), 
                panel.grid.major.y = element_blank()) # Remove horizontal grid
    }) 
  
  ### HUMOR PLOT ----
    output$distPlotHumor <- renderPlot({
      
sample_data_humor <- reactive({
        sample_data <- test_data |> filter(Humor > input$sliderH[1],
                                      Humor < input$sliderH[2],
                                      Word %in% input$fooH) |> 
          mutate(word_type = if_else(Humor < 2.5, 'unfunny', 'funny'),
                                     Humor = Humor - 2.5)
      })

      ggplot(sample_data_humor(), aes(x = reorder(Word, Humor), y = Humor)) +
          geom_point(
            stat = 'identity', aes(col=word_type), size = 6
            ) + 
          scale_color_manual(
            name = 'Word Type', 
#            labels = c('unfunny', 'funny'),
            values = c('unfunny' = 'red', 'funny' = 'blue') # weird levels 
            ) + 
          geom_segment(
            aes(y = 0, x = Word, yend = Humor, xend = Word, color = 'grey')
            ) +
          geom_hline(yintercept = 0, color = 1, lwd = 0.2) +
          geom_text(aes(label = paste0(Word, ' ', round(2.5 + Humor, 2)), # Text with groups
                hjust = ifelse(Humor < 0, 1.5, -1),
                vjust = 0.5), size = 5) +
          xlab("Humor") +
          ylab("Least humorous -- Most humorous") +
          scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
          coord_flip() +
          theme_minimal() +
          theme(axis.text.y = element_blank(),  # Remove Y-axis texts
                axis.ticks.y = element_blank(), # Remove Y-axis ticks
                axis.text.x = element_blank(), 
                panel.grid.major.y = element_blank()) # Remove horizontal grid
    }) 
  ### AROUSAL PLOT ----
    output$distPlotArousal <- renderPlot({
      
sample_data_arousal <- reactive({
        sample_data <- test_data |> filter(Arousal > input$sliderA[1],
                                      Arousal < input$sliderA[2],
                                      Word %in% input$fooA) |> 
          mutate(word_type = if_else(Arousal < 5, 'boring', 'exciting'),
                                     Arousal = Arousal - 5)
      })

      ggplot(sample_data_arousal(), aes(x = reorder(Word, Arousal), y = Arousal)) +
          geom_point(
            stat = 'identity', aes(col=word_type), size = 6
            ) + 
          scale_color_manual(
            name = 'Word Type', 
         #   labels = c('boring', 'exciting'),
            values = c('boring' = 'red', 'exciting' = 'blue')
            ) + 
          geom_segment(
            aes(y = 0, x = Word, yend = Arousal, xend = Word, color = 'grey')
            ) +
          geom_hline(yintercept = 0, color = 1, lwd = 0.2) +
          geom_text(aes(label = paste0(Word, ' ', round(5 + Arousal, 2)), # Text with groups
                hjust = ifelse(Arousal < 0, 1.5, -1),
                vjust = 0.5), size = 5) +
          xlab("Arousal") +
          ylab("Unarousing-- Neutral -- Arousing") +
          scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
          coord_flip() +
          theme_minimal() +
          theme(axis.text.y = element_blank(),  # Remove Y-axis texts
                axis.ticks.y = element_blank(), # Remove Y-axis ticks
                axis.text.x = element_blank(), 
                panel.grid.major.y = element_blank()) # Remove horizontal grid
    }) 
  ### CONCRETENESS PLOT ----
    output$distPlotConcreteness <- renderPlot({
      
sample_data_conc <- reactive({
        sample_data <- test_data |> filter(Concreteness > input$sliderC[1],
                                      Concreteness < input$sliderC[2],
                                      Word %in% input$fooC) |> 
          mutate(word_type = if_else(Concreteness < 2.5, 'abstract', 'concrete'),
                                     Concreteness = Concreteness - 2.5)
      })

      ggplot(sample_data_conc(), aes(x = reorder(Word, Concreteness), y = Concreteness)) +
          geom_point(
            stat = 'identity', aes(col=word_type), size = 6
            ) + 
          scale_color_manual(
            name = 'Word Type', 
#            labels = c('abstract', 'concrete'),
            values = c('abstract' = 'red', 'concrete' = 'blue')
            ) + 
          geom_segment(
            aes(y = 0, x = Word, yend = Concreteness, xend = Word, color = 'grey')
            ) +
          geom_hline(yintercept = 0, color = 1, lwd = 0.2) +
          geom_text(aes(label = paste0(Word, ' ', round(2.5 + Concreteness, 2)), # Text with groups
                hjust = ifelse(Concreteness < 0, 1.5, -1),
                vjust = 0.5), size = 5) +
          xlab("Concreteness") +
          ylab("Abstract -- Concreteness") +
          scale_y_continuous(breaks = seq(-2.5, 2.5, by = 1),
                     limits = c(-2.5, 2.5)) +
          coord_flip() +
          theme_minimal() +
          theme(axis.text.y = element_blank(),  # Remove Y-axis texts
                axis.ticks.y = element_blank(), # Remove Y-axis ticks
                axis.text.x = element_blank(), 
                panel.grid.major.y = element_blank()) # Remove horizontal grid
    }) 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
