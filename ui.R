#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(markdown)

navbarPage("Singlish norms",
           
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
