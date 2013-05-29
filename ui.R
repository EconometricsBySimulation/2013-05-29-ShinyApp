# ui.R
library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(

  # Application title.
  headerPanel("Visual Reasoning - Item Response Evaluation"),

  # This is the left hand panel.
  sidebarPanel(
    # This image is just loaded from another image and placed as a thumbnail into the shiny GUI.
      imageOutput("preImage", width = "100px", height = "100px"),
    
    # This allows the user to specify what the look of this input device will be.
    # In this case a slider that has a min of 1 and max of 92.
    # sliderInput("obs", "Choose Item:", 
    #            min = 1, max = 92, value = 1, step= 1, 
    #            ticks=c(1,25,50,75,92) , animate=TRUE), 
                
     selectInput("obs", "Choose Item:", 1:92),
    
    # This is the histogram of item difficulty
    plotOutput("distPlot", height = "300px"),
    
    # This displays text below the histogram
    helpText("Though histograms are organized into bins we know exactly in the range from 0 to 1 where this particular item falls.")          
  ),

  # Now let's define the main panel.
  mainPanel(
    # Display the title.
    h4("Item Summary"),
    # Display the item summary table.
    verbatimTextOutput("summary"),
    
    # Display sub heading.
    h4("User Responses"),
    # Display user response table.
    plotOutput("respPlot", height = "300px"),
    # Display the note.
    helpText("Note: Answer values are masked to mitigate potential cheating."),

    # Display sub heading
    h4("Observations"),
    # Display the table output of item responses.
    tableOutput("view"),
    
    helpText("Order is the order that the item was given in in this particular user's experience.")  
  )
))
