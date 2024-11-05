

library(rsconnect)
library(shiny)
library(glmnet) # Make sure you have glmnet for the model

# Load the trained model
model <- readRDS("lol_LASSO_min_AUC.rds")

# Load required libraries
ui <- fluidPage(
  titlePanel("The anti Azzap mentality"),
  
  # Create a fluid row for layout
  fluidRow(
    # Left sidebar for Blue side inputs
    column(3,
           wellPanel(
             h4("Blue Side"),
             
             numericInput("t1_towerKills", "Tower Kills:", 0, min = 0, max= 11, width = '100%'),
             numericInput("t1_inhibitorKills", "Inhibitor Kills:", 0, min = 0, width = '100%'),
             numericInput("t1_baronKills", "Baron Kills:", 0, min = 0, width = '100%'),
             numericInput("t1_dragonKills", "Dragon Kills:", 0, min = 0, width = '100%'),
             numericInput("t1_riftHeraldKills", "Rift Herald Kills:", 0, min = 0, max = 1, width = '100%')
           )
    ),
    
    # Center area for objectives
    column(6,
           wellPanel(
             h4("Objectives"),
             selectInput("firstBlood", "First Blood:", choices = c("No Team" = 0, "Blue side" = 1, "Red side" = 2), width = '100%'),
             selectInput("firstTower", "First Tower:", choices = c("No Team" = 0, "Blue side" = 1, "Red side" = 2), width = '100%'),
             selectInput("firstInhibitor", "First Inhibitor:", choices = c("No Team" = 0, "Blue side" = 1, "Red side" = 2), width = '100%'),
             selectInput("firstBaron", "First Baron:", choices = c("No Team" = 0, "Blue side" = 1, "Red side" = 2), width = '100%'),
             selectInput("firstDragon", "First Dragon:", choices = c("No Team" = 0, "Blue side" = 1, "Red side" = 2), width = '100%'),
             selectInput("firstRiftHerald", "First Rift Herald:", choices = c("No Team" = 0, "Blue side" = 1, "Red side" = 2), width = '100%'),
             actionButton("predict", "Predict", style = "width: 100%; height: 50px; font-size: 18px;")
           )
    ),
    
    # Right sidebar for Red side inputs
    column(3,
           wellPanel(
             h4("Red Side"),
             numericInput("t2_towerKills", "Tower Kills:", 0, min = 0,max= 11, width = '100%'),
             numericInput("t2_inhibitorKills", "Inhibitor Kills:", 0, min = 0, width = '100%'),
             numericInput("t2_baronKills", "Baron Kills:", 0, min = 0, width = '100%'),
             numericInput("t2_dragonKills", "Dragon Kills:", 0, min = 0, width = '100%'),
             numericInput("t2_riftHeraldKills", "Rift Herald Kills:", 0, min = 0,max = 1, width = '100%')
           )
    )
  ),
  
  # Main panel for displaying the prediction result
  mainPanel(
    h4("Prediction Result:"),
    verbatimTextOutput("prediction", placeholder = TRUE)
  )
)

# Define server logic required to perform predictions
server <- function(input, output) {
  observeEvent(input$predict, {
    # Create the input vector for prediction
    input_vector <- c(
      as.numeric(input$firstBlood),
      as.numeric(input$firstTower),
      as.numeric(input$firstInhibitor),
      as.numeric(input$firstBaron),
      as.numeric(input$firstDragon),
      as.numeric(input$firstRiftHerald),
      input$t1_towerKills,
      input$t1_inhibitorKills,
      input$t1_baronKills,
      input$t1_dragonKills,
      input$t1_riftHeraldKills,
      input$t2_towerKills,
      input$t2_inhibitorKills,
      input$t2_baronKills,
      input$t2_dragonKills,
      input$t2_riftHeraldKills
    )
    
    # Convert the input vector to a matrix for prediction
    model_matrix <- matrix(input_vector, nrow = 1)
    
    # Make the prediction using the loaded model
    prediction <- predict(model, newx = model_matrix, type = "response")
    
    # Calculate probabilities for both teams
    prob_team_1 <- prediction * 100  # Convert to percentage
    prob_team_2 <- (1 - prediction) * 100  # Convert to percentage
    
    # Show the prediction result at the top
    output$prediction <- renderPrint({
      result_1 <- paste("Blue side is likely to win: ", round(prob_team_1, 2), "%", sep = "")
      result_2 <- paste("Red side is likely to win: ", round(prob_team_2, 2), "%", sep = "")
      cat(result_1, "\n", result_2)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)