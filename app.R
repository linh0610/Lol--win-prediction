

library(rsconnect)
library(shiny)
library(glmnet) # Make sure you have glmnet for the model

# Load the trained model
model <- readRDS("lol_LASSO_min_AUC.rds")

# Load required libraries
ui <- fluidPage(
  titlePanel("Should you ff bruh"),
  sidebarLayout(
    sidebarPanel(
      selectInput("firstBlood", "First Blood:", choices = c("No Team" = 0, "Blue side" = 1, "Red side" = 2)),
      selectInput("firstTower", "First Tower:", choices = c("No Team" = 0, "Blue side" = 1, "Red side" = 2)),
      selectInput("firstInhibitor", "First Inhibitor:", choices = c("No Team" = 0, "Blue side" = 1, "Red side" = 2)),
      selectInput("firstBaron", "First Baron:", choices = c("No Team" = 0, "Blue side" = 1, "Red side" = 2)),
      selectInput("firstDragon", "First Dragon:", choices = c("No Team" = 0, "Blue side" = 1, "Red side" = 2)),
      selectInput("firstRiftHerald", "First Rift Herald:", choices = c("No Team" = 0, "Blue side" = 1, "Red side" = 2)),
      numericInput("t1_towerKills", "Blue side Tower Kills:", 0, min = 0),
      numericInput("t1_inhibitorKills", "Blue side Inhibitor Kills:", 0, min = 0),
      numericInput("t1_baronKills", "Blue side Baron Kills:", 0, min = 0),
      numericInput("t1_dragonKills", "Blue side Dragon Kills:", 0, min = 0),
      numericInput("t1_riftHeraldKills", "Blue side Rift Herald Kills:", 0, min = 0),
      numericInput("t2_towerKills", "Red side Tower Kills:", 0, min = 0),
      numericInput("t2_inhibitorKills", "Red side Inhibitor Kills:", 0, min = 0),
      numericInput("t2_baronKills", "Red side Baron Kills:", 0, min = 0),
      numericInput("t2_dragonKills", "Red side Dragon Kills:", 0, min = 0),
      numericInput("t2_riftHeraldKills", "Red side Rift Herald Kills:", 0, min = 0),
      actionButton("predict", "Predict")
    ),
    mainPanel(
      h4("Prediction Result:"),
      verbatimTextOutput("prediction")
    )
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
    
    # Show the prediction result
    output$prediction <- renderPrint({
      result_1 <- paste("Blue side is likely to win: ", round(prob_team_1, 2), "%", sep = "")
      result_2 <- paste("Red side is likely to win: ", round(prob_team_2, 2), "%", sep = "")
      
      # Combine the results into a single string with a newline
      paste(result_1, result_2, sep = "-")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)





