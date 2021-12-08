# nutrition calculator app using a reactive function
# load libraries and data####
library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)

# What's in this Rdata file: save(ca_conversion_factor, ca_food_choices, ca_food_group, ca_food_name, ca_food_source, ca_measure_name, ca_nutrient_amount, ca_nutrient_name, ca_nutrient_source, ca_refuse_amount, ca_refuse_name, ca_yield_amount, ca_yield_name, daily_value, file = "nutrient_data.Rdata")
load("../data/nutrient_data.Rdata")

# Let's define our user interface (UI)####
ui <- fluidPage(
  # Application title
  titlePanel("Health Canada Nutrient Calculator"),
  
  # Sidebar with a selector for food item and for the amount of food
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "ingredient", label = "Which item?", choices = ca_food_name$FoodDescription, multiple = F),
      sliderInput(inputId = "amount", label = "How much?", min = 2, max = 500, step = 20, value = 250), 
      actionButton("update_calculation", "Update nutrients")
    ),
    
    # Show interactive tables and plots of the mineral and macronutrient content of the selected food item
    mainPanel(
      dataTableOutput("nutrientTable"),
      plotlyOutput("nutrientPlot")
    )
  )
)

# Let's define our server logic####
server <- function(input, output){
  # The calculations for each output we've defined for the mainPanel() should go here
  
  nutrient_reactive <- reactive({
    # If you'd like to pause the calculation until you click the update button, swap the line of code above with: nutrient_reactive <- eventReactive(input$update_calculation, {
    
    food_choice <- input$ingredient
    food_amount <- input$amount
    
    measure_df <- ca_food_name %>%
      filter(FoodDescription == food_choice) %>% 
      select(FoodID) %>%
      left_join(ca_conversion_factor) %>% 
      left_join(ca_measure_name) %>% 
      select(numeric, units, description, ConversionFactorValue, MeasureID, FoodID) 
    
    measure_food_df <- measure_df %>%
      filter(numeric == min(numeric)) %>%
      left_join(ca_nutrient_amount) %>%
      left_join(ca_nutrient_name) %>%
      mutate(NutrientName = tolower(NutrientName)) %>%
      mutate(NutrientValue = NutrientValue * ConversionFactorValue * food_amount / numeric) %>%
      select(NutrientName, NutrientValue, NutrientID, NutrientUnit, ConversionFactorValue, FoodID) %>% 
      group_by(NutrientName) %>% 
      summarize(Value = round(sum(NutrientValue, na.rm = T), digits = 2),
                Unit = NutrientUnit,
                NutrientID = NutrientID)
    
    select_nutrients <- c("calcium", "carbohydrate, total (by difference)", "cholesterol", "energy (kilocalories)", "fat (total lipids)", "fatty acids, saturated, total", "fatty acids, trans, total", "fibre, total dietary", "iron", "protein", "retinol activity equivalents", "sodium", "sugars, total", "vitamin c")
    
    nutrient_df <- measure_food_df %>% filter(NutrientName %in% select_nutrients) %>%
      select(NutrientName, NutrientID, Value, Unit) %>%
      arrange(-Value)
    
    scaled_nutrient_df <- daily_value %>% 
      left_join(nutrient_df) %>%
      mutate(Scaled_dv = round(Value/DV, digits = 3) * 100) %>%
      na.omit()
    
    # fix funky units
    scaled_nutrient_df[scaled_nutrient_df$Unit == "\xb5g", "Unit"] <- "g"
    scaled_nutrient_df
    
  }) 
  
  output$nutrientTable <- renderDataTable({
    
    nutrient_reactive()
    
  })
  
  output$nutrientPlot <- renderPlotly({
    
    scaled_nutrient_df <- nutrient_reactive()
    
    nutrient_plot <- ggplot(scaled_nutrient_df) +
      geom_bar(stat = "identity", aes(x = reorder(NutrientName, Scaled_dv), Scaled_dv)) +
      xlab("Nutrient name") +
      ylab("% Daily value") +
      coord_flip()
    
    # interactive version of the plot with the value as a hovering tooltip
    ggplotly(nutrient_plot, tooltip = "y")
    
  })
  
} # end server logic

# With UI and server defined, we're ready to run the app!####
shinyApp(ui = ui, server = server)

# At this point, we've explored some basic principles of the reactive paradigm that Shiny uses to control the flow of information through the app. In the next section, we'll take a more complex version of this app and start styling it with flexdashboard.

