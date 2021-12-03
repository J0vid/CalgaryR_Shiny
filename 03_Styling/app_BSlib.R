# nutrition calculator app with some flair added to it
# load libraries and data####
library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(bslib)

# What's in this Rdata file: save(ca_conversion_factor, ca_food_choices, ca_food_group, ca_food_name, ca_food_source, ca_measure_name, ca_nutrient_amount, ca_nutrient_name, ca_nutrient_source, ca_refuse_amount, ca_refuse_name, ca_yield_amount, ca_yield_name, daily_value, file = "nutrient_data.Rdata")
load("../data/nutrient_data.Rdata")

# Let's define a bstheme to use. Custom themes below come from https://unleash-shiny.rinterface.com/beautify-with-bootstraplib.html#beautify-with-bootstraplib
# very bright theme
bslib_theme <-  bs_theme(
  version = 5,
  bg = "#000000",
  fg = "#FFFFFF",
  primary = "#9600FF",
  secondary = "#1900A0",
  success = "#38FF12",
  info = "#00F5FB",
  warning = "#FFF100",
  danger = "#FF00E3",
  base_font = "Marker Felt",
  heading_font = "Marker Felt",
  code_font = "Chalkduster"
)

# # win98 look?!?
# win98_cdn <- "https://cdn.jsdelivr.net/npm/98.css@0.1.16/"
# win98_css <- paste0(win98_cdn, "dist/98.min.css")
# windows_grey <- "#c0c0c0"
# bslib_theme <- bs_theme(
#   version = 4,
#   bg = windows_grey,
#   fg = "#222222",
#   primary = "#03158b",
#   base_font = c("Times", "Arial"),
#   secondary = windows_grey,
#   success = windows_grey,
#   danger = windows_grey,
#   info = windows_grey,
#   light = windows_grey,
#   dark = windows_grey,
#   warning = windows_grey,
#   "font-size-base" = "0.75rem",
#   "enable-rounded" = FALSE
# ) %>%
#   bs_add_rules(
#     sprintf('@import "%s"', win98_css)
#   )

#preset themes with the bootswatch argument: bslib_theme <- bs_theme(bootswatch = "minty")

# Let's define our user interface (UI)####
ui <- fluidPage(
  theme = bslib_theme,
  # Application title
  titlePanel("Health Canada Nutrient Calculator"),
  fluidRow(
  column(width = 3, selectInput(inputId = "ingredient", label = "Which item?", choices = ca_food_name$FoodDescription, multiple = F)),
  column(width = 3, selectizeInput('selected_units', 'What unit to use', choices = "ml")),
  column(width = 3, sliderInput("amount", "How much",  1,200, 1)),
  column(width = 3, actionButton("update_calculation", "Update nutrients")),
  hr(),
    # Show interactive tables and plots of the mineral and macronutrient content of the selected food item
      column(width = 6,
             dataTableOutput("nutrientTable"),
             br(),
             plotlyOutput("macroPlot")
             ),
      
      column(width = 6, 
             plotlyOutput("vitaminPlot"),
             br(),
             plotlyOutput("mineralPlot")
             )
  )
)

# Let's define our server logic####
server <- function(input, output, session){
  # The calculations for each output we've defined for the mainPanel() should go here
  
  nutrient_reactive <- reactive({
    #if you'd like to pause the calculation until you click the update button, swap the line of code above with: nutrient_reactive <- eventReactive(input$update_calculation, {
    print(input$selected_units)
    
    food_choice <- input$ingredient
    selected_units <- input$selected_units
    food_amount <- input$amount
    
    
    measure_df <- ca_food_name %>%
      filter(FoodDescription == food_choice) %>% 
      select(FoodID) %>%
      left_join(ca_conversion_factor) %>% 
      left_join(ca_measure_name) %>% 
      select(numeric, units, description, ConversionFactorValue, MeasureID, FoodID) 
    
    measure_food_df <- measure_df %>%
      filter(units == selected_units) %>%
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
    
    #fix funky units
    scaled_nutrient_df[scaled_nutrient_df$Unit == "\xb5g", "Unit"] <- "g"
    list(scaled_nutrient_df, measure_df)
    
  }) 
  
  unit_selector <- observe({
    reactive_data <- nutrient_reactive()[[2]]
    reactive_data <- reactive_data[reactive_data$units == input$selected_units,]
    
    selected_units <- reactive_data$units[1]
    unit_dims <- nrow(reactive_data)
    slider_min <- min(reactive_data$numeric)
    slider_max <- max(reactive_data$numeric)
    
    # we want to update the options for units based on what's available in the dataset for the selected food item
    updateSelectizeInput(session, "selected_units", label = "Which units?", choices = unique(nutrient_reactive()[[2]][["units"]]), selected = selected_units)
    
    # we also want to update the slider for the amount of the selected food in the units selected. This requires a bit of checking for situations like checking for a range of values when ml/g are selected, or changing the scale for integer style units like 1 "order/serving"
    if(input$selected_units != ""){
      if(input$selected_units == "ml" | input$selected_units == "g"){
        if(unit_dims > 1) updateSliderInput(session, "amount", label = paste0("Amount in ", selected_units), min = slider_min, max = slider_max)
        if(unit_dims == 1) updateSliderInput(session, "amount", label = paste0("Amount in ", selected_units), min = slider_min, max = slider_max + 200)
      } else{updateSliderInput(session, "amount", label = paste0("Amount in ", selected_units), min = 1, max = 5, step = 1)}
    }
  })
  
  output$nutrientTable <- renderDataTable({
    
    nutrient_reactive()[[1]] %>%
      select(c("Nutrient", "Group", "Value", "Unit", "Scaled_dv"))
    
  })
  
  output$macroPlot <- renderPlotly({
    
    scaled_nutrient_df <- nutrient_reactive()[[1]] %>%
                            filter(Group == "macronutrients")
    
    nutrient_plot <- ggplot(scaled_nutrient_df) +
      geom_bar(stat = "identity", aes(x = reorder(NutrientName, Scaled_dv), Scaled_dv)) +
      xlab("Macro") +
      ylab("% Daily value") +
      coord_flip()
    
    #interactive version of the plot with the value as a hovering tooltip
    ggplotly(nutrient_plot, tooltip = "y")
    
  })
  
  output$vitaminPlot <- renderPlotly({
    
    scaled_nutrient_df <- nutrient_reactive()[[1]] %>%
      filter(Group == "vitamin")
    
    nutrient_plot <- ggplot(scaled_nutrient_df) +
      geom_bar(stat = "identity", aes(x = reorder(NutrientName, Scaled_dv), Scaled_dv)) +
      xlab("Vitamin") +
      ylab("% Daily value") +
      coord_flip()
    
    #interactive version of the plot with the value as a hovering tooltip
    ggplotly(nutrient_plot, tooltip = "y")
    
  })
  
  output$mineralPlot <- renderPlotly({
    
    scaled_nutrient_df <- nutrient_reactive()[[1]] %>%
      filter(Group == "mineral")
    
    nutrient_plot <- ggplot(scaled_nutrient_df) +
      geom_bar(stat = "identity", aes(x = reorder(NutrientName, Scaled_dv), Scaled_dv)) +
      xlab("Mineral") +
      ylab("% Daily value") +
      coord_flip()
    
    #interactive version of the plot with the value as a hovering tooltip
    ggplotly(nutrient_plot, tooltip = "y")
    
  })
  
} # end server logic

# With UI and server defined, we're ready to run the app!####
shinyApp(ui = ui, server = server)

# Now we've got something that looks...interesting, if not better that the default options. When you're happy with the look and feature set of your app, it's time to start thinking about how to share it. The next folder "04_Deploying" goes over exactly that.

