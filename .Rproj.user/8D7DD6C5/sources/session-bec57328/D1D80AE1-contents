unitConverterUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    actionButton(ns("back_home"), "Home", class = "btn btn-primary",icon = icon("arrow-left")),
    
    titlePanel("🔄 Unit Converter"),
    
    sidebarLayout(
      sidebarPanel(
        
        selectInput(ns("conversion_type"), "Select Conversion Type:",
                    choices = c("Concentration", "Volume", "Mass", "Time")),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Concentration'", ns("conversion_type")),
          selectInput(ns("from_unit_c"), "From:", choices = c("mg/mL", "µg/mL", "ng/mL", "mol/L")),
          selectInput(ns("to_unit_c"), "To:", choices = c("mg/mL", "µg/mL", "ng/mL", "mol/L")),
          numericInput(ns("value_c"), "Value:", value = 1)
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Volume'", ns("conversion_type")),
          selectInput(ns("from_unit_v"), "From:", choices = c("L", "mL", "µL")),
          selectInput(ns("to_unit_v"), "To:", choices = c("L", "mL", "µL")),
          numericInput(ns("value_v"), "Value:", value = 1)
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Mass'", ns("conversion_type")),
          selectInput(ns("from_unit_m"), "From:", choices = c("g", "mg", "µg", "ng")),
          selectInput(ns("to_unit_m"), "To:", choices = c("g", "mg", "µg", "ng")),
          numericInput(ns("value_m"), "Value:", value = 1)
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Time'", ns("conversion_type")),
          selectInput(ns("from_unit_t"), "From:", choices = c("hr", "min", "sec")),
          selectInput(ns("to_unit_t"), "To:", choices = c("hr", "min", "sec")),
          numericInput(ns("value_t"), "Value:", value = 1)
        ),
        
        actionButton(ns("convert"), "Convert", class = "btn-primary"),
        actionButton(ns("refresh_converter"), "Refresh", class = "btn btn-primary"),
        
        
      ),
      
      mainPanel(
        wellPanel(
          h4("Converted Value:"),
          verbatimTextOutput(ns("result"))
        )
      )
      
    )
  )
}
unitConverterServer <- function(input, output, session, current_page) {
  ns <- session$ns
  result_text <- reactiveVal("")
  
  conversion_factors <- list(
    concentration = list(
      "mg/mL" = 1,
      "µg/mL" = 1000,
      "ng/mL" = 1e6,
      "mol/L" = NA  # Placeholder for unsupported
    ),
    volume = list("L" = 1, "mL" = 1000, "µL" = 1e6),
    mass = list("g" = 1, "mg" = 1000, "µg" = 1e6, "ng" = 1e9),
    time = list("hr" = 1, "min" = 60, "sec" = 3600)
  )
  
  observeEvent(input$convert, {
    type <- input$conversion_type
    value <- NULL
    result <- "Invalid input."
    
    if (type == "Concentration") {
      from <- input$from_unit_c
      to <- input$to_unit_c
      value <- input$value_c
      if (from == "mol/L" || to == "mol/L") {
        result <- "⚠️ Molar conversions need molar mass and are not supported yet."
      } else {
        factor <- conversion_factors$concentration[[to]] / conversion_factors$concentration[[from]]
        result <- paste(value * factor, to)
      }
      
    } else if (type == "Volume") {
      from <- input$from_unit_v
      to <- input$to_unit_v
      value <- input$value_v
      factor <- conversion_factors$volume[[to]] / conversion_factors$volume[[from]]
      result <- paste(value * factor, to)
      
    } else if (type == "Mass") {
      from <- input$from_unit_m
      to <- input$to_unit_m
      value <- input$value_m
      factor <- conversion_factors$mass[[to]] / conversion_factors$mass[[from]]
      result <- paste(value * factor, to)
      
    } else if (type == "Time") {
      from <- input$from_unit_t
      to <- input$to_unit_t
      value <- input$value_t
      factor <- conversion_factors$time[[to]] / conversion_factors$time[[from]]
      result <- paste(value * factor, to)
    }
    
    result_text(result)
  })
  
  output$result <- renderText({
    result_text()
  })
  
  # 🔙 Go back to home
  observeEvent(input$back_home, {
    current_page("home")
  })
  
  # 🔄 Refresh unit converter
  observeEvent(input$refresh_converter, {
    result_text("")
    updateNumericInput(session, "value_c", value = NULL)
    updateNumericInput(session, "value_v", value = NULL)
    updateNumericInput(session, "value_m", value = NULL)
    updateNumericInput(session, "value_t", value = NULL)
    updateSelectInput(session, "from_unit_c", selected = "mg/mL")
    updateSelectInput(session, "to_unit_c", selected = "µg/mL")
    updateSelectInput(session, "from_unit_v", selected = "mL")
    updateSelectInput(session, "to_unit_v", selected = "µL")
    updateSelectInput(session, "from_unit_m", selected = "mg")
    updateSelectInput(session, "to_unit_m", selected = "µg")
    updateSelectInput(session, "from_unit_t", selected = "min")
    updateSelectInput(session, "to_unit_t", selected = "sec")
  })
}
