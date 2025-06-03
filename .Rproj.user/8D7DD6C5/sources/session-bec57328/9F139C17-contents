growthUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    actionButton(ns("back"), "Home",class = "btn btn-primary", icon = icon("arrow-left")),
    
    titlePanel("Microbial Growth Kinetics (Bacterial Growth Curve)"),
    
    sidebarLayout(
      sidebarPanel(
        h3("Input Data"),
        textInput(ns("time_values"), "Enter Time (hours, comma-separated):"),
        textInput(ns("od_values"), "Enter OD Values (comma-separated):"),
        actionButton(ns("calculate_growth"), "Calculate Growth Rate & Doubling Time"),
        br(),
        actionButton(ns("refresh"), "Refresh"),
        br(),
        downloadButton(ns("download_plot"), "Download Plot"),
        downloadButton(ns("download_result"), "Download Results"),
        hr(),
        actionButton(ns("generate_report"), "Generate Report", icon = icon("file-alt"), class = "btn btn-info btn-block")
      ),
      
      mainPanel(
        wellPanel(
        plotOutput(ns("growth_plot")),
        verbatimTextOutput(ns("growth_result"))
        ),
        wellPanel(
          sliderInput(ns("xrange"), "X-axis Range (Time)", min = -100, max = 100, value = c(0, 100), step = 1),
          sliderInput(ns("yrange"), "Y-axis Range (OD Values)", min = -10, max = 10, value = c(-10, 10), step = 0.1),
          checkboxInput(ns("show_eqn_r2"), "Show Equation and R² on Plot", value = TRUE),
          checkboxInput(ns("show_intercepts"), "Show Intercepts", value = TRUE),
          checkboxInput(ns("show_grid"), "Show Grid Lines", value = TRUE),
          checkboxInput(ns("show_box"), "Show Box Around the Graph", value = TRUE),
        )
      )
    )
  )
}

growthServer <- function(input, output, session, current_page) {
  ns <- session$ns
  results_text <- reactiveVal("")
  plot_obj <- reactiveVal(NULL)
  
  # Growth model fitting (log10 OD vs time)
  fit <- reactive({
    req(input$time_values, input$od_values)
    time_values <- as.numeric(unlist(strsplit(as.character(input$time_values), ",")))
    od_values <- as.numeric(unlist(strsplit(as.character(input$od_values), ",")))
    
    if (length(time_values) != length(od_values)) {
      stop("Time values and OD values must be of the same length!")
    }
    
    log_od <- log10(od_values)
    lm(log_od ~ time_values)
  })
  
  # Refresh logic
  observeEvent(input$refresh, {
    updateTextInput(session, "time_values", value = "")
    updateTextInput(session, "od_values", value = "")
    updateSliderInput(session, "xrange", value = c(0, 100))
    updateSliderInput(session, "yrange", value = c(-10, 10))
    updateCheckboxInput(session, "show_eqn_r2", value = TRUE)
    updateCheckboxInput(session, "show_intercepts", value = TRUE)
    updateCheckboxInput(session, "show_grid", value = TRUE)
    updateCheckboxInput(session, "show_box", value = TRUE)
    results_text("")
    plot_obj(NULL)
  })
  
  # Calculate and plot
  observeEvent(input$calculate_growth, {
    req(input$time_values, input$od_values)
    
    time_values <- as.numeric(unlist(strsplit(as.character(input$time_values), ",")))
    od_values <- as.numeric(unlist(strsplit(as.character(input$od_values), ",")))
    
    if (length(time_values) != length(od_values)) {
      showNotification("Time and OD values must be the same length!", type = "error")
      return()
    }
    
    log_od <- log10(od_values)
    model <- fit()
    
    intercept <- coef(model)[1]
    slope <- coef(model)[2]
    r2 <- summary(model)$r.squared
    doubling_time <- log10(2) / slope
    x_intercept <- -intercept / slope
    
    plot_obj(function() {
      plot(time_values, log_od, 
           type = "p", col = "blue", pch = 20,
           xlab = "Time (hrs)", ylab = "log10(OD600)",
           main = "Bacterial Growth Curve (log OD vs Time)",
           xlim = input$xrange, ylim = input$yrange, axes = FALSE)
      
      # Add custom axes centered at (0,0)
      abline(h = 0, col = "gray40", lwd = 2)  # y=0 axis
      abline(v = 0, col = "gray40", lwd = 2)  # x=0 axis
      axis(1, pos = 0)  # x-axis ticks at y=0
      axis(2, pos = 0)  # y-axis ticks at x=0
      
      x_vals <- seq(min(time_values) - 1.5 * abs(min(time_values)),
                    max(time_values) + 1.5 * abs(max(time_values)),
                    length.out = 100)
      y_vals <- slope * x_vals + intercept
      lines(x_vals, y_vals, col = "red", lwd = 2)
      
      if (input$show_grid) grid()
      
      if (input$show_box) box()
      
      if(input$show_intercepts){
        # Mark Y-intercept (x = 0)
        points(0, intercept, pch = 4, col = "green", cex = 1.5)
        text(0, intercept, labels = sprintf("Y-Intercept= %.3f",intercept), pos = 3, col = "green")
        
        # Mark X-intercept (y = 0)
        
        points(x_intercept, 0, pch = 4, col = "green", cex = 1.5)
        text(x_intercept, 0, labels = sprintf("X-Intercept= %.3f",x_intercept), pos = 3, col = "green")
        
      }
      
      if (input$show_eqn_r2) {
        eqn <- sprintf("y = %.4fx + %.4f", slope, intercept)
        r2_text <- sprintf("R² = %.4f", r2)
        usr <- par("usr")
        text(x = usr[1] + 0.05 * (usr[2] - usr[1]),
             y = usr[4] - 0.05 * (usr[4] - usr[3]),
             labels = eqn, adj = 0, col = "purple", cex = 0.9)
        text(x = usr[1] + 0.05 * (usr[2] - usr[1]),
             y = usr[4] - 0.1 * (usr[4] - usr[3]),
             labels = r2_text, adj = 0, col = "purple", cex = 0.9)
      }
    })
    
    results_text(sprintf(
      "Growth curve plotted!\nGrowth Rate (\\u03bc): %.4f per hour\nDoubling Time (td): %.3f hours\nY-Intercept: %.3f\nX-Intercept: %.3f\nSlope: %.3f",
      slope, doubling_time,x_intercept, intercept, slope
    ))
  })
  
  # Render plot and result
  output$growth_plot <- renderPlot({
    plot_fn <- plot_obj()
    if (!is.null(plot_fn)) plot_fn()
  })
  
  output$growth_result <- renderText({
    results_text()
  })
  
  # Plot download
  output$download_plot <- downloadHandler(
    filename = function() { "growth_curve.png" },
    content = function(file) {
      if (!is.null(plot_obj())) {
        png(file)
        plot_obj()()
        dev.off()
      }
    }
  )
  
  # Text download
  output$download_result <- downloadHandler(
    filename = function() { "growth_results.txt" },
    content = function(file) {
      writeLines(results_text(), file)
    }
  )
  
  observeEvent(input$generate_report, {
    req(results_text(), plot_obj())  # Ensure required outputs are available
    
    ns <- session$ns  # Namespace for modular use
    
    temp_dir <- tempdir()
    report_rmd <- file.path(temp_dir, "growth_report.Rmd")
    plot_file <- file.path(temp_dir, "growth_plot.png")
    
    # Save the plot to a PNG file
    png(filename = plot_file, width = 800, height = 600)
    plot_obj()()  # Call the reactive plot function to draw the plot
    dev.off()
    
    # Create RMarkdown content
    rmd_content <- c(
      "---",
      "title: 'Growth Kinetics Report'",
      "output: pdf_document",
      "---",
      "",
      "# Experiment Name",
      "Bacterial Growth Curve Analysis",
      "",
      "# Aim",
      "To analyze bacterial growth by plotting log(OD600) versus time and estimate the doubling time.",
      "",
      "# Input Data",
      paste0("- Time Values (hours): ", input$time_values),
      paste0("- OD600 Values: ", input$od_values),
      "",
      "# Results",
      results_text(),
      "",
      "# Interpretation",
      "The bacterial growth was modeled by fitting a linear regression line to the log-transformed OD600 values over time. From this, the specific growth rate (μ) and doubling time (td) were estimated.",
      "",
      "# Growth Curve Plot",
      paste0("![](", basename(plot_file), ")")
    )
    
    # Write to .Rmd
    writeLines(rmd_content, report_rmd)
    
    # Render to PDF
    pdf_path <- rmarkdown::render(report_rmd, output_dir = temp_dir, quiet = TRUE)
    
    # Download handler
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("Growth_Kinetics_Report_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        file.copy(pdf_path, file, overwrite = TRUE)
      }
    )
    
    # Modal dialog for download
    showModal(modalDialog(
      title = "Report Ready",
      "Click below to download your report:",
      downloadButton(ns("download_report"), "Download Report"),
      easyClose = TRUE
    ))
  })
  
  
  
  # Go back to home
  observeEvent(input$back, {
    current_page("home")
  })
}

