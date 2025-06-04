# UI Function
thermalUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    actionButton(ns("back"), "Home",class = "btn btn-primary", icon = icon("arrow-left")),
    
    titlePanel("Thermal Death Kinetics (Microbial Inactivation)"),
    
    sidebarLayout(
      sidebarPanel(
        h3("Choose Analysis"),
        radioButtons(ns("option"), "Select Option:",
                     choices = c("1) Calculate D-value and TDT" = "dtdt",
                                 "2) Calculate Z-value" = "zval")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'dtdt'", ns("option")),
          textInput(ns("times"), "Enter Time (min, comma-separated):"),
          textInput(ns("colonies"), "Enter Number of Colonies (comma-separated):"),
          numericInput(ns("dilution_factor"), "Dilution Factor (e.g., 1000):", value = 1000),
          numericInput(ns("plated_volume"), "Plated Volume (in mL):", value = 1),
          actionButton(ns("calc_d"), "Calculate D-value and TDT")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'zval'", ns("option")),
          textInput(ns("temps"), "Enter Temperatures (°C, comma-separated):"),
          textInput(ns("dvals"), "Enter D-values (min, comma-separated):"),
          actionButton(ns("calc_z"), "Calculate Z-value")
        ),
        br(),
        actionButton(ns("refresh"), "Refresh"),
        br(),
        downloadButton(ns("download_plot"), "Download Plot"),
        downloadButton(ns("download_result"), "Download Results"),
        hr(),
        #actionButton(ns("generate_report"), "Generate Report", icon = icon("file-alt"), class = "btn btn-info btn-block")
      ),
      
      mainPanel(
        wellPanel(
        plotOutput(ns("thermal_plot")),
        verbatimTextOutput(ns("thermal_result"))
        ),
        wellPanel(
          sliderInput(ns("xrange"), "X-axis Range ", min = -1000, max = 1000, value = c(-100, 100), step = 1),
          sliderInput(ns("yrange"), "Y-axis Range ", min = -100, max = 100, value = c(-10, 10), step = 0.1),
          checkboxInput(ns("show_ref1"), "Show Refernce Lines for TDT", value = FALSE),
          checkboxInput(ns("show_ref2"), "Show Refernce Lines for D Value", value = FALSE),
          checkboxInput(ns("show_ref3"), "Show Refernce Lines for Z Value", value = FALSE),
          checkboxInput(ns("show_eqn_r2"), "Show Equation and R² on Plot", value = TRUE),
          checkboxInput(ns("show_grid"), "Show Grid Lines", value = TRUE),
          checkboxInput(ns("show_box"), "Show Box Around the Graph", value = TRUE),
          checkboxInput(ns("mark_unknown_point1"), "Mark Estimated Point for TDT", value = TRUE),
          checkboxInput(ns("mark_unknown_point2"), "Mark Estimated Point for D value", value = TRUE),
          checkboxInput(ns("mark_unknown_point3"), "Mark Estimated Point for Z Value", value = TRUE),
          checkboxInput(ns("mark_intercepts"), "Show and Mark Intercepts with its Value", value = TRUE),
                  )
      )
    )
  )
}

thermalServer <- function(input, output, session, current_page) {
  ns <- session$ns
  results_text <- reactiveVal("")
  plot_obj <- reactiveVal(NULL)
  
  # D-value and TDT Calculation
  observeEvent(input$calc_d, {
    times <- as.numeric(unlist(strsplit(input$times, ",")))
    colonies <- as.numeric(unlist(strsplit(input$colonies, ",")))
    dilution_factor <- input$dilution_factor
    plated_volume <- input$plated_volume
    
    cfu <- colonies * dilution_factor / plated_volume
    logN <- log10(cfu)
    
    model <- lm(logN ~ times)
    r2 <- summary(model)$r.squared
    slope <- coef(model)[2]
    intercept <- coef(model)[1]
    x_intercept <- -intercept / slope
    
    D_value <- -1 / slope
    TDT <- (log10(cfu[1]) - 0) * D_value
    
    plot_obj(function() {
      # Base scatter plot
      plot(times, logN,
           main = "D-value & TDT Plot",
           xlab = "Time (min)",
           ylab = "log(N) (CFU/mL)",
           xlim = input$xrange,
           ylim = input$yrange,
           col = "blue",
           pch = 19,
           axes = FALSE)  # Disable default axes
      
      # Add custom x and y axes at zero
      abline(h = 0, col = "gray40", lwd = 2)  # x-axis at y = 0
      abline(v = 0, col = "gray40", lwd = 2)  # y-axis at x = 0
      
      # Add custom axis tick marks and labels
      axis(1, pos = 0)  # x-axis ticks at y = 0
      axis(2, pos = 0)  # y-axis ticks at x = 0
      
      # Optional: draw a box around the plot
      if (input$show_box) box()
      
      # Draw extended regression line
      x_vals <- seq(min(times) - 1.5 * abs(min(times)), max(times) + 1.5 * abs(max(times)), length.out = 100)
      y_vals <- slope * x_vals + intercept
      lines(x_vals, y_vals, col = "red", lwd = 2)
      
      
      if (input$show_grid) grid()
      
      if (input$show_ref1) {
        abline(h = 0, col = "purple", lty = 2, lwd = 1.5)          # Horizontal at predicted μ
        abline(v = TDT, col = "green", lty = 2, lwd = 1.5)     # Vertical at S
      }
      if (input$mark_unknown_point1) {
        # Plot the predicted point
        points(TDT, 0,pch = 19, col = "blue", cex = 1.5)
        
        # Add a label near the point
        label_text <- sprintf("(%.2f min,%.2f CFU/ml )", TDT, 0)
        text(TDT,0, labels = label_text,
             pos = 4, col = "red", cex = 0.9, offset = 0.5)
      }
      
      if (input$show_ref2) {
        abline(h = intercept - 1, col = "purple", lty = 2, lwd = 1.5)          # Horizontal at predicted μ
        abline(v = (intercept - 1 - intercept) / slope, col = "green", lty = 2, lwd = 1.5)     # Vertical at S
      }
      if (input$mark_unknown_point2) {
        # Plot the predicted point
        points((intercept - 1 - intercept) / slope, intercept - 1,pch = 19, col = "blue", cex = 1.5)
        
        # Add a label near the point
        label_text <- sprintf("(%.2f min,%.2f CFU/ml )", (intercept - 1 - intercept) / slope, intercept - 1)
        text((intercept - 1 - intercept) / slope,intercept - 1, labels = label_text,
             pos = 4, col = "red", cex = 0.9, offset = 0.5)
      }
      if (input$show_eqn_r2) {
        # Format the equation: log10(N) = slope * time + intercept
        eqn <- sprintf("y = %.4f x + %.4f", slope, intercept)
        r2_text <- sprintf("R² = %.4f", r2)
        
        usr <- par("usr")  # Get plot coordinates
        
        text(x = usr[2] - 0.02 * (usr[2] - usr[1]), 
             y = usr[4] - 0.05 * (usr[4] - usr[3]), 
             labels = eqn, adj = 1, col = "purple", cex = 0.9)
        
        text(x = usr[2] - 0.02 * (usr[2] - usr[1]), 
             y = usr[4] - 0.1 * (usr[4] - usr[3]), 
             labels = r2_text, adj = 1, col = "purple", cex = 0.9)
      }
      
      if (input$mark_intercepts){
        points(0, intercept, pch = 4, col = "green", cex = 1.5)
        text(0, intercept, labels = sprintf("Y-Intercept =%.2f", intercept), pos = 3, col = "green")
        points(x_intercept, 0, pch = 4, col = "green", cex = 1.5)
        text(x_intercept, 0, labels = sprintf("X-Intercept =%.2f", x_intercept), pos = 3, col = "green")
        
      }
      
    })
    
    interpretation <- if (D_value < 5) "Good thermal resistance from the Microorganism" else "Poor resistance to temperature by the Micro Organism"
    results_text(sprintf(
      "D-value: %.6f min\nThermal Death Time (TDT): %.6f min\nInterpretation: %s\nSlope:%.3f\nR Square: %.3f\nY- Intercept: %.3f\n X - Intercept: %.3f\n Equation: %s",
      D_value, TDT, interpretation, slope, r2, intercept,x_intercept, eqn
    ))
  })
  
  # Z-value Calculation
  observeEvent(input$calc_z, {
    temps <- as.numeric(unlist(strsplit(input$temps, ",")))
    dvals <- as.numeric(unlist(strsplit(input$dvals, ",")))
    logD <- log10(dvals)
    model <- lm(logD ~ temps)
    r2 <- summary(model)$r.squared
    
    slope <- coef(model)[2]
    intercept <- coef(model)[1]
    x_intercept <- -intercept / slope
    
    Z_value <- -1 / slope
    
    plot_obj(function() {
      plot(temps, logD,
           main = "Z-value Plot",
           xlab = "Temperature (°C)",
           ylab = "log(D) (min)",
           xlim = input$xrange,
           ylim = input$yrange,
           col = "blue",
           pch = 19,
           axes = FALSE)
      
      # Add custom central axes at (0, 0)
      abline(h = 0, col = "gray40", lwd = 2)
      abline(v = 0, col = "gray40", lwd = 2)
      axis(1, pos = 0)
      axis(2, pos = 0)
      if (input$show_box) box()
      
      # Extended regression line
      x_vals <- seq(min(temps) - 1.5 * abs(min(temps)),
                    max(temps) + 1.5 * abs(max(temps)), length.out = 100)
      y_vals <- slope * x_vals + intercept
      lines(x_vals, y_vals, col = "red", lwd = 2)
      
      if (input$show_grid) grid()
      
      if (input$show_ref3) {
        
        # Horizontal line: log(D2 = D1/10), which is 1 log unit lower
        abline(h = intercept - 1, col = "purple", lty = 2, lwd = 1.5)
        
        # Vertical line: Temperature at which log(D2) occurs
        T_z <- (intercept - 1 - intercept) / slope
        abline(v = T_z, col = "green", lty = 2, lwd = 1.5)
      }
      
      if (input$mark_unknown_point3) {
        # Coordinates of the Z-value point (T_z, log(D2))
        T_z <- (intercept - 1 - intercept) / slope
        logD2 <- intercept - 1
        
        # Plot the point
        points(T_z, logD2, pch = 19, col = "blue", cex = 1.5)
        
        # Annotate the point
        label_text <- sprintf("(%.2f °C, %.2f log10 min)", T_z, logD2)
        text(T_z, logD2, labels = label_text, pos = 4, col = "red", cex = 0.9, offset = 0.5)
      }
      
      if (input$show_eqn_r2) {
        # Equation for Z-value plot
        eqn <- sprintf("y = %.4f x + %.4f", slope, intercept)
        r2_text <- sprintf("R² = %.4f", r2)
        
        usr <- par("usr")  # Get plot bounds
        
        text(x = usr[2] - 0.02 * (usr[2] - usr[1]), 
             y = usr[4] - 0.05 * (usr[4] - usr[3]), 
             labels = eqn, adj = 1, col = "purple", cex = 0.9)
        
        text(x = usr[2] - 0.02 * (usr[2] - usr[1]), 
             y = usr[4] - 0.1 * (usr[4] - usr[3]), 
             labels = r2_text, adj = 1, col = "purple", cex = 0.9)
      }
      
      if (input$mark_intercepts){
        points(0, intercept, pch = 4, col = "green", cex = 1.5)
        text(0, intercept, labels = sprintf("Y-Intercept =%.2f", intercept), pos = 3, col = "green")
        points(x_intercept, 0, pch = 4, col = "green", cex = 1.5)
        text(x_intercept, 0, labels = sprintf("X-Intercept =%.2f", x_intercept), pos = 3, col = "green")
        
      }
      
      
    })
    
    interpretation <- if (Z_value > 5) "Micro Organism is Stable across temperatures." else "Micro Organism is Sensitive to temperature change."
    results_text(sprintf("Z-value: %.6f °C\nInterpretation: %s\nSlope:%.3f\nR Square: %.3f\nY- Intercept: %.3f\n X - Intercept: %.3f\n Equation: %s", Z_value, interpretation, slope, r2, intercept,x_intercept, eqn))
  })
  
  output$thermal_plot <- renderPlot({
    plot_fn <- plot_obj()
    if (!is.null(plot_fn)) plot_fn()
  })
  
  output$thermal_result <- renderText({ results_text() })
  
  observeEvent(input$refresh, {
    # Reset radio button selection (optional)
    updateRadioButtons(session, "option", selected = character(0))
    
    # Clear all input fields
    updateTextInput(session, "times", value = "")
    updateTextInput(session, "colonies", value = "")
    updateTextInput(session, "temps", value = "")
    updateTextInput(session, "dvals", value = "")
    updateNumericInput(session, "dilution_factor", value = 1000)
    updateNumericInput(session, "plated_volume", value = 1)
    updateSliderInput(session, "xrange", value = c(-100, 100))  # Adjust as needed for Lowry
    updateSliderInput(session, "yrange", value = c(-10, 10))
    updateCheckboxInput(session, "show_intercepts", value = FALSE)
    updateCheckboxInput(session, "show_eqn_r2", value = TRUE)
    updateCheckboxInput(session, "show_grid", value = TRUE)
    updateCheckboxInput(session, "show_box", value = TRUE)
    updateCheckboxInput(session, "mark_unknown_point", value = TRUE)
    updateCheckboxInput(session, "mark_intercepts", value = TRUE)
    
    # Clear results and plot
    results_text("")
    plot_obj(NULL)
  })
  
  
  
  # ✅ Download Handlers
  output$download_plot <- downloadHandler(
    filename = function() { "thermal_plot.png" },
    content = function(file) {
      if (!is.null(plot_obj())) {
        png(file)
        plot_obj()()
        dev.off()
      }
    }
  )
  
  output$download_result <- downloadHandler(
    filename = function() { "thermal_results.txt" },
    content = function(file) {
      writeLines(results_text(), file)
    }
  )
  
  # ✅ Report Generation
  observeEvent(input$generate_report, {
    req(results_text(), plot_obj())  # Ensure required outputs are available
    
    temp_dir <- tempdir()
    report_rmd <- file.path(temp_dir, "thermal_report.Rmd")
    plot_file <- file.path(temp_dir, "thermal_plot.png")
    
    # Save the plot to a PNG file
    png(filename = plot_file)
    dev.off()
    
    # Namespace for modular usage
    ns <- session$ns
    
    # Create the RMarkdown content
    rmd_content <- c(
      "---",
      "title: 'Thermal Death Kinetics Report'",
      "output: pdf_document",
      "---",
      "",
      "# Experiment Name",
      "Thermal Death Kinetics",
      "",
      "# Aim",
      "To determine the D-value, Z-value, and Thermal Death Time (TDT) of a microbial culture.",
      "",
      "# Input Data",
      paste0("- Time Points (min): ", input$times),
      paste0("- Colony Counts (CFU): ", input$colonies),
      paste0("- Dilution Factor: ", input$dilution_factor),
      paste0("- Plated Volume (mL): ", input$plated_volume),
      paste0("- Temperature Points (°C): ", input$temps),
      paste0("- D-values (min): ", input$dvals),
      "",
      "# Results",
      results_text(),
      "",
      "# Interpretation",
      "The microbial culture's heat resistance was evaluated using D-value (time to reduce 90% of population), Z-value (temperature change for 10× reduction in D-value), and TDT (total time to kill population at set temperature).",
      "",
      "# Plot",
      paste0("![](", basename(plot_file), ")")
    )
    
    # Write content to .Rmd file
    writeLines(rmd_content, report_rmd)
    
    # Render the Rmd file into a PDF
    pdf_path <- rmarkdown::render(report_rmd, output_dir = temp_dir, quiet = TRUE)
    
    # Define the download handler
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("Thermal_Death_Kinetics_Report_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        file.copy(pdf_path, file, overwrite = TRUE)
      }
    )
    
    # Show download modal
    showModal(modalDialog(
      title = "Report Ready",
      "Click below to download your report:",
      downloadButton(ns("download_report"), "Download Report"),
      easyClose = TRUE
    ))
  })
  
  
  observeEvent(input$back, {
    current_page("home")
  })
}
