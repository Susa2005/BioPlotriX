anthroneUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    actionButton(ns("back"),  "Home",class = "btn btn-primary", icon = icon("arrow-left")),
    
    titlePanel("Anthrone Method (Carbohydrate Estimation)"),
    
    sidebarLayout(
      sidebarPanel(
        h3("Input Data"),
        textInput(ns("od_values"), "Enter OD Values (comma-separated):"),
        textInput(ns("concentrations"), "Enter Carbohydrate Concentrations (mg/mL, comma-separated):"),
        actionButton(ns("calculate_anthrone"), "Plot Calibration Curve"),
        textInput(ns("unknown_od"), "Enter OD for Unknown Sample:"),
       
        
        actionButton(ns("estimate_anthrone"), "Estimate Carbohydrate Concentration"),
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
        plotOutput(ns("anthrone_plot")),
        verbatimTextOutput(ns("anthrone_result"))
        ),
        wellPanel(
          sliderInput(ns("xrange"), "X-axis Range (Concentration)", min = 0, max = 1000, value = c(0, 1000), step = 10),
          sliderInput(ns("yrange"), "Y-axis Range (OD)", min = 0, max = 2, value = c(0, 1), step = 0.01),
          checkboxInput(ns("show_ref"), "Show Reference Lines", value = FALSE),
          checkboxInput(ns("show_eqn_r2"), "Show Equation and R² on Plot", value = TRUE),
          checkboxInput(ns("show_grid"), "Show Grid Lines", value = TRUE),
          checkboxInput(ns("show_box"), "Show Box Around the Graph", value = TRUE),
          checkboxInput(ns("mark_unknown_point"), "Mark Estimated Point on Graph", value = TRUE)
          
        )
      )
    )
  )
}

anthroneServer <- function(input, output, session, current_page) {
  ns <- session$ns
  results_text <- reactiveVal("")
  plot_obj <- reactiveVal(NULL)
  model_stats <- reactiveVal(list())
  
  fit <- reactive({
    req(input$od_values, input$concentrations)
    od_values <- as.numeric(unlist(strsplit(input$od_values, ",")))
    concentrations <- as.numeric(unlist(strsplit(input$concentrations, ",")))
    
    if (length(od_values) != length(concentrations)) {
      stop("OD values and concentrations must have the same length!")
    }
    
    lm(od_values ~ concentrations)
  })
  
  # Refresh
  observeEvent(input$refresh, {
    updateTextInput(session, "concentrations", value = "")
    updateTextInput(session, "od_values", value = "")
    updateNumericInput(session, "unknown_od", value = NA)
    updateSliderInput(session, "xrange", value = c(0, 1000))
    updateSliderInput(session, "yrange", value = c(0, 1))
    updateCheckboxInput(session, "show_ref", value = FALSE)
    updateCheckboxInput(session, "show_eqn_r2", value = TRUE)
    updateCheckboxInput(session, "show_grid", value = TRUE)
    updateCheckboxInput(session, "show_box", value = TRUE)
    updateCheckboxInput(session, "mark_unknown_point", value = TRUE)
    results_text("")
    plot_obj(NULL)
    model_stats(list())
  })
  
  # Plot Calibration Curve
  observeEvent(input$calculate_anthrone, {
    od_values <- as.numeric(unlist(strsplit(input$od_values, ",")))
    concentrations <- as.numeric(unlist(strsplit(input$concentrations, ",")))
    model <- fit()
    
    r2 <- summary(model)$r.squared
    slope <- coef(model)[2]
    intercept <- coef(model)[1]
    
    model_stats(list(r2 = r2, slope = slope, intercept = intercept))
    
    plot_obj(function() {
      plot(concentrations, od_values, type = "p", col = "blue", pch = 20,
           xlab = "Protein Concentration (mg/mL)", ylab = "OD Values",
           main = "Lowry Calibration Curve",
           xlim = input$xrange, ylim = input$yrange)
      
      abline(model, col = "red", lwd = 1.5)
      
      # ✅ Show grid lines if enabled
      if (input$show_grid) grid()
      
      if (input$show_box) box()
      
      # ✅ Intercept lines if enabled and unknown OD provided
      if (input$show_ref && !is.na(input$unknown_od)) {
        unknown_od_val <- suppressWarnings(as.numeric(input$unknown_od))
        if (!is.na(unknown_od_val)) {
          predicted_conc <- (unknown_od_val - intercept) / slope
          
          abline(v = predicted_conc, col = "darkgreen", lty = 2, lwd = 1.5)
          abline(h = unknown_od_val, col = "darkgreen", lty = 2, lwd = 1.5)
          
          # ✅ Mark estimated point if enabled
          if (input$mark_unknown_point) {
            points(predicted_conc, unknown_od_val, pch = 19, col = "red", cex = 1.2)
            text(predicted_conc, unknown_od_val,
                 labels = sprintf("(%.2f mg/mL, %.2f)", predicted_conc, unknown_od_val),
                 pos = 4, col = "red", cex = 0.9)
          }
        }
      }
      
      # ✅ Show equation and R² on right side if enabled
      if (input$show_eqn_r2) {
        eqn <- sprintf("y = %.4fx + %.4f", slope, intercept)
        r2_text <- sprintf("R² = %.4f", r2)
        usr <- par("usr")
        
        text(x = usr[2] - 0.02 * (usr[2] - usr[1]), 
             y = usr[4] - 0.05 * (usr[4] - usr[3]), 
             labels = eqn, adj = 1, col = "purple", cex = 0.9)
        text(x = usr[2] - 0.02 * (usr[2] - usr[1]), 
             y = usr[4] - 0.1 * (usr[4] - usr[3]), 
             labels = r2_text, adj = 1, col = "purple", cex = 0.9)
      }
    })
    
    
    
    # Text summary output
    result_msg <- sprintf(
      paste(
        "Calibration curve plotted!",
        "Slope: %.4f",
        "Intercept: %.4f",
        "R-squared (R²): %.4f",
        "Enter an unknown OD value for estimation.",
        sep = "\n"
      ),
      slope, intercept, r2
    )
    results_text(result_msg)
  })
  
  # Estimate Unknown OD
  observeEvent(input$estimate_anthrone, {
    req(input$unknown_od)
    unknown_od <- as.numeric(input$unknown_od)
    model <- fit()
    
    slope <- coef(model)[2]
    intercept <- coef(model)[1]
    predicted_concentration <- (unknown_od - intercept) / slope
    
    results_text(sprintf("Estimated Carbohydrate Concentration: %.3f mg/mL", predicted_concentration))
  })
  
  
  output$anthrone_plot <- renderPlot({
    plot_fn <- plot_obj()
    if (!is.null(plot_fn)) plot_fn()
  })
  
  output$anthrone_result <- renderText({

    paste0(
      results_text()
    )
  })
  
  output$download_plot <- downloadHandler(
    filename = function() { "anthrone_calibration_curve.png" },
    content = function(file) {
      if (!is.null(plot_obj())) {
        png(file)
        plot_obj()()
        dev.off()
      }
    }
  )
  
  output$download_result <- downloadHandler(
    filename = function() { "anthrone_results.txt" },
    content = function(file) {
      writeLines(results_text(),file)
    }
  )
  
  observeEvent(input$generate_report, {
    req(results_text(), plot_obj())  # Ensure results and plot are available
    
    temp_dir <- tempdir()
    report_rmd <- file.path(temp_dir, "anthrone_report.Rmd")
    plot_file <- file.path(temp_dir, "anthrone_plot.png")
    
    # Save plot to PNG file
    png(filename = plot_file)
    dev.off()
    
    # Create R Markdown content
    rmd_content <- c(
      "---",
      "title: 'Anthrone Method Report'",
      "output: pdf_document",
      "---",
      "",
      "# Experiment Name",
      "Anthrone Method",
      "",
      "# Aim",
      "Quantify unknown carbohydrate concentration using a calibration curve.",
      "",
      "# Input Data",
      paste0("- Standard Carbohydrate Concentrations: ", input$concentrations),
      paste0("- Standard OD Values: ", input$od_values),
      paste0("- Sample OD: ", input$unknown_od),
      "",
      "# Results",
      results_text(),
      "",
      "# Interpretation",
      "The carbohydrate concentration of the sample was determined using linear regression on the standard curve.",
      "",
      "# Plot",
      paste0("![](", basename(plot_file), ")")
    )
    
    # Write Rmd content to file
    writeLines(rmd_content, report_rmd)
    
    # Render Rmd to PDF
    pdf_path <- rmarkdown::render(report_rmd, output_dir = temp_dir, quiet = TRUE)
    
    # Namespacing for modular Shiny
    ns <- session$ns
    
    # Create download handler
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("Anthrone_Method_Report_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        file.copy(pdf_path, file, overwrite = TRUE)
      }
    )
    
    # Modal dialog with download button
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
