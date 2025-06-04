ureaUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    actionButton(ns("back"),  "Home",class = "btn btn-primary", icon = icon("arrow-left")),
    
    titlePanel("Estimation of Urea by Diacetyl Monoxime Method"),
    
    sidebarLayout(
      sidebarPanel(
        h3("Input Data"),
        textInput(ns("od_values"), "Enter OD Values (comma-separated):"),
        textInput(ns("concentrations"), "Enter Urea Concentrations (mg/mL, comma-separated):"),
        actionButton(ns("calculate_urea"), "Plot Calibration Curve"),
        textInput(ns("unknown_od"), "Enter OD for Unknown Sample:"),
        actionButton(ns("estimate_urea"), "Estimate Urea Concentration"),
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
        plotOutput(ns("urea_plot")),
        verbatimTextOutput(ns("urea_result"))
        ),
        wellPanel(
          sliderInput(ns("xrange"), "X-axis Range (Concentration)", min = 0, max = 1000, value = c(0, 1000), step = 1),
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

ureaServer <- function(input, output, session, current_page) {
  ns <- session$ns
  results_text <- reactiveVal("")
  plot_obj <- reactiveVal(NULL)
  model_stats <- reactiveVal(list())
  
  
  fit <- reactive({
    req(input$concentrations, input$od_values)
    concentrations <- as.numeric(unlist(strsplit(input$concentrations, ",")))
    od_values <- as.numeric(unlist(strsplit(input$od_values, ",")))
    
    validate(
      need(length(concentrations) == length(od_values),
           "Urea concentrations and OD values must be of equal length.")
    )
    
    lm(od_values ~ concentrations)
  })
  
  
  observeEvent(input$refresh, {
    updateTextInput(session, "concentrations", value = "")
    updateTextInput(session, "od_values", value = "")
    updateNumericInput(session, "unknown_od", value = NA)
    updateSliderInput(session, "xrange", value = c(0, 200))
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
  
  # Plot Calibration Curve for Urea
  observeEvent(input$calculate_urea, {
    concentrations <- as.numeric(unlist(strsplit(input$concentrations, ",")))
    od_values <- as.numeric(unlist(strsplit(input$od_values, ",")))
    model <- fit()
    
    r2 <- summary(model)$r.squared
    slope <- coef(model)[2]
    intercept <- coef(model)[1]
    
    model_stats(list(r2 = r2, slope = slope, intercept = intercept))
    
    plot_obj(function() {
      plot(concentrations, od_values, type = "p", col = "blue", pch = 20,
           xlab = "Urea Concentration (mg/mL)", ylab = "OD Values",
           main = "Urea Calibration Curve",
           xlim = input$xrange, ylim = input$yrange)
      
      abline(model, col = "red", lwd = 1.5)
      
      # Show grid lines if enabled
      if (input$show_grid) grid()
      if (input$show_box) box()
      # Intercept lines if enabled and unknown OD provided
      if (input$show_ref && !is.na(input$unknown_od)) {
        unknown_od_val <- as.numeric(input$unknown_od)
        if (!is.na(unknown_od_val)) {
          predicted_conc <- (unknown_od_val - intercept) / slope
          abline(v = predicted_conc, col = "darkgreen", lty = 2, lwd = 1.5)
          abline(h = unknown_od_val, col = "darkgreen", lty = 2, lwd = 1.5)
          
          # Mark estimated point if enabled
          if (input$mark_unknown_point) {
            points(predicted_conc, unknown_od_val, pch = 19, col = "red", cex = 1.2)
            text(predicted_conc, unknown_od_val,
                 labels = sprintf("(%.2f, %.2f)", predicted_conc, unknown_od_val),
                 pos = 4, col = "red", cex = 0.9)
          }
        }
      }
      
      # Show equation and R²
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
  
  observeEvent(input$estimate_urea, {
    req(input$unknown_od)
    unknown_od <- as.numeric(input$unknown_od)
    model <- fit()
    
    slope <- coef(model)[2]
    intercept <- coef(model)[1]
    predicted_concentration <- (unknown_od - intercept) / slope
    
    new_msg <- sprintf("Estimated Urea Concentration: %.3f mg/mL", predicted_concentration)
    
    # Combine old + new
    full_text <- paste(results_text(), new_msg, sep = "\n\n")
    results_text(full_text)
  })
  
  output$urea_plot <- renderPlot({
    plot_fn <- plot_obj()
    if (!is.null(plot_fn)) plot_fn()
  })
  
  output$urea_result <- renderText({

    paste(
      results_text()  )
  })
  
  output$download_plot <- downloadHandler(
    filename = function() { "urea_calibration_curve.png" },
    content = function(file) {
      if (!is.null(plot_obj())) {
        png(file)
        plot_obj()()
        dev.off()
      }
    }
  )
  
  output$download_result <- downloadHandler(
    filename = function() { "urea_results.txt" },
    content = function(file) {
      writeLines(results_text(),file)
    }
  )
  
  observeEvent(input$generate_report, {
    req(results_text(), plot_obj())  # Ensure required data is available
    
    temp_dir <- tempdir()
    report_rmd <- file.path(temp_dir, "urea_report.Rmd")
    plot_file <- file.path(temp_dir, "urea_plot.png")
    
    # Save the plot to PNG file
    png(filename = plot_file)
    dev.off()
    
    # Create the RMarkdown content
    rmd_content <- c(
      "---",
      "title: 'Urea Estimation Report'",
      "output: pdf_document",
      "---",
      "",
      "# Experiment Name",
      "Urea Estimation using Calibration Curve",
      "",
      "# Aim",
      "Quantify unknown urea concentration using a calibration curve.",
      "",
      "# Input Data",
      paste0("- Standard Urea Concentrations: ", input$concentrations),
      paste0("- Standard OD Values: ", input$od_values),
      paste0("- Sample OD: ", input$unknown_od),
      "",
      "# Results",
      results_text(),
      "",
      "# Interpretation",
      "The urea concentration of the sample was determined using linear regression on the standard curve.",
      "",
      "# Plot",
      paste0("![](", basename(plot_file), ")")
    )
    
    # Write content to .Rmd
    writeLines(rmd_content, report_rmd)
    
    # Render to PDF
    pdf_path <- rmarkdown::render(report_rmd, output_dir = temp_dir, quiet = TRUE)
    
    # Namespace for Shiny modules
    ns <- session$ns
    
    # Download handler
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("Urea_Estimation_Report_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        file.copy(pdf_path, file, overwrite = TRUE)
      }
    )
    
    # Modal dialog to trigger download
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

