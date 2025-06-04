lowryUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    actionButton(ns("back"), "Home",class = "btn btn-primary", icon = icon("arrow-left")),
    
    titlePanel("Lowry's Method - Protein Estimation"),
    
    sidebarLayout(
      sidebarPanel(
        h3("Input Data"),
        textInput(ns("od_values"), "Enter OD Values (comma-separated):"),
        textInput(ns("concentrations"), "Enter Protein Concentrations (mg/mL, comma-separated):"),
        actionButton(ns("calculate_lowry"), "Plot Calibration Curve"),
        textInput(ns("unknown_od"), "Enter OD for Unknown Sample:"),
        actionButton(ns("estimate_lowry"), "Estimate Protein Concentration"),
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
        plotOutput(ns("lowry_plot")),
        verbatimTextOutput(ns("lowry_result"))
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
lowryServer <- function(input, output, session, current_page) {
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
           "Protein concentrations and OD values must be of equal length.")
    )
    
    lm(od_values ~ concentrations)
  })
  
  observeEvent(input$refresh, {
    updateTextInput(session, "concentrations", value = "")
    updateTextInput(session, "od_values", value = "")
    updateNumericInput(session, "unknown_od", value = NA)
    updateSliderInput(session, "xrange", value = c(0, 200))  # Adjust as needed for Lowry
    updateSliderInput(session, "yrange", value = c(0, 2))
    updateCheckboxInput(session, "show_ref", value = FALSE)
    updateCheckboxInput(session, "show_eqn_r2", value = TRUE)
    updateCheckboxInput(session, "show_grid", value = TRUE)
    updateCheckboxInput(session, "show_box", value = TRUE)
    updateCheckboxInput(session, "mark_unknown_point", value = TRUE)
    results_text("")
    plot_obj(NULL)
    model_stats(list())
  })
  
  observeEvent(input$calculate_lowry, {
    concentrations <- as.numeric(unlist(strsplit(input$concentrations, ",")))
    od_values <- as.numeric(unlist(strsplit(input$od_values, ",")))
    model <- fit()
    
    r2 <- summary(model)$r.squared
    slope <- coef(model)[2]
    intercept <- coef(model)[1]
    
    model_stats(list(r2 = r2, slope = slope, intercept = intercept))
    
    plot_obj(function() {
      plot(concentrations, od_values, type = "p", col = "blue", pch = 20,
           xlab = "Protein Concentration (mg/mL)", ylab = "OD Values",
           main = "Lowry Method Calibration Curve",
           xlim = input$xrange, ylim = input$yrange)
      
      abline(model, col = "red", lwd = 1.5)
      
      if (input$show_grid) grid()
      
      if (input$show_box) box()
      
      if (input$show_ref && !is.na(input$unknown_od)) {
        unknown_od_val <- as.numeric(input$unknown_od)
        if (!is.na(unknown_od_val)) {
          predicted_conc <- (unknown_od_val - intercept) / slope
          abline(v = predicted_conc, col = "darkgreen", lty = 2, lwd = 1.5)
          abline(h = unknown_od_val, col = "darkgreen", lty = 2, lwd = 1.5)
          
          if (input$mark_unknown_point) {
            points(predicted_conc, unknown_od_val, pch = 19, col = "red", cex = 1.2)
            text(predicted_conc, unknown_od_val,
                 labels = sprintf("(%.3f, %.3f)", predicted_conc, unknown_od_val),
                 pos = 4, col = "red", cex = 0.9)
          }
        }
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
    
    result_msg <- sprintf(
      paste(
        "Calibration curve plotted!",
        "Slope: %.4f",
        "Intercept: %.4f",
        "R-squared (R²): %.4f",
        "Enter an unknown OD value for protein concentration estimation.",
        sep = "\n"
      ),
      slope, intercept, r2
    )
    results_text(result_msg)
  })
  
  observeEvent(input$estimate_lowry, {
    req(input$unknown_od)
    unknown_od <- as.numeric(input$unknown_od)
    model <- fit()
    
    slope <- coef(model)[2]
    intercept <- coef(model)[1]
    predicted_concentration <- (unknown_od - intercept) / slope
    
    # Append the estimated concentration to the existing results_text
    new_msg <- sprintf("Estimated Protein Concentration: %.3f mg/mL", predicted_concentration)
    
    # Combine old + new
    full_text <- paste(results_text(), new_msg, sep = "\n\n")
    results_text(full_text)
  })
  
  output$lowry_plot <- renderPlot({
  plot_fn <- plot_obj()
  if (!is.null(plot_fn)) plot_fn()
})

output$lowry_result <- renderText({

  paste(
    results_text()
  )
})
  
  
  output$download_plot <- downloadHandler(
    filename = function() { "lowry_calibration_curve.png" },
    content = function(file) {
      if (!is.null(plot_obj())) {
        png(file)
        plot_obj()()
        dev.off()
      }
    }
  )
  
  output$download_result <- downloadHandler(
    filename = function() { "lowry_results.txt" },
    content = function(file) {
      writeLines(results_text(),file)
    }
  )
  
  observeEvent(input$generate_report, {
    req(results_text(), plot_obj())  # Ensure results and plot are available
    
    temp_dir <- tempdir()
    report_rmd <- file.path(temp_dir, "lowry_report.Rmd")
    plot_file <- file.path(temp_dir, "lowry_plot.png")
    
    # Save plot to PNG file
    png(filename = plot_file)

    dev.off()
    
    # Create R Markdown content with plot embedded using the correct relative path
    rmd_content <- c(
      "---",
      "title: 'Lowry Method Report'",
      "output: pdf_document",
      "---",
      "",
      "# Experiment Name",
      "Lowry Protein Estimation",
      "",
      "# Aim",
      "Quantify unknown protein concentration using Lowry's method calibration curve.",
      "",
      "# Input Data",
      paste0("- Standard Protein Concentrations: ", input$concentrations),
      paste0("- Standard OD Values: ", input$od_values),
      paste0("- Sample OD: ", input$unknown_od),
      "",
      "# Results",
      results_text(),
      "",
      "# Interpretation",
      "The protein concentration of the sample was determined using linear regression on the standard curve.",
      "",
      "# Plot",
      paste0("![](", basename(plot_file), ")")
    )
    
    # Write Rmd content to file
    writeLines(rmd_content, report_rmd)
    
    # Render Rmd to PDF in the temp directory
    pdf_path <- rmarkdown::render(report_rmd, output_dir = temp_dir, quiet = TRUE)
    
    # Create download handler inside observeEvent using session$ns for namespacing if needed
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("Lowry_Method_Report_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        file.copy(pdf_path, file, overwrite = TRUE)
      }
    )
    
    # Show modal with download button (ensure ns() used if in a module)
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


