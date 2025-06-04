monodUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    actionButton(ns("back"),  "Home",class = "btn btn-primary", icon = icon("arrow-left")),
    
    titlePanel("Microbial Growth Kinetics using Monod's Model"),
    
    sidebarLayout(
      sidebarPanel(
        h4("Enter Data"),
        textInput(ns("substrate"), "Substrate Concentrations (S) [comma-separated, g/L]:"),
        textInput(ns("growth_rate"), "Specific Growth Rates (μ) [comma-separated, 1/h]:"),
        actionButton(ns("calc_monod"), "Fit Monod Model"),
        br(),
        numericInput(ns("predict_s"), "Predict μ for Substrate (S)", value = NA),
        actionButton(ns("predict_monod"), "Predict Growth Rate"),
        br(),
        actionButton(ns("refresh"), "Refresh"),
        br(),
        downloadButton(ns("download_plot"), "Download Plot"),
        downloadButton(ns("download_result"), "Download Result"),
        hr(),
        #actionButton(ns("generate_report"), "Generate Report", icon = icon("file-alt"), class = "btn btn-info btn-block")
      ),
      mainPanel(
        wellPanel(
        plotOutput(ns("monod_plot")),
        verbatimTextOutput(ns("monod_result")),
        verbatimTextOutput(ns("monod_prediction"))
        ),
        wellPanel(
          sliderInput(ns("xrange"), "X-axis Range (Concentration)", min = 0, max = 1000, value = c(0, 100), step = 1),
          sliderInput(ns("yrange"), "Y-axis Range (Growth rates)", min = 0, max = 100, value = c(0, 100), step = 1),
          checkboxInput(ns("show_intercepts"), "Show Intercept Lines", value = TRUE),
          checkboxInput(ns("show_ref"), "Show Reference Lines", value = TRUE),
          checkboxInput(ns("show_eqn_r2"), "Show Equation and R² on Plot", value = TRUE),
          checkboxInput(ns("show_grid"), "Show Grid Lines", value = TRUE),
          checkboxInput(ns("show_box"), "Show Box Around the Graph", value = TRUE),
          checkboxInput(ns("mark_unknown_point"), "Mark Estimated Point on Graph", value = TRUE)
        )
      )
    )
  )
}
monodServer <- function(input, output, session, current_page) {
  ns <- session$ns
  results_text <- reactiveVal("")
  prediction_text <- reactiveVal("")
  plot_obj <- reactiveVal(NULL)
  fitted_model <- reactiveVal(NULL)
  result <- reactiveValues(mu_pred1 = NULL)
  
  
  
  observeEvent(input$calc_monod, {
    
    x_min <- input$xrange[1]
    x_max <- input$xrange[2]
    y_min <- input$yrange[1]
    y_max <- input$yrange[2]
    
    S <- as.numeric(unlist(strsplit(input$substrate, ",")))
    mu <- as.numeric(unlist(strsplit(input$growth_rate, ",")))
    
    # Input validation
    if (length(S) != length(mu) || any(is.na(S)) || any(is.na(mu)) || length(S) < 3) {
      results_text("⚠️ Error: Please enter equal-length numeric substrate and growth rate values with at least 3 data points.")
      plot_obj(NULL)
      fitted_model(NULL)
      return()
    }
    
    # Attempt model fitting
    model <- tryCatch({
      nls(mu ~ mumax * S / (Ks + S),
          start = list(mumax = max(mu, na.rm = TRUE), Ks = mean(S, na.rm = TRUE)),
          control = nls.control(maxiter = 100, warnOnly = TRUE))
    }, error = function(e) {
      results_text("⚠️ Model fitting failed. Please check input values or provide better initial estimates.")
      plot_obj(NULL)
      fitted_model(NULL)
      return(NULL)
    })
    
    # If model fitting failed
    if (is.null(model)) return()
    
    fitted_model(model)
    coeffs <- coef(model)
    mumax <- coeffs["mumax"]
    Ks <- coeffs["Ks"]
    half_mumax <- mumax/2
   
    
    # Calculate predicted values and R²
    fitted_mu <- predict(model)
    ss_total <- sum((mu - mean(mu))^2)
    ss_res <- sum((mu - fitted_mu)^2)
    r2 <- 1 - ss_res / ss_total
    
    
    results_text(sprintf("μmax = %.4f 1/h\nKs = %.4f g/L\nR² = %.4f\nμmax/2 = %.4f", mumax, Ks, r2,half_mumax))
    fitted_model(model)
    
    plot_obj(function() {
      plot(S, mu, pch = 19, col = "blue", 
           xlim = input$xrange, ylim = input$yrange,
           xlab = "Substrate Concentration (S)",
           ylab = "Specific Growth Rate (μ)", main = "Monod Growth Curve")
      curve(predict(model, newdata = data.frame(S = x)), add = TRUE, col = "red", lwd = 2)
      
      if (input$show_grid) grid()
      
      if (input$show_box) box()
      
      if (input$show_ref) {
        abline(h = result$mu_pred1, col = "purple", lty = 2, lwd = 1.5)          # Horizontal at predicted μ
        abline(v = input$predict_s, col = "purple", lty = 2, lwd = 1.5)     # Vertical at S
      }
      if (input$mark_unknown_point && !is.null(result$mu_pred1) && is.finite(result$mu_pred1)) {
        # Plot the predicted point
        points(input$predict_s, result$mu_pred1, pch = 19, col = "blue", cex = 1.5)
        
        # Add a label near the point
        label_text <- sprintf("(%.2f g/L , %.2f 1/h)", input$predict_s, result$mu_pred1)
        text(input$predict_s, result$mu_pred1, labels = label_text,
             pos = 4, col = "red", cex = 0.9, offset = 0.5)
      }
      
      
      if (input$show_eqn_r2) {
        eqn <- sprintf("μ = %.4f·S / (%.4f + S)", mumax, Ks)
        r2_text <- sprintf("R² = %.4f", r2)
        usr <- par("usr")  # Get plot boundaries
        
        text(
          x = usr[2] - 0.02 * (usr[2] - usr[1]),
          y = usr[4] - 0.05 * (usr[4] - usr[3]),
          labels = eqn, adj = 1, col = "purple", cex = 0.9
        )
        text(
          x = usr[2] - 0.02 * (usr[2] - usr[1]),
          y = usr[4] - 0.1 * (usr[4] - usr[3]),
          labels = r2_text, adj = 1, col = "purple", cex = 0.9
        )
      }
      
      if(input$show_intercepts){
        points(0, mumax, pch = 19, col = "blue", cex = 1.5)
        text(0, mumax, labels = sprintf("μmax =%.2f", mumax), pos = 3, col = "red")
        abline(h = mumax, col = "green", lty = 2, lwd = 1.5)
        points(0, half_mumax, pch = 19, col = "blue", cex = 1.5)
        points(Ks, 0, pch = 19, col = "blue", cex = 1.5)
        text(0, half_mumax, labels = sprintf("μmax/2 =%.2f", half_mumax), pos = 3, col = "red")
        text(Ks, 0, labels = sprintf("Ks =%.2f", Ks), pos = 3, col = "red")
        abline(h = half_mumax, col = "green", lty = 2, lwd = 1.5)
        abline(v = Ks, col = "green", lty = 2, lwd = 1.5)
        
      }
      
      
      
    })
  })
  
  observeEvent(input$predict_monod, {
    req(fitted_model())
    S_input <- input$predict_s
    if (is.na(S_input)) {
      prediction_text("⚠️ Please enter a valid numeric value for substrate concentration.")
      return()
    }
    
    mu_pred <- predict(fitted_model(), newdata = data.frame(S = S_input))
    result$mu_pred1 <- mu_pred
    prediction_text(sprintf("🔍 Predicted Specific Growth Rate(μ) at S = %.2f g/L: %.4f 1/h", S_input, mu_pred))
  })
  
  output$monod_result <- renderText({
    paste(results_text(), prediction_text(), sep = "\n\n")
  })
  
  output$monod_plot <- renderPlot({
    plot_fn <- plot_obj()
    if (!is.null(plot_fn)) plot_fn()
  })
  
  observeEvent(input$refresh, {
    updateTextInput(session, "substrate", value = "")
    updateTextInput(session, "growth_rate", value = "")
    updateNumericInput(session, "predict_s", value = NA)
    updateSliderInput(session, "xrange", value = c(0, 100))
    updateSliderInput(session, "yrange", value = c(0, 100))
    updateCheckboxInput(session, "show_intercepts", value = TRUE)
    updateCheckboxInput(session, "show_ref", value = TRUE)
    updateCheckboxInput(session, "show_eqn_r2", value = TRUE)
    updateCheckboxInput(session, "show_grid", value = TRUE)
    updateCheckboxInput(session, "show_box", value = TRUE)
    updateCheckboxInput(session, "mark_unknown_point", value = TRUE)
    results_text("")
    prediction_text("")
    plot_obj(NULL)
    fitted_model(NULL)
  })
  
  output$download_plot <- downloadHandler(
    filename = function() { "monod_plot.png" },
    content = function(file) {
      if (!is.null(plot_obj())) {
        png(file)
        plot_obj()()
        dev.off()
      }
    }
  )
  
  output$download_result <- downloadHandler(
    filename = function() { "monod_results.txt" },
    content = function(file) {
      writeLines(paste(results_text(), prediction_text(), sep = "\n"), file)
    }
  )
  
  observeEvent(input$generate_report, {
    req(results_text(), plot_obj())  # Ensure results and plot are available
    
    temp_dir <- tempdir()
    report_rmd <- file.path(temp_dir, "monod_report.Rmd")
    plot_file <- file.path(temp_dir, "monod_plot.png")
    
    # Save the plot to PNG file
    png(filename = plot_file)
    dev.off()
    
    # Namespace for module usage
    ns <- session$ns
    
    # Create the RMarkdown content
    rmd_content <- c(
      "---",
      "title: 'Monod Growth Kinetics Report'",
      "output: pdf_document",
      "---",
      "",
      "# Experiment Name",
      "Monod's Growth Kinetics",
      "",
      "# Aim",
      "To determine the Specific Growth Rate (μ) and Half-Saturation Constant (Ks) using Monod’s model.",
      "",
      "# Input Data",
      paste0("- Substrate Concentrations: ", input$substrate),
      paste0("- Growth Rates: ", input$growth_rate),
      "",
      "# Results",
      results_text(),
      "",
      "# Prediction",
      prediction_text(),
      "",
      "# Interpretation",
      "The microbial growth was modeled using Monod kinetics. The parameters μmax and Ks were estimated using non-linear regression.",
      "",
      "# Growth Curve",
      paste0("![](", basename(plot_file), ")")
    )
    
    # Write content to Rmd
    writeLines(rmd_content, report_rmd)
    
    # Render PDF
    pdf_path <- rmarkdown::render(report_rmd, output_dir = temp_dir, quiet = TRUE)
    
    # Download handler
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("Monod_Growth_Kinetics_Report_", Sys.Date(), ".pdf")
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
  
  
  observeEvent(input$back, {
    current_page("home")
  })
}

