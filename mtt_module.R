mttUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    actionButton(ns("back"), "Home", class = "btn btn-primary", icon = icon("arrow-left")),
    br(),
    titlePanel("MTT Assay (IC50 Value)"),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("analysis_type"), "Select Analysis Type", choices = c("Single Drug", "Two Drugs")),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Single Drug'", ns("analysis_type")),
          textInput(ns("drug1_conc"), "Drug Concentrations (µg/ml) (comma-separated)"),
          textInput(ns("drug1_od"), "OD Values for Drug (Don't include control value)"),
          numericInput(ns("control_od1"), "Control OD Value", value = NA)
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Two Drugs'", ns("analysis_type")),
          textInput(ns("drug1_conc"), "Drug 1 Concentrations (µg/ml) (comma-separated)"),
          textInput(ns("drug1_od"), "OD Values for Drug 1 (Don't include control value)"),
          numericInput(ns("control_od1"), "Control OD for Drug 1", value = NA),
          textInput(ns("drug2_conc"), "Drug 2 Concentrations(µg/ml) (comma-separated)"),
          textInput(ns("drug2_od"), "OD Values for Drug 2 (Don't include control value)"),
          numericInput(ns("control_od2"), "Control OD for Drug 2", value = NA)
        ),
        
        actionButton(ns("analyze"), "Analyze"),
        actionButton(ns("refresh"), "Refresh"),
        downloadButton(ns("download_plot"), "Download Plot"),
        downloadButton(ns("download_result"), "Download Result"),
        hr(),
        #actionButton(ns("generate_report"), "Generate Report", icon = icon("file-alt"), class = "btn btn-info btn-block")
      ),
      
      mainPanel(
        wellPanel( 
          plotOutput(ns("mtt_plot")),
          verbatimTextOutput(ns("mtt_result"))
        ),
        wellPanel(
          sliderInput(ns("xrange"), "X-axis Range (Concentration)", min = 0, max = 1000, value = c(0, 100), step = 1),
          sliderInput(ns("yrange"), "Y-axis Range (Viability %)", min = 0, max = 100, value = c(0, 100), step = 1),
          checkboxInput(ns("show_intercepts"), "Show Reference Lines", value = TRUE),
          checkboxInput(ns("show_eqn_r2"), "Show Equation and R² on Plot", value = TRUE),
          checkboxInput(ns("show_grid"), "Show Grid Lines", value = TRUE),
          checkboxInput(ns("show_box"), "Show Box Around the Graph", value = TRUE),
          checkboxInput(ns("mark_unknown_point"), "Mark Estimated Point on Graph", value = TRUE)
        )
      )
    )
  )
}


mttServer <- function(input, output, session, current_page) {
  ns <- session$ns
  results_text <- reactiveVal("")
  plot_obj <- reactiveVal(NULL)
  model_stats <- reactiveVal(list())
  
  observeEvent(input$refresh, {
    updateTextInput(session, "drug1_conc", value = "")
    updateTextInput(session, "drug1_od", value = "")
    updateNumericInput(session, "control_od1", value = NA)
    updateTextInput(session, "drug2_conc", value = "")
    updateTextInput(session, "drug2_od", value = "")
    updateNumericInput(session, "control_od2", value = NA)
    updateSliderInput(session, "xrange", value = c(0, 100))
    updateSliderInput(session, "yrange", value = c(0, 100))
    updateCheckboxInput(session, "show_intercepts", value = TRUE)
    updateCheckboxInput(session, "show_eqn_r2", value = TRUE)
    updateCheckboxInput(session, "show_grid", value = TRUE)
    updateCheckboxInput(session, "show_box", value = TRUE)
    updateCheckboxInput(session, "mark_unknown_point", value = TRUE)
    results_text("")
    plot_obj(NULL)
  })
  
  calculate_ic50 <- function(conc, viability) {
    valid_idx <- !is.na(conc) & !is.na(viability) & conc > 0
    conc <- conc[valid_idx]
    viability <- viability[valid_idx]
    
    if (length(conc) < 4) return(NULL)
    
    tryCatch({
      fit <- nls(viability ~ 100 / (1 + (conc / IC50)^slope),
                 start = list(IC50 = median(conc), slope = 1),
                 control = nls.control(maxiter = 100),
                 algorithm = "port",
                 lower = c(IC50 = min(conc), slope = 0.01),
                 upper = c(IC50 = max(conc), slope = 10))
      
      ic50 <- coef(fit)["IC50"]
      slope <- coef(fit)["slope"]
      intercept <- mean(viability) - (slope * mean(conc))  # Estimated intercept
      
      predicted <- predict(fit)
      ss_res <- sum((viability - predicted)^2)
      ss_tot <- sum((viability - mean(viability))^2)
      r_squared <- 1 - (ss_res / ss_tot)
      
      list(
        IC50 = ic50,
        slope = slope,
        intercept = intercept,
        r_squared = r_squared,
        fitted = data.frame(conc = conc, predicted = predicted)
      )
    }, error = function(e) {
      return(NULL)
    })
  }
  
  
  observeEvent(input$analyze, {
    type <- input$analysis_type
    x_min <- input$xrange[1]
    x_max <- input$xrange[2]
    y_min <- input$yrange[1]
    y_max <- input$yrange[2]
    
    conc <- as.numeric(unlist(strsplit(input$drug1_conc, ",")))
    od <- as.numeric(unlist(strsplit(input$drug1_od, ",")))
    ctrl <- as.numeric(input$control_od1)
    
    if (length(conc) < 4 || length(od) < 4 || is.na(ctrl)) {
      showNotification("⚠️ Please ensure at least 4 valid concentration and OD values.", type = "error")
      return(NULL)
    }
    
    viability <- (od / ctrl) * 100
    
    if (length(unique(viability)) == 1 || all(diff(viability) >= 0)) {
      showNotification("⚠️ Viability values must show a decreasing trend.", type = "error")
      return(NULL)
    }
    
    result <- calculate_ic50(conc, viability)
    
    plot_obj(function() {
      plot(conc, viability, pch = 19, col = "blue",
           xlim = input$xrange, ylim = input$yrange,
           xlab = "Drug Concentration (µg/ml)", ylab = "Cell Viability (%)",
           main = "MTT Assay - Single Drug")
      
      lines(conc, viability, type = "b", col = "lightblue", lwd = 1)
      
      if (!is.null(result)) {
        curve_expr <- function(x) 100 / (1 + (x / result$IC50)^result$slope)
        curve(curve_expr, from = x_min, to = x_max, add = TRUE, col = "darkblue", lwd = 2)
       
        
        # ✅ Ensure viability reference line appears if selected
        if (input$show_intercepts) {
          abline(v = result$IC50, col = "purple", lty = 2, lwd = 1.5)
          abline(h = 50, col = "red", lty = 2, lwd = 1.5)  # Horizontal reference at 50% viability
        }
        
        # ✅ Add unknown point (concentration at viability = 50%) if selected
        if (input$mark_unknown_point && !is.null(result$IC50) && result$IC50 >= x_min && result$IC50 <= x_max) {
          ##abline(v = result$IC50, col = "darkgreen", lty = 2, lwd = 1.5)  # Vertical intercept at IC50
          points(result$IC50, 50, pch = 19, col = "blue", cex = 1.5)
          text(result$IC50, 50, labels = sprintf("(%.2f µg/mL, 50%%)", result$IC50), pos = 4, col = "blue", cex = 0.9)
        }
        
        
        
        if (input$show_eqn_r2) {
          eqn <- sprintf("y = %.4fx + %.4f", result$slope, result$intercept)
          r2_text <- sprintf("R² = %.4f", result$r_squared)
          usr <- par("usr")  # Get plot boundaries
          
          # ✅ Position text on the right side
          text(x = usr[2] - 0.02 * (usr[2] - usr[1]), y = usr[4] - 0.05 * (usr[4] - usr[3]), 
               labels = eqn, adj = 1, col = "purple", cex = 0.9)
          text(x = usr[2] - 0.02 * (usr[2] - usr[1]), y = usr[4] - 0.1 * (usr[4] - usr[3]), 
               labels = r2_text, adj = 1, col = "purple", cex = 0.9)
        }
        
      }
      
      if (input$show_grid) grid()
      
      if (input$show_box) box()
      dev.flush()
    })
    
    if (!is.null(result)) {
      results_text(sprintf("Single Drug Analysis\nIC50: %.2f µg/ml\nSlope: %.2f\nR²: %.2f",
                           result$IC50, result$slope, result$r_squared))
    } else {
      results_text("IC50 calculation failed. Please check your inputs.")
    }
  })
  
  
  observeEvent(input$analyze, {
    type <- input$analysis_type
    x_min <- input$xrange[1]
    x_max <- input$xrange[2]
    y_min <- input$yrange[1]
    y_max <- input$yrange[2]
    
    conc1 <- as.numeric(unlist(strsplit(input$drug1_conc, ",")))
    od1 <- as.numeric(unlist(strsplit(input$drug1_od, ",")))
    ctrl1 <- as.numeric(input$control_od1)
    viability1 <- (od1 / ctrl1) * 100
    
    conc2 <- as.numeric(unlist(strsplit(input$drug2_conc, ",")))
    od2 <- as.numeric(unlist(strsplit(input$drug2_od, ",")))
    ctrl2 <- as.numeric(input$control_od2)
    viability2 <- (od2 / ctrl2) * 100
    
    if (length(conc1) < 4 || length(viability1) < 4 || length(conc2) < 4 || length(viability2) < 4) {
      showNotification("⚠️ Both drugs must have at least 4 data points for IC50 calculation.", type = "error")
      return(NULL)
    }
    
    result1 <- calculate_ic50(conc1, viability1)
    result2 <- calculate_ic50(conc2, viability2)
    
    if (is.null(result1) || is.null(result2)) {
      showNotification("⚠️ IC50 calculation failed. Please check your input data.", type = "error")
      return(NULL)  # Avoid running plot code if result is missing
    }
    
    
    plot_obj(function() {
      plot(conc1, viability1, pch = 19, col = "blue",
           xlim = c(x_min, x_max), ylim = c(y_min, y_max),
           xlab = "Drug Concentration (µg/ml)", ylab = "Cell Viability (%)",
           main = "MTT Assay - Two Drug IC50 Comparison")
      
      points(conc2, viability2, pch = 17, col = "darkgreen")
      
      # ✅ Drug 1 curve
      if (!is.null(result1)) {
        curve_expr1 <- function(x) 100 / (1 + (x / result1$IC50)^result1$slope)
        curve(curve_expr1, from = x_min, to = x_max, add = TRUE, col = "blue", lwd = 2)
        
        if (input$show_intercepts) {
          abline(h = 50, col = "red", lty = 2)  # Reference line at 50% viability
          abline(v = result1$IC50, col = "purple", lty = 2)  # IC50 marker
        }
        
        # ✅ Mark IC50 point visually
        if (input$mark_unknown_point && !is.null(result1$IC50) && result1$IC50 >= x_min && result1$IC50 <= x_max) {
        points(result1$IC50, 50, pch = 19, col = "blue", cex = 1.5)
        text(result1$IC50, 50, labels = sprintf("Drug 1 IC50: %.2f µg/mL", result1$IC50), pos = 4, col = "blue", cex = 0.9)
        }
      }
      
      # ✅ Drug 2 curve
      if (!is.null(result2)) {
        curve_expr2 <- function(x) 100 / (1 + (x / result2$IC50)^result2$slope)
        curve(curve_expr2, from = x_min, to = x_max, add = TRUE, col = "darkgreen", lwd = 2)
        
        if (input$show_intercepts) {
          abline(h = 50, col = "red", lty = 2)
          abline(v = result2$IC50, col = "purple", lty = 2)
        }
        if (input$mark_unknown_point && !is.null(result2$IC50) && result2$IC50 >= x_min && result2$IC50 <= x_max) {
        points(result2$IC50, 50, pch = 19, col = "darkgreen", cex = 1.5)
        text(result2$IC50, 50, labels = sprintf("Drug 2 IC50: %.2f µg/mL", result2$IC50), pos = 4, col = "darkgreen", cex = 0.9)}
      }
      
      
      # ✅ Show equation & R² on right side if enabled
      if (input$show_eqn_r2) {
        usr <- par("usr")
        
        if (!is.null(result1)) {
          eqn1 <- sprintf("Drug 1: y = %.4fx + %.4f", result1$slope, result1$intercept)
          r2_1 <- sprintf("R² = %.4f", result1$r_squared)
          text(usr[2] - 0.02 * (usr[2] - usr[1]), usr[4] - 0.05 * (usr[4] - usr[3]), labels = eqn1, adj = 1, col = "blue", cex = 0.9)
          text(usr[2] - 0.02 * (usr[2] - usr[1]), usr[4] - 0.1 * (usr[4] - usr[3]), labels = r2_1, adj = 1, col = "blue", cex = 0.9)
        }
        
        if (!is.null(result2)) {
          eqn2 <- sprintf("Drug 2: y = %.4fx + %.4f", result2$slope, result2$intercept)
          r2_2 <- sprintf("R² = %.4f", result2$r_squared)
          text(usr[2] - 0.02 * (usr[2] - usr[1]), usr[4] - 0.18 * (usr[4] - usr[3]), labels = eqn2, adj = 1, col = "darkgreen", cex = 0.9)
          text(usr[2] - 0.02 * (usr[2] - usr[1]), usr[4] - 0.23 * (usr[4] - usr[3]), labels = r2_2, adj = 1, col = "darkgreen", cex = 0.9)
        }
      }
      
      if (input$show_grid) grid()
    })
    
    
    results_text(sprintf("Two Drug Analysis\nDrug 1 IC50: %.2f µg/ml\nDrug 2 IC50: %.2f µg/ml",
                         result1$IC50, result2$IC50))
  })
  
  
  
  output$mtt_plot <- renderPlot({
    plot_fn <- plot_obj()
    
    if (is.null(plot_fn)) {
      plot.new()
      text(0.5, 0.5, "No valid data available", cex = 1.5)
    } else {
      plot_fn()
    }
  })
  
  output$mtt_result <- renderText({ results_text() })
  
  output$download_plot <- downloadHandler(
    filename = function() { "mtt_plot.png" },
    content = function(file) {
      png(file); plot_obj()(); dev.off()
    }
  )
  
  output$download_result <- downloadHandler(
    filename = function() { "mtt_results.txt" },
    content = function(file) {
      writeLines(results_text(), file)
    }
  )
  
  observeEvent(input$generate_report, {
    req(results_text(), plot_obj())
    
    temp_dir <- tempdir()
    report_rmd <- file.path(temp_dir, "mtt_report.Rmd")
    plot_file <- file.path(temp_dir, "mtt_plot.png")
    
    # Save the current plot with fixed size
    png(filename = plot_file, width = 800, height = 600)
  
    dev.off()
    
    # Namespace for modular Shiny modules (if any)
    ns <- session$ns
    
    # Determine experiment name with fallback
    exp_name <- if (is.null(input$experiment_name) || input$experiment_name == "") {
      "MTT Assay - Two Drug IC50 Estimation"
    } else {
      input$experiment_name
    }
    
    # Compose RMarkdown content
    rmd_content <- c(
      "---",
      "title: 'MTT Assay Report - Two Drug Analysis'",
      "output: pdf_document",
      "---",
      "",
      "# Experiment Name",
      exp_name,
      "",
      "# Aim",
      "To evaluate cell viability and determine the IC50 values for two different drugs using the MTT assay.",
      "",
      "# Input Data",
      paste0("**Drug 1 Concentrations:** ", input$drug1_conc),
      paste0("\n**Drug 1 OD Values:** ", input$drug1_od),
      paste0("\n**Control OD 1:** ", input$control_od1),
      "",
      paste0("**Drug 2 Concentrations:** ", input$drug2_conc),
      paste0("\n**Drug 2 OD Values:** ", input$drug2_od),
      paste0("\n**Control OD 2:** ", input$control_od2),
      "",
      "# Results",
      results_text(),
      "",
      "# Interpretation",
      "The IC50 value is defined as the concentration of drug at which 50% of the cells remain viable. This was calculated by fitting the viability vs. concentration data to a sigmoidal dose-response curve using nonlinear regression.",
      "",
      "# Plot",
      paste0("![](", basename(plot_file), ")")
    )
    
    # Write to Rmd file
    writeLines(rmd_content, report_rmd)
    
    # Render the PDF silently
    pdf_path <- rmarkdown::render(report_rmd, output_dir = temp_dir, quiet = TRUE)
    
    # Setup download handler for the generated PDF
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("MTT_Two_Drug_Report_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        file.copy(pdf_path, file, overwrite = TRUE)
      }
    )
    
    # Show modal dialog with download button
    showModal(modalDialog(
      title = "Report Ready",
      "Click below to download your MTT Two-Drug report:",
      downloadButton(ns("download_report"), "Download Report"),
      easyClose = TRUE
    ))
  })
  
  
  
  observeEvent(input$back, { current_page("home") })
}

