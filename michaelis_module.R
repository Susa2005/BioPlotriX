# UI Function
michaelisUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    actionButton(ns("back"), "Home",class = "btn btn-primary", icon = icon("arrow-left")),
    titlePanel("Michaelis-Menten Enzyme Kinetics"),
    sidebarLayout(
      sidebarPanel(
        h4("Calculate one variable using Michaelis-Menten equation"),
        selectInput(ns("calc_var"), "Select variable to calculate:", 
                    choices = c("v", "Vmax", "Km", "S")),
        
        conditionalPanel(
          condition = sprintf("input['%s'] != 'v'", ns("calc_var")),
          numericInput(ns("v"), "Enter v (μM/mol):", value = NA)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] != 'Vmax'", ns("calc_var")),
          numericInput(ns("Vmax"), "Enter Vmax (μM/mol):", value = NA)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] != 'Km'", ns("calc_var")),
          numericInput(ns("Km"), "Enter Km (mM):", value = NA)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] != 'S'", ns("calc_var")),
          numericInput(ns("S"), "Enter S (mM):", value = NA)
        ),
        
        actionButton(ns("calculate_single"), "Calculate"),
        br(), br(),
        hr(),
        h4("Estimate Vmax and Km from Plot"),
        selectInput(ns("plot_type"), "Select Plot Type:", 
                    choices = c("Hanes-Woolf", "Eadie-Hofstee", "Lineweaver-Burk")),
        textInput(ns("v_values"), "Enter v values (μM/mol) (comma-separated):"),
        textInput(ns("s_values"), "Enter S values (mM) (comma-separated):"),
        actionButton(ns("generate_plot"), "Generate Plot"),
        br(),
        downloadButton(ns("download_plot"), "Download Plot"),
        downloadButton(ns("download_result"), "Download Result"),
        br(), br(),
        actionButton(ns("refresh"), "Refresh All"),
        
        hr(),
        #actionButton(ns("generate_report"), "Generate Report", icon = icon("file-alt"), class = "btn btn-info btn-block")
        
      ),
      mainPanel(
        wellPanel(
        plotOutput(ns("kinetics_plot")),
        verbatimTextOutput(ns("results"))
        ),
        wellPanel(
          sliderInput(ns("xrange"), "X-axis Range ", min = -100, max = 100, value = c(-50, 50), step = 0.1),
          sliderInput(ns("yrange"), "Y-axis Range ", min = -100, max = 100, value = c(-10, 10), step = 0.1),
          checkboxInput(ns("show_intercepts"), "Show Intercept Points", value = TRUE),
          checkboxInput(ns("show_eqn_r2"), "Show Equation and R² on Plot", value = TRUE),
          checkboxInput(ns("show_grid"), "Show Grid Lines", value = TRUE),
          checkboxInput(ns("show_box"), "Show Box Around the Graph", value = TRUE),
          checkboxInput(ns("mark_unknown_point"), "Show Intercept Points and its Value", value = TRUE)
        )
      )
    )
  )
}

michaelisServer <- function(input, output, session, current_page) {
  ns <- session$ns
  results_text <- reactiveVal("")
  plot_obj <- reactiveVal(NULL)
  
  observeEvent(input$refresh, {
    updateSelectInput(session, "calc_var", selected = "v")
    updateSelectInput(session, "plot_type", selected = "Hanes-Woolf")
    updateTextInput(session, "v_values", value = "")
    updateTextInput(session, "s_values", value = "")
    updateNumericInput(session, "v", value = NA)
    updateNumericInput(session, "Vmax", value = NA)
    updateNumericInput(session, "Km", value = NA)
    updateNumericInput(session, "S", value = NA)
    updateSliderInput(session, "xrange", value = c(-100, 100))  # Adjust as needed for Lowry
    updateSliderInput(session, "yrange", value = c(-10, 10))
    updateCheckboxInput(session, "show_intercepts", value = FALSE)
    updateCheckboxInput(session, "show_eqn_r2", value = TRUE)
    updateCheckboxInput(session, "show_grid", value = TRUE)
    updateCheckboxInput(session, "show_box", value = TRUE)
    
    updateCheckboxInput(session, "mark_unknown_point", value = TRUE)
    results_text("")  # ✅ Ensures results reset
    plot_obj(NULL)  # ✅ Clears the plot reactive object
  })
  
  
  # Option 1: Calculate One Variable
  observeEvent(input$calculate_single, {
    req(input$calc_var)
    tryCatch({
      v <- input$v
      Vmax <- input$Vmax
      Km <- input$Km
      S <- input$S
      result <- ""
      
      if (input$calc_var == "v") {
        result <- sprintf("v = %.3f μM/mol", (Vmax * S) / (Km + S))
      } else if (input$calc_var == "Vmax") {
        result <- sprintf("Vmax = %.3f μM/mol", (v * (Km + S)) / S)
      } else if (input$calc_var == "Km") {
        result <- sprintf("Km = %.3f mM", ((Vmax * S) - (v * S)) / v)
      } else if (input$calc_var == "S") {
        result <- sprintf("S = %.3f mM", (v * Km) / (Vmax - v))
      }
      
      results_text(result)
    }, error = function(e) {
      results_text("Invalid input values or calculation error.")
    })
  })
  
  # Option 2: Plot and Estimate Vmax & Km
  observeEvent(input$generate_plot, {
    req(input$v_values, input$s_values)
    v <- as.numeric(unlist(strsplit(input$v_values, ",")))
    S <- as.numeric(unlist(strsplit(input$s_values, ",")))
    
    if (length(v) != length(S)) {
      results_text("Error: v and S must be the same length.")
      return()
    }
    
    df <- data.frame(v = v, S = S)
    plot_type <- input$plot_type
    model <- NULL
    result <- ""
    
    if (plot_type == "Hanes-Woolf") {
      df$S_over_v <- df$S / df$v
      model <- lm(S_over_v ~ S, data = df)
      slope <- coef(model)[2]
      intercept <- coef(model)[1]
      Vmax <- 1 / slope
      Km <- intercept * Vmax
      y_int <- Km / Vmax
      x_int <- -Km
      
      result <- sprintf("Plot: Hanes-Woolf\nVmax = %.3f µM/mol\nKm = %.3f mM\nSlope(1/v)=%.3f\nY-Intercept(Km/Vmax)=%.3f\nX-Intercept(-Km)=%.3f", Vmax, Km, slope, y_int, x_int)
      
      plot_obj(function() {
        # Plot data
        plot(df$S, df$S_over_v,
             main = "Hanes-Woolf Plot",
             xlab = "[S] (mM)",
             ylab = "[S]/v",
             xlim = input$xrange,
             ylim = input$yrange,
             col = "blue",
             pch = 19,
             axes = FALSE)  # Disable default axes
        
        # Add custom axes at zero
        abline(h = 0, col = "gray40", lwd = 2)  # x-axis at y=0
        abline(v = 0, col = "gray40", lwd = 2)  # y-axis at x=0
        
        # Add tick marks and labels
        axis(1, pos = 0)  # x-axis ticks at y=0
        axis(2, pos = 0)  # y-axis ticks at x=0
        
        # Optional: add box around the plot
        if (input$show_box) box()
        
        # Draw extended regression line
        x_vals <- seq(min(df$S) - 1.5 * abs(min(df$S)), max(df$S) + 1.5 * abs(max(df$S)), length.out = 100)
        y_vals <- slope * x_vals + intercept
        lines(x_vals, y_vals, col = "red", lwd = 2)
        
        if (input$show_intercepts) {
          points(x_int, 0, pch = 19, col = "blue")
          points(0, y_int, pch = 19, col = "blue")
          text(x_int, 0, labels = sprintf("X-Intercept = -Km = %.2f", x_int), col = "purple", pos = 2)
          text(0, y_int, labels = sprintf("Y-Intercept = Km/Vmax = %.2f", y_int), col = "purple", pos = 4)
        }
        
        if (input$show_grid) grid()
        
        # Mark intercepts with values (numerical)
        if (input$mark_unknown_point) {
          points(x_int, 0, pch = 19, col = "blue")
          points(0, y_int, pch = 19, col = "blue")
          text(x_int, -0.5, labels = sprintf("-Km = %.2f", x_int), col = "purple", pos = 2)
          text(mean(df$S), y_int + 0.5, labels = sprintf("Km/Vmax = %.2f", y_int), col = "purple", pos = 4)
          }
        
        # Equation and R²
        if (input$show_eqn_r2) {
          r_squared <- summary(model)$r.squared
          eqn <- sprintf("y = %.4f x + %.4f", slope, intercept)
          r2_text <- sprintf("R² = %.4f", r_squared)
          slope_text <- sprintf("Slope = 1/Vmax = %.2f", slope)  # Use separate variable for text
          usr <- par("usr")
          
          text(x = usr[2] - 0.02 * (usr[2] - usr[1]), y = usr[4] - 0.05 * (usr[4] - usr[3]),
               labels = eqn, adj = 1, col = "black", cex = 0.9)
          text(x = usr[2] - 0.02 * (usr[2] - usr[1]), y = usr[4] - 0.1 * (usr[4] - usr[3]),
               labels = r2_text, adj = 1, col = "black", cex = 0.9)
          text(mean(df$S), mean(df$S_over_v), labels = slope_text, col = "black", pos = 3, cex = 0.9)
        }
        
      })
      
    } else if (plot_type == "Eadie-Hofstee") {
      df$v_over_S <- df$v / df$S
      model <- lm(v ~ v_over_S, data = df)
      slope <- coef(model)[2]
      intercept <- coef(model)[1]
      Km <- -slope
      Vmax <- intercept
      
      x_int <- Vmax / Km  # where v = 0 ⇒ v/S = Vmax / Km
      y_int <- Vmax       # where v/S = 0 ⇒ v = Vmax
      
      result <- sprintf("Plot: Eadie-Hofstee\nVmax = %.3f µM/mol\nKm = %.3f mM\nSlope = -Km = %.3f\nY-Intercept (Vmax) = %.3f\nX-Intercept (Vmax/Km) = %.3f", 
                        Vmax, Km, slope, y_int, x_int)
      
      plot_obj(function() {
        # Plot data
        plot(df$v_over_S, df$v,
             main = "Eadie-Hofstee Plot",
             xlab = "v/[S] (1/mM)",
             ylab = "v (µM/min)",
             xlim = input$xrange,
             ylim = input$yrange,
             col = "darkgreen",
             pch = 19,
             axes = FALSE)
        
        abline(h = 0, col = "gray40", lwd = 2)
        abline(v = 0, col = "gray40", lwd = 2)
        axis(1, pos = 0)
        axis(2, pos = 0)
        box()
        
        # Extended regression line
        x_vals <- seq(min(df$v_over_S) - 1.5 * abs(min(df$v_over_S)), max(df$v_over_S) + 1.5 * abs(max(df$v_over_S)), length.out = 100)
        y_vals <- slope * x_vals + intercept
        lines(x_vals, y_vals, col = "red", lwd = 2)
        
        if (input$show_intercepts) {
          points(x_int, 0, pch = 19, col = "blue")
          points(0, y_int, pch = 19, col = "blue")
          text(x_int, 0, labels = sprintf("X-Intercept = Vmax/Km = %.2f", x_int), col = "purple", pos = 2)
          text(0, y_int, labels = sprintf("Y-Intercept = Vmax = %.2f", y_int), col = "purple", pos = 4)
        }
        
        if (input$show_grid) grid()
        
        if (input$mark_unknown_point) {
          points(x_int, 0, pch = 19, col = "blue")
          points(0, y_int, pch = 19, col = "blue")
          text(x_int, -0.5, labels = sprintf("Vmax/Km = %.2f", x_int), col = "purple", pos = 2)
          text(mean(df$v_over_S), y_int + 0.5, labels = sprintf("Vmax = %.2f", y_int), col = "purple", pos = 4)
        }
        
        if (input$show_eqn_r2) {
          r_squared <- summary(model)$r.squared
          eqn <- sprintf("y = %.4f x + %.4f", slope, intercept)
          r2_text <- sprintf("R² = %.4f", r_squared)
          slope_text <- sprintf("Slope = -Km = %.2f", slope)
          
          usr <- par("usr")
          text(x = usr[2] - 0.02 * (usr[2] - usr[1]), y = usr[4] - 0.05 * (usr[4] - usr[3]),
               labels = eqn, adj = 1, col = "black", cex = 0.9)
          text(x = usr[2] - 0.02 * (usr[2] - usr[1]), y = usr[4] - 0.1 * (usr[4] - usr[3]),
               labels = r2_text, adj = 1, col = "black", cex = 0.9)
          text(mean(df$v_over_S), mean(df$v), labels = slope_text, col = "black", pos = 3, cex = 0.9)
        }
      })
    }else if (plot_type == "Lineweaver-Burk") {
      df$inv_S <- 1 / df$S
      df$inv_v <- 1 / df$v
      model <- lm(inv_v ~ inv_S, data = df)
      slope <- coef(model)[2]
      intercept <- coef(model)[1]
      Vmax <- 1 / intercept
      Km <- slope / intercept
      
      # Calculate intercepts for axes:
      # y-intercept = 1/Vmax = intercept
      y_int <- intercept
      
      # x-intercept = -1/Km (where y=0, 0 = slope * x + intercept => x = -intercept/slope)
      x_int <- -intercept / slope
      
      result <- sprintf("Plot: Lineweaver-Burk\nVmax = %.3f µM/mol\nKm = %.3f mM\nSlope = Km/Vmax = %.3f\nY-Intercept (1/Vmax) = %.3f\nX-Intercept (-1/Km) = %.3f",
                        Vmax, Km, slope, y_int, x_int)
      
      plot_obj(function() {
        # Plot data points
        plot(df$inv_S, df$inv_v,
             main = "Lineweaver-Burk Plot",
             xlab = "1/[S] (1/mM)",
             ylab = "1/v (min/µM)",
             xlim = input$xrange,
             ylim = input$yrange,
             col = "purple",
             pch = 19,
             axes = FALSE)
        
        # Axes at zero
        abline(h = 0, col = "gray40", lwd = 2)
        abline(v = 0, col = "gray40", lwd = 2)
        axis(1, pos = 0)
        axis(2, pos = 0)
        box()
        
        # Extended regression line
        x_vals <- seq(min(df$inv_S) - 1.5 * abs(min(df$inv_S)), max(df$inv_S) + 1.5 * abs(max(df$inv_S)), length.out = 100)
        y_vals <- slope * x_vals + intercept
        lines(x_vals, y_vals, col = "red", lwd = 2)
        
        if (input$show_intercepts) {
          points(x_int, 0, pch = 19, col = "blue")
          points(0, y_int, pch = 19, col = "blue")
          text(x_int, 0, labels = sprintf("X-Intercept = -1/Km = %.2f", x_int), col = "purple", pos = 2)
          text(0, y_int, labels = sprintf("Y-Intercept = 1/Vmax = %.2f", y_int), col = "purple", pos = 4)
        }
        
        if (input$show_grid) grid()
        
        if (input$mark_unknown_point) {
          points(x_int, 0, pch = 19, col = "blue")
          points(0, y_int, pch = 19, col = "blue")
          text(x_int, -0.5, labels = sprintf("-1/Km = %.2f", x_int), col = "purple", pos = 2)
          text(mean(df$inv_S), y_int + 0.5, labels = sprintf("1/Vmax = %.2f", y_int), col = "purple", pos = 4)
        }
        
        if (input$show_eqn_r2) {
          r_squared <- summary(model)$r.squared
          eqn <- sprintf("y = %.4f x + %.4f", slope, intercept)
          r2_text <- sprintf("R² = %.4f", r_squared)
          slope_text <- sprintf("Slope = Km/Vmax = %.2f", slope)
          
          usr <- par("usr")
          text(x = usr[2] - 0.02 * (usr[2] - usr[1]), y = usr[4] - 0.05 * (usr[4] - usr[3]),
               labels = eqn, adj = 1, col = "black", cex = 0.9)
          text(x = usr[2] - 0.02 * (usr[2] - usr[1]), y = usr[4] - 0.1 * (usr[4] - usr[3]),
               labels = r2_text, adj = 1, col = "black", cex = 0.9)
          text(mean(df$inv_S), mean(df$inv_v), labels = slope_text, col = "black", pos = 3, cex = 0.9)
        }
      })
    }
    results_text(result)
  })
  
  
  output$kinetics_plot <- renderPlot({
    plot_fn <- plot_obj()
    if (!is.null(plot_fn)) plot_fn()
  })
  
  output$results <- renderText({ results_text() })
  
  
  
  # ✅ Download Handlers
  output$download_plot <- downloadHandler(
    filename = function() { "michaelis_plot.png" },
    content = function(file) {
      if (!is.null(plot_obj())) {
        png(file)
        plot_obj()()
        dev.off()
      }
    }
  )
  
  output$download_result <- downloadHandler(
    filename = function() { "michaelis_results.txt" },
    content = function(file) {
      writeLines(results_text(), file)
    }
  )
  
  observeEvent(input$generate_report, {
    req(results_text(), plot_obj())  # Ensure required data is available
    
    temp_dir <- tempdir()
    report_rmd <- file.path(temp_dir, "michaelis_report.Rmd")
    plot_file <- file.path(temp_dir, "michaelis_plot.png")
    
    # Save the plot to a PNG file
    png(filename = plot_file)
    print(plot_obj())  # Ensure plot is rendered
    dev.off()
    
    # Namespace (if inside a module)
    ns <- session$ns
    
    # Create the RMarkdown report content
    rmd_content <- c(
      "---",
      "title: 'Michaelis-Menten Kinetics Report'",
      "output: pdf_document",
      "---",
      "",
      "# Experiment Name",
      "Michaelis-Menten Kinetics",
      "",
      "# Aim",
      "To determine kinetic parameters (Vmax and Km) of an enzyme using the Michaelis-Menten equation.",
      "",
      "# Input Data",
      paste0("- Substrate Concentrations (S): ", input$s_values),
      paste0("- Reaction Rates (V): ", input$v_values),
      "",
      "# Results",
      results_text(),
      "",
      "# Interpretation",
      "The enzyme kinetics were analyzed by fitting data to the Michaelis-Menten equation. Vmax and Km were estimated based on non-linear regression.",
      "",
      "# Plot",
      paste0("![](", basename(plot_file), ")")
    )
    
    # Write the RMarkdown content to file
    writeLines(rmd_content, report_rmd)
    
    # Render the Rmd file to PDF
    pdf_path <- rmarkdown::render(report_rmd, output_dir = temp_dir, quiet = TRUE)
    
    # Set up the download handler
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("Michaelis_Menten_Kinetics_Report_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        file.copy(pdf_path, file, overwrite = TRUE)
      }
    )
    
    # Show modal with download button
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

