# Source the Lowry's Module
source("lowry_module.R")
source("urea_module.R")
source("anthrone_module.R")
source("growth_module.R")
source("thermal_module.R")
source("monod_module.R")
source("michaelis_module.R")
source("mtt_module.R")
source("unitConverter_module.R")

function(input, output, session) {
  # Reactive value to track the current page
  current_page <- reactiveVal("home")  # Default page is "home"
  
  # Update the UI conditionally based on the current page
  output$page <- reactive(current_page())
  outputOptions(output, "page", suspendWhenHidden = FALSE)
  
  
  # Navigation logic
  observeEvent(input$lowry, {
    current_page("lowry")  # Switch to Lowry's Method page
  })
  
  observeEvent(input$urea, {
    current_page("urea")  # Switch to Urea Estimation page
  })
  
  observeEvent(input$anthrone, {
    current_page("anthrone")  # Switch to Anthrone Method page
  })
  
  observeEvent(input$michaelis, {
    current_page("michaelis")  # Switch to Michaelis Kinetics page
  })
  
  observeEvent(input$growth, {
    current_page("growth")  # Switch to Microbial Growth Kinetics page
  })
  
  observeEvent(input$thermal, {
    current_page("thermal")  # Switch to Microbial Death  Kinetics page
  })
  
  observeEvent(input$monod, {
    current_page("monod")  # Switch to Monod's  Kinetics page
  })
  
  observeEvent(input$mtt, {
    current_page("mtt")  # Switch to MTT  Kinetics page
  })
  
  observeEvent(input$unit_converter, {
    current_page("unit_converter")  # Switch to UnitConverter page
  })
  
  
  observeEvent(input$back, {
    current_page("home")  # Return to Home page
  })
  
  observeEvent(input$view_tutorial, {
    showModal(modalDialog(
      title = "📺 Watch Tutorial on YouTube",
      HTML("<p style='font-size:16px;'>
           Click the link below to open the YouTube playlist in a new tab:
           <br><br>
           <a href='https://www.youtube.com/@BioPlotriX' target='_blank'>
             📺 BioPlotriX YouTube Channel
           </a>
         </p>"),
      easyClose = TRUE
    ))
  })
  
  
  
  # User Instructions
  observeEvent(input$user_instructions, {
    showModal(modalDialog(
      title = "📘 User Instructions",
      HTML("<ul style='line-height: 1.6; font-size: 15px;'>
    <li><strong>Welcome to BioPlotriX!</strong> This platform helps you analyze and visualize results from common biochemical and bioprocess experiments.</li>
    <li><strong>1. Selecting an Experiment:</strong> Click on the desired experiment on the home page (e.g., Lowry's Method, MTT Assay).</li>
    <li><strong>2. Entering Your Data:</strong> Fill in the input fields shown. Enter comma-separated values (e.g., 0.1, 0.2, 0.3) for OD, concentration, or time.</li>
    <li><strong>3. Analyzing Results:</strong> After entering data, click the <em>'Analyze'</em> or <em>'Calculate'</em> button to see plots and calculations.</li>
    <li><strong>4. Viewing Output:</strong> A plot and interpretation will be displayed based on your input.</li>
    <li><strong>5. Download Options:</strong> Use <em>'Download Plot'</em> and <em>'Download Result'</em> buttons to save your analysis locally.</li>
    <li><strong>6. Refreshing Inputs:</strong> Use the <em>'Refresh'</em> button to clear current inputs and start over.</li>
    <li><strong>7. Navigation:</strong> Use the <em>'Back to Home'</em> button at the bottom to return to the home page.</li>
    <li><strong>8. Unit Conversion:</strong> For quick unit help (e.g., µg/ml to mg/L), use the <em>'🔄 Unit Converter'</em> from the home page.</li>
    <li><strong>9. Learn Visually:</strong> Click <em>'📽️ View Tutorial'</em> to watch how to use the software.</li>
  </ul>"),
      easyClose = TRUE
    ))
    
  })
  
  # About Creator
  observeEvent(input$about_creator, {
    showModal(modalDialog(
      title = "👨‍🔬 About the Creator",
      HTML("<div style='font-size: 15px; line-height: 1.6;'>
    <p><strong>Name:</strong> Sudharsun M, Sudhir J</p>
    
    <p><strong>Background:</strong><br>
    A passionate biotechnology students dedicated to merging life sciences with cutting-edge technologies. With hands-on experience in wet lab work and a deep interest in software development, We developed <strong>BioPlotriX</strong> to simplify and automate data analysis for common biochemical and bioprocess experiments.</p>
    
    <p><strong>Project Vision:</strong><br>
    The goal behind BioPlotriX is to make experimental calculations, graph plotting, and report generation easier for students, researchers, and educators — especially those with limited programming or analysis experience. This tool aims to reduce human error, save time, and encourage data-driven learning in life sciences.</p>
    
    <p><strong>Technologies Used:</strong><br>
    Developed using R and Shiny with integration of data visualization, statistical analysis, and report generation features in a user-friendly interface.</p>
    
    <p><strong>Contact:</strong><br>
    📧 <a href='mailto:bioplotrix@gmail.com'>bioplotrix@gmail.com</a></p>
    
    <p><strong>Thank you for using BioGeniX!</strong><br>
    Your feedback and suggestions are always welcome to help improve and expand this tool.</p>
  </div>"),
      easyClose = TRUE
    ))
    
  })
  
  # Home Page UI (Always Present, Hidden When in Lowry)
  output$home_ui <- renderUI({
    if (current_page() == "home") {
      fluidPage(
        titlePanel(tags$h1("🧬BioPlotriX", align = "center")),
        
        h3("Select an Experiment:", align = "center"),
        br(),
        
        wellPanel(
          actionButton("lowry", "Lowry's Method (Protein Estimation)", width = "100%"),
          br(),
          actionButton("urea", "Estimation of Urea by Diacetyl Monoxime Method", width = "100%"),
          br(),
          actionButton("anthrone", "Anthrone Method (Carbohydrate Estimation)", width = "100%"),
          br(),
          actionButton("michaelis", "Michaelis-Menten Enzyme Kinetics", width = "100%"),
          br(),
          actionButton("growth", "Growth Kinetics (Bacterial Growth Curve)", width = "100%"),
          br(),
          actionButton("monod", "Growth Kinetics (Monod Model)", width = "100%"),
          br(),
          actionButton("thermal", "Thermal Death Kinetics (Microbial Inactivation)", width = "100%"),
          br(),
          actionButton("mtt", "MTT Assay (Cell Viability & Cytotoxicity)", width = "100%")
        ),
        
        hr(),
        
        fluidRow(
          column(3, align = "center",
                 actionButton("view_tutorial", "📽️ View Tutorial", class = "footer-button btn-danger")),
          column(3, align = "center",
                 actionButton("user_instructions", "📘 User Instructions", class = "footer-button btn-primary")),
          column(3, align = "center",
                 actionButton("about_creator", "👨‍🔬 About Creator", class = "footer-button btn-success")),
          column(3, align = "center",
                 actionButton("unit_converter", "🔄 Unit Converter", class = "footer-button btn-warning"))
        )
      )
    }
  })
  
  
  # Lowry UI (Hidden When on Home)
  output$lowry_ui <- renderUI({
    if (current_page() == "lowry") {
      lowryUI("lowry_experiment")
    }
  })
  # Urea UI
  output$urea_ui <- renderUI({
    if (current_page() == "urea") {
      ureaUI("urea_experiment")
    }
  })
  # Anthrone UI
  output$anthrone_ui <- renderUI({
    if (current_page() == "anthrone") {
      anthroneUI("anthrone_experiment")
    }
  })
  
  # Michaelis UI
  output$michaelis_ui <- renderUI({
    if (current_page() == "michaelis") {
      michaelisUI("michaelis_experiment")
    }
  })
  
  # Growth UI
  output$growth_ui <- renderUI({
    if (current_page() == "growth") {
      growthUI("growth_experiment")
    }
  })
  
  # Monod UI
  output$monod_ui <- renderUI({
    if (current_page() == "monod") {
      monodUI("monod_experiment")
    }
  })
  
  # Thermal UI
  output$thermal_ui <- renderUI({
    if (current_page() == "thermal") {
      thermalUI("thermal_experiment")
    }
  })
  
  # Thermal UI
  output$mtt_ui <- renderUI({
    if (current_page() == "mtt") {
      mttUI("mtt_experiment")
    }
  })
  
  output$unitConverter_ui <- renderUI({
    if (current_page() == "unit_converter") {
      unitConverterUI("unit_converter")
    }
  })
  
  # Call the Lowry's module and pass `current_page`
  callModule(lowryServer, "lowry_experiment", current_page)
  # Call the Urea's module
  callModule(ureaServer, "urea_experiment", current_page)
  # Call the Anthrone's module
  callModule(anthroneServer, "anthrone_experiment", current_page)
  # Call the Michaelis module
  callModule(michaelisServer, "michaelis_experiment", current_page)
  # Call the Growth's module
  callModule(growthServer, "growth_experiment", current_page)
  # Call the Monod's module
  callModule(monodServer, "monod_experiment", current_page)
  # Call the Thermal's module
  callModule(thermalServer, "thermal_experiment", current_page)
  # Call the MTT module
  callModule(mttServer, "mtt_experiment", current_page)
  
  callModule(unitConverterServer, "unit_converter", current_page)
  
}
