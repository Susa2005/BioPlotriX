fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f4f8f5;
        font-family: 'Segoe UI', sans-serif;
      }
      h1, h3 {
        color: #2d4739;
      }
      .well {
        background-color: #ffffff;
        border-radius: 12px;
        padding: 20px;
        box-shadow: 0px 4px 10px rgba(0,0,0,0.1);
      }
      .exp-button {
        background-color: #3c8d7e;
        color: white;
        border: none;
        padding: 12px;
        font-size: 16px;
        border-radius: 8px;
        margin-bottom: 15px;
        width: 100%;
        transition: background-color 0.3s;
      }
      .exp-button:hover {
        background-color: #327567;
      }
      .footer-button {
        width: 100%;
        font-size: 15px;
        border-radius: 8px;
        padding: 10px;
      }
    "))
  ),
  
  titlePanel(h1("🧬 BioPlotriX", align = "center")),
  
  conditionalPanel(
    condition = "output.page === 'home'",
    
    br(),
    h3("🔬 Select an Experiment", align = "center"),
    br(),
    
    fluidRow(
      column(6, class = "col-sm-6 col-12",
             actionButton("lowry", "Lowry's Method (Protein Estimation)", class = "exp-button"),
             actionButton("urea", "Urea Estimation by DAM Method", class = "exp-button"),
             actionButton("anthrone", "Anthrone Method (Carbohydrate Estimation)", class = "exp-button"),
             actionButton("mtt", "MTT Assay (IC50 Value)", class = "exp-button")
            
      ),
      column(6, class = "col-sm-6 col-12",
             actionButton("growth", "Growth Kinetics (Bacterial Growth)", class = "exp-button"),
             actionButton("monod", "Growth Kinetics (Monod Model)", class = "exp-button"),
             actionButton("thermal", "Thermal Death Kinetics", class = "exp-button"),
             actionButton("michaelis", "Michaelis-Menten Enzyme Kinetics", class = "exp-button")
             
      )
    ),
    
    br(), hr(), br(),
    
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
  ),
  
  # UI Outputs for modules
  uiOutput("lowry_ui"),
  uiOutput("urea_ui"),
  uiOutput("anthrone_ui"),
  uiOutput("michaelis_ui"),
  uiOutput("growth_ui"),
  uiOutput("monod_ui"),
  uiOutput("thermal_ui"),
  uiOutput("mtt_ui"),
  uiOutput("unitConverter_ui")
)
