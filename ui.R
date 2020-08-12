
library(shiny)

shinyUI(fluidPage(
  titlePanel('ptcAssist 1.0'),
  
  sidebarLayout(
    position = 'left',
    sidebarPanel(
      # img(src="logo.jpg",height = 80, width = '100%', align='center'),
      # includeMarkdown("help.Rmd"),
      includeHTML("help.html"),
      # br(),
      
      fluidRow(
        h5("Frequentist"),
        column(3, #numericInput( "n1", label = "n1", value=20)
               textInput( "ns",
                          label="N",
                          value="20, 30")
        ),
        column(6,
               textInput( "xs",
                          label="Num of Cases",
                          value="0, 3, 5, 10, 15")
        )
      ),
      fluidRow(
        h5("Bayesian"),
        column(3,
               textInput( "bns",
                          label="N",
                          value="20, 120")
        ),
        column(9,
               textInput( "brs",
                          label="True RR",
                          value="0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35")
        ),
        column(3, numericInput( "r0", label = "True rate", value=0.25)),
        column(3, numericInput( "cl", label = "Confidence", value=0.6)),
        column(3, numericInput( "a", label = "Shape a", value=1)),
        column(3, numericInput( "b", label = "Shape b", value=1)),
        column(4,
               textInput( "xvec",
                          label="Obs. #responses",
                          value="0, 0")
        ),
        column(4,
               textInput( "nvec",
                          label="Out of",
                          value="6, 10")
        ),
        column(4,
               textInput( "r0vec",
                          label="Under true RR",
                          value=".25, .15")
        )
      ),
      
      tags$head(
        tags$style(HTML('#goButton{background-color:rgba(0,255,0,.3)}'))
      ),
      tags$hr(), actionButton("goButton", "Update")
      
    ),
    
    mainPanel(
      tabsetPanel( 
        
        tabPanel(
          "Results",
          tableOutput('ftab'), 
          br(), br(), 
          tableOutput('btab'),
          br(), 
          tableOutput('btab2')
        ),

        #   conditionalPanel( 
        #     condition = "input.type=='pb'",
        #     fluidRow(
        #       column(2,
        #              numericInput("niter", "Number of iterations", value=500, min=50,max=1e5,step=1)),
        #       column(2,
        #              numericInput("burn", "Burn-in", value=250, min=0,max=1e5,step=1)),
        #       column(2,
        #              numericInput("thin", "Thin", value=1, min=1,max=100,step=1))
        #     ), 
        #     actionButton("goButton", "Run MCMC"), 
        #     br(), br(), 
        #     plotOutput("plot3", width = "700px", height = "300px"),
        #     htmlOutput("text3"),
        #     br(),
        #     textOutput("header"),
        #     d3heatmapOutput("heatmap1", width = "700px", height = "350px")
        #     #textOutput("text3")
        #   )
        #   
        # ), 
        # 
        # tabPanel(
        #   "Reference", 
        #   tags$iframe(
        #     style="height:660px; width:70%; scrolling=yes; frameBorder='0'", 
        #     src="slides.pdf#page=1&zoom=120")), 
        # 
        # tabPanel(
        #   "Document", 
        #   tags$iframe(style="height:660px; width:70%; scrolling=yes", 
        #               src="ptcAssist.pdf#page=1&zoom=75")), 

        tabPanel("Code",
                 div(style="display: inline-block;vertical-align:top; width: 1000px;",
                     includeHTML("codes.html")
                     # includeMarkdown("codes.Rmd")
                 )
        )
        
      )             
    )
  )
  
))

