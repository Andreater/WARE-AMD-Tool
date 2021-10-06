source("libraries.r")
source("myreferences.r")
source("risk_plot.r")

# Disable scientific notation
options(scipen=999)

# Set plots dimension
plot_dim = 100

# User interface ----
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "AMD risk tool"),
  
  dashboardSidebar(disable = T),
  
  dashboardBody(
    fluidRow(
      box(
        width = 2,
        
        radioButtons(inputId = df$gene[1],
                     label   = paste0(df$gene[1], " - ", unique(ref$snps[ref$gene == df$gene[1]])),
                     choices  = ref$genotipi[ref$gene == df$gene[1]],
                     selected = character(0)),
        
        radioButtons(inputId = df$gene[2],
                     label   = paste0(df$gene[2], " - ", unique(ref$snps[ref$gene == df$gene[2]])),
                     choices  = ref$genotipi[ref$gene == df$gene[2]],
                     selected = character(0)),
        
        radioButtons(inputId = df$gene[3],
                     label   = paste0(df$gene[3], " - ", unique(ref$snps[ref$gene == df$gene[3]])),
                     choices  = ref$genotipi[ref$gene == df$gene[3]],
                     selected = character(0))
        
      ),
      box(
        width = 2,
        
        radioButtons(inputId = df$gene[4],
                     label   = paste0(df$gene[4], " - ", unique(ref$snps[ref$gene == df$gene[4]])),
                     choices  = ref$genotipi[ref$gene == df$gene[4]],
                     selected = character(0)),
        
        radioButtons(inputId = df$gene[5],
                     label   = paste0(df$gene[5], " - ", unique(ref$snps[ref$gene == df$gene[5]])),
                     choices  = ref$genotipi[ref$gene == df$gene[5]],
                     selected = character(0)),
        
        radioButtons(inputId = df$gene[6],
                     label   = paste0(df$gene[6], " - ", unique(ref$snps[ref$gene == df$gene[6]])),
                     choices  = ref$genotipi[ref$gene == df$gene[6]],
                     selected = character(0))
      ),
      box(width = 2,
          radioButtons(inputId = df$gene[7],
                       label   = paste0(df$gene[7], " - ", unique(ref$snps[ref$gene == df$gene[7]])),
                       choices  = ref$genotipi[ref$gene == df$gene[7]],
                       selected = character(0)),
          
          radioButtons(inputId = df$gene[8],
                       label   = paste0(df$gene[8], " - ", unique(ref$snps[ref$gene == df$gene[8]])),
                       choices  = ref$genotipi[ref$gene == df$gene[8]],
                       selected = character(0)),
          
          actionButton(inputId = "execute",
                       label   = "Execute"),
          
          actionButton(inputId = "ResetButton",
                       label   = "Reset")),
      
      box(
        
        textInput(inputId = "inText", 
                  label   = "Total risk")),
      box(
        title = "Risk plot",
        
        plotOutput(outputId = "plot1", height = 110),
        
        textInput(inputId = "riskText",
                  label   = NULL),
        
        height = 200
      )
    )
  )
)


######## SERVER #########
server <- function(input, output, session) {
  
  # set empty plot
  pl0 <- reactive({empty_plot(combs)})
  # Show plot
  output$plot1 <- renderPlot({pl0()}, height = plot_dim)
  
  # Update text box to show info about risk
  updateTextInput(session = session, 
                  inputId = "riskText",
                  value   = "Are you at risk of developing AMD?")
  
  # set radio buttons
  observeEvent(input$ResetButton,{
    updateRadioButtons(session  = session,
                       inputId  = df$gene[1],
                       choices  = ref$genotipi[ref$gene == df$gene[1]],
                       selected = character(0))
    
    updateRadioButtons(session  = session,
                       inputId  = df$gene[2],
                       choices  = ref$genotipi[ref$gene == df$gene[2]],
                       selected = character(0))
    
    updateRadioButtons(session  = session,
                       inputId  = df$gene[3],
                       choices  = ref$genotipi[ref$gene == df$gene[3]],
                       selected = character(0))
    
    updateRadioButtons(session  = session,
                       inputId  = df$gene[4],
                       choices  = ref$genotipi[ref$gene == df$gene[4]],
                       selected = character(0))
    
    updateRadioButtons(session  = session,
                       inputId  = df$gene[5],
                       choices  = ref$genotipi[ref$gene == df$gene[5]],
                       selected = character(0))
    
    updateRadioButtons(session  = session,
                       inputId  = df$gene[6],
                       choices  = ref$genotipi[ref$gene == df$gene[6]],
                       selected = character(0))
    
    updateRadioButtons(session  = session,
                       inputId  = df$gene[7],
                       choices  = ref$genotipi[ref$gene == df$gene[7]],
                       selected = character(0))
    
    updateRadioButtons(session  = session,
                       inputId  = df$gene[8],
                       choices  = ref$genotipi[ref$gene == df$gene[8]],
                       selected = character(0))
  })
  
  observeEvent(input$execute, {
    
    # Assign selected genotype for each gene
    try(df$selected_genotype[df$gene == "ARMS2"]     <- input$ARMS2)
    try(df$selected_genotype[df$gene == "CFH"]       <- input$CFH)
    try(df$selected_genotype[df$gene == "IL8"]       <- input$IL8)
    try(df$selected_genotype[df$gene == "VEGFA"]     <- input$VEGFA)
    try(df$selected_genotype[df$gene == "TIMP3"]     <- input$TIMP3)
    try(df$selected_genotype[df$gene == "SLC16A8"]   <- input$SLC16A8)
    try(df$selected_genotype[df$gene == "COL8A1"]    <- input$COL8A1)
    try(df$selected_genotype[df$gene == "RAD51B"]    <- input$RAD51B)
    
    # Assign normalized OR for selected genotype
    try(df$or.norm[df$gene == "ARMS2"]   <- ref$or.normalizzato[ref$gene == "ARMS2"   & ref$genotipi == input$ARMS2])
    try(df$or.norm[df$gene == "CFH"]     <- ref$or.normalizzato[ref$gene == "CFH"     & ref$genotipi == input$CFH])
    try(df$or.norm[df$gene == "IL8"]     <- ref$or.normalizzato[ref$gene == "IL8"     & ref$genotipi == input$IL8])
    try(df$or.norm[df$gene == "VEGFA"]   <- ref$or.normalizzato[ref$gene == "VEGFA"   & ref$genotipi == input$VEGFA])
    try(df$or.norm[df$gene == "TIMP3"]   <- ref$or.normalizzato[ref$gene == "TIMP3"   & ref$genotipi == input$TIMP3])
    try(df$or.norm[df$gene == "SLC16A8"] <- ref$or.normalizzato[ref$gene == "SLC16A8" & ref$genotipi == input$SLC16A8])
    try(df$or.norm[df$gene == "COL8A1"]  <- ref$or.normalizzato[ref$gene == "COL8A1"  & ref$genotipi == input$COL8A1])
    try(df$or.norm[df$gene == "RAD51B"]  <- ref$or.normalizzato[ref$gene == "RAD51B"  & ref$genotipi == input$RAD51B])
    
    print(df)
    
    if (any(is.na(df)) == TRUE) {
      updateTextInput(session = session, 
                      inputId = "inText",
                      value   = "You must insert all the genotype values!")
    } else {
      # Calculate risk by multiplying Odd Ratios
      risk = round(prod(df$or.norm),4)
      
      # Update text box to show total risk
      updateTextInput(session = session, 
                      inputId = "inText",
                      value   = as.character(risk))
      
      # Pheno summary plot
      pl1 <- reactive({risk_plot(combs, calculated_risk = risk)})
      
      # Show plot
      output$plot1 <- renderPlot({pl1()}, height = plot_dim)
    }
  })
}

shinyApp(ui, server)