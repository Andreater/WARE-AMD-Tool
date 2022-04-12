# Server ----
server <- function(input, output, session) {
  
  #############
  # set plots
  # set empty plots
  pl0 <- reactive({empty_risk_plot(combs)})
  
  pie_empty1 <- reactive({empty_pie_chart()})
  pie_empty2 <- reactive({empty_pie_chart()})
  
  legend <- reactive({draw_legend()})
  
  density1_empty <- reactive({empty_density_plot_cases(risk)})
  density2_empty <- reactive({empty_density_plot_ctrl(risk)})
  
  ## for env tab
  pl0_env <- reactive({empty_risk_plot(combs_env)})
  
  # Show plots
  output$plot1 <- renderPlot({pl0()}, 
                             height = plot_dim)
  
  output$piecases <- renderPlot({pie_empty1()})
  output$piectrl  <- renderPlot({pie_empty2()})
  
  output$pielegend <- renderPlot({legend()})
  
  output$densitycases  <- renderPlot({density1_empty()})
  output$densityctrl  <- renderPlot({density2_empty()})
  
  # for env tab
  output$plot1_env <- renderPlot({pl0_env()},
                                 height = plot_dim)
  
  # Update text box to show info about risk
  updateTextInput(session = session, 
                  inputId = "riskText",
                  value   = "What's your genetic risk for AMD?")
  
  #########
  # reset button
  observeEvent(input$ResetButton,{
    # set radio buttons
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
    
    # Update text box to show total risk
    updateTextInput(session = session, 
                    inputId = "inText",
                    value   = "")
    
    # Update text box to show info about risk
    updateTextInput(session = session, 
                    inputId = "riskText",
                    value   = "What's your genetic risk for AMD?")
    
    # set empty plots
    pl0 <- reactive({empty_risk_plot(combs)})
    
    pie_empty1 <- reactive({empty_pie_chart()})
    pie_empty2 <- reactive({empty_pie_chart()})
    
    legend <- reactive({draw_legend()})
    
    density1_empty <- reactive({empty_density_plot_cases(risk)})
    density2_empty <- reactive({empty_density_plot_ctrl(risk)})
    
    # Show plots
    output$plot1 <- renderPlot({pl0()}, height = plot_dim)
    
    output$piecases <- renderPlot({pie_empty1()})
    output$piectrl  <- renderPlot({pie_empty2()})
    
    output$pielegend <- renderPlot({legend()})
    
    output$densitycases  <- renderPlot({density1_empty()})
    output$densityctrl  <- renderPlot({density2_empty()})
    
    risk <<- NA
  })
  
  #############
  observeEvent(input$execute, {
    
    # Assign selected genotype for each gene
    try(df$selected_genotype[df$gene == "ARMS2"]     <- input$ARMS2, silent = T)
    try(df$selected_genotype[df$gene == "CFH"]       <- input$CFH, silent = T)
    try(df$selected_genotype[df$gene == "IL8"]       <- input$IL8, silent = T)
    try(df$selected_genotype[df$gene == "VEGFA"]     <- input$VEGFA, silent = T)
    try(df$selected_genotype[df$gene == "TIMP3"]     <- input$TIMP3, silent = T)
    try(df$selected_genotype[df$gene == "SLC16A8"]   <- input$SLC16A8, silent = T)
    try(df$selected_genotype[df$gene == "COL8A1"]    <- input$COL8A1, silent = T)
    try(df$selected_genotype[df$gene == "RAD51B"]    <- input$RAD51B, silent = T)
    
    # Assign normalized OR for selected genotype
    try(df$or.norm[df$gene == "ARMS2"]   <- ref$or.normalizzato[ref$gene == "ARMS2"   & ref$genotipi == input$ARMS2], silent = T)
    try(df$or.norm[df$gene == "CFH"]     <- ref$or.normalizzato[ref$gene == "CFH"     & ref$genotipi == input$CFH], silent = T)
    try(df$or.norm[df$gene == "IL8"]     <- ref$or.normalizzato[ref$gene == "IL8"     & ref$genotipi == input$IL8], silent = T)
    try(df$or.norm[df$gene == "VEGFA"]   <- ref$or.normalizzato[ref$gene == "VEGFA"   & ref$genotipi == input$VEGFA], silent = T)
    try(df$or.norm[df$gene == "TIMP3"]   <- ref$or.normalizzato[ref$gene == "TIMP3"   & ref$genotipi == input$TIMP3], silent = T)
    try(df$or.norm[df$gene == "SLC16A8"] <- ref$or.normalizzato[ref$gene == "SLC16A8" & ref$genotipi == input$SLC16A8], silent = T)
    try(df$or.norm[df$gene == "COL8A1"]  <- ref$or.normalizzato[ref$gene == "COL8A1"  & ref$genotipi == input$COL8A1], silent = T)
    try(df$or.norm[df$gene == "RAD51B"]  <- ref$or.normalizzato[ref$gene == "RAD51B"  & ref$genotipi == input$RAD51B], silent = T)
    
    # print(df)
    
    if (any(is.na(df)) == TRUE) {
      updateTextInput(session = session, 
                      inputId = "inText",
                      value   = "You must insert all the genotype values!")
    } else {
      # Calculate risk by multiplying Odd Ratios
      risk <<- round(prod(df$or.norm), round_to_decimals)
      
      # Update text box to show total risk
      updateTextInput(session = session, 
                      inputId = "inText",
                      value   = as.character(risk))
      
      # Pheno summary plot
      pl1 <- reactive({risk_plot(combs, calculated_risk = risk)})
      
      # pie charts
      pie1 <- reactive({pie_chart_casi(risk, intorno_dim)})
      pie2 <- reactive({pie_chart_ctrl(risk, intorno_dim)})
      
      # density plots
      density1 <- reactive({density_plot_cases(risk)})
      density2 <- reactive({density_plot_ctrl(risk)})
      
      # Show plots
      output$plot1 <- renderPlot({pl1()}, height = plot_dim)
      
      output$piecases <- renderPlot({pie1()})
      output$piectrl  <- renderPlot({pie2()})
      
      output$densitycases  <- renderPlot({density1()})
      output$densityctrl  <- renderPlot({density2()})
      
      
      # Update text box to show info about risk
      z_score = z_score_calc(risk = log(risk), or_vec = df_casi$log.comb.or)
      
      updateTextInput(session = session, 
                      inputId = "riskText",
                      value   = paste0("With a risk = ",
                                       risk,
                                       " your subject is ",
                                       z_score,
                                       " standard deviations away from the mean risk for AMD population."))
    }
  })
  
  observeEvent(input$ResetButton_env, {
    updateRadioButtons(session  = session,
                       inputId  = df_env$factor[1],
                       choices  = ref_env$Level[ref_env$Factor == df_env$factor[1]],
                       selected = character(0))
    
    updateRadioButtons(session  = session,
                       inputId  = df_env$factor[2],
                       choices  = ref_env$Level[ref_env$Factor == df_env$factor[2]],
                       selected = character(0))
    
    updateTextInput(session = session, 
                    inputId = "envRisk",
                    value   = "")
    
    # set plot
    ## for env tab
    pl0_env <- reactive({empty_risk_plot(combs_env)})
    
    # show plot
    ## for env tab
    output$plot1_env <- renderPlot({pl0_env()}, height = plot_dim)
    
    # set risk text box
    updateTextInput(session = session, 
                    inputId = "riskText_env",
                    value   = "What's your non-genetic risk for AMD?")
  })
  
  observeEvent(input$execute_env, {
    
    try(df_env$selected_level[df_env$factor == "Familiarity"] <- input$Familiarity, silent = T)
    try(df_env$selected_level[df_env$factor == "Smoke"]       <- input$Smoke, silent = T)
    
    try(df_env$or[df_env$factor == "Familiarity"]  <- ref_env$Risk[ref_env$Factor == "Familiarity" & ref_env$Level == input$Familiarity], silent = T)
    try(df_env$or[df_env$factor == "Smoke"]        <- ref_env$Risk[ref_env$Factor == "Smoke" & ref_env$Level == input$Smoke], silent = T)
    
    #print(df_env)
    
    if (is.na(risk)) {
      updateTextInput(session = session, 
                      inputId = "envRisk",
                      value   = "You must calculate the genetic risk first!")
    } else if (any(is.na(df_env$selected_level))) {
      
      updateTextInput(session = session, 
                      inputId = "envRisk",
                      value   = "You must set a value for both the non-genetic factors!")
    } else {
      # Calculate risk by multiplying Odd Ratios
      risk_env = round(prod(df_env$or) * risk, round_to_decimals)
      
      # Update text box to show total risk
      updateTextInput(session = session, 
                      inputId = "envRisk",
                      value   = as.character(risk_env))
      
      # Update text box to show info about risk
      z_score = z_score_calc(risk = log(risk_env), or_vec = combs_env$log.comb.or)
      
      updateTextInput(session = session, 
                      inputId = "riskText_env",
                      value   = paste0("With a risk = ",
                                       risk_env,
                                       " your subject is ",
                                       z_score,
                                       " standard deviations away from the mean of risks."))
      
      # set plot
      ## for env tab
      pl1_env <- reactive({risk_plot(combs_env, calculated_risk = risk_env)})
      
      # show plot
      ## for env tab
      output$plot1_env <- renderPlot({pl1_env()}, height = plot_dim)
    }
  })
}