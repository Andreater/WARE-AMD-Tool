# User interface ----
ui <- dashboardPage(
  skin = "green",
  title = "Wet AMD Risk Evaluation",
  dashboardHeader(title = strong("WARE")),
  
  dashboardSidebar(disable = F,
                   sidebarMenu(
                     menuItem(
                       "Genetic risk", 
                       tabName = "gen"
                     ),
                     menuItem(
                       "Environmental risk",
                       tabName = "env"
                     )
                   )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "gen",
              fluidRow(
                box(
                  title = strong("PERSONAL GENOTYPE"),
                  
                  h4(style="text-align: justify;",
                     "Genes and SNPs are reported below. Add your genotype at each SNP and click on the “Execute” button."),
                  br()
                  
                ),
                box(
                  title = strong("INDIVIDUALIZED GENETIC AMD RISK"),
                  
                  h4(style="text-align: justify;",
                     "Genetic risk for wet AMD is calculated according to [ref] and it ranges between 0.0002 and 3418.366. A graphical representation of your position in the scale of genetic risks is reported below.")
                  
                )
              ),
              fluidRow(
                box(
                  width = 2,
                  
                  radioButtons(inputId  = df$gene[1],
                               label    = paste0(df$gene[1], " - ", unique(ref$snps[ref$gene == df$gene[1]])),
                               choices  = ref$genotipi[ref$gene == df$gene[1]],
                               selected = character(0)),
                  
                  radioButtons(inputId  = df$gene[2],
                               label    = paste0(df$gene[2], " - ", unique(ref$snps[ref$gene == df$gene[2]])),
                               choices  = ref$genotipi[ref$gene == df$gene[2]],
                               selected = character(0)),
                  
                  radioButtons(inputId  = df$gene[3],
                               label    = paste0(df$gene[3], " - ", unique(ref$snps[ref$gene == df$gene[3]])),
                               choices  = ref$genotipi[ref$gene == df$gene[3]],
                               selected = character(0)),
                  
                  height = radiobox_height
                ),
                box(
                  width = 2,
                  
                  radioButtons(inputId  = df$gene[4],
                               label    = paste0(df$gene[4], " - ", unique(ref$snps[ref$gene == df$gene[4]])),
                               choices  = ref$genotipi[ref$gene == df$gene[4]],
                               selected = character(0)),
                  
                  radioButtons(inputId  = df$gene[5],
                               label    = paste0(df$gene[5], " - ", unique(ref$snps[ref$gene == df$gene[5]])),
                               choices  = ref$genotipi[ref$gene == df$gene[5]],
                               selected = character(0)),
                  
                  radioButtons(inputId  = df$gene[6],
                               label    = paste0(df$gene[6], " - ", unique(ref$snps[ref$gene == df$gene[6]])),
                               choices  = ref$genotipi[ref$gene == df$gene[6]],
                               selected = character(0)),
                  
                  height = radiobox_height
                ),
                box(width = 2,
                    radioButtons(inputId  = df$gene[7],
                                 label    = paste0(df$gene[7], " - ", unique(ref$snps[ref$gene == df$gene[7]])),
                                 choices  = ref$genotipi[ref$gene == df$gene[7]],
                                 selected = character(0)),
                    
                    radioButtons(inputId  = df$gene[8],
                                 label    = paste0(df$gene[8], " - ", unique(ref$snps[ref$gene == df$gene[8]])),
                                 choices  = ref$genotipi[ref$gene == df$gene[8]],
                                 selected = character(0)),
                    
                    br(),
                    br(),
                    br(),
                    
                    actionButton(inputId = "execute",
                                 label   = "Execute"),
                    
                    actionButton(inputId = "ResetButton",
                                 label   = "Reset"),
                    
                    height = radiobox_height
                ),
                box(
                  textInput(inputId = "inText", 
                            label   = "Genetic risk")
                ),
                box(
                  title = strong("Genetic risk plot"),
                  
                  plotOutput(outputId = "plot1",
                             height   = plot1_height),
                  
                  textInput(inputId = "riskText",
                            label   = NULL,
                            value   = "")
                )
              ),
              fluidRow(
                box(
                  title = strong("CHART OF RISKS"),
                  
                  h4(style="text-align: justify;",
                     "The pie charts report relative risks in AMD population and in general population. The red portion of the chart represents people with a risk higher than yours, the green portion represents people with a risk lower than yours, and the grey portion represents people with risk similar to yours.")
                ),
                box(
                  title = strong("RISK DISTRIBUTION"),
                  
                  h4(style="text-align: justify;",
                     "The graphics below represent the distribution of genotypes in AMD patients and general population. The dashed lines delimitate low and high risk zones, and the solid red line shows your individualized genetic risk according to genotypes reported in the “personal genotype” box."),
                  br()
                )
              ),
              fluidRow(
                box(
                  plotOutput(outputId = "piecases",
                             height = pie_plot_height),
                  
                  plotOutput(outputId = "piectrl",
                             height = pie_plot_height),
                  
                  plotOutput(outputId = "pielegend",
                             height = pie_legend_height),
                  
                  height = plotbox_height
                ),
                box(
                  plotOutput(outputId = "densitycases",
                             height = density_plot_height),
                  
                  plotOutput(outputId = "densityctrl",
                             height = density_plot_height),
                  
                  height = plotbox_height
                )
              )
      ),
      tabItem(tabName = "env",
              fluidRow(
                box(
                  title = strong("ENVIRONMENTAL RISK EXPOSURE"),
                  
                  h4(style="text-align: justify;",
                     "Environmental risk factors are reported below. Make your selection based on patient's profile and click on the “Execute” button."),
                  br(),
                  br()
                ),
                box(
                  title = strong("INDIVIDUALIZED GENETIC AMD RISK"),
                  
                  h4(style="text-align: justify;",
                     "Environmental risk factors are combined with genetic risk for wet AMD, calculated according to [ref]. When genetic risk is combined with environmental risk it ranges between 0.0002 and 23313.26. A graphical representation of your position in the scale of combined environmental and genetic risks is reported below.")
                )
              ),
              fluidRow(
                box(
                  radioButtons(inputId  = df_env$factor[1],
                               label    = df_env$factor[1],
                               choices  = ref_env$Level[ref_env$Factor == df_env$factor[1]],
                               selected = character(0)),
                  
                  radioButtons(inputId  = df_env$factor[2],
                               label    = df_env$factor[2],
                               choices  = ref_env$Level[ref_env$Factor == df_env$factor[2]],
                               selected = character(0)),
                  
                  actionButton(inputId = "execute_env",
                               label   = "Execute"),
                  
                  actionButton(inputId = "ResetButton_env",
                               label   = "Reset"),
                  
                  height = radiobox_height
                ),
                box(
                  textInput(inputId = "envRisk",
                            label   = "Environmental risk",
                            value   = "")
                ),
                box(
                  title = "Environmental risk plot",
                  
                  plotOutput(outputId = "plot1_env",
                             height = plot1_height),
                  
                  textInput(inputId = "riskText_env",
                            label   = NULL,
                            value   = "What's your environmental risk for AMD?")
                )
              )
      )
    )
  )
)
