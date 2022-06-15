source("libraries.r")
source("references.r")
source("parameters.r")
source("funcs.r")
source("plots.r")
source("ui.r")
source("server.r")

# Disable scientific notation
options(scipen=999)

# set base variable
risk <<- NA

# start app
shinyApp(ui, server)