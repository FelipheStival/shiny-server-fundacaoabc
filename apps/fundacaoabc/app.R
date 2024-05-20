#==============================================#
# Carregando pacotes a serem utilizados
app.LoadPackages = function()
{
    #=============================================#
    # Iniciando bibliotecas web
    require(shiny) 
    require(shinydashboard)
    require(shinyjs)
    require(shinymanager)
    require(shiny.router)
    require(leaflet)
    require(shinycssloaders)
    require(RJDBC)
    require(seas)
    require(ggplot2)
    require(plotly)
    require(reshape2)
    require(dplyr)
    require(lubridate)
    require(stringr)
    require(ggrepel)
    require(ggthemes)
    require(scatterD3)
    require(lme4)
    require(shinyWidgets)
    require(forcats)
    require(emmeans)
    require(gge)
    require(GGEBiplots)
    require(ape)
    require(rmarkdown)
    require(knitr)
    require(tinytex)
    require(DT)
    require(shinythemes)
    require(shinyalert)
    require(tidyverse)
    require(RVAideMemoire)
    require(caret)
    require(lmerTest)
    require(broom.mixed)
    require(plyr)
    require(cluster)
    require(fdm2id)
    require(cowplot)
    require(shinybusy)
    require(flexclust)
    require(plotly)
  
    #==============================================#
}

#==============================================#
# Carregando arquivos compilados
app.LoadModules = function() {
  
    # Carregando modulos
    modulos = list.files(path = 'modules',
                         pattern = ".R$",
                         recursive = T,
                         full.names = T
                         )
    log = sapply(modulos,source,encoding="utf-8")
    
}
#==============================================#

#==============================================#
# Carregando funções globais
app.globalFunctions = function(){
  
  customSpinner <<- function(outputFun){
    f = withSpinner(outputFun, image = 'logos//loader.png', image.width = '100px', image.height = '70px')
    return(f)
  }
  
}
#==============================================#
app.LoadPackages()
app.globalFunctions()
app.LoadModules()

shinyApp(ui, server)
