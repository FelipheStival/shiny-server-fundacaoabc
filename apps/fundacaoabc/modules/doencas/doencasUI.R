#=====================================================================
# Pagina Doencas
#=====================================================================
doencasUI = div(
  id = "clima-container",
  dashboardPage(
    
    #======================== header =========================
    dashboardHeader(
      title = tags$img(src='logos//abc.png', width = 50),
      tags$li(
        class = "dropdown",
        actionLink(
          inputId = "btnRetonarDoencas",
          label = "Voltar",
          icon = icon("arrow-circle-left"),
          style = "font-size: 1.3em; color: #161925"
        )
      ),
      titleWidth = 260
    ),
    #=========================================================
    
    #======================== Side bar =======================
    dashboardSidebar(
      width = 260,
      sidebarMenu(
      menuItem(
        text = "Gráficos doenças",
        tabName = "graficosDoenca",
        icon = icon("line-chart"),
        selected = T
      ),
      menuItem(
        text = "Variaveis",
        icon = icon("hand-point-up"),
        selectizeInput(
          inputId = "culturaInputDoencas2",
          label = "Seleciona a cultura:",
          choices = "Feijão",
          options = list(maxItems = 1)
        ),
        selectizeInput(
          inputId = "safraInputDoencas2",
          label = "Selecione a safra:",
          choices = "15/16",
          options = list(maxItems = 1)
        )
     )
   )
  ),
  
  #=========================================================
  
  dashboardBody(
    useShinyjs(),
    tabItems(
      
      #============ Pagina de gráficos de doencas ==============
      tabItem(tabName = "graficosDoenca",
              column(
                width = 3,
                box(
                  width = 12,
                  selectInput(
                    inputId = "select_doencas_local",
                    label = "Selecione a cidade:",
                    choices = c('Arapoti'),
                    selected = 'Arapoti'
                  ),
                )
              ),
              column(
                width = 9,
                tabBox(
                  width = '100%',
                  tabsetPanel(
                    id = 'doencasTabGraficos',
                    tabPanel('Gráfico geral',plotOutput('graficoDoencasPlot2', width = '100%', height = '80vh') %>% customSpinner()),
                    tabPanel('Gráfico local',plotOutput('graficoDoencasPlot1', width = '100%', height = '80vh') %>% customSpinner())
                  )
                )     
              )
      
        )
      
      #=========================================================#
      
      )
    )
  )
)
