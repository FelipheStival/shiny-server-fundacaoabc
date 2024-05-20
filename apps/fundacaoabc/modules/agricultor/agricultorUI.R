#=====================================================================
# Página agricultor
#=====================================================================
agricultorUI = div(
  id = "clima-container",
  dashboardPage(
    
    #========================header=========================
    dashboardHeader(
      title = tags$img(src='logos//abc.png', width = 50),
      tags$li(
        class = "dropdown",
        actionLink(
          inputId = "btnRetonarGerenciar",
          label = "Voltar",
          icon = icon("arrow-circle-left"),
          style = "font-size: 1.3em; color: #161925"
        )
      ),
      titleWidth = 260
    ),
    
    #=======================================================
    
    #=======================SiderBar========================
    
    dashboardSidebar(
      includeCSS('www//styles//styles.css'),
      sidebarMenu(
        menuItem(
          text = "Resultado",
          tabName = "resultado",
          icon = icon("line-chart"),
          selected = T
        ),
        menuItem(
          text = "Variaveis",
          icon = icon("hand-point-up"),
          selectInput(
            inputId = "inputTipoArvoreDecisao",
            label = "Selecione o tipo de sistema:",
            choices = c('Irrigado', 'Sequeiro')
          )
        )
      )
    ),
    
    #========================================================
    
    
    #=======================body=============================
    
    dashboardBody(
      tabItems(
        
        #========= Página de resultado==================
        tabItem(
          tabName = "resultado",
          column(
            width = 2,
            actionButton('btnAddGenotipo', 'Adicionar simulação', icon = icon("hand-point-up"), class = "btn-primary btn-lg text-light btn-white")
          ),
          column(
            width = 10,
            box(
              width = 12,
              uiOutput('resultadoArvoreDecisao', width = '100%')
            )
          )
        )
      )
    )
    #========================================================
  )
)