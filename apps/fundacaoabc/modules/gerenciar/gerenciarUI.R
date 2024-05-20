#==================================================================
# Gerenciar UI
#==================================================================
gerenciarUI = function(){
  div(id = "clima-container",
      dashboardPage(
        #========================header=========================
        
        dashboardHeader(title = tags$img(src='logos//abc.png', width = 50),
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
        
        dashboardSidebar(width = 260,
                         sidebarMenu(
                           menuItem(
                             text = "Atualizar dados",
                             tabName = "atualizarDados",
                             icon = icon("upload"),
                             selected = T
                           ),
                           menuItem(
                             text = "Configurações",
                             icon = icon("cog"),
                             selectInput(
                               inputId = "baseGerenciarInput",
                               label = "Selecione a base: ",
                               choices = c("doenças" = "doencas",
                                           "experimentos" = "experimentos",
                                           "clima" = "clima"
                                           )
                             )
                           )
                         )
                        ),
        #========================================================
        
        #=======================body=============================
        dashboardBody(
          tabItems(
            tabItem(tabName = "atualizarDados",
              fluidRow(
                
                box(width = 3,
                    title = 'Escolha um arquivo CSV',
                    solidHeader = TRUE,
                    status = "primary",
                    #===================Escolher arquivo=======================
                    
                    fileInput(inputId = 'arquivo',
                              label = 'Escolha um arquivo CSV',
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
                    
                    #===========================================================
                    
                    tags$hr(),
                    
                    #===================Header arquivo==========================
                    
                    checkboxInput(inputId = 'cabecalho',
                                  label = 'Cabecalho',
                                  value = TRUE),
                    
                    #===========================================================
                    
                    #===================Separador arquivo=======================
                    
                    radioButtons(inputId = 'separador',
                                 label = 'Separador',
                                 choices = c("," = ",",
                                             ";" = ";",
                                             "t" = "\t"),
                                 selected = ","),
                    
                    #===================Realizar Upload=======================
                    
                    actionButton(inputId = 'btnnatualizarDados',
                                 label = 'Atualizar dados')
                ),
                
                #===========================================================
                
                
                
                #==================Saida tabela==============================
                
                box(width = 9,
                    title = 'Tabela',
                    solidHeader = TRUE,
                    align="center",
                    status = "primary",
                    uiOutput("UItabela") %>% withSpinner(color="#0dc5c1")
                )
              )
            )
          )
        )
      )
  )
} 