#=====================================================================
# Pagina Experimentos
#=====================================================================
experimentosUI =  div(
  id = "clima-container",
  dashboardPage(
    
    #========================header=========================
    dashboardHeader(
      title = tags$img(src='logos//abc.png', width = 50),
      tags$li(
        class = "dropdown",
        actionLink(
          inputId = "btnRetonarExperimentos",
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
      useShinyjs(),
      sidebarMenu(
        menuItem(
          "Gráficos",
          tabName = "dados-perdidos",
          icon = icon("line-chart"),
         #menuSubItem(
            #"Dados Perdidos",
            #tabName = "dados-perdidos",
            #icon = icon("line-chart"),
            #selected = T
          #),
          menuSubItem(
            "Diagnostico",
            tabName = "diagnostico",
            icon = icon("line-chart"),
            selected = T
          ),
          menuItem(
            "Analise Estatistica",
            tabName = "Container-analise-estatistica",
            icon = icon("line-chart"),
            menuSubItem("Analise", tabName = "analise-estatistica"),
            menuSubItem("Heatmap", tabName = "analise-hetmap"),
            menuSubItem("Grafico Linhas", tabName = "grafico-linhas")
          ),
          menuSubItem(
            "Analise GGE",
            tabName = "analise-gge",
            icon = icon("line-chart")
          ),
          menuSubItem(
            "Potencial genótipo produtivo",
            tabName = "analise-genotipo-produtivo",
            icon = icon("line-chart")
          )
        ),
        menuItem(
          text = "Variaveis",
          icon = icon("hand-point-up"),
          selectInput(
            inputId = "culturaInputDoencas",
            label = "Selecione a cultura:",
            choices = "Feijão"
          ),
          selectizeInput(
            inputId = "safraInputDoencas",
            label = "Selecione a safra:",
            choices = "15/16",
            options = list(maxItems = 2)
          ),
          selectInput(
            inputId = "estadoInputDoencas",
            label = "Selecione o estado:",
            choices = "Todos",
            multiple = T,
            selectize = T
          ),
          selectInput(
            inputId = "cidadeInputDoencas",
            label = "Selecione a cidade:",
            choices = "Todos",
            multiple = T,
            selectize = T
          ),
          selectInput(
            inputId = "irrigacaoInputDoencas",
            label = "irrigacao:",
            choices = c("Sim" = "sim",
                        "Nao" = "nao"),
            selected = "nao"
          ),
          selectInput(
            inputId = "fungicidaInputDoencas",
            label = "fungicida: ",
            choices = c("Sim" = "com",
                        "Nao" = "sem"),
            selected = "sem"
          ),
          selectInput(
            inputId = "tipodegraoInputDoencas",
            label = "Selecione o tipo de grao: ",
            choices = "Todos",
            selected = "Todos",
            multiple = T,
            selectize = T,
          ),
          selectInput(
            inputId = "grupoMaturacaoInputDoencas",
            label = "Selecione o grupo de maturação: ",
            choices = "Todos",
            selected = "Todos"
          )
        )
      )
    ),
    
    #========================================================
    
    
    #=======================body=============================
    
    dashboardBody(
      tabItems(
        
        #========= Página análise gge ==================
        tabItem(
          tabName = "analise-gge",
          column(
            width = 2,
            box(
              status = 'warning',
              width = '100%',
              selectInput(
                inputId = 'inputGenotiposGGE',
                label = 'Escolha os genótipos',
                choices = c('Todos'),
                selected = 'Todos',
                multiple = T
              )
            )
          ),
          column(
            width = 10,
            tabBox(
              width = "100%",
              height = "90vh",
              tabsetPanel(
                tabPanel(
                  title = "Quem vence e aonde",
                  plotOutput("grafico_analiseGGE_QuemVenceEAonde",width = "100%",height = "85vh") %>% customSpinner()
                ),
                
                tabPanel(
                  title = "Ordem de ambiente",
                  plotOutput("grafico_analiseGGE_OrdemDeAmbiente", width = "100%",height = "85vh") %>% customSpinner()
                ),
                
                tabPanel(
                  title = "Ordem de genotipo",
                  plotOutput("grafico_analiseGGE_OrdemDeGenotipo",width = "100%",height = "85vh") %>% customSpinner()
                ),
                
                tabPanel(
                  title = "Relacao entre ambientes",
                  plotOutput("grafico_analiseGGE_RelacaoEntreAmbientes",width = "100%",height = "85vh") %>% customSpinner()
                ),
                
                tabPanel(
                  title = "Estabilidade / Media",
                  plotOutput("grafico_analiseGGE_EstabilidadeMedia", width = "100%", height = "85vh") %>% customSpinner()
                ),
                
                tabPanel(
                  title = "Dendrograma",
                  plotOutput("grafico_analiseGGE_Denograma", width = "100%", height = "85vh") %>% customSpinner()
                )
              )
            )
          )
        ),
        #========================================================
        
        #=============== Página heat-map ========================
        tabItem(tabName = "analise-hetmap",
                box(
                  width = 12,
                  plotlyOutput("grafico_analiseEstatistica_Heatmap", width = "100%", height = "85vh") %>% customSpinner()
                )  
        ),
        #========================================================
        
        #============ Página gráfico de linhas ==================
        tabItem(tabName = "grafico-linhas",
                box(
                  width = 2,
                  selectInput(
                    "GenotipoSelectDoencas",
                    "Escolha os genotipos",
                    selected = NULL,
                    choices = NULL,
                    multiple = T,
                    selectize = T
                  )
                ),
                box(
                  width = 10,
                  plotOutput("graficolinha", width = "100%", height = "80vh")  %>% customSpinner()
                )
        ),
        
        #========================================================
        
        #=========== Página Análise estatistica =================
        tabItem(tabName = "analise-estatistica",
                column(
                  width = 3,
                  box(
                    width = 12,
                    status = "warning",
                    
                    selectInput(
                      inputId = "select_analiseEstatistica_local",
                      label = "selecione o local:",
                      choices = c("AL_TRA"),
                      selected = "AL_TRA"
                    ),
                    
                    selectInput(
                      inputId = "select_analiseEstatistica_media",
                      label = "Filtro por média:",
                      choices = c("Acima da média" = "ACIMA",
                                  "Abaixo da média" = "ABAIXO",
                                  "Todos" = "TODOS"),
                      selected = "ACIMA"
                    ), 
                  ),
                  box(
                    title = "Download relatorio",
                    width = 12,
                    status = "warning",
                    radioButtons(
                      inputId = "inputRelatorioFormato",
                      label = "Formato relatorio:",
                      choices = c("HTML" = "HTML"),
                      inline = T
                    ),
                    downloadButton(
                      outputId = 'downloadRelatorio',
                      label = "Relatório",
                      class = NULL
                    )
                  )
                ),
                column(
                  width = 9,
                  tabName = "analise-gge",
                  tabBox(
                    width = "100%",
                    height = "75vh",
                    tabsetPanel(
                      id = 'tabGraficosExperimentos',
                      tabPanel(
                        title = "Gráfico geral",
                        tabsetPanel(
                          id = "subTabGraficosExperimentos",
                          tabPanel(
                            title = 'Media geral',
                            plotlyOutput("grafico_analiseEstatistica_Resumo", width = "100%", height = "75vh") %>% customSpinner(),          
                          ),
                          tabPanel(
                            title = 'Gráfico cluster',
                            plotlyOutput("grafico_geral_cluster", width = "100%", height = "75vh") %>%  customSpinner(), 
                          )
                        )
                      ),
                      
                      tabPanel(
                        title =  "Gráfico media local",
                        plotlyOutput("grafico_analiseEstatistica_Unitario", width = "100%", height = "80vh") %>% customSpinner()
                      )
                    )
                  )
                )
        ),
        #========================================================
        
        #============== Página dados perdidos ===================
        #tabItem(tabName = "dados-perdidos",
                #plotOutput("grafico_dadosPerdidos_Estatistica", width = "100%", height = "90vh") %>% customSpinner()
        #),
        #========================================================
        
        #=============== Página diagnostico =====================
        tabItem(tabName = "diagnostico",
                box(
                  width = 12 ,
                  dataTableOutput("tabela_diagnostico_Exibir") %>% customSpinner(),
                  downloadButton("botao_diagnostico_Download", 'Download (csv)')
                )
        ),
        
        #========================================================
        
        #============ Página Pontecial produtivo ================
        tabItem(tabName = "analise-genotipo-produtivo",
                column(
                  width = 3,
                  box(
                    width = 12,
                    status = "warning",
                    selectInput(
                      inputId =  "select_analiseEstatistica_local_potencial_genotipo",
                      label = "selecione o local:",
                      choices = c("AL_TRA"),
                      selected = "AL_TRA"
                    )
                  )
                ),
                column(
                  width = 9,
                  box(
                    width = 12,
                    plotOutput('potencialGenotipoPlot', width = '100%', height = "80vh") %>% customSpinner()
                  )
                )
        )
        #========================================================
        
      )
    )
  )
)