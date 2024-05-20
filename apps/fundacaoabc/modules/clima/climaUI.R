#=====================================================================
# Pagina clima
#=====================================================================
climaUI = div(
    id = "clima-container",
    dashboardPage(
    
    #========================header=========================
    dashboardHeader(
      title = tags$img(src='logos//abc.png', width = 50),
      tags$li(
        class = "dropdown",
        actionLink(
          inputId = "btnRetonarClima",
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
          text = "Mapa",
          tabName = "mapaTab",
          icon = icon("map"),
          selected = T
        ),
        menuItem(
          text = "Tabela",
          tabName = "tabelaTab",
          icon = icon("table")
        ),
        menuItem(
          text = "Gráficos",
          tabName = "analiseUI",
          icon = icon("line-chart"),
          menuSubItem(
            text = "Gráfico básico",
            tabName = "graficosPerdidosPlot",
            icon = icon("bar-chart")
          ),
          menuSubItem(
            text = "Mapa matriz",
            tabName = "mapaMatrizplot",
            icon = icon("bar-chart")
          ),
          menuSubItem(
            text = "Precipitação",
            tabName = "Precipitacaoplot",
            icon = icon("bar-chart")
          ),
          menuSubItem(
            text = "Anomalia Precipitação",
            tabName = "AnomaliaPrecipitacaoplot",
            icon = icon("bar-chart")
          ),
          menuSubItem(
            text = "Anomalia Temperatura Precipitação",
            tabName = "AnomaliaTemperaturaPlot",
            icon = icon("bar-chart")
          ),
          menuSubItem(
            text = "Precipitação cumulativa",
            tabName = "PrecipitacaoCumulativa",
            icon = icon("bar-chart")
          ),
          menuSubItem(
            text = "Dia seco e umido",
            tabName = "diaSecoUmido",
            icon = icon("bar-chart")
          ),
          menuSubItem(
            text = "Periodo Chuvoso",
            tabName = "periodoChuvosoPlot",
            icon = icon("bar-chart")
          )
        ),
        menuItem(
          text = "Selecione a estação",
          icon = icon("street-view"),
          selectInput(
            inputId = "estadoInput",
            label = "Selecione o estado:",
            choices = "SC"
          ),
          selectInput(
            inputId = "cidadeInput",
            label = "Selecione a cidade: ",
            choices = NULL
          ),
          dateRangeInput(
            inputId = "periodoInput",
            label = "Selecione o período:",
            start = '2000-02-11',
            end = '2021-02-15'
          )
        )
      )),
    
    #========================================================
    
    
    #=======================body=============================
    
    dashboardBody(
      tabItems(
        
        # =========== Aba do mapa ====================
        tabItem(tabName = "mapaTab",
                leafletOutput(outputId = "mapaEstacoes", width = "100%", height = "90vh") %>% customSpinner()
        ),
        #=============================================
        
        # =========== Aba da tabela ==================
        tabItem(tabName = "tabelaTab",
                box(
                  width = 12,
                  dataTableOutput('tabelaDados') %>% customSpinner(),
                  downloadButton("downloadDados", "Download")
                )
        ),
        #=============================================
        
        # ==== Gráfico precipitacao acumulativa ======
        tabItem(tabName = "PrecipitacaoCumulativa",
                box(
                  width = 12, 
                  plotOutput("PrecipitacaoCumulativaPlot", width = "100%", height = "85vh") %>% customSpinner()
                )
        ),
        #=============================================
        
        # ====== Gráfico anomalia temperatura ========
        tabItem(tabName = "AnomaliaTemperaturaPlot",
                box(
                  width = 3,
                  selectInput(
                    inputId = "safraGrupoInput",
                    label = "Selecione o grupo de dias:",
                    choices = c("Safra",
                                "Verao",
                                "Outono",
                                "Inverno",
                                "Primavera"),
                    selected = "Safra"
                  )
                ),
                box(
                  width = 9,
                  plotOutput("AnomaliaTemperaturaPlot", width = "100%", height = "85vh") %>% customSpinner()
                )
        ),
        #=============================================
        
        # ========= Gráfico precipitação =============
        tabItem(tabName = "Precipitacaoplot",
                box(
                  width = 4,
                  selectInput(
                    inputId = "grupoDiasSelectPrec",
                    label = "Selecione o grupo de dias:",
                    choices = c(
                      "10 dias" = 10,
                      "21 dias" = 21,
                      "Mensal" = "mon"
                    )
                  )
                ),
                box(
                  width = 8,
                  plotOutput("plotPrecipitacao", width = "100%", height = "85vh") %>% customSpinner()
                )
        ),
        #=============================================
        
        # ========= Dia seco e umido =================
        tabItem(tabName = "diaSecoUmido",
                box(
                  width = 4,
                  selectInput(
                    inputId = "secoUmidoGrupoDias",
                    label = "Selecione o grupo de dias:",
                    choices = c(
                      "10 dias" = 10,
                      "21 dias" = 21,
                      "Mensal" = "mon"
                    )
                  )
                ),
                box(
                  width = 8,
                  plotOutput("secoUmidoPlot", width = "100%", height = "85vh") %>% customSpinner()
                )
        ),
        #=============================================
        
        # ========= Gráfico período chuvoso ==========
        tabItem(tabName = "periodoChuvosoPlot",
                box(
                  width = 12,
                  height = "85vh",
                  plotOutput("periodoChuvosoPlot", width = "100%", height = "85vh") %>% customSpinner()
                )
        ),
        #=============================================
        
        # ========== Anomalia precipitacao ===========
        tabItem(tabName = "AnomaliaPrecipitacaoplot",
                box(
                  width = 3,
                  selectInput(
                    inputId = "anoSelectAnomalia",
                    label = "Selecione o ano: ",
                    choices = NULL
                  )
                ),
                box(
                  width = 9,
                  plotOutput("anomaliaPrecipitacaoPlot", width = "100%", height = "85vh") %>% customSpinner()
                )
        ),
        #=============================================
        
        # ============ Gráfico básico ===============
        tabItem(tabName = "graficosPerdidosPlot",
                box(
                  width = 4,
                  selectInput(
                    inputId = "boxplotVariavel",
                    label = "Selecione a variavel:",
                    choices = climaProviderEscolhasGenericas()
                  ),
                  selectInput(
                    inputId = "grupodiasBoxPlot",
                    label = "Selecione o grupo de dias:",
                    choices = c(
                      "10 Dias" = 10,
                      "21 Dias" = 21,
                      "Mensal" = "Mon"
                    )
                  )
                ),
                box(
                  width = 8,
                  plotOutput("graficosPerdidosPlot", width = "100%", height = "80vh") %>% customSpinner()
                )
        ),
        #=============================================
        
        # ============ Gráfico Matriz ================
        tabItem(tabName = "mapaMatrizplot",
                box(
                  width = 4,
                  selectInput(
                    inputId = "variavelSelect",
                    label = "Selecione a variavel:",
                    choices = climaProviderEscolhasGenericas()
                  ),
                  selectInput(
                    inputId = "grupoDiasSelect",
                    label = "Selecione o grupo de dias:",
                    choices = c(
                      "10 dias" = 10,
                      "21 dias" = 21,
                      "Mensal" = "mon"
                    )
                  )
                ),
                box(
                  width = 8,
                  plotOutput("Matrizplot", width = "100%", height = "85vh") %>% customSpinner()
                )
        )
        #=============================================
      )
    )
    #========================================================
  )
)
