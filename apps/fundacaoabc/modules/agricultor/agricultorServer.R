# Agricultor server
agricultorServer = function(input, output, session) {
  
  # Variaveis globais
  genotiposPasso1irrigado = c(
    "BMXExtremaIPRO",
    "HOAporéIPRO",
    "HOIguaçuIPRO",
    "HOJuruenaIPRO",
    "HOMamoréIPRO"
  )
  
  genotipopasso3irrigado = c(
    "HOAporéIPRO",
    "HOJuruenaIPRO",
    "HOMaracaíIPRO"
  )
  
  todosGenotipoSequeiro = c(
    "BMXBônusIPRO", 
    "BMXFocoIPRO", 
    "BS2606IPRO*", 
    "HOMaracaíIPRO",
    "M8372IPRO*",
    "NA5909RG*",
    "NS5445IPRO*",
    "NS5959IPRO*", 
    "TMG7062IPRO*",
    "TMG7063IPRO*",
    "TMG7067IPRO*",
    "TMG7260IPRO",
    "55I57RSFIPRO*",
    "55I57RSFIPRO*[Zeus]",
    "58I60RSF(BMXLançaIPRO)*",
    "58I60RSFIPRO*[Lança]",
    "59I60RSFIPRO*[Delta]",
    "64I61RSFIPRO*[Fibra]",
    "AS3590IPRO*",
    "FTR1157RR*",
    "FTR2557RR*",
    "M5838IPRO*",
    "M5947IPRO*",
    "57I52RSFIPRO*",
    "58I60RSF(BMXLançaIPRO)*",
    "58I60RSFIPRO*[Lança]",
    "64I61RSFIPRO*[Fibra]",
    "AS3590IPRO*",
    "FTR1157RR*",
    "FTR2557RR*",
    "K6221",
    "M5838IPRO*",
    "M5917IPRO*",
    "M5947IPRO*",
    "NS6209*",
    "58I60RSFIPRO*[Lança]",
    "59I60RSFIPRO*[Delta]",
    "64I61RSFIPRO*[Fibra]",
    "AS3590IPRO*",
    "M5947IPRO*",
    "57I52RSFIPRO*",
    "64I61RSFIPRO*[Fibra]",
    "FTR1157RR*",
    "FTR2557RR*",
    "K6221",
    "M5838IPRO*",
    "M5947IPRO*",
    "NS6209*"
  )
  
  genotiposPasso1Sequeiro = c(
    "BMXBônusIPRO", 
    "BMXFocoIPRO", 
    "BS2606IPRO*", 
    "HOMaracaíIPRO",
    "M8372IPRO*",
    "NA5909RG*",
    "NS5445IPRO*",
    "NS5959IPRO*", 
    "TMG7062IPRO*",
    "TMG7063IPRO*",
    "TMG7067IPRO*",
    "TMG7260IPRO"
  )
  
  genotiposPasso3EsquerdoSequeiro = c(
    "55I57RSFIPRO*",
    "55I57RSFIPRO*[Zeus]",
    "58I60RSF(BMXLançaIPRO)*",
    "58I60RSFIPRO*[Lança]",
    "59I60RSFIPRO*[Delta]",
    "64I61RSFIPRO*[Fibra]",
    "AS3590IPRO*",
    "FTR1157RR*",
    "FTR2557RR*",
    "M5838IPRO*",
    "M5947IPRO*"
  )
  
  genotipopasso3DireitoSequeiro = c(
    "57I52RSFIPRO*",
    "58I60RSF(BMXLançaIPRO)*",
    "58I60RSFIPRO*[Lança]",
    "64I61RSFIPRO*[Fibra]",
    "AS3590IPRO*",
    "FTR1157RR*",
    "FTR2557RR*",
    "K6221",
    "M5838IPRO*",
    "M5917IPRO*",
    "M5947IPRO*",
    "NS6209*"
  )
  
  genotipopasso6EsquerdoSequeiro = c(
    "58I60RSFIPRO*[Lança]",
    "59I60RSFIPRO*[Delta]",
    "64I61RSFIPRO*[Fibra]",
    "AS3590IPRO*",
    "M5947IPRO*"
  )
  
  genotipoPasso6DireitoSequeiro = c(
    "57I52RSFIPRO*",
    "64I61RSFIPRO*[Fibra]",
    "FTR1157RR*",
    "FTR2557RR*",
    "K6221",
    "M5838IPRO*",
    "M5947IPRO*",
    "NS6209*"
  )
  
  solosPasso2Sequeiro = c(
    "Argissolos Vermelho-Amarelos Distróficos",
    "Cambissolos Háplicos Alumínicos",
    "Cambissolos Háplicos Tb Distróficos",
    "Cambissolos Húmicos Alumínicos",
    "Gleissolo Háplico",
    "Gleissolo Sálico",
    "Gleissolos Háplicos",
    "Latossolos Vermelhos Distroférricos",
    "Latossolos Vermelhos Distróficos",
    "Luvissolo Crômico",
    "Neossolo Litólicos Distróficos",
    "Neossolo Litólicos Eutrófico",
    "Neossolos Litólicos Distróficos",
    "Neossolos Litólicos Eutróficos",
    "Neossolos Litólicos Húmicos",
    "Nitossolos Háplicos Alumínicos",
    "Planossolo Háplico",
    "Vertissolo Ebânico"
  )
  
  
  # Botao retornar
  observeEvent(input$btnRetonarGerenciar,
               change_page('/')
  )
  
  # Resultado da simulação 
  resultadoSimulacao = reactiveValues(
    data = data.frame(),
    columns = character(0)
  )
  
  # Iniciando dados da simulação para irrigado
  data = reactiveValues(
    step = 1,
    direction = "",
    father_direction = "",
    genotipo = "",
    altitude = "",
    radiacao = "",
    precipitacao = "",
    urmax = "",
    urmin = "",
    tmed = "",
    tmax = "",
    prec_sum = "",
    rad_total = "",
    ciclo_colheita = "",
    solo = "",
    classe = ""
  )
  
  #==============================================================
  # Modal para simulação do irrigado
  #==============================================================
  dataModalIrrigado = function(falhou = FALSE, messagem = '') {
    modalDialog(
      title = 'Cadastrar nova simulação',
      
      fluidRow(
        uiOutput("irrigadoOutputUI")
      ),
      
      footer = tagList(
        
      )
      
    )
  }
  #===================================================================
  #===================================================================
  observe({
    
    output$irrigadoOutputUI = renderUI({
      
      #======================================================
      # Primeira passo é escolhero genotipo
      #======================================================
      if(data$step == 1) {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'genotipoInput',
              label = 'Selecione o genotipo',
              choices =  c(genotiposPasso1irrigado, 'Outro genótipo')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #======================================================
      # lado esquerda da arvore primeiro passo
      #======================================================
      if(data$step == 2 && data$direction == 'left' && data$father_direction == 'left') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'altitude',
              label = 'Selecione a altitude (m)',
              choices =  c('< 987', '>= 987')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #======================================================
      # lado direita da arvore primeiro passo
      #======================================================
      if(data$step == 2 && data$direction == 'right' && data$father_direction == 'right') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'prec_sum',
              label = 'Selecione a precipitação (acumulada por ciclo, em mm)',
              choices =  c('>= 742', '< 742')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #====================================================================
      # lado esquerdo da arvore terceiro passo, ramoe esquerdo pai esquerdo
      #====================================================================
      if(data$step == 3 && data$direction == 'left' && data$father_direction == 'left') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'genotipoInput',
              label = 'Selecione o genotipo',
              choices =  c(genotipopasso3irrigado, 'Outros')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #======================================================
      ## lado direito da arvore terceiro passo, ramoe esquerdo pai esquerdo
      #======================================================
      if(data$step == 3 && data$direction == 'right' && data$father_direction == 'left') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'radiacaoTotal',
              label = 'Selecione a radiação global (acumulada por ciclo, em MJ/m2.dia)',
              choices =  c('>= 2663', '< 2663')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #=========================================================================
      ## lado esquerdo da arvore terceiro passo, ramo direito direito
      #=========================================================================
      if(data$step == 3 && data$direction == 'left' && data$father_direction == 'right') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'urMin',
              label = 'Selecione umidade mínima (acumulada por ciclo)',
              choices =  c('>= 9120', '< 9120')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #=========================================================================
      ## lado direito da arvore terceiro passo, ramo direito direito
      #=========================================================================
      if(data$step == 3 && data$direction == 'right' && data$father_direction == 'right') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'solo',
              label = 'Selecione o solo',
              choices =  c('Luvissolo Crômico', 'Outro')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #=========================================================================
      # lado direto da arvore terceiro passo, ramo esquerdo
      #=========================================================================
      if(data$step == 4 && data$direction == 'right' && data$father_direction == 'left') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'prec_sum',
              label = 'Selecione a precipitação (acumulada por ciclo, em mm)',
              choices =  c('>= 1040', '< 1040')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #=========================================================================
      # lado direto da arvore terceiro passo, ramo esquerdo
      #=========================================================================
      if(data$step == 4 && data$direction == 'right' && data$father_direction == 'right') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'altitude',
              label = 'Selecione a altitude (m)',
              choices =  c('< 630', '>= 630')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #=========================================================================
      # lado direto da arvore quinto passo, ramo esquerdo
      #=========================================================================
      if(data$step == 5 && data$direction == 'right' && data$father_direction == 'left') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'urmax',
              label = 'Selecione umidade máxima (acumulada por ciclo)',
              choices =  c('< 11000', '>= 11000')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #=========================================================================
      # lado esquerdo da arvore quinto passo, ramo direito
      #=========================================================================
      if(data$step == 5 && data$direction == 'right' && data$father_direction == 'right') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'prec_sum',
              label = "Selecione a precipitação (acumulada por ciclo, em mm)",
              choices =  c('< 756', '>= 756')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #=========================================================================
      # lado direito da arvore sexto passo, ramo direito
      #=========================================================================
      if(data$step == 6 && data$direction == 'right' && data$father_direction == 'right') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'tmax',
              label = "Selecione a temperatura máxima (acumulada por ciclo, em °C)",
              choices =  c('< 3666', '>= 3666')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #=========================================================================
      # lado direito da arvore sexto passo, ramo direito
      #=========================================================================
      if(data$step == 7 && data$direction == 'left' && data$father_direction == 'right') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'tmax',
              label = "Selecione novamente a temperatura máxima (acumulada por ciclo, em °C)",
              choices =  c('< 3538', '>= 3538')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #======================================================
      # teceiro passo caso a altitude seja menor que 987
      #======================================================
      do.call(tagList, inputs)
      
      
    })
    
  })
  
  
  #===================================================================
  # Modal para simulação do sequeiro
  #====================================================================
  dataModalSequeiro = function(falhou = FALSE, messagem = '') {
      
    modalDialog(
      title = 'Cadastrar nova simulação',
      
      fluidRow(
        uiOutput("sequeiroOutputUI")
      ),
      
      footer = tagList(
        
      )
      
    )
  }
  #===================================================================
  #===================================================================
  
  observe({
    
    output$sequeiroOutputUI = renderUI({
      
      #======================================================
      # Primeira passo é escolhero genotipo
      #======================================================
      if(data$step == 1) {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'genotipoInput',
              label = 'Selecione o genotipo',
              choices =  todosGenotipoSequeiro
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #======================================================
      # lado esquerdo da segunda etapa da arvore
      #======================================================
      if(data$step == 2 && data$direction == 'left') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'soloInput',
              label = 'Selecione o solo',
              choices =  c(solosPasso2Sequeiro, 'Outros')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #======================================================
      # lado esquerdo teceira etapa
      #======================================================
      if(data$step == 3 && data$direction == 'left') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'genotipoInput',
              label = 'Selecione o Genotipo',
              choices =  c(genotiposPasso3EsquerdoSequeiro, 'Outro genótipo')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #======================================================
      # lado Direito Terceira etapa
      #======================================================
      if(data$step == 3 && data$direction == 'right') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'genotipoInput',
              label = 'Selecione o Genotipo',
              choices =  c(genotipopasso3DireitoSequeiro, 'Outro genótipo')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #======================================================
      # lado esquerdo quarta etapa
      #======================================================
      if(data$step == 4 && data$direction == 'left') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'tmed',
              label = 'Selecione a temperatura média (acumulada por ciclo, (°C)',
              choices =  c('>= 3297', '< 3297')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #======================================================
      # lado direito quarta etapa
      #======================================================
      if(data$step == 4 && data$direction == 'right') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'urmin',
              label = 'Selecione umidade mínima (acumulada por ciclo)',
              choices =  c('>= 9107', '< 9107')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #======================================================
      # lado esquerdo quinta etapa
      #======================================================
      if(data$step == 5 && data$direction == 'right' && data$father_direction == 'left') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'cicloColheita',
              label = 'Selecione o ciclo da cultura em dias após o plantio',
              choices =  c('>= 121', '< 121')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #======================================================
      # lado direito quinta etapa
      #======================================================
      if(data$step == 5 && data$direction == 'right' && data$father_direction == 'right') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'precSum',
              label = 'Selecione a precipitação (acumulada por ciclo, em mm)',
              choices =  c('>= 605', '< 605')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      
      #======================================================
      # lado direito, pai esquerdo sexta etapa
      #======================================================
      if(data$step == 6 && data$direction == 'right' && data$father_direction == 'left') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'genotipoInput',
              label = 'Selecione o genotipo',
              choices =  c(genotipopasso6EsquerdoSequeiro, 'Outro')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #======================================================
      # lado direito, pai esquerdo sexta etapa
      #======================================================
      if(data$step == 6 && data$direction == 'right' && data$father_direction == 'right') {
        
        inputs = tagList(
          column(
            width = 12,
            selectInput(
              width = '100%',
              inputId = 'genotipoInput',
              label = 'Selecione o genotipo',
              choices =  c(genotipoPasso6DireitoSequeiro, 'Outro')
            ),
            actionButton('cancelar', 'Cancelar Simulação', class = "btn btn-danger btn-white"),
            actionButton("proximo", "Próximo Passo", class = "btn-success btn-white")
          )
        )
        
      }
      
      #======================================================
      # Caso o genotipo não peternca a lista principal
      #======================================================
      
      # Escrevendo interface
      do.call(tagList, inputs)
      
    })
    
  })
  
  
  #===================================================================
  # Atualização dos dados ao clicar em "Próximo"
  #===================================================================
  onclick('proximo', {
    
    if(input$inputTipoArvoreDecisao == 'Irrigado') {
      
      proximoPasso = FALSE
      
      if(data$step == 1) {
        data$genotipo = input$genotipoInput
        
        if(data$genotipo %in% genotiposPasso1irrigado) {
          
          data$direction = 'left'
          data$father_direction = 'left'
          proximoPasso = TRUE
          
          
        } else {
          data$direction = 'right'
          data$father_direction = 'right'
          proximoPasso = TRUE
          
        }
      }
      
      if(data$step == 2) {
        
        if(data$direction == 'left' &&  data$father_direction == 'left') {
          data$altitude = input$altitude
          
          if(data$altitude == '< 987') {
            data$direction = 'left'
            data$step = data$step + 1
          } else {
            data$direction = 'right'
          }
          
          proximoPasso = TRUE
          
          
        } else if(data$direction == 'right' && data$father_direction == 'right') {
          data$prec_sum = input$prec_sum
          
          if(data$prec_sum == '>= 742') {
            data$direction = 'left'
          } else {
            data$direction = 'right'
          }
          
          proximoPasso = TRUE
          
        }
        
      }
      
      if(data$step == 3) {
        
        if(data$father_direction == 'left') {
          
          if(data$direction == 'left') {
            
            if(data$genotipo %in% genotipopasso3irrigado) {
              data$classe = 1
            } else {
              data$direction = 'right'
              proximoPasso = TRUE
            }
            
          } else {
            data$rad_total = input$radiacaoTotal
            
            if(data$rad_total == '>= 2663') {
              data$classe = 1
            } else {
              data$classe = 2
            }
          }
          
        } else if(data$father_direction == 'right') {
          
          if(data$direction == 'left') {
            data$urmin = input$urMin
            
            if(data$urmin == '>= 9120') {
              data$classe = '2'
            } else {
              data$direction = 'right'
              proximoPasso = TRUE
            }
          } else {
            data$solo = input$solo
            
            if(data$solo %in% 'Luvissolo Crômico') {
              data$classe = 2
            } else {
              data$classe = 3
            }
          }
          
        }
        
      }
      
      if(data$step == 4) {
        
        if(data$father_direction == 'left') {
          
          if(data$direction == 'right') {
            data$prec_sum = input$prec_sum
            
            if(data$prec_sum == '>= 1040') {
              data$classe = 1 
            } else {
              data$direction = 'right'
              proximoPasso = TRUE
            }
          }
          
        } else if(data$father_direction == 'right') {
          data$altitude = input$altitude
          
          if(data$altitude == '< 630') {
            data$classe = 2
          } else {
            data$direction = 'right'
            proximoPasso = TRUE
          }
          
        }
        
      }
      
      if(data$step == 5) {
        
        if(data$father_direction == 'left') {
          
          if(data$direction == 'right') {
            data$urmax = input$urmax
            
            if(data$urmax == '< 11000') {
              data$classe = 1
            } else {
              data$classe = 2
            }
            
          }
           
        } else if(data$father_direction == 'right') {
          
          if(data$direction == 'right') {
            data$prec_sum = input$prec_sum
            
            if(data$prec_sum == '< 756') {
              data$classe = 2
            } else {
              data$direction = 'right'
              proximoPasso =  TRUE
            }
            
          }
          
        }
        
      }
      
      if(data$step == 6) {
        
        if(data$father_direction == 'right') {
          data$tmax = input$tmax
          
          if(data$tmax == '< 3666') {
            data$direction = 'left'
            proximoPasso = TRUE
          } else {
            data$classe = 3
          }
        }
        
      }
      
      if(data$step == 7) {
        
        if(data$father_direction == 'right' && data$direction == 'left') {
          data$tmax = input$tmax
          
          
          if(data$tmax == '>= 3538') {
            data$classe = 2
          } else {
            data$classe = 3
          }
           
        }
        
      }
      
    } else {
      
      proximoPasso = FALSE
      
      # Validando primeira etapa da arvore
      if(data$step == 1) {
        data$genotipo = input$genotipoInput
        
        if(data$genotipo %in% genotiposPasso1Sequeiro) {
          data$classe = 3
          proximoPasso = TRUE
        } else {
          proximoPasso = TRUE
          data$direction = 'left'
        }
        
      }
      
      # Validando segunda etapa da arvore
      if(data$step == 2) {
        data$solo = input$soloInput
        
        if(data$solo %in% solosPasso2Sequeiro) {
          proximoPasso = TRUE
          data$direction = 'left'
          data$father_direction = 'left'
        } else {
          proximoPasso = TRUE
          data$direction = 'right'
          data$father_direction = 'right'
        }
        
        data$step = data$step + 1
        
      }
      
      # Validando terceira etapa da arvore
      if(data$step ==  3) {
        
        if(data$direction == 'left') {
           
          if(data$genotipo %in% genotiposPasso3EsquerdoSequeiro) {
            data$direction = 'left'
            proximoPasso = TRUE
          } else {
            data$classe = 2
          }
          
        } else if(data$direction == 'right') {
          
          if(data$genotipo %in% genotipopasso3DireitoSequeiro) {
            data$direction = 'right'
            proximoPasso = TRUE
          } else {
            data$classe = 3
          }
          
        }
      }
      
      # Validando quarto passo
      if(data$step == 4) {
        
        if(data$direction == 'left' &&  data$father_direction == 'left') {
          data$tmed = input$tmed
          
          if(data$tmed == '>= 3297') {
            data$classe = 1
          } else {
            data$direction = 'right'
            proximoPasso = TRUE
          }
          
        } else if(data$direction == 'right' && data$father_direction == 'right') {
          data$urmin = input$urmin
          
          if(data$urmin == '< 9107') {
            data$classe = 2
          } else {
            data$direction = 'right'
            proximoPasso = TRUE
          }
        }
        
      }
      
      # Validando quinto passo
      if(data$step == 5) {
        
        if(data$direction == 'right' && data$father_direction == 'left') {
          data$ciclo_colheita = input$cicloColheita
          
          if(data$ciclo_colheita == '< 121') {
            data$classe = 1
          } else {
            data$direction = 'right'
            data$step = data$step + 1
            proximoPasso = TRUE
          }
          
        } else if(data$direction == 'right' && data$father_direction == 'right') {
          data$prec_sum = input$precSum
          
          if(data$prec_sum == '>= 605') {
            data$direction == 'left'
            proximoPasso = TRUE
            data$step = data$step + 1
          } else {
            data$classe = 3
          }
          
        }
        
      }
      
      # Validando passo 6 
      if(data$step == 6) {
        
        if(data$father_direction == 'left') {
          
          if(data$genotipo %in% genotipopasso6EsquerdoSequeiro) {
            data$classe = 2
          } else {
            data$classe = 3
          }
            
        } else if(data$father_direction == 'right') {
          data$genotipo = input$genotipoInput
          
          if(data$genotipo %in% genotipoPasso6DireitoSequeiro) {
            data$classe = 2
          } else {
            data$classe = 3
          }
          
        }
        
      }
    }
    
    if(proximoPasso) {
      data$step = data$step + 1
    }
    
    if(data$classe != '') {
      
      resultado = data.frame(
        genotipo = data$genotipo,
        altitude = data$altitude,
        radiacao = data$radiacao,
        umidade_maxima = data$urmax,
        umidade_minima = data$urmin,
        temperatura_media = data$tmed,
        soma_precipitacao = data$prec_sum,
        ciclo_colheita = data$ciclo_colheita,
        solo = data$solo,
        classe = data$classe
      )
      
      resultado$classe = case_when(
        resultado$classe == 1 ~ "Baixa produtividade",
        resultado$classe == 2 ~ "Média produtividade",
        resultado$classe == 3 ~ "Alta produtividade",
      )
      
      names(resultado) = c(
        'Genotipo', 
        'Altitude (m)', 
        'Radiação global (acumulada por ciclo, em MJ/m2.dia)', 
        'Umidade máxima (acumulada por ciclo, em mm)',
        'Umidade mínima (acumulada por ciclo)', 
        'Temperatura Média (acumulada por ciclo, em °C)',
        'Precipitação (acumulada por ciclo, em mm)',
        'Ciclo da cultura em dias após o plantio',
        'Solo',
        'Classe'
      )
      
      # Resetando dados para a próxima simulação
      data$genotipo = ''
      data$altitude = ''
      data$radiacao = ''
      data$urmax = ''
      data$urmin = ''
      data$tmed  = ''
      data$prec_sum = ''
      data$ciclo_colheita = ''
      data$step = 1
      data$classe = ''
      
      resultadoSimulacao$data = rbind(resultadoSimulacao$data, resultado)
      removeModal()
      
    }
    
  })
  
  #==================================================================
  # Observa o evento para cancelar a simulação
  #==================================================================
  observeEvent(input$cancelar, {
    
    # Resetando dados para a próxima simulação
    data$genotipo = ''
    data$altitude = ''
    data$radiacao = ''
    data$urmax = ''
    data$urmin = ''
    data$tmed  = ''
    data$prec_sum = ''
    data$ciclo_colheita = ''
    data$step = 1
    data$classe = ''
    
    # fechando modal
    removeModal()
    
  })
  
  
  #==================================================================
  # Observa o evento para abrir o modal de acordo com sistema
  #==================================================================
  observeEvent(input$btnAddGenotipo, {
    
    if(input$inputTipoArvoreDecisao == 'Irrigado') {
      showModal(dataModalIrrigado())
    } else {
      showModal(dataModalSequeiro())
    }
    
  })
  #==================================================================
  #==================================================================
  
  #==================================================================
  # Observa evento de mudança de sistema para resetar a variável
  #==================================================================
  observeEvent(input$inputTipoArvoreDecisao, {
    
    resultadoSimulacao$data = data.frame()
    resultadoSimulacao$columns = character(0)
    
  })
  
  observe({
    
    input$ok
    
    if(length(resultadoSimulacao$data)) {
      
      # Criando interface com o resultado
      output$resultadoArvoreDecisao = renderUI({
        dataTableOutput('resultadoTabela')
      })
      
    } else {
      
      # Mensagem para começar a simulaçao
      output$resultadoArvoreDecisao = renderUI({
        HTML('<center><h4>Clique no botão ao lado para começar a simulação</h4></center>')
      })
      
    }
    
  })
  #===================================================================
  #===================================================================
  
  output$resultadoTabela = renderDataTable({
    resultadoSimulacao$data
  },options = list(lengthMenu = c(5, 10, 15, 20), pageLength = 10, scrollX = TRUE,  scrollY = TRUE))
  
}