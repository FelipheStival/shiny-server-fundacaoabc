#======================================================================
# Método com a arvore de decisao para o irrigado
#======================================================================
arvoreDecisaoIrrigado = function(altitude_esc, genotipo_esc, PREC_sum_esc, RAD.TOT_sum_esc, solo_esc, T.MAX_sum_esc, URMAX_sum_esc, URMIN_sum_esc) {
  
  # Lista de genótipos para sim (primeira bifurcação)
  sim_gens1 = c(
    "BMXExtremaIPRO",
    "HOAporéIPRO",
    "HOIguaçuIPRO",
    "HOJuruenaIPRO",
    "HOMamoréIPRO",
    "HOMaracaíIPRO"
  )
  
  solo_ibge = c(
    "Massa d´Água",
    "Luvissolo Crômico",
    "Gleissolo Sálico"
  )
  
  sim_gens2 = c(
    "HOAporéIPRO",
    "HOJuruenaIPRO",
    "HOMaracaíIPRO"
  )
  
  
  # Genótipo selecionado é sim ####
  if(genotipo_esc %in% sim_gens1 == T){
    
    ## Altitude menor que 987 ####
    if(altitude_esc < 987){
        
      ## Genótipo selecionado é sim ####
      
      ### Classe 1 ####
      if(genotipo_esc %in% sim_gens2 == TRUE){
        
        classe = "Baixa produtividade"
        
      } 
      
      ## Genótipo selecionado é não ####
      if(genotipo_esc %in% sim_gens2 == FALSE){
        
        #### PREC_sum >= 1040 ####
        #### Classe 1 ####
        if(PREC_sum_esc >= 1040){
          
          classe = "Baixa produtividade"
          
        }
        
        ### PREC_sum < 1040 ####
        
        if(PREC_sum_esc < 1040){
            
          
          ### URMAX_sum < 11e+3 Classe 1 ####
          if(URMAX_sum_esc < 11e+3){
            classe = "Baixa produtividade"
          }
          
          ### URMAX_sum >= 11e+3 Classe 2 ####
          if(URMAX_sum_esc >= 11e+3){
            classe = "Média produtividade"
          } 
        }
      } 
    }
    
    # Altitude maior que 987 ####
    if(altitude_esc >= 987){
      
      ### RAD.TOT_sum >= 2663 Classe 1 ####
      if(RAD.TOT_sum_esc >= 2663){
        classe = "Baixa produtividade"
      }
      ## RAD.TOT_sum < 2663 Classe 2 ####
      if(RAD.TOT_sum_esc < 2663){
        classe = "Média produtividade"
      }
    }
  }
  
  # Genótipo selecionado é não ####
  if(genotipo_esc %in% sim_gens1 == F){
    
    ## PREC_sum >= 742 ####
    if(PREC_sum_esc >= 742){
      
      ### URMIN_sum_esc >= 9120 Classe 2####
      if(URMIN_sum_esc >= 9120){
        classe = "Média produtividade"
      }
      
      ## URMIN_sum_esc < 9120 ####
      if(URMIN_sum_esc < 9120){

        
        ### altitude_esc < 630 classe 2 ####
        if(altitude_esc < 630){
          classe = "Média produtividade"
        }
        
        ### altitude_esc >= 630 ####
        if(altitude_esc >= 630){
          
          #### PREC_sum < 756 ####
          if(PREC_sum_esc < 756){
            classe = "Média produtividade"
          }
          
          ### PREC_sum >= 756 ####
          if(PREC_sum_esc >= 756){
            
            #### T.MAX_sum_esc > 3666 ####
            if(T.MAX_sum_esc < 3666){
              
              if(T.MAX_sum_esc >= 3538){
                classe = "Média produtividade"
              }
              if(T.MAX_sum_esc < 3538){
                classe = "Alta produtividade"
              }
            }
            
            if(T.MAX_sum_esc >= 3666){
              classe = "Alta produtividade"
            }
          }
        }
      }
      
    }
    
    # PREC_sum < 742 ####
    if(PREC_sum_esc < 742){
      
      ### Solos_Grupo_IBGE == Luvissolo Crômico classe 2 ####
      if(solo_esc %in% c("Luvissolo Crômico")){
        classe = "Média produtividade"
      }       
      # Solos_Grupo_IBGE == Luvissolo Crômico classe 3 #### 
      if(!solo_esc %in% c("Luvissolo Crômico")){
        classe = "Alta produtividade"
      }
    }
    
  }
  
  return(classe)
  
}

#======================================================================
# Método com a arvore de decisao para o Sequeiro
#======================================================================
arvoreDecisaoSequeiro = function(genotipo_esc, solo_esc, T.MED_sum_esc, URMIN_sum_esc, PREC_sum_esc, Cic_Semead_Colh_dias){
  
  # Genotipos que irão para o primeiro lado "sim" da árvore
  genotipos_seq_1 = c(
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
  
  # Genótipos que irão para o lado sim da terceira bifurcação (lado esquerdo)
  genotipos_seq_2_1 = c(
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
  
  # Genótipos que irão para o lado sim da terceira bifurcação (lado direito)
  genotipos_seq_2_2 = c(
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
  
  # Genótipos que irão para o lado sim da sexta bifurcação (lado esquerdo)
  genotipos_seq_3_1 = c(
    "58I60RSFIPRO*[Lança]",
    "59I60RSFIPRO*[Delta]",
    "64I61RSFIPRO*[Fibra]",
    "AS3590IPRO*",
    "M5947IPRO*"
  )
  
  # Genótipos que irão para o lado sim da sexta bifurcação (lado direito)
  genotipos_seq_3_2 = c(
    "57I52RSFIPRO*",
    "64I61RSFIPRO*[Fibra]",
    "FTR1157RR*",
    "FTR2557RR*",
    "K6221",
    "M5838IPRO*",
    "M5947IPRO*",
    "NS6209*"
  )
  
  # Todos solos sequeiros (a seleção de solo deve ser realizada com base nesse vetor)
  todos_solos_seq = c(
    "Luvissolo Crômico",
    "Gleissolo Sálico",
    "Gleissolo Háplico",
    "Vertissolo Ebânico",
    "Latossolo Vermelho-Amarelo",
    "Neossolo Litólicos Eutrófico",
    "Planossolo Háplico",
    "Cambissolos Háplicos Tb Distróficos",
    "Latossolos Vermelhos Distróficos",
    "Organossolo Mésicos",
    "Argissolos Vermelho-Amarelos Distróficos",
    "Latossolos Brunos Ácricos",
    "Neossolo Litólicos Húmicos",
    "Nitossolos Háplicos Alumínicos",
    "Cambissolos Húmicos Alumínicos",
    "Neossolo Litólicos Distróficos",
    "Cambissolos Háplicos Alumínicos",
    "Organossolos Básicos",
    "Gleissolos Melânicos",
    "Planossolos Háplicos",
    "Gleissolos Háplicos",
    "Neossolos Litólicos Húmicos",
    "Neossolos Litólicos Distróficos",
    "Neossolos Litólicos Eutróficos",
    "Latossolos Vermelhos Distroférricos"
  )
  
  # Solos que irão para o "sim" na segunda bifurcação
  solos_seq_1 = c(
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
  
  # genotipo_esc está dentro de genotipos_seq_1 ####
  if(genotipo_esc %in% genotipos_seq_1 == TRUE){
    classe = "Baixa produtividade"
  }
  
  # genotipo_esc não está dentro de genotipos_seq_1 ####
  if(genotipo_esc %in% genotipos_seq_1 == FALSE){
    
    ## solo_esc está dentro de solos_seq_1 ####
    if(solo_esc %in% solos_seq_1 == TRUE){
      
      ## genotipo_esc está dentro de genotipos_seq_2_1 ####
      if(genotipo_esc %in% genotipos_seq_2_1 == TRUE){
        
        ### T.MED_sum_esc >= 3297 ####
        if(T.MED_sum_esc >= 3297){
          classe = "Baixa produtividade"
        }
        
        ## T.MED_sum_esc < 3297 ####
        if(T.MED_sum_esc < 3297){
          
          ### Cic_Semead_Colh_dias < 121 ####
          if(Cic_Semead_Colh_dias < 121){
            classe = "Baixa produtividade"
          }
          
          ### Cic_Semead_Colh_dias >= 121 ####
          if(Cic_Semead_Colh_dias >= 121){
            
            #### genotipo_esc está dentro de genotipos_seq_3_1 ####
            if(genotipo_esc %in% genotipos_seq_3_1 == TRUE){
              classe = "Média produtividade"
            }
            
            ### genotipo_esc não está dentro de genotipos_seq_3_1 ####
            if(genotipo_esc %in% genotipos_seq_3_1 == FALSE){
              classe = "Alta produtividade"
            }
            
          }
        }
        
      }
      
      ## genotipo_esc não está dentro de genotipos_seq_2_1 ####
      if(genotipo_esc %in% genotipos_seq_2_1 == FALSE){
        classe = "Média produtividade"
      }
      
    }
    
    # solo_esc não está dentro de solos_seq_1 ####
    if(solo_esc %in% solos_seq_1 == FALSE){
      
      ## genotipo_esc está dentro de genotipos_seq_2_2 ####
      if(genotipo_esc %in% genotipos_seq_2_2 == TRUE){
        
        ### URMIN_sum_esc < 9107 ####
        if(URMIN_sum_esc < 9107){
          classe = "Média produtividade"
        }
        
        ## URMIN_sum_esc >= 9107 ####
        if(URMIN_sum_esc >= 9107){
          
          ### PREC_sum_esc >= 605 ####
          if(PREC_sum_esc >= 605){
            
            #### genotipo_esc está dentro de genotipos_seq_3_2 ####
            if(genotipo_esc %in% genotipos_seq_3_2 == TRUE){
              classe = "Média produtividade"
            }
            
            ### genotipo_esc não está dentro de genotipos_seq_3_2 ####
            if(genotipo_esc %in% genotipos_seq_3_2 == FALSE){
              classe = "Alta produtividade"
            }
            
          }
          
          ### PREC_sum_esc < 605 ####
          if(PREC_sum_esc < 605){
            classe = "Alta produtividade"
          }
        }
        
      }
      
      ## genotipo_esc não está dentro de genotipos_seq_2_2 ####
      if(genotipo_esc %in% genotipos_seq_2_2 == FALSE){
        classe = "Alta produtividade"
      }
    }
  }
  
  return(classe)
  
}

selectInputDinamico = function(inputName, label, choices, value = '') {
  
  input = tagList(column(
    width = 12,
    selectInput(
      width = '100%',
      inputId = inputName,
      label = label,
      choices = choices,
      selected = value
    )
  ))
  
  return(input)
  
}

