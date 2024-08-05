#==============================================#
# Grafico "Contagem"
#==============================================#
grafico.diagnostico_Contagem = function(tabela) {
  validate.vx = is.na(tabela$h2) | is.infinite(tabela$h2)
  validate.vy = is.na(tabela$rgg) | is.infinite(tabela$rgg)
  
  row.index = which(!validate.vx & !validate.vy)
  col.index = which(names(tabela) %in% c("h2", "rgg", "Diagnostico"))
  tabela = tabela[row.index, col.index]
  
  color.palette = c("#7cb342", "#e53935")
  scatterD3(
    data = tabela,
    x = h2,
    y = rgg,
    col_var = Diagnostico,
    colors = color.palette,
    xlab = "Herdabilidade",
    ylab = "Correlacao genetica entre valor fenotipo e genotipo",
    menu = F,
    xlim = c(0, 1),
    ylim = c(0, 1)
  )
}

#==============================================#
# Aba "Dados Perdidos"
#===============================================
graphics.dadosPerdidos_Estatistica = function(tabela) {
  
  namesIndex = which(
    names(tabela) %in% c(
      "id_ensaio",
      "flor_das",
      "flor_dae",
      "ciclo_das",
      "ciclo_dae",
      "produtividade"
    )
  )
  subsetTabela = tabela[, namesIndex]
  
  naTabela = melt(subsetTabela, id.vars = "id_ensaio")
  naTabela = dcast(naTabela,
                   id_ensaio ~ variable,
                   value.var = "value",
                   fun.aggregate = naCounter)
  naTabela = melt(naTabela, id.vars = "id_ensaio")
  
  names(naTabela) = c("Experimento", "Variavel", "Valor")
  
  ggplot(data = naTabela, aes(x = Variavel, y = Experimento)) + geom_tile(aes(fill = Valor), colour = "white") +
    scale_fill_gradient(low = "#7cb342", high = "#e53935") + theme_minimal() +
    theme(text = element_text(size = 15))
}

#==============================================#
# Aba "Estatistica"
# Grafico "Resumo"
#===============================================
grafico.analiseEstatistica_Resumo = function(tabela, mediaSelect = 'TODOS') {
  
  modified_data = tabela %>% 
    # Agrupar por genotipo e calcular media e mediana
    dplyr::group_by(gid) %>% 
    dplyr::summarise(mean_pred = mean(predicts), median = median(predicts))
  
  
  # Filtrando de acordo com a media selecionada
  mediaPredict = mean(modified_data$mean_pred)
  if(mediaSelect == 'ACIMA'){
    modified_data =  modified_data[modified_data$mean_pred > mediaPredict,]
  } else if(mediaSelect == "ABAIXO") {
    modified_data =  modified_data[modified_data$mean_pred < mediaPredict,]
  }
  
  # Muda o df para formato long
  long = melt(modified_data,id.vars="gid")
  
  g = ggplot(data = long, aes(x=reorder(gid,value), y=value, fill=variable)) + 
    geom_bar(stat = "identity",
             position="dodge") +
    xlab("Genótipos") +
    ylab("Produtividade estimada") +
    coord_flip() + 
    scale_fill_discrete(name="",
                        labels=c("Média", "Mediana"))+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme_light() +
    labs(
      title = paste('Média geral', round(mediaPredict), sep = ":")
    )
  
  return(ggplotly(g))
  
}
#==============================================#

#==============================================#
# Aba "Estatistica"
# Grafico "Unitario"
grafico.analiseEstatistica_Unitario = function(data_plot, site = "", media = 'TODOS') {
  
  data_plot = data_plot[data_plot$site == site,]
  
  g = ggplot(data = data_plot, aes(x=reorder(gid,predicts), y=predicts)) + 
    geom_boxplot( fill = "lightyellow") + 
    stat_boxplot(geom ='errorbar') + 
    xlab("Genótipos") +
    ylab("Produtividade estimada") +
    coord_flip() + 
    theme_light() +
    facet_grid(~site)
  gly = ggplotly(g)
  return(gly)
  
}
#==============================================#

#==============================================#
# Aba "Estatistica"
# Grafico "Heatmap"
#==============================================#
grafico.analiseEstatistica_Heatmap = function(tabela) {
  tabela_media = tabela %>% group_by(gid, site, year) %>%
    summarise_at(vars("predicts"), mean)
  grafico = tabela_media %>%
    ggplot(aes(x = site, y = gid, fill = predicts)) +
    geom_tile(height = 1.1, color = 'black') +
    scale_fill_gradientn(colors = c("red","green")) + 
    theme_minimal() +
    labs(
      x = 'Locais',
      y = 'Genótipos',
      fill = 'Produtividade predita (kg/ha)'
    ) +
    theme(
      strip.background = element_blank(),
      panel.border = element_rect(color = 'black', fill = NA, size = 0.8),
      legend.title.align = 0.5
    )
  
  g2 = ggplotly(grafico)
  return(g2)
}
#==============================================#

#==============================================#
# Aba "Estatistica"
# Grafico "Linhas"
#==============================================#
grafico.GraficoLinhas = function(dados) {
  
  dados$gid = as.character(dados$gid)
  dados$gid = factor(dados$gid)
  
  #Calculando media por local
  
  dados_mean = dados %>%
    group_by(site) %>%
    dplyr::summarize(Mean = mean(mean, na.rm = TRUE))
  dados_mean = as.data.frame(dados_mean)
  
  #Gerando grafico
  p = ggplot(data = NULL) +
    geom_line(data = dados, aes(
      x = site,
      y = mean,
      group = gid,
      colour = gid
    )) +
    geom_point(
      data = dados_mean,
      aes(x = site, y = Mean),
      shape = 17,
      size = 3
    ) +
    theme(axis.text.x = element_text(angle = 90),
          text = element_text(size = 15)) +
    xlab("Local") +
    ylab("Produtividade (kg/ha)") +
    labs(colour = "Genotipo") +
    theme_minimal()
  
  
  return(p)
  
}

#==============================================#
# Aba "Estatistica"
# Grafico "Linhas"
#==============================================
grafico.analiseCluster = function(data_plot){
  
  # Clusters
  cluster_data = data_plot %>% 
    # Agrupar por genotipo e calcular media
    dplyr::group_by(gid) %>% 
    dplyr::summarise(mean_pred = mean(predicts))
  
  # Selecionar o numero de clusters
  k_val = 5
  # Salva os clusters referentes a cada observacao
  set.seed(123)
  clusters_obs = kmeans(cluster_data[2], k_val, nstart = 50)$cluster
  # Adiciona os clusters no df
  cluster_data = data.frame(cluster_data, grupos = (clusters_obs))
  # Coloca o df em ordem crescente
  cluster_data = cluster_data[order(cluster_data$mean_pred),]
  rownames(cluster_data) = NULL
  
  # Fixa um df para colocar o numero de clusters em ordem crescente
  cluster_data_pin = cluster_data
  s_want = 1:length(unique(clusters_obs))
  s_now = unique(cluster_data$grupos)
  # Finalmente coloca em ordem crescente
  for(i in 1:length(unique(clusters_obs))){
    
    cluster_data_pin[cluster_data == s_now[i]] = s_want[i]
    
  }
  
  # Precisamos obter tambem os intervalos de cada cluster
  # Simulando de 1 em 1
  set.seed(123)
  clusters_obs_sim = kmeans(min(cluster_data$mean_pred):max(cluster_data$mean_pred), k_val, nstart = 50)$cluster
  
  sim_data = data.frame(predicts = min(cluster_data$mean_pred):max(cluster_data$mean_pred), grupos = clusters_obs_sim) %>% group_by(grupos)
  sim_data = data.frame(sim_data)
  
  sim_data = sim_data[order(sim_data$predicts),]
  sim_data_pin = sim_data
  
  #seq_want = 1:length(unique(res.km.real))
  s_now_sim = unique(sim_data_pin$grupos)
  
  for(i in 1:length(unique(clusters_obs_sim))){
    
    sim_data_pin[sim_data == s_now_sim[i]] = s_want[i]
    
  }
  
  # Definir intervalos entre os clusters
  intervals = matrix(nrow = length(unique(sim_data_pin$grupos)),ncol = 2)
  for(i in 1:length(unique(sim_data_pin$grupos))){
    
    intervals[i,1] = round(min(sim_data_pin %>% filter(grupos == i) %>% summarise(predicts)),0)-1
    intervals[i,2] = round(max(sim_data_pin %>% filter(grupos == i) %>% summarise(predicts)),0)
    
  }
  
  # Intervalos em string para o grafico
  fill_label = c()
  if(intervals[1,1] == intervals[1,2]){
    fill_label[1] = paste0(intervals[1,1])
  } else {
    fill_label[1] = paste0("[",intervals[1,1],",",intervals[1,2],"]")
  }
  
  for(i in 2:length(unique(cluster_data_pin$grupos))){
    
    if(intervals[i,1] == intervals[i,2]){
      fill_label[i] = paste0(intervals[i,1])
    } else {
      fill_label[i] = paste0("[",intervals[i,1],",",intervals[i,2],"]")
    }
  }
  
  # Filtrando de acordo com a media selecionada
  mediaPredict = mean(cluster_data_pin$mean_pred)
  
  # Enfim o grafico do cluster
  g = ggplot(cluster_data_pin, aes(x=grupos, y=reorder(gid,grupos), fill = as.factor(grupos))) +
    geom_bar(stat='identity') +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(y = "Genótipos", 
         x = "",
         fill = "Grupos (kg/ha)",
    ) +
    scale_fill_discrete(labels = fill_label) +
    labs(
      title = paste('Média geral', round(mediaPredict), sep = ":")
    ) +
    theme_minimal()
  
  ggplotly(g)
  
}

#==============================================#
# Aba "Estatistica"
# Grafico "Media harmonica"
#==============================================
grafico.mediaHarmonica = function(dados, experimentoSelected = NULL) {
  
  # Verifique se a função calcula_predict retorna os dados esperados
  modeloData = calcula_predict(dados, "produtividade", "repeticao", "local", "genotipo", "safra", "TODOS", experimentoSelected)
  
  modelo = modeloData$mdl
  dfHmrpgv = modeloData$pred
  
  # Calcular as médias por ambiente
  meanByEnv = tapply(dfHmrpgv$predicts, dfHmrpgv$site, mean, simplify = TRUE)
  
  # Verifique se meanByEnv está correto
  print(meanByEnv)
  
  # Calcular a performance relativa
  dfHmrpgv$PerformanceRelative = dfHmrpgv$predicts / meanByEnv[dfHmrpgv$site]
  
  # Calcular a média harmônica das performances relativas para cada genótipo
  hmrpgv = aggregate(PerformanceRelative ~ gid, dfHmrpgv, function(x) {
    length(x) / sum(1 / x)
  })
  
  # Renomear a coluna para MHPRVG
  names(hmrpgv)[2] = "MHPRVG"
  
  # Ordenar pelo valor de MHPRVG
  hmrpgv = hmrpgv[order(hmrpgv$MHPRVG),]
  
  # Criar o gráfico
  plot = ggplot(hmrpgv, aes(x = MHPRVG, y = reorder(gid, MHPRVG))) +
    geom_col(fill = "cyan4") +
    theme_minimal() +
    ylab("") +
    labs(
      title = "Média Harmônica das Performances Relativas dos Genótipos",
      caption = if (length(unique(dfHmrpgv$site)) == 1) {
        "Apenas um local: a média harmônica é igual ao valor genotípico."
      } else {
        paste0("Número máximo de ambientes incluídos: ", length(unique(dfHmrpgv$site)))
      }
    )
  
  # Retornar o gráfico interativo com plotly
  return(ggplotly(plot))
  
}

#==============================================#
# Aba "Analise GGE"
# Grafico "Quem vence e aonde"
#==============================================
grafico.analiseGGE_QuemVenceEAonde = function(gge.model) {
  plot = WhichWon(gge.model, largeSize = 3, axis_expand = 1.75) + theme_bw()
  label_plot = plot[["data"]][["label"]]
  
  plot$layers[[7]] = NULL
  plot$layers[[6]] = NULL
  plot$layers[[5]] = NULL
  
  plot = plot + geom_text_repel(label = label_plot, 
                          color = ifelse(
                            label_plot %in% plot[["plot_env"]][["labelgen"]], "forestgreen", "blue"
                          ),
                          max.overlaps  = 65.,
                          max.iter = 10000)
  
  return(plot)
}
#==============================================#

#==============================================#
# Aba "Analise GGE"
# Grafico "Ordem de Ambiente"
#==============================================
grafico.analiseGGE_OrdemDeAmbiente = function(gge.model) {
  plot = RankEnv(gge.model, largeSize = 4, axis_expand = 1.75) + theme_bw()
  return(plot)
}

#==============================================#
# Aba "Analise GGE"
# Grafico "Ordem de genotipo"
#===============================================
grafico.analiseGGE_OrdemDeGenotipo = function(gge.model) {
  plot = RankGen(gge.model,largeSize = 4, axis_expand = 1.6) + theme_bw()
  
  label_plot = plot[["data"]][["label"]]
  plot$layers[[10]] = NULL
  plot$layers[[9]] = NULL
  plot$layers[[8]] = NULL
  
  plot = plot + geom_text_repel(label = label_plot, 
                             color = ifelse(
                               label_plot %in% plot[["plot_env"]][["labelgen"]], "forestgreen", "blue"
                             ),
                             max.overlaps  = 65.,
                             max.iter = 10000)
  return(plot)
  
}
#==============================================#

#==============================================#
# Aba "Analise GGE"
# Grafico "Relacao entre ambientes"
#==============================================
grafico.analiseGGE_RelacaoEntreAmbientes = function(gge.model) {
  EnvRelationship(gge.model, largeSize = 4, axis_expand = 1.75) + theme_bw()
}
#==============================================#

#==============================================#
# Aba "Analise GGE"
# Grafico "Estabilidade / Media"
#==============================================
grafico.analiseGGE_EstabilidadeMedia = function(gge.model) {
  plot = MeanStability(gge.model, largeSize = 4, axis_expand = 1.75) + theme_bw()
  
  label_plot = plot[["data"]][["label"]]
  plot$layers[[8]] = NULL
  plot$layers[[7]] = NULL
  
  plot = plot + geom_text_repel(label = label_plot, 
                             color = ifelse(
                               label_plot %in% plot[["plot_env"]][["labelgen"]], "forestgreen", "blue"
                             ),
                             max.overlaps  = 65.,
                             max.iter = 10000)
  
  return(plot)
}
#==============================================#

#==============================================#
# Aba "Analise GGE"
# Grafico "Denograma"
#==============================================
grafico.analiseGGE_Denograma = function(deno) {
  plot(as.phylo(deno), cex = 0.7, label.offset = 0.7, width = 10)
}
#==============================================#

#==============================================#
# Aba "Potencial genótipo produtivo"
# Grafico "Potencial Produtivo"
#==============================================
grafico.pontecialProdutivo = function(dados, localInput) {
  
  dados = dados %>% 
    filter(local == localInput) %>%
    top_n(n = 50, notas)
  
  plot = ggplot(dados, aes(x = reorder(genotipo, -notas), y = notas, fill = notas, label = round(notas,1))) +
    geom_col(width = 0.85, colour = "black") +
    coord_polar() +
    scale_fill_gradientn(colors = c("red","yellow","green")) +
    theme_minimal() +
    geom_text(position=position_stack(vjust=0.8), size = 2.8) +
    theme(axis.title=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_line(size = 0.5, linetype = 'dashed',
                                          colour = "darkgray"),
          panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                          colour = "darkgray")) +
    labs(fill = "PGP")
  
  return(plot)
  
}
#==============================================#

#==============================================#
# Aba "Gráfico de linhas"
# Grafico Ambiental relativo
#==============================================
grafico.ambientalRelativo = function(dados, genotiposSelected) {
  
  tabela_uso = dados %>%
    select(produtividade, repeticao, local, genotipo, safra, id_ensaio, cidade)
  
  ids = unique(tabela_uso$id_ensaio)
  vec_ids =  c()
  i = 1
  
  for(id in ids){
    tabela_uso_2 = tabela_uso[tabela_uso$id_ensaio == id,]
    
    if(all(is.element(genotiposSelected, tabela_uso_2$genotipo))){
      
      vec_ids[i] = id
      i = i+1 
      
    }
  }
  
  if(length(vec_ids) > 1) {
    
    tab_med_gen = tabela_uso %>% 
      filter(genotipo %in% genotiposSelected, id_ensaio %in% vec_ids) %>%
      dplyr::group_by(genotipo,id_ensaio) %>% 
      dplyr::summarise(media = mean(produtividade))
    
    split_tab_gen = split(tab_med_gen, (tab_med_gen$genotipo))
    cbind_tab_gen = split_tab_gen[[1]][,2:3]
    sorted_gen = sort(genotiposSelected)
    names(cbind_tab_gen)[2] = sorted_gen[1]
    
    for(i in 2:length(split_tab_gen)){
      cbind_tab_gen = cbind(cbind_tab_gen, split_tab_gen[[i]][3])
      names(cbind_tab_gen)[i+1] = sorted_gen[i]
    }
    
    tabela_uso1 = tabela_uso %>%
      dplyr::filter(id_ensaio %in% vec_ids) %>%
      group_by(id_ensaio, cidade) %>%
      dplyr::summarise(media_local = mean(produtividade))
    
    
    tabela_final = cbind(cbind_tab_gen, tabela_uso1[2], tabela_uso1[3], tabela_uso1[3])
    names(tabela_final)[ncol(tabela_final)] = "Média local"
    
    tabela_final = tabela_final %>% mutate(across(c(gens, "Média local")), 100*(across(c(gens, "Média local")) - `Média local`)/(`Média local`))
    melt_tab = reshape2::melt(tabela_final, id.vars = c("id_ensaio","media_local","cidade"))
    
    plot = ggplot(melt_tab, aes(y = value, x = media_local, colour = variable, linetype = variable)) +
      geom_point() +
      geom_smooth(method = "lm", se = F, aes(y = value, x = media_local)) +
      #scale_linetype_discrete() +
      scale_linetype_manual(name="variable", 
                            breaks= unique(melt_tab$variable),
                            values = c(rep(1,length(unique(melt_tab$variable))-1),5)) +
      #labs(caption = caption_bonf) +
      xlab("Média Local (kg/ha)") +
      ylab("") +
      theme_bw() +
      theme(legend.title = element_blank()) +
      ggtitle("Índice ambiental relativizado")
    
    
    return(plot)
    
  }
  
}

#==============================================#
# Aba "Gráfico de linhas"
# Grafico Ambiental Local
#==============================================
grafico.graficoAmbientalLocal = function(dados, genotiposSelect) {
  
  cat_to_bonferroni = function(lme_model, cat_var) {
    
    # Primeiramente retiramos a tabela com os p-valores do summary
    coef_tab = data.frame(coef(summary(lme_model)))
    col_change = c("Estimate","Std_Error","DF", "t_value","p_value")
    colnames(coef_tab) = col_change
    
    # Pegando o nome das categorias para iterar no for
    cat_names = sort(unique(cat_var))[-1:0]
    
    # Abrindo um data.frame limpo pra ser preenchido
    cat_to_intercept = data.frame(cat = c(), p.value = c())
    
    for (cat in cat_names){
      # pega p-valor correspondente a cada classe 
      p.values = coef_tab[grepl(paste(cat, sep = ""), row.names(coef_tab)), "p_value"]
      #print(p.values)
      
      if (p.values > 0.1){
        cat_to_intercept = rbind(cat_to_intercept,
                                  data.frame(cat = cat, p.value = p.values))
      }
      
      if (nrow(cat_to_intercept) != 0){
        cat_to_intercept = cat_to_intercept %>% arrange(desc(p.value))
      }
      else{
        0
      }  
    }
    
    return(cat_to_intercept)
    
  }
  
  bonf_correction = function(lme_mdl, column_var){
    
    non_sig = cat_to_bonferroni(lme_mdl, column_var)
    
    if(nrow(non_sig != 0)){
      # Divide os valores não significativos pela quantidade de categorias não significativas
      n_cat = length(non_sig$cat)
      p_correc = non_sig[2]/n_cat
      # Média entre os valores resultantes
      p_correc = mean(p_correc$p.value)
      
      # Cria legenda para o gráfico
      cap = paste0("H0: ")
      categs = non_sig$cat
      categs[1:n_cat-1]
      for(categ in categs[1:length(categs)-1]){
        cap = paste0(cap, categ,", ")
      }
      cap = paste0(cap, categs[n_cat], " (p = ", round(p_correc,2), ")")
      
      cap
    } else {
      cap = " "
      cap
    }
  }
  
  tabela_uso = dados %>%
    select(produtividade, repeticao, local, genotipo, safra, id_ensaio, cidade)
  
  ids = unique(tabela_uso$id_ensaio)
  vec_ids = c()
  i = 1
  
  for(id in ids){
    tabela_uso_2 = tabela_uso[tabela_uso$id_ensaio == id,]
    
    if(all(is.element(genotiposSelect, tabela_uso_2$genotipo))){
      vec_ids[i] = id
      i = i+1 
    }
  }
  
  if(length(vec_ids) > 1) {
    
    tab_med_gen = tabela_uso %>% 
      filter(genotipo %in% genotiposSelect, id_ensaio %in% vec_ids) %>%
      dplyr::group_by(genotipo,id_ensaio) %>% 
      dplyr::summarise(media = mean(produtividade))
    
    split_tab_gen = split(tab_med_gen, (tab_med_gen$genotipo))
    cbind_tab_gen = split_tab_gen[[1]][,2:3]
    sorted_gen = sort(genotiposSelect)
    names(cbind_tab_gen)[2] = sorted_gen[1]
    
    for(i in 2:length(split_tab_gen)){
      cbind_tab_gen = cbind(cbind_tab_gen, split_tab_gen[[i]][3])
      names(cbind_tab_gen)[i+1] = sorted_gen[i]
    }
    
    tabela_uso1 = tabela_uso %>%
      dplyr::filter(id_ensaio %in% vec_ids) %>%
      group_by(id_ensaio, cidade) %>%
      dplyr::summarise(media_local = mean(produtividade))
    
    tabela_final = cbind(cbind_tab_gen, tabela_uso1[2], tabela_uso1[3], tabela_uso1[3])
    names(tabela_final)[ncol(tabela_final)] = "media_local_y"
    melt_tab = reshape2::melt(tabela_final, id.vars = c("id_ensaio","media_local","cidade"))
    lmdl1 = lmer(data = melt_tab %>% filter(variable != "media_local_y"), value ~ variable + (1|cidade) + (1|id_ensaio) + (1|id_ensaio:cidade))
    
    melt_tab = cbind(melt_tab, pred_vals = c(predict(lmdl1), melt_tab[melt_tab$variable == "media_local_y", 5]))
    vartab = summary(lmdl1)[["varcor"]]
    vartab = as.data.frame(vartab)
    perc_ranerr = vartab$vcov[length(vartab$vcov)]/sum(vartab$vcov)
    
    caption_bonf = bonf_correction(lmdl1, melt_tab$variable[melt_tab$variable != "media_local_y"])
    
    plot = ggplot(melt_tab, aes(y = value, x = media_local, colour = variable, linetype = variable)) +
      geom_point() +
      geom_smooth(method = "lm", se = F, aes(y = pred_vals, x = media_local)) +
      #scale_linetype_discrete() +
      scale_linetype_manual(name="variable", 
                            breaks= unique(melt_tab$variable),
                            values = c(rep(1,length(unique(melt_tab$variable))-1),5)) +
      #labs(caption = caption_bonf) +
      xlab("Média Local (kg/ha)") +
      ylab("") +
      theme_bw() +
      theme(legend.title = element_blank()) +
      annotate("label", x = (min(melt_tab$media_local)+max(melt_tab$media_local))/2, y = Inf, hjust = "inward", vjust = 1.5, label = paste0("Percentual de erro aleatório: ",round(perc_ranerr*100,2),"%"," \n", caption_bonf))
    
    
    return(plot)
    
  }
  
}

