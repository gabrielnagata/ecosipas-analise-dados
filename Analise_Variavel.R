# ==============================================================================
# SCRIPT 05B & 05C: INTERPRETAÇÃO E PERFIL DA TIPOLOGIA RURAL (VERSÃO GOWER)
# ==============================================================================

# 1. CARREGAR PACOTES CHAVE
packages <- c("tidyverse", "car", "FSA", "vegan", "cluster", "openxlsx", "ggplot2")

installed_packages <- rownames(installed.packages())
for(pkg in packages){
  if(!(pkg %in% installed_packages)) install.packages(pkg)
}
invisible(lapply(packages, library, character.only = TRUE))

# Definir caminho base para salvar os resultados
pasta_agrupamento <- "C:/Users/biel/Desktop/Analise"
dir.create(pasta_agrupamento, recursive = TRUE, showWarnings = FALSE)

cat("=====================================================\n")
cat("PASSO 1: IMPORTAÇÃO DO DICIONÁRIO DE VARIÁVEIS\n")
cat("=====================================================\n\n")

# Esperado no Clipboard: Colunas 'Variable' e 'Statistical_type'
variable_info <- read.delim("clipboard")
colnames(variable_info) <- c("Variable", "Statistical_type")

# Limpeza contra espaços extras e caixa
variable_info$Variable <- trimws(variable_info$Variable)
variable_info$Statistical_type <- trimws(tolower(variable_info$Statistical_type))

print(variable_info)

# Pausa estratégica para copiar o banco de dados real
readline(prompt="\nSe o dicionário acima estiver correto, copie seu DATASET REAL (com as variáveis e a coluna Cluster) e aperte [Enter]...")

###############################################################################
# 2. IMPORT REAL DATASET WITH CLUSTER MEMBERSHIP
###############################################################################
cat("\n=====================================================\n")
cat("PASSO 2: IMPORTAÇÃO E TIPAGEM DO DATASET REAL\n")
cat("=====================================================\n\n")

dados <- read.delim("clipboard")
dados <- as.data.frame(dados)

# Faxina imediata (trata vírgula decimal BR)
dados <- dados %>%
  mutate(across(everything(), ~ trimws(gsub(",", ".", as.character(.x)))))

# Aplicamos a tipagem com base no dicionário
for(i in 1:nrow(variable_info)) {
  var_nome <- variable_info$Variable[i]
  var_tipo <- variable_info$Statistical_type[i]
  
  if(var_nome %in% names(dados)) {
    if(grepl("quant|num", var_tipo)) {
      dados[[var_nome]] <- as.numeric(dados[[var_nome]])
    } else {
      dados[[var_nome]] <- as.factor(dados[[var_nome]])
    }
  }
}

# Forçar a coluna Cluster a ser Fator
dados$Cluster <- as.factor(dados$Cluster)

# Vetores de variáveis
quantitative_vars <- names(dados)[sapply(dados, is.numeric) & !names(dados) %in% c("Code", "Cluster")]
categorical_vars  <- names(dados)[sapply(dados, is.factor) & !names(dados) %in% c("Code", "Cluster")]

cat("\n--- DETECÇÃO DE SEGURANÇA ---")
cat("\nVariáveis Quantitativas detectadas (", length(quantitative_vars), "): ", paste(quantitative_vars, collapse=", "))
cat("\nVariáveis Categóricas detectadas   (", length(categorical_vars), "): ", paste(categorical_vars, collapse=", "))
cat("\n------------------------------\n\n")

cat("Dataset carregado! Amostras:", nrow(dados), "| Clusters:", length(unique(dados$Cluster)), "\n")

###############################################################################
# 3. ESCALA DE ANÁLISE
###############################################################################
cat("\n=====================================================\n")
cat("PASSO 3: DEFINIÇÃO DA ESCALA DE ANÁLISE\n")
cat("=====================================================\n")

opcao_escala <- 1 

if(opcao_escala == 2){
  cat("\nAplicando transformação log1p nas variáveis quantitativas...\n")
  dados <- dados %>%
    mutate(across(all_of(quantitative_vars), ~ log1p(.x)))
} else {
  cat("\nSeguindo com os dados originais brutos.\n")
}

###############################################################################
# 4. ANÁLISE INFERENCIAL E DESCRITIVA (QUANTITATIVAS)
###############################################################################
cat("\nProcessando variáveis quantitativas (Tabelas, Testes e Boxplots)...\n")

descriptive_summary <- data.frame()
global_comparison   <- data.frame()
posthoc_summary     <- data.frame()

pasta_boxplots <- file.path(pasta_agrupamento, "Boxplots_Interpretacao")
if(!dir.exists(pasta_boxplots)) dir.create(pasta_boxplots, recursive = TRUE)

for(v in quantitative_vars){
  
  # 1. Estatística Descritiva por Cluster
  valores <- dados[[v]]
  grupos  <- dados$Cluster
  
  for(g in sort(unique(grupos))){
    subvetor <- valores[grupos == g]
    subvetor_limpo <- subvetor[!is.na(subvetor)]
    
    n_val      <- length(subvetor_limpo)
    mean_val   <- ifelse(n_val > 0, mean(subvetor_limpo), NA)
    median_val <- ifelse(n_val > 0, median(subvetor_limpo), NA)
    sd_val     <- ifelse(n_val > 1, sd(subvetor_limpo), NA)
    cv_val     <- ifelse(!is.na(mean_val) && mean_val != 0 && !is.na(sd_val), (sd_val/mean_val)*100, NA)
    
    temp_desc <- data.frame(
      Cluster = g,
      Variable = v,
      N = n_val,
      Mean = mean_val,
      Median = median_val,
      SD = sd_val,
      CV_percent = cv_val
    )
    descriptive_summary <- rbind(descriptive_summary, temp_desc)
  }
  
  # 2. Testes de Comparação Global (Kruskal-Wallis)
  formula_test <- as.formula(paste(v, "~ Cluster"))
  test_res <- kruskal.test(formula_test, data = dados)
  p_val <- test_res$p.value
  
  global_comparison <- rbind(
    global_comparison,
    data.frame(Variable = v, Method = "Kruskal-Wallis", P_value = p_val, Significant = ifelse(p_val < 0.05, "Yes", "No"))
  )
  
  # 3. Teste Post-Hoc (Dunn-Holm)
  if(!is.na(p_val) && p_val < 0.05){
    dunn_res <- dunnTest(formula_test, data = dados, method = "holm")$res
    dunn_res$Variable = v
    posthoc_summary <- rbind(posthoc_summary, dunn_res)
  }
  
  # 4. Boxplots
  p <- ggplot(dados, aes(x = Cluster, y = .data[[v]], fill = Cluster)) +
    geom_boxplot(alpha = 0.7, outlier.stroke = 0.5) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    theme_minimal(base_size = 12) +
    labs(title = paste("Distribuição de:", v, "por Cluster"), y = v, x = "Clusters") +
    scale_fill_brewer(palette = "Set2") +
    theme(legend.position = "none", panel.grid.minor = element_blank())
  
  ggsave(filename = file.path(pasta_boxplots, paste0("Boxplot_", v, ".png")), plot = p, width = 7, height = 5, dpi = 300)
}

###############################################################################
# 5. ANÁLISE DE VARIÁVEIS CATEGÓRICAS (QUI-QUADRADO COM MONTE CARLO)
###############################################################################
cat("Processando variáveis categóricas (Tabelas de Contingência e Qui-Quadrado)...\n")

categorical_tests <- data.frame()

for(v in categorical_vars){
  tab_abs <- table(dados[[v]], dados$Cluster)
  
  chisq_res <- chisq.test(tab_abs, simulate.p.value = TRUE, B = 2000)
  
  categorical_tests <- rbind(
    categorical_tests,
    data.frame(
      Variable = v,
      Method = "Chi-Square (Monte Carlo)",
      P_value = chisq_res$p.value,
      Significant = ifelse(chisq_res$p.value < 0.05, "Yes", "No")
    )
  )
}

###############################################################################
# 6. ANÁLISE MULTIVARIADA GLOBAL COM DISTÂNCIA DE GOWER (QUANT + CATEG)
###############################################################################
cat("Processando Análise Multivariada Global via Gower (PERMANOVA, ANOSIM e Betadisper)...\n")

# 1. Selecionamos todas as variáveis preditoras (removendo Code e Cluster)
matriz_todas_vars <- dados[, !(names(dados) %in% c("Code", "Cluster"))]

# 2. CORREÇÃO: Converte qualquer coluna 'character' remanescente para 'factor'
matriz_todas_vars <- matriz_todas_vars %>%
  mutate(across(where(is.character), as.factor))

# 3. Matriz de Distância de Gower Real (Mistas)
dist_reais <- daisy(matriz_todas_vars, metric = "gower")

# 4. Testes Multivariados Globais
perm_res   <- adonis2(dist_reais ~ Cluster, data = dados, permutations = 999)
anosim_res <- anosim(dist_reais, dados$Cluster, permutations = 999)
disp_res   <- betadisper(dist_reais, dados$Cluster)
perm_disp  <- permutest(disp_res, permutations = 999)

multivariate_summary <- data.frame(
  Test = c(
    "PERMANOVA (Separação Global dos Grupos)", 
    "ANOSIM (Força do Agrupamento R)", 
    "BETADISPER (Homogeneidade Multivariada)"
  ),
  Statistic = c(
    perm_res$F[1], 
    anosim_res$statistic, 
    perm_disp$statistic[1]
  ),
  P_value = c(
    perm_res$`Pr(>F)`[1], 
    anosim_res$signif, 
    perm_disp$tab$`Pr(>F)`[1]
  )
)

print(multivariate_summary)

###############################################################################
# 7. CONSOLIDAR E EXPORTAR RELATÓRIO QUANTITATIVO E MULTIVARIADO
###############################################################################
cat("\nExportando relatórios consolidados para Excel...\n")

wb <- createWorkbook()
addWorksheet(wb, "Multivariate_Validation"); writeData(wb, "Multivariate_Validation", multivariate_summary)
addWorksheet(wb, "Quant_Descriptive"); writeData(wb, "Quant_Descriptive", descriptive_summary)
addWorksheet(wb, "Quant_Global_Tests"); writeData(wb, "Quant_Global_Tests", global_comparison)

if(nrow(posthoc_summary) > 0){
  addWorksheet(wb, "Quant_PostHoc"); writeData(wb, "Quant_PostHoc", posthoc_summary)
}
addWorksheet(wb, "Categ_Tests"); writeData(wb, "Categ_Tests", categorical_tests)

caminho_relatorio <- file.path(pasta_agrupamento, "Relatorio_Interpretacao_Tipologia.xlsx")
saveWorkbook(wb, caminho_relatorio, overwrite = TRUE)

###############################################################################
# 8. SCRIPT 05C: POST-HOC CATEGÓRICO POR ANÁLISE DE RESÍDUOS
###############################################################################
cat("\nGerando análise de resíduos ajustados para variáveis categóricas (Script 05C)...\n")

wb_cat <- createWorkbook()

for(v in categorical_vars){
  if(v %in% c("Code", "Cluster")) next
  
  tab <- table(dados[[v]], dados$Cluster)
  chisq_full <- chisq.test(tab, simulate.p.value = TRUE, B = 2000)
  residuos <- chisq_full$stdres
  nomes_categorias <- rownames(residuos)
  
  interpretacao <- matrix("", nrow = nrow(residuos), ncol = ncol(residuos))
  colnames(interpretacao) <- colnames(residuos)
  
  for(r in 1:nrow(residuos)){
    for(c in 1:ncol(residuos)){
      val <- residuos[r, c]
      if(!is.na(val) && val > 1.96)      { interpretacao[r, c] <- "SUPERIOR (Mais que o esperado)" }
      else if(!is.na(val) && val < -1.96) { interpretacao[r, c] <- "INFERIOR (Menos que o esperado)" }
      else { interpretacao[r, c] <- "Igual à média geral" }
    }
  }
  
  df_interp <- as.data.frame(interpretacao, stringsAsFactors = FALSE)
  df_final  <- cbind(Categoria = nomes_categorias, df_interp)
  
  aba_nome <- substr(v, 1, 31) 
  addWorksheet(wb_cat, aba_nome)
  writeData(wb_cat, aba_nome, paste("Variável Categórica:", v), startRow = 1)
  writeData(wb_cat, aba_nome, df_final, startRow = 3)
}

caminho_categoricas <- file.path(pasta_agrupamento, "Diferenciacao_Variaveis_Categoricas.xlsx")
saveWorkbook(wb_cat, caminho_categoricas, overwrite = TRUE)

cat("\n=====================================================\n")
cat("PROCESSAMENTO CONCLUÍDO COM SUCESSO!\n")
cat("Arquivos gerados em:", pasta_agrupamento, "\n")
cat("1.", caminho_relatorio, "\n")
cat("2.", caminho_categoricas, "\n")
cat("=====================================================\n")
