###############################################################################
# SCRIPT 05
# CLUSTER VALIDATION
#
# Objective:
# Evaluate the quality of a clustering solution independently of the
# clustering algorithm adopted.
#
# Supported methods:
# - K-means
# - PAM
# - Hierarchical clustering
# - GMM
# - Any other clustering algorithm
#
# INPUTS
# 1) Dataset used during clustering
# 2) Cluster membership
# 3) Variable metadata
###############################################################################

###############################################################################
# 1. INSTALL AND LOAD PACKAGES
###############################################################################

packages <- c(
  "cluster",
  "clusterCrit",
  "fpc",
  "factoextra",
  "dplyr",
  "openxlsx",
  "ggplot2"
)

installed_packages <- rownames(installed.packages())

for(pkg in packages){
  
  if(!(pkg %in% installed_packages)){
    
    install.packages(pkg)
    
  }
  
}

invisible(
  lapply(
    packages,
    library,
    character.only = TRUE
  )
)

###############################################################################
# 2. IMPORT CLUSTERING DATASET
###############################################################################
#
# IMPORTANT
#
# Import EXACTLY the dataset used to build the clustering.
#
# Examples
#
# Gower + PAM
# -> transformed variables
#
# FAMD + K-means
# -> retained FAMD dimensions
#
###############################################################################

dados <- read.delim("clipboard")

dados <- as.data.frame(dados)

###############################################################################
# Expected structure
#
# Code   Variable1 Variable2 ...
###############################################################################

###############################################################################
# 3. IMPORT CLUSTER MEMBERSHIP
###############################################################################
#
# Expected structure
#
# Code    Cluster
#
###############################################################################

cluster_solution <- read.delim("clipboard")

cluster_solution <- as.data.frame(cluster_solution)

###############################################################################
# 4. IMPORT VARIABLE METADATA
###############################################################################
#
# Expected structure
#
# Variable
# Statistical_type
#
# total_area     Quantitative
# education      Categorical
# income         Quantitative
#
###############################################################################

variable_info <- read.delim("clipboard")

###############################################################################
# 5. CHECK INPUT FILES
###############################################################################

required_metadata <-
  c(
    "Variable",
    "Statistical_type"
  )

if(
  !all(
    required_metadata %in%
    names(variable_info)
  )
){
  
  stop(
    "Metadata must contain Variable and Statistical_type."
  )
  
}

if(
  !"Code" %in%
  names(dados)
){
  
  stop(
    "Dataset must contain column Code."
  )
  
}

if(
  !"Code" %in%
  names(cluster_solution)
){
  
  stop(
    "Cluster table must contain column Code."
  )
  
}

if(
  !"Cluster" %in%
  names(cluster_solution)
){
  
  stop(
    "Cluster table must contain column Cluster."
  )
  
}

missing_codes <-
  setdiff(
    dados$Code,
    cluster_solution$Code
  )

if(length(missing_codes)>0){
  
  stop(
    "Some observations are missing in the cluster solution."
  )
  
}

###############################################################################
# 6. IDENTIFY VARIABLE TYPES
###############################################################################

quantitative_variables <-
  variable_info$Variable[
    variable_info$Statistical_type=="Quantitative"
  ]

categorical_variables <-
  variable_info$Variable[
    variable_info$Statistical_type=="Categorical"
  ]

###############################################################################
# 7. CONVERT CATEGORICAL VARIABLES
###############################################################################

common_categorical <-
  intersect(
    categorical_variables,
    names(dados)
  )

dados[
  common_categorical
] <-
  lapply(
    dados[
      common_categorical
    ],
    factor
  )

###############################################################################
# 8. MERGE DATASET AND CLUSTER SOLUTION
###############################################################################

dados <-
  merge(
    dados,
    cluster_solution,
    by="Code"
  )

###############################################################################
# 9. CREATE ANALYSIS MATRICES
###############################################################################

cluster <-
  as.factor(
    dados$Cluster
  )

data_analysis <-
  dados[
    ,
    !(names(dados) %in%
        c("Code","Cluster"))
  ]

###############################################################################
# 10. GENERAL SUMMARY
###############################################################################

cat("\n")

cat("=====================================================\n")
cat("CLUSTER VALIDATION\n")
cat("=====================================================\n")

cat(
  "Number of observations : ",
  nrow(dados),
  "\n"
)

cat(
  "Number of variables    : ",
  ncol(data_analysis),
  "\n"
)

cat(
  "Number of clusters     : ",
  length(unique(cluster)),
  "\n"
)

cat(
  "Quantitative variables : ",
  length(quantitative_variables),
  "\n"
)

cat(
  "Categorical variables  : ",
  length(categorical_variables),
  "\n"
)

cat("=====================================================\n")

###############################################################################
# 11. IDENTIFY DATA STRUCTURE
###############################################################################

cat("\n")
cat("=====================================================\n")
cat("DATA STRUCTURE\n")
cat("=====================================================\n")
cat("1 - Original dataset (mixed variables)\n")
cat("2 - FAMD dimensions (all quantitative)\n")
cat("=====================================================\n")

data_structure <-
  as.numeric(
    readline(
      "Select data structure (1 or 2): "
    )
  )

if(!(data_structure %in% c(1,2))){
  stop("Invalid option.")
}

##############################################################################
# 12. DEFINE VARIABLE TYPES
###############################################################################

if(data_structure == 1){
  
  cat("\nUsing original mixed dataset.\n")
  
  quantitative_variables <-
    variable_info$Variable[
      variable_info$Statistical_type == "Quantitative"
    ]
  
  categorical_variables <-
    variable_info$Variable[
      variable_info$Statistical_type == "Categorical"
    ]
  
} else {
  
  cat("\nUsing FAMD dimensions.\n")
  
  quantitative_variables <-
    setdiff(
      names(dados),
      c("Code", "Cluster")
    )
  
  categorical_variables <- NULL
  
}

###############################################################################
# 13. DATASET SUMMARY
###############################################################################

cat("\n")
cat("=====================================================\n")
cat("DATA SUMMARY\n")
cat("=====================================================\n")

cat("Observations               :",nrow(dados),"\n")
cat("Variables                 :",ncol(dados)-2,"\n")
cat("Clusters                  :",length(unique(dados$Cluster)),"\n")

if(data_structure==1){
  
  cat("Quantitative variables    :",length(quantitative_variables),"\n")
  cat("Categorical variables     :",length(categorical_variables),"\n")
  
}else{
  
  cat("FAMD dimensions           :",length(quantitative_variables),"\n")
  cat("All variables quantitative.\n")
  
}

cat("=====================================================\n")
setdiff(quantitative_variables, names(dados))


###############################################################################
# 14. CREATE ANALYSIS MATRICES
###############################################################################

if(data_structure==1){
  
  quantitative_data <-
    dados[
      ,
      quantitative_variables,
      drop=FALSE
    ]
  
  categorical_data <-
    dados[
      ,
      categorical_variables,
      drop=FALSE
    ]
  
}else{
  
  quantitative_data <-
    dados[
      ,
      quantitative_variables,
      drop=FALSE
    ]
  
  categorical_data <- NULL
  
}

###############################################################################
# 15. CHECK CLUSTER SIZES
###############################################################################

cluster_size <-
  table(
    dados$Cluster
  )

cluster_size <-
  data.frame(
    Cluster=names(cluster_size),
    Observations=as.numeric(cluster_size)
  )

print(cluster_size)

cat("\n")

if(any(cluster_size$Observations<5)){
  
  warning(
    "One or more clusters contain fewer than five observations."
  )
  
}else{
  
  cat("All clusters have at least five observations.\n")
  
}

###############################################################################
# 16. DESCRIPTIVE STATISTICS BY CLUSTER
###############################################################################

library(dplyr)

descriptive_statistics <- data.frame()

variables_to_analyze <-
  quantitative_variables

for(v in variables_to_analyze){
  
  temp <-
    dados %>%
    group_by(Cluster) %>%
    summarise(
      
      N =
        sum(!is.na(.data[[v]])),
      
      Mean =
        mean(.data[[v]],na.rm=TRUE),
      
      Median =
        median(.data[[v]],na.rm=TRUE),
      
      SD =
        sd(.data[[v]],na.rm=TRUE),
      
      CV =
        ifelse(
          Mean==0,
          NA,
          SD/Mean*100
        ),
      
      Minimum =
        min(.data[[v]],na.rm=TRUE),
      
      Maximum =
        max(.data[[v]],na.rm=TRUE)
      
    )
  
  temp$Variable <- v
  
  descriptive_statistics <-
    rbind(
      descriptive_statistics,
      temp
    )
  
}

descriptive_statistics <-
  descriptive_statistics[
    ,
    c(
      "Variable",
      "Cluster",
      "N",
      "Mean",
      "Median",
      "SD",
      "CV",
      "Minimum",
      "Maximum"
    )
  ]

print(descriptive_statistics)


library(writexl)

# 1. Definir o caminho da pasta de destino
pasta_agrupamento <- "C:/Users/biel/Desktop/Agrupamento"

# Garantir que a pasta exista
dir.create(pasta_agrupamento, recursive = TRUE, showWarnings = FALSE)

# 2. Exportar a tabela para Excel
caminho_arquivo <- file.path(pasta_agrupamento, "descriptive_statistics_clusters.xlsx")

write_xlsx(
  descriptive_statistics, 
  path = caminho_arquivo
)

cat("Tabela de estatísticas descritivas salva com sucesso em:\n", caminho_arquivo, "\n")


###############################################################################
# 17. SHAPIRO-WILK NORMALITY TEST
###############################################################################

normality_results <- data.frame()

for(v in quantitative_variables){
  
  for(cl in unique(dados$Cluster)){
    
    x <-
      dados[
        dados$Cluster==cl,
        v
      ]
    
    x <- na.omit(x)
    
    if(length(unique(x))>=3 &
       length(x)>=3){
      
      test <-
        shapiro.test(x)
      
      normality_results <-
        rbind(
          normality_results,
          
          data.frame(
            
            Variable=v,
            
            Cluster=cl,
            
            W=test$statistic,
            
            P_value=test$p.value,
            
            Normal=
              ifelse(
                test$p.value>0.05,
                "Yes",
                "No"
              )
            
          )
          
        )
      
    }
    
  }
  
}

print(normality_results)

library(writexl)

# 1. Definir o caminho da pasta de destino
pasta_agrupamento <- "C:/Users/biel/Desktop/Agrupamento"

# Garantir que a pasta exista
dir.create(pasta_agrupamento, recursive = TRUE, showWarnings = FALSE)

# 2. Exportar os resultados de normalidade para Excel
caminho_arquivo <- file.path(pasta_agrupamento, "normality_results_shapiro.xlsx")

write_xlsx(
  normality_results, 
  path = caminho_arquivo
)

cat("Tabela de testes de normalidade salva com sucesso em:\n", caminho_arquivo, "\n")



###############################################################################
# 18. HOMOGENEITY OF VARIANCES
###############################################################################

install.packages("car")
library(car)

variance_results <- data.frame()

for(v in quantitative_variables){
  
  # Forçamos o R a entender o Cluster como Fator adicionando factor() na fórmula
  formula <-
    as.formula(
      paste(
        v,
        "~ factor(Cluster)"
      )
    )
  
  test <-
    leveneTest(
      formula,
      data=dados
    )
  
  variance_results <-
    rbind(
      variance_results,
      
      data.frame(
        Variable=v,
        F_value=test$`F value`[1],
        P_value=test$`Pr(>F)`[1],
        Homogeneous=
          ifelse(
            test$`Pr(>F)`[1]>0.05,
            "Yes",
            "No"
          )
      )
    )
}

print(variance_results)

library(writexl)

# 1. Definir o caminho da pasta de destino
pasta_agrupamento <- "C:/Users/biel/Desktop/Agrupamento"

# Garantir que a pasta exista
dir.create(pasta_agrupamento, recursive = TRUE, showWarnings = FALSE)

# 2. Exportar os resultados de homogeneidade para Excel
caminho_arquivo <- file.path(pasta_agrupamento, "homogeneity_results_levene.xlsx")

write_xlsx(
  variance_results, 
  path = caminho_arquivo
)

cat("Tabela de homogeneidade de variâncias salva com sucesso em:\n", caminho_arquivo, "\n")

###############################################################################
# 19. GLOBAL COMPARISON BETWEEN CLUSTERS
###############################################################################

comparison_results <- data.frame()

for(v in quantitative_variables){
  
  normal <-
    all(
      normality_results$Normal[
        normality_results$Variable==v
      ] == "Yes"
    )
  
  homogeneous <-
    variance_results$Homogeneous[
      variance_results$Variable==v
    ] == "Yes"
  
  # Adaptação: Forçando o Cluster a ser fator na fórmula
  formula <-
    as.formula(
      paste(
        v,
        "~ factor(Cluster)"
      )
    )
  
  if(normal & homogeneous){
    
    test <-
      aov(
        formula,
        data=dados
      )
    
    pvalue <-
      summary(test)[[1]][["Pr(>F)"]][1]
    
    method <-
      "ANOVA"
    
  }else{
    
    test <-
      kruskal.test(
        formula,
        data=dados
      )
    
    pvalue <-
      test$p.value
    
    method <-
      "Kruskal-Wallis"
    
  }
  
  comparison_results <-
    rbind(
      comparison_results,
      
      data.frame(
        Variable=v,
        Method=method,
        P_value=pvalue,
        Significant=
          ifelse(
            pvalue < 0.05,
            "Yes",
            "No"
          )
      )
    )
  
}

print(comparison_results)

library(writexl)

# 1. Definir o caminho da pasta de destino
pasta_agrupamento <- "C:/Users/biel/Desktop/Agrupamento"

# Garantir que a pasta exista
dir.create(pasta_agrupamento, recursive = TRUE, showWarnings = FALSE)

# 2. Exportar os resultados das comparações para Excel
caminho_arquivo <- file.path(pasta_agrupamento, "comparison_results_anova_kruskal.xlsx")

write_xlsx(
  comparison_results, 
  path = caminho_arquivo
)

cat("Tabela de comparação entre clusters salva com sucesso em:\n", caminho_arquivo, "\n")


###############################################################################
# 20. POST-HOC TESTS E SALVAMENTO DETALHADO (PAR A PAR)
###############################################################################

if(!require(FSA)) install.packages("FSA")
if(!require(writexl)) install.packages("writexl")

library(FSA)
library(writexl)

posthoc_results <- data.frame()

for(v in comparison_results$Variable){
  
  method <-
    comparison_results$Method[
      comparison_results$Variable == v
    ]
  
  significant <-
    comparison_results$Significant[
      comparison_results$Variable == v
    ]
  
  if(significant == "Yes"){
    
    formula <-
      as.formula(
        paste(
          v,
          "~ factor(Cluster)"
        )
      )
    
    if(method == "ANOVA"){
      
      model <-
        aov(
          formula,
          data = dados
        )
      
      tukey <-
        TukeyHSD(model)
      
      temp_raw <- as.data.frame(tukey$`factor(Cluster)`)
      
      temp <- data.frame(
        Variable   = v,
        Comparison = rownames(temp_raw),
        Z_or_Diff  = temp_raw$diff,
        P_adj      = temp_raw$`p adj`,
        Significant = ifelse(temp_raw$`p adj` < 0.05, "Yes", "No"),
        Method     = "Tukey"
      )
      
    } else {
      
      dunn <-
        dunnTest(
          formula,
          data = dados,
          method = "holm"
        )
      
      temp_raw <- dunn$res
      
      temp <- data.frame(
        Variable   = v,
        Comparison = temp_raw$Comparison,
        Z_or_Diff  = temp_raw$Z,
        P_adj      = temp_raw$P.adj,
        Significant = ifelse(temp_raw$P.adj < 0.05, "Yes", "No"),
        Method     = "Dunn-Holm"
      )
      
    }
    
    posthoc_results <-
      rbind(
        posthoc_results,
        temp
      )
    
  }
  
}

# Organizar visualmente as colunas do Post-Hoc
if(nrow(posthoc_results) > 0){
  posthoc_results <- posthoc_results[, c("Variable", "Comparison", "Z_or_Diff", "P_adj", "Significant", "Method")]
  print(head(posthoc_results, 20)) # Imprime os primeiros no console sem poluir
} else {
  cat("Nenhuma variável apresentou diferença significativa entre os clusters.\n")
}

###############################################################################
# EXPORTAÇÃO DOS ARQUIVOS (PAR A PAR + TESTE GLOBAL)
###############################################################################

# 1. Definir diretório de destino
pasta_agrupamento <- "C:/Users/biel/Desktop/Agrupamento"
dir.create(pasta_agrupamento, recursive = TRUE, showWarnings = FALSE)

# 2. Salvar em um ÚNICO arquivo Excel com 2 Abas (Mais limpo e organizado)
caminho_consolidado <- file.path(pasta_agrupamento, "Resultados_Comparacao_e_PostHoc.xlsx")

write_xlsx(
  list(
    "Teste_Global_Kruskal_ANOVA" = comparison_results,
    "PostHoc_Par_a_Par"          = posthoc_results
  ),
  path = caminho_consolidado
)

# 3. Opção de salvar também o Post-Hoc em arquivo .xlsx individual dedicado
caminho_posthoc <- file.path(pasta_agrupamento, "posthoc_results_dunn_tukey.xlsx")
write_xlsx(posthoc_results, path = caminho_posthoc)

cat("\n=======================================================\n")
cat("Arquivos salvos com sucesso na pasta:\n", pasta_agrupamento, "\n\n")
cat("1. Consolidado (2 abas) : 'Resultados_Comparacao_e_PostHoc.xlsx'\n")
cat("2. Detalhado Par a Par  : 'posthoc_results_dunn_tukey.xlsx'\n")
cat("=======================================================\n")



###############################################################################
# 21. PREPARAR MATRIZ DE DISTÂNCIA DE GOWER (DADOS MISTOS) - GOWER
###############################################################################

if(!require(cluster)) install.packages("cluster")
if(!require(fpc)) install.packages("fpc")
if(!require(clusterSim)) install.packages("clusterSim")
if(!require(writexl)) install.packages("writexl")

library(cluster)
library(fpc)
library(clusterSim)
library(writexl)

# 1. Isolamos os dados da análise (removendo Code e Cluster)
dados_analise <- dados[, !(names(dados) %in% c("Code", "Cluster"))]

# 2. Vetor de clusters
cluster_vector <- as.numeric(dados$Cluster)

# 3. Matriz de Distância de GOWER (Preserva qualitativas e quantitativas)
dist_matrix <- daisy(dados_analise, metric = "gower")

###############################################################################
# 22. CÁLCULO DA SILHUETA
###############################################################################

sil_res <- silhouette(cluster_vector, dist_matrix)
avg_silhouette <- mean(sil_res[, "sil_width"])

###############################################################################
# 23. ESTATÍSTICAS E ÍNDICES DE VALIDAÇÃO (GOWER ADAPTED)
###############################################################################

# Estatísticas gerais via fpc baseadas na matriz de distância
cluster_stats <- cluster.stats(dist_matrix, cluster_vector)

calinski_harabasz <- cluster_stats$ch
dunn_index        <- cluster_stats$dunn

# Davies-Bouldin para matrizes de dissimilaridade/medoides (clusterSim)
# O argumento 'd' recebe a matriz de distâncias (como matriz)
davies_bouldin <- index.DB(
  x = as.matrix(dist_matrix), 
  cl = cluster_vector, 
  d = as.matrix(dist_matrix), 
  centrotypes = "medoids"
)$DB

# Razão de Dissimilaridade (Substitui a razão de variâncias em dados mistos)
# Soma das distâncias dentro dos clusters / Soma das distâncias entre clusters
ss_within  <- cluster_stats$within.cluster.ss
ss_between <- cluster_stats$between.cluster.ss
dissimilarity_ratio <- ifelse(!is.null(ss_within) && ss_within > 0, ss_between / ss_within, NA)

###############################################################################
# 24. CONSOLIDAR TABELA DE MÉTRICAS
###############################################################################

metrics_summary <- data.frame(
  Metric = c(
    "Average Silhouette Width",
    "Davies-Bouldin Index",
    "Calinski-Harabasz Index",
    "Dunn Index",
    "Between/Within Dissimilarity Ratio"
  ),
  Value = c(
    avg_silhouette,
    davies_bouldin,
    calinski_harabasz,
    dunn_index,
    dissimilarity_ratio
  ),
  Ideal_Behavior = c(
    "Closer to 1 (Well separated)",
    "Lower (More compact)",
    "Higher (Better separation)",
    "Higher (More compact/separated)",
    "Higher (More distinct groups)"
  )
)

print(metrics_summary)

###############################################################################
# 25. EXPORT METRICS TO EXCEL
###############################################################################

pasta_agrupamento <- "C:/Users/biel/Desktop/Agrupamento"
dir.create(pasta_agrupamento, recursive = TRUE, showWarnings = FALSE)

caminho_metricas <- file.path(pasta_agrupamento, "Gower_Cluster_Quality_Metrics.xlsx")

write_xlsx(metrics_summary, path = caminho_metricas)

cat("\nMétricas de validação do Gower salvas com sucesso em:\n", caminho_metricas, "\n")


###############################################################################
# 21. PREPARAR MATRIZ DE DISTÂNCIA UNIVERSAL (Gower)
###############################################################################

# Isolamos apenas o que é numérico e ignoramos as colunas de controle (Code e Cluster)
colunas_numericas <- names(dados)[sapply(dados, is.numeric) & !names(dados) %in% c("Code", "Cluster")]

dados_cluster <- dados[, colunas_numericas, drop = FALSE]

# Garantimos que o vetor de cluster está perfeitamente alinhado por linha
cluster_vector <- as.numeric(dados$Cluster)

# Matriz de distância Euclidiana (Universal para espaços numéricos/reduzidos)
dist_matrix <- dist(dados_cluster, method = "euclidean")

###############################################################################
# 22. CÁLCULO DA SILHUETA
###############################################################################
library(cluster)

sil_res <- silhouette(cluster_vector, dist_matrix)
avg_silhouette <- mean(sil_res[, "sil_width"])

###############################################################################
# 23. ÍNDICES DE VALIDAÇÃO (CH, DUNN E DAVIES-BOULDIN UNIVERSAL)
###############################################################################
library(fpc)
cluster_stats <- cluster.stats(dist_matrix, cluster_vector)

calinski_harabasz <- cluster_stats$ch
dunn_index        <- cluster_stats$dunn

# CÁLCULO DIRETO E UNIVERSAL DO DAVIES-BOULDIN (Sem depender do fpc)
centroids <- aggregate(dados_cluster, list(cluster_vector), mean)[,-1]
dists_centroids <- as.matrix(dist(centroids))
r_matrix <- matrix(0, nrow=nrow(centroids), ncol=nrow(centroids))

for(i in 1:nrow(centroids)){
  for(j in 1:nrow(centroids)){
    if(i != j){
      # Isola os dados de cada cluster de forma matricial agnóstica
      df_i <- as.matrix(dados_cluster[cluster_vector == i, , drop = FALSE])
      df_j <- as.matrix(dados_cluster[cluster_vector == j, , drop = FALSE])
      
      # Calcula a dispersão interna (S_i e S_j)
      s_i <- mean(rowSums(sweep(df_i, 2, as.numeric(centroids[i,]), "-")^2)^0.5)
      s_j <- mean(rowSums(sweep(df_j, 2, as.numeric(centroids[j,]), "-")^2)^0.5)
      
      # R_ij = (S_i + S_j) / d(M_i, M_j)
      r_matrix[i,j] <- (s_i + s_j) / dists_centroids[i,j]
    }
  }
}
# O índice DB é a média dos máximos de R_ij para cada cluster
davies_bouldin <- mean(apply(r_matrix, 1, max))

# Razão de Variâncias Intra/Inter
ss_total   <- sum(scale(dados_cluster, scale = FALSE)^2)
ss_within  <- cluster_stats$within.cluster.ss
ss_between <- ss_total - ss_within
variance_ratio <- ss_between / ss_within

###############################################################################
# 24. CONSOLIDAR TABELA DE MÉTRICAS
###############################################################################

metrics_summary <- data.frame(
  Metric = c(
    "Average Silhouette Width",
    "Davies-Bouldin Index",
    "Calinski-Harabasz Index",
    "Dunn Index",
    "Between/Within Variance Ratio"
  ),
  Value = c(
    avg_silhouette,
    davies_bouldin,      # Agora calculado via matriz universal
    calinski_harabasz,
    dunn_index,
    variance_ratio
  ),
  Ideal_Behavior = c(
    "Closer to 1 (Well separated)",
    "Lower (More compact)",
    "Higher (Better separation)",
    "Higher (More compact/separated)",
    "Higher (More distinct groups)"
  )
)

print(metrics_summary)
