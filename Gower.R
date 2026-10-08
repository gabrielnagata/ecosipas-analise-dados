###############################################################################
# SCRIPT COMPLETO: COMPARAÇÃO DE MÉTODOS DE AGRUPAMENTO (PADRÃO BRASILEIRO)
###############################################################################

# 1. CARREGAR PACOTES CHAVE
packages <- c("tidyverse", "cluster", "factoextra", "fpc")
invisible(lapply(packages, library, character.only = TRUE))

###############################################################################
# 3. MÉTODO A: PAM (PARTITIONING AROUND MEDOIDS) - CONFIGURAÇÃO K=4
###############################################################################
cat("\nRodando o agrupamento PAM (K=4)...\n")
set.seed(1234)
fit_pam <- pam(dist_gower, k = 4, diss = TRUE)


# Adiciona a coluna dinâmica com base no K escolhido
dados_modelo[[paste0("Cluster_PAM_K", k_selecionado)]] <- as.factor(fit_pam$clustering)

cat("\n--- FAZENDAS MEDOIDES IDENTIFICADAS PELO PAM ---\n")
print(dados_modelo[fit_pam$id.med, !names(dados_modelo) %in% c(paste0("Cluster_PAM_K", k_selecionado))])

# --- MÉTODO B: CLUSTER HIERÁRQUICO (WARD.D2) ---
cluster_hcl <- cutree(fit_hierarquico_base, k = k_selecionado)
dados_modelo[[paste0("Cluster_Hie_K", k_selecionado)]] <- as.factor(cluster_hcl)



# Adiciona a classificação do PAM de volta ao dataset do modelo
dados_modelo$Cluster_PAM_K4 <- as.factor(fit_pam$clustering)

# Identificar os Medoides (as fazendas/amostras reais centrais de cada grupo)
cat("\n--- FAZENDAS MEDOIDES IDENTIFICADAS PELO PAM ---\n")
print(dados_modelo[fit_pam$id.med, !names(dados_modelo) %in% c("Cluster_PAM_K4")])

###############################################################################
# 4. MÉTODO B: CLUSTER HIERÁRQUICO (WARD.D2) - CONFIGURAÇÃO K=4
###############################################################################
cat("\nRodando o agrupamento Hierárquico (Ward.D2, K=4)...\n")
fit_hierarquico <- hclust(dist_gower, method = "ward.D2")

# Cortar a árvore hierárquica em 4 grupos
cluster_hcl <- cutree(fit_hierarquico, k = 4)
dados_modelo$Cluster_Hie_K4 <- as.factor(cluster_hcl)


###############################################################################
# 5. AVALIAÇÃO E COMPARAÇÃO DOS AJUSTES (MÉTRICAS BASEADAS EM DISTÂNCIA REAL)
###############################################################################
cat("\n=====================================================\n")
cat("AVALIAÇÃO COMPARATIVA DOS MODELOS (MÉTRICAS PURAS)\n")
cat("=====================================================\n")

# --- 1. SILHOUETTE MÉDIA ---
# PAM
sil_pam <- silhouette(fit_pam$clustering, dist_gower)
avg_sil_pam <- mean(as.matrix(sil_pam)[, 3])

# Hierárquico
sil_hie <- silhouette(as.numeric(cluster_hcl), dist_gower)
avg_sil_hie <- mean(as.matrix(sil_hie)[, 3])


# --- 2. ÍNDICE DUNN (Via fpc) ---
library(fpc)
stats_pam <- cluster.stats(dist_gower, fit_pam$clustering)
stats_hie <- cluster.stats(dist_gower, as.numeric(cluster_hcl))

dunn_pam <- stats_pam$dunn
dunn_hie <- stats_hie$dunn


# --- 3. RAZÃO DE VARIÂNCIAS ADAPTADA (Pseudo-F da PERMANOVA via vegan) ---
# Se não tiver o pacote vegan, ele instalará automaticamente
if(!require(vegan)) install.packages("vegan")
library(vegan)

# Extrai o Pseudo-F que mede a separação dos grupos na distância real
permanova_pam <- adonis2(dist_gower ~ fit_pam$clustering)
pseudoF_pam   <- permanova_pam$F[1]

permanova_hie <- adonis2(dist_gower ~ as.numeric(cluster_hcl))
pseudoF_hie   <- permanova_hie$F[1]

###############################################################################
# 5. AVALIAÇÃO E COMPARAÇÃO DOS AJUSTES (MÉTRICAS BASEADAS EM DISTÂNCIA REAL)
###############################################################################
cat("\n=====================================================\n")
cat("AVALIAÇÃO COMPARATIVA DOS MODELOS (MÉTRICAS PURAS)\n")
cat("=====================================================\n")

# 1. Garante que os pacotes necessários estão ativos
library(cluster)
if(!require(fpc)) install.packages("fpc"); library(fpc)
if(!require(vegan)) install.packages("vegan"); library(vegan)

# 2. Roda as estatísticas globais do fpc para os dois modelos
stats_pam <- cluster.stats(dist_gower, fit_pam$clustering)
stats_hie <- cluster.stats(dist_gower, as.numeric(cluster_hcl))

# 3. Roda a PERMANOVA para extrair o Pseudo-F de cada um
permanova_pam <- adonis2(dist_gower ~ fit_pam$clustering)
permanova_hie <- adonis2(dist_gower ~ as.numeric(cluster_hcl))

###############################################################################
# 5B. TABELA CONSOLIDADA DE VALIDAÇÃO DO GOWER (MONTAGEM DIRETA)
###############################################################################

tabela_validacao_gower <- data.frame(
  Metodo = c(
    "Gower + PAM (K=4)", 
    "Gower + Hierárquico (K=4)"
  ),
  
  Silhouette_Media = c(
    mean(as.matrix(silhouette(fit_pam$clustering, dist_gower))[, 3]),
    mean(as.matrix(silhouette(as.numeric(cluster_hcl), dist_gower))[, 3])
  ),
  
  Dunn_Index = c(
    stats_pam$dunn,
    stats_hie$dunn
  ),
  
  Pseudo_F = c(
    permanova_pam$F[1],
    permanova_hie$F[1]
  )
)

cat("\n--- PLACAR DE VALIDAÇÃO PARA O PROCESSO DE GOWER ---\n")
print(tabela_validacao_gower)
cat("-----------------------------------------------------\n")

###############################################################################
# 6. VERIFICAÇÃO DE HOMOGENEIDADE DOS GRUPOS
###############################################################################
cat("\n=====================================================\n")
cat("TAMANHO DOS GRUPOS NOS NOVOS MÉTODOS\n")
cat("=====================================================\n")

cat("\nTamanho dos Grupos - Gower + PAM:\n")
print(table(dados_modelo$Cluster_PAM_K4))

cat("\nTamanho dos Grupos - Gower + Hierárquico:\n")
print(table(dados_modelo$Cluster_Hie_K4))
cat("-----------------------------------------------------\n\n")

###############################################################################
# 7. SALVAR ARQUIVO COMPLETO DE COMPARAÇÃO E MÉTRICAS
###############################################################################

library(writexl)
library(tibble)

# 1. Definir o caminho da pasta Gower
pasta_gower <- "C:/Users/biel/Desktop/Gower"

# Garantir que a pasta exista
dir.create(pasta_gower, recursive = TRUE, showWarnings = FALSE)

# 2. Preparar os dados para exportação
#dados_exportacao <- dados_modelo %>% rownames_to_column("Code")

# 3. Exportar a comparação de modelos para .xlsx
caminho_comparacao <- file.path(pasta_gower, "Resultados_Comparacao_Modelos.xlsx")
#write_xlsx(dados_exportacao, path = caminho_comparacao)
write_xlsx(dados_modelo, path = caminho_comparacao)

# 4. Exportar a tabela de métricas de validação para .xlsx
caminho_metricas <- file.path(pasta_gower, "Metricas_Validacao_Gower.xlsx")
write_xlsx(tabela_validacao_gower, path = caminho_metricas)

cat("Arquivos 'Resultados_Comparacao_Modelos.xlsx' e 'Metricas_Validacao_Gower.xlsx' gerados em:", pasta_gower, "\n")


###############################################################################
# EXTRAÇÃO DE DADOS PARA O SCRIPT 05A (UNIVERSALIZAÇÃO DO GOWER)
###############################################################################

# 1. Transforma a matriz de Gower em dimensões numéricas puras (PCoA)
pcoa_gower <- cmdscale(dist_gower, k = 4)
colnames(pcoa_gower) <- paste0("Dim", 1:4)

# 2. Cria a tabela de dados que será copiada para o Passo 2 do Script 05A
dados_para_05A <- as.data.frame(pcoa_gower)
dados_para_05A <- tibble::rownames_to_column(dados_para_05A, "Code")

# 3. Cria a tabela de partição que será copiada para o Passo 3 do Script 05A
cluster_para_05A <- data.frame(
  Code = dados_modelo$Code,
  #Cluster = dados_modelo$Cluster_PAM_K4 
  # Se quiser testar o Hierárquico, mude para Cluster_Hie_K4
  Cluster = dados_modelo$Cluster_Hie_K4
)

# Exporta em arquivos de texto rápidos para você abrir e copiar para o Clipboard
write.table(dados_para_05A, "Gower_Dados_para_05A.txt", sep="\t", row.names=FALSE, quote=FALSE)
write.table(cluster_para_05A, "Gower_Cluster_para_05A.txt", sep="\t", row.names=FALSE, quote=FALSE)
getwd()

cat("\n=====================================================\n")
cat("Arquivos de exportação para o Script 05A gerados!\n")
cat("Abra o 'Gower_Dados_para_05A.txt' e use no Passo 2 do 05A.\n")
cat("Abra o 'Gower_Cluster_para_05A.txt' e use no Passo 3 do 05A.\n")
cat("=====================================================\n")
