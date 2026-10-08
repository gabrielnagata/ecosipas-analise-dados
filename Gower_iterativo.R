###############################################################################
# SCRIPT COMPLETO: DISTÂNCIA DE GOWER COM IDENTIFICAÇÃO DE K IDEAL
# FAMD+KMEANS vs GOWER+PAM vs GOWER+HIERÁRQUICO (PADRÃO BRASILEIRO)
###############################################################################

# 1. CARREGAR PACOTES CHAVE
install.packages("fpc")
packages <- c("tidyverse", "cluster", "factoextra", "fpc")
invisible(lapply(packages, library, character.only = TRUE))

cat("=====================================================\n")
cat("PASSO 1: IMPORTAÇÃO E MONTAGEM DO DICIONÁRIO\n")
cat("=====================================================\n\n")

# --- LEITURA DO DICIONÁRIO DE VARIÁVEIS DO CLIPBOARD ---
cat("Por favor, garanta que a tabela de variáveis está copiada no Excel...\n")
meta_variaveis <- read.table("clipboard", header = TRUE, sep = "\t", dec = ".")

# Padronização e limpeza dos metadados
colnames(meta_variaveis) <- c("Variable", "Statistical_type")
meta_variaveis$Variable <- trimws(meta_variaveis$Variable)
meta_variaveis$Statistical_type <- trimws(tolower(meta_variaveis$Statistical_type))

print(meta_variaveis)
cat("\n-----------------------------------------------------\n")

# Pausa estratégica para você copiar o segundo bloco de dados
readline(prompt="Se o dicionário acima estiver correto, copie o seu dataset transformado no Excel e aperte [Enter] para continuar...")

cat("\n=====================================================\n")
cat("PASSO 2: IMPORTAÇÃO E TRATAMENTO DO DATASET\n")
cat("=====================================================\n\n")

# --- LEITURA DO DATASET TRANSFORMADO DE AMOSTRAS DO CLIPBOARD ---
dados_originais <- read.table("clipboard", header = TRUE, sep = "\t", dec = ".")
dados_originais <- as.data.frame(dados_originais)

###AQUI colocar o dataset transformado COM a coluna CODE inserido MANUALMENTE
dados_modelo <- read.table("clipboard", header = TRUE, sep = "\t", dec = ".")

# Forçar a tipagem correta de cada coluna baseando-se estritamente no dicionário
for(i in 1:nrow(meta_variaveis)) {
  var_nome <- meta_variaveis$Variable[i]
  var_tipo <- meta_variaveis$Statistical_type[i]
  
  if(var_nome %in% names(dados_originais)) {
    if(var_tipo == "quantitative") {
      dados_originais[[var_nome]] <- as.numeric(as.character(dados_originais[[var_nome]]))
    } else if(var_tipo %in% c("categorical", "qualitative")) {
      dados_originais[[var_nome]] <- as.factor(as.character(dados_originais[[var_nome]]))
    }
  }
}

###############################################################################
# PASSO 3: CÁLCULO DA MATRIZ DE DISTÂNCIA DE GOWER
###############################################################################
cat("\nCalculando a matriz de distância de Gower...\n")
dist_gower <- daisy(dados_originais, metric = "gower")

###############################################################################
# PASSO 4: ANÁLISE DE K IDEAL (ANÁLISE ITERATIVA DE 2 A 10 CLUSTERS)
###############################################################################
cat("\nAnalisando a estrutura das silhuetas de K=2 até K=10...\n")

sil_pam_vetor <- numeric(10)
sil_hie_vetor <- numeric(10)

# Pré-calcula a árvore hierárquica base para otimizar o loop
fit_hierarquico_base <- hclust(dist_gower, method = "ward.D2")

for(k_teste in 2:10) {
  # Teste para o PAM
  fit_pam_teste <- pam(dist_gower, k = k_teste, diss = TRUE)
  sil_pam_teste <- silhouette(fit_pam_teste$clustering, dist_gower)
  sil_pam_vetor[k_teste] <- mean(as.matrix(sil_pam_teste)[, 3])
  
  # Teste para o Hierárquico
  cluster_hcl_teste <- cutree(fit_hierarquico_base, k = k_teste)
  sil_hie_teste <- silhouette(cluster_hcl_teste, dist_gower)
  sil_hie_vetor[k_teste] <- mean(as.matrix(sil_hie_teste)[, 3])
}

# Estruturando dados para o gráfico de linhas
df_k_ideal <- data.frame(
  K = 2:10,
  PAM = sil_pam_vetor[2:10],
  Hierarquico = sil_hie_vetor[2:10]
) %>% 
  pivot_longer(cols = c(PAM, Hierarquico), names_to = "Metodo", values_to = "Silhouette")

# Exibe o gráfico de validação na tela
g_k_ideal <- ggplot(df_k_ideal, aes(x = K, y = Silhouette, group = Metodo, color = Metodo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2:10) +
  labs(
    title = "Largura Média da Silhueta (ASW) por Número de Clusters (K)",
    subtitle = "Análise baseada puramente na Matriz de Distância de Gower",
    x = "Número de Clusters (K)",
    y = "Silhouette Médio Global",
    color = "Método"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(size = 12))

print(g_k_ideal)

cat("\n=====================================================\n")
cat("   ESPAÇO PARA ADAPTAÇÃO E DEFINIÇÃO DO K FINAL      \n")
cat("=====================================================\n")
cat("Analise o gráfico gerado na tela para verificar o pico de estabilidade.\n")

# Entrada interativa no console com loop de insistência
k_selecionado <- NA

# Enquanto o K for "NA" (vazio/letra) ou menor que 2, o R vai ficar repetindo a pergunta
while(is.na(k_selecionado) || k_selecionado < 2) {
  
  # suppressWarnings evita mensagens vermelhas caso você digite uma letra sem querer
  k_selecionado <- suppressWarnings(as.integer(readline(prompt="Digite o valor de K escolhido para os modelos finais (ex: 5): ")))
  
  # Se o valor ainda for inválido, ele exibe um aviso antes de perguntar de novo
  if(is.na(k_selecionado) || k_selecionado < 2) {
    cat("⚠️ Valor inválido! Você precisa digitar um número inteiro maior ou igual a 2.\n\n")
  }
}

cat("K definido com sucesso! O script seguirá com K =", k_selecionado, "\n")

cat("\nExecutando modelos finais com K =", k_selecionado, "...\n")
