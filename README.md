# Tipologia de Propriedades Rurais — Análise de Agrupamento (Clustering)

Conjunto de scripts em R desenvolvidos no âmbito da pesquisa de Iniciação Científica (PIBIC/FAPEMIG), no pacote de importância socioeconômica do consórcio internacional **EcoSiPaS**, com foco na construção de tipologias de propriedades rurais no Cerrado Mineiro a partir de dados produtivos, sociais e ambientais.

O pipeline realiza o pré-processamento, transformação e agrupamento (clustering) de dados mistos (quantitativos e categóricos) via **distância de Gower**, seguido da validação estatística e interpretação dos grupos (tipologias) obtidos.

## Pipeline / Ordem de execução

Os scripts foram desenvolvidos para rodar em sequência, dentro do fluxo de trabalho da pesquisa:

| Ordem | Script | Função |
|---|---|---|
| 1 | `pre_processamento.R` | Avalia propriedades estatísticas das variáveis (distribuição, outliers, frequências) antes da modelagem |
| 2 | `transformacao_data.R` | Aplica transformações matemáticas (log, log1p, sqrt, raiz cúbica) às variáveis quantitativas e compara assimetria/curtose antes e depois |
| 3 | `Gower_iterativo.R` | Calcula a matriz de distância de Gower e testa o número ideal de clusters (K=2 a 10) via silhueta, para PAM e Hierárquico |
| 4 | `Gower.R` | Ajusta os modelos finais (PAM e Hierárquico) com o K escolhido, compara as duas abordagens (silhueta, índice Dunn, Pseudo-F) e identifica as propriedades medoides de cada grupo |
| 5 | `Analise_agrupamento.R` | Valida a qualidade do agrupamento (silhueta, Davies-Bouldin, Calinski-Harabasz, Dunn) e realiza testes estatísticos de diferença entre clusters (ANOVA/Kruskal-Wallis, post-hoc) |
| 6 | `Analise_Variavel.R` | Interpreta e caracteriza o perfil de cada tipologia: testes por variável, análise multivariada global (PERMANOVA, ANOSIM, Betadisper) e análise de resíduos para variáveis categóricas |

> **Nota:** `Gower_iterativo.R` e `Gower.R` foram escritos para rodar na mesma sessão do R — o segundo depende de objetos criados pelo primeiro (`dist_gower`, `dados_modelo`).

## Pacotes utilizados

`tidyverse`, `cluster`, `clusterCrit`, `fpc`, `factoextra`, `dplyr`, `vegan`, `car`, `FSA`, `moments`, `openxlsx`, `writexl`, `ggplot2`, `tibble`

## Observações técnicas

Scripts desenvolvidos para fins de análise exploratória no contexto da pesquisa; funcionais, mas não otimizados para produção. Alguns pontos a considerar antes de reutilizar.

## Contexto

Projeto: **EcoSiPaS** (consórcio internacional de pesquisa socioambiental)
Financiamento: PIBIC/FAPEMIG
Autor: Gabriel Nagata
