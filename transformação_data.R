###########################################################
# Farm Typology Protocol
# Script 03 - Data Transformation
#
# Purpose:
# Apply predefined mathematical transformations to
# quantitative variables and evaluate their effects on
# statistical properties before cluster analysis.
###########################################################

###########################################################
# 1. Install and load packages
###########################################################

packages <- c(
  "tidyverse",
  "moments"
)

installed <- packages %in% installed.packages()[,1]

if(any(!installed)){
  install.packages(packages[!installed])
}

lapply(packages, library, character.only = TRUE)

###########################################################
# 2. Data verification
str(dados)

###########################################################
# 3. Import transformation table
###########################################################
# Required columns:
#
# Variable
# Transformation
#
# Example:
#
# total_area      log
# v_credit        log1p
# age             none
#
###########################################################

transformation_table <- read.delim("clipboard")

###########################################################
# 4. Store original data
###########################################################

dados_original <- dados

###########################################################
# 5. Apply transformations
###########################################################

for(i in 1:nrow(transformation_table)){
  
  variable <- transformation_table$Variable[i]
  
  method <- transformation_table$Transformation[i]
  
  if(method == "log"){
    
    dados[[variable]] <- log(
      dados[[variable]]
    )
    
  }
  
  if(method == "log1p"){
    
    dados[[variable]] <- log1p(
      dados[[variable]]
    )
    
  }
  
  if(method == "sqrt"){
    
    dados[[variable]] <- sqrt(
      dados[[variable]]
    )
    
  }
  
  if(method == "cuberoot"){
    
    dados[[variable]] <- sign(
      dados[[variable]]
    ) *
      abs(
        dados[[variable]]
      )^(1/3)
    
  }
  
  if(method == "none"){
    
    next
    
  }
  
}


###########################################################
# 6. Compare statistical properties
###########################################################

transformation_summary <- data.frame()

for(i in 1:nrow(transformation_table)){
  
  variable <- transformation_table$Variable[i]
  
  before <- dados_original[[variable]]
  
  after <- dados[[variable]]
  
  transformation_summary <- rbind(
    
    transformation_summary,
    
    data.frame(
      
      Variable = variable,
      
      Transformation =
        transformation_table$Transformation[i],
      
      Skewness_before =
        moments::skewness(before, na.rm = TRUE),
      
      Skewness_after =
        moments::skewness(after, na.rm = TRUE),
      
      Kurtosis_before =
        moments::kurtosis(before, na.rm = TRUE),
      
      Kurtosis_after =
        moments::kurtosis(after, na.rm = TRUE)
      
    )
    
  )
  
}

transformation_summary


# Instale o pacote se ainda não tiver: install.packages("writexl")
library(writexl)

write_xlsx(
  transformation_summary, 
  path = "C:/Users/biel/Desktop/Transformation/transformation_summary.xlsx"
)



# 7. Histograms before and after
###########################################################

# 1. Definir o caminho da pasta e do arquivo PDF final
pasta_destino <- "C:/Users/biel/Desktop/Transformation"
caminho_pdf <- file.path(pasta_destino, "histogramas_antes_depois.pdf")

# Garantir que a pasta existe
dir.create(pasta_destino, recursive = TRUE, showWarnings = FALSE)

# 2. Abrir o dispositivo PDF (ajustando largura e altura para caberem 2 gráficos lado a lado)
pdf(caminho_pdf, width = 10, height = 5)

# 3. Loop para gerar os histogramas
for(i in 1:nrow(transformation_table)){
  
  variable <- transformation_table$Variable[i]
  
  # Pula variáveis que não sofreram transformação
  if(transformation_table$Transformation[i] == "none"){
    next
  }
  
  # Configura a página do PDF para ter 1 linha e 2 colunas
  par(mfrow = c(1, 2))
  
  # Histograma Antes
  hist(
    dados_original[[variable]],
    main = paste(variable, "- Before"),
    xlab = variable,
    col = "skyblue",
    border = "white"
  )
  
  # Histograma Depois
  hist(
    dados[[variable]],
    main = paste(variable, "- After"),
    xlab = variable,
    col = "lightgreen",
    border = "white"
  )
  
}

# 4. Fechar e salvar o arquivo PDF
dev.off()

###########################################################
# 8. Export transformed data
###########################################################

library(writexl)

# 1. Definir a pasta de destino
pasta_destino <- "C:/Users/biel/Desktop/Transformation"

# Garantir que a pasta exista
dir.create(pasta_destino, recursive = TRUE, showWarnings = FALSE)

# 2. Exportar o banco de dados transformado para .xlsx
write_xlsx(
  dados,
  path = file.path(pasta_destino, "transformed_dataset.xlsx")
)

# 3. Exportar o resumo das transformações para .xlsx
write_xlsx(
  transformation_summary,
  path = file.path(pasta_destino, "transformation_summary.xlsx")
)



