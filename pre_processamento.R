###############################################################################
# PROJECT : Farm Typology Framework
# SCRIPT  : 02 - Variable Properties Assessment
# PURPOSE : Evaluate statistical properties of variables before preprocessing
###############################################################################


########################################
# 1. Install and load packages
########################################

packages <- c(
  "tidyverse",
  "moments",
  "ggplot2"
)

new_packages <- packages[
  !(packages %in% installed.packages()[,"Package"])
]

if(length(new_packages) > 0){
  install.packages(new_packages)
}

invisible(
  lapply(packages, library, character.only = TRUE)
)


########################################
# 2. Import dataset
########################################

dados <- read.delim("clipboard")
dados <- dados %>%
  # 1º Passo: Transformar as letras em números específicos
  mutate(
    gender = case_when(
      gender == "M" ~ 1,
      gender == "F" ~ 2,
      TRUE ~ NA_real_ # Se houver alguma célula vazia, ele mantém como NA
    )
  ) %>% # 
  # 2º Passo: Conversão em número
  mutate(
    across(
      -Code,
      ~as.numeric(.x)
    )
  )

str(dados)

########################################
# 3. Import variable classification
########################################

variable_classification <- read.delim("clipboard")


########################################
# 4. Remove identification variable
########################################

ID <- dados$Code

dados <- dados %>%
  select(-Code)



########################################
# 5. Check variables
########################################

missing_variables <- setdiff(
  variable_classification$Variable,
  names(dados)
)


if(length(missing_variables) > 0){
  
  stop(
    paste(
      "The following variables were not found:",
      paste(missing_variables, collapse=", ")
    )
  )
  
}



########################################
# 6. Create variable groups
########################################


continuous_vars <- variable_classification %>%
  filter(Statistical_Type=="Continuous") %>%
  pull(Variable)


count_vars <- variable_classification %>%
  filter(Statistical_Type=="Count") %>%
  pull(Variable)


binary_vars <- variable_classification %>%
  filter(Statistical_Type=="Binary") %>%
  pull(Variable)


ordinal_vars <- variable_classification %>%
  filter(Statistical_Type=="Ordinal") %>%
  pull(Variable)


nominal_vars <- variable_classification %>%
  filter(Statistical_Type=="Nominal") %>%
  pull(Variable)



quantitative_vars <- c(
  continuous_vars,
  count_vars
)


categorical_vars <- c(
  binary_vars,
  ordinal_vars,
  nominal_vars
)



########################################
# 7. Create datasets
########################################


quantitative_data <- dados %>%
  select(all_of(quantitative_vars))


categorical_data <- dados %>%
  select(all_of(categorical_vars))



###############################################################################
# PART A - QUANTITATIVE VARIABLES
###############################################################################


########################################
# 8. Quantitative properties
########################################

quantitative_data %>%
  summarise(
    across(
      everything(),
      ~class(.x)[1]
    )
  )

quantitative_summary <- data.frame()


for(v in names(quantitative_data)){
  
  
  x <- quantitative_data[[v]]
  
  
  # Outlier calculation (IQR)
  
  Q1 <- quantile(x,0.25,na.rm=TRUE)
  
  Q3 <- quantile(x,0.75,na.rm=TRUE)
  
  IQR_value <- Q3-Q1
  
  
  lower <- Q1 - 1.5*IQR_value
  
  upper <- Q3 + 1.5*IQR_value
  
  
  outliers <- sum(
    x < lower | x > upper,
    na.rm=TRUE
  )
  
  
  quantitative_summary <- rbind(
    
    quantitative_summary,
    
    data.frame(
      
      Variable = v,
      
      Mean = mean(x,na.rm=TRUE),
      
      Median = median(x,na.rm=TRUE),
      
      SD = sd(x,na.rm=TRUE),
      
      CV = (sd(x,na.rm=TRUE)/
              mean(x,na.rm=TRUE))*100,
      
      Minimum = min(x,na.rm=TRUE),
      
      Maximum = max(x,na.rm=TRUE),
      
      Skewness = moments::skewness(
        x,
        na.rm=TRUE
      ),
      
      Kurtosis = moments::kurtosis(
        x,
        na.rm=TRUE
      ),
      
      Zero_percentage =
        mean(x==0,na.rm=TRUE)*100,
      
      Missing_percentage =
        mean(is.na(x))*100,
      
      Number_outliers = outliers,
      
      Outlier_percentage =
        (outliers/length(x))*100
      
    )
    
  )
  
}



quantitative_summary

# Instale o pacote se ainda não tiver: 
install.packages("writexl")
library(writexl)

write_xlsx(
  quantitative_summary, 
  path = "C:/Users/biel/Desktop/Preprocessing/quantitative_summary.xlsx"
)




########################################
# 9. Quantitative graphs
########################################


library(ggplot2)

# 1. Definir o caminho da pasta e do arquivo PDF final
pasta_destino <- "C:/Users/biel/Desktop/Preprocessing"
caminho_pdf <- file.path(pasta_destino, "graficos_quantitativos.pdf")

# 2. Abrir o dispositivo PDF
pdf(caminho_pdf, width = 8, height = 6)

# 3. Loop para gerar e enviar cada gráfico para o PDF
for(v in names(quantitative_data)){
  
  # Gráfico 1: Histograma + Densidade
  p_hist <- ggplot(
    quantitative_data,
    aes(.data[[v]])
  ) +
    geom_histogram(
      bins = 30
    ) +
    geom_density(
      aes(y = after_stat(count))
    ) +
    labs(
      title = paste("Distribution:", v)
    )
  
  print(p_hist)
  
  # Gráfico 2: Boxplot
  p_box <- ggplot(
    quantitative_data,
    aes(y = .data[[v]])
  ) +
    geom_boxplot() +
    labs(
      title = paste("Extreme observations:", v)
    )
  
  print(p_box)
  
}

# 4. Fechar o dispositivo PDF (obrigatório para salvar o arquivo)
dev.off()


###############################################################################
# PART B - CATEGORICAL VARIABLES
###############################################################################


########################################
# 10. Categorical properties
########################################


categorical_summary <- data.frame()



for(v in names(categorical_data)){
  
  
  x <- categorical_data[[v]]
  
  
  freq <- table(x,useNA="ifany")
  
  
  prop <- prop.table(freq)*100
  
  
  rare_categories <-
    sum(prop < 5)
  
  
  missing_categories <-
    sum(is.na(x))
  
  
  categorical_summary <- rbind(
    
    categorical_summary,
    
    
    data.frame(
      
      Variable=v,
      
      Number_categories=
        length(freq),
      
      Most_frequent_category=
        names(freq)[which.max(freq)],
      
      
      Maximum_percentage=
        round(max(prop),2),
      
      
      Minimum_percentage=
        round(min(prop),2),
      
      
      Rare_categories=
        rare_categories,
      
      
      Missing_values=
        missing_categories
      
    )
    
  )
  
  
}



categorical_summary


library(writexl)

write_xlsx(
  categorical_summary, 
  path = "C:/Users/biel/Desktop/Preprocessing/categorical_summary.xlsx"
)



########################################
# 11. Frequency tables
########################################


categorical_frequency <- list()


for(v in names(categorical_data)){
  
  
  categorical_frequency[[v]] <-
    
    round(
      prop.table(
        table(categorical_data[[v]],
              useNA="ifany")
      )*100,
      2
    )
  
  
}


# Instale o pacote se não tiver: install.packages("openxlsx")
library(openxlsx)

# Converte cada elemento da lista para um data.frame formatado
frequencias_df_list <- lapply(names(categorical_frequency), function(v) {
  as.data.frame(categorical_frequency[[v]], responseName = "Percentage")
})
names(frequencias_df_list) <- names(categorical_frequency)

# Salva todas as abas no mesmo arquivo Excel
write.xlsx(
  frequencias_df_list, 
  file = "C:/Users/biel/Desktop/Preprocessing/categorical_frequencies.xlsx"
)





###############################################################################
# FINAL OBJECT
###############################################################################


variable_properties <- list(
  
  ID = ID,
  
  quantitative_summary =
    quantitative_summary,
  
  categorical_summary =
    categorical_summary,
  
  categorical_frequency =
    categorical_frequency
  
)



cat("\n")
cat("====================================\n")
cat("Variable properties assessment completed\n")
cat("====================================\n")


categorical_frequency
