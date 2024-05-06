#Carregando banco de dados
library(readr)
dados <- read_csv("Banco/banco_final.csv")

#Instalando pacotes
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate)

#Padronização cores Estat
estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091", 
  "#041835", "#666666" )

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}

#Criando e alterando Variável 
dados$Ano <- year(dados$date_aired)
dados$Decada <- 10 * floor(dados$Ano / 10)

dados$format <- ifelse(dados$format == "Movie", "Filmes", 
                       ifelse(dados$format == "Serie", "Séries", dados$format))
#Agrupamento e cálculo
dados_agregados <- dados %>%
  group_by(Decada, format) %>%
  summarise(Numero_lancamentos = n()) %>%
  group_by(Decada) %>%
  mutate(freq_relativa = Numero_lancamentos / sum(Numero_lancamentos) * 100)

dados_agregados <- dados_agregados %>%
  mutate(label_absoluta = paste(Numero_lancamentos),
         label_relativa = paste(round(freq_relativa, 1), "%"))

#Mudando diretório
caminho_resultados <- "C:\\Users\\victo\\Documents\\ESTAT\\Projeto-fantasma-1-24\\Resultado"

#Gráfico
# Gráfico de linhas
ggplot(dados_agregados, aes(x = as.factor(Decada), y = Numero_lancamentos, color = format)) +
  geom_line() +
  geom_point() +  # Adiciona pontos para destacar os valores
  labs(x = "Década", y = "Número de Lançamentos", color = "Formato") +
  theme_estat()


#salvando
ggsave(filename = file.path(caminho_resultados, "colunas-lancamentos-format.pdf"), width = 158, height = 93, units = "mm")

#Medidas resumo
medidas_absolutas <- dados_agregados %>%
  group_by(format) %>%
  summarise(
    media = mean(Numero_lancamentos),
    mediana = median(Numero_lancamentos),
    minimo = min(Numero_lancamentos),
    maximo = max(Numero_lancamentos),
    desvio_padrao = sd(Numero_lancamentos)
  ) %>%
  distinct()

print(medidas_absolutas)

