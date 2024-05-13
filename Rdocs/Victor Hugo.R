#Carregando banco de dados
library(readr)
dados <- read_csv("Banco/banco_final.csv")

#Instalando pacotes
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate)

#Padronização cores Estat
cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")

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
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
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
  group_by(Decada)

#Mudando diretório
caminho_resultados <- "C:\\Users\\victo\\Documents\\ESTAT\\Projeto-fantasma-1-24\\Resultado"

#Gráfico de linhas 
ggplot(dados_agregados) +
  aes(x = Decada, y = Numero_lancamentos, group = format, color = format) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Década", y = "Número de Lançamentos", color = "Formato") +
  theme_estat()
ggsave(filename = file.path(caminho_resultados, "linhas-lancamentos-format.pdf"), width = 158, height = 93, units = "mm")

#Medidas para tabela do grafico de linhas
dados_agregados_wide <- dados_agregados %>%
  pivot_wider(names_from = format, values_from = Numero_lancamentos)

print(dados_agregados_wide)

##SEGUNDA ANÁLISE

#Alterando a variável season
dados$season <- ifelse(dados$season == "Movie", "Filmes", 
                       ifelse(dados$season == "Special", "Especial", dados$season))


#Separando as temporadas
dados_filtrados <- dados %>%
  filter(season %in% c(1, 2, 3, 4))

# Gráfico boxplot
ggplot(dados_filtrados) +
  aes(x = reorder(season, imdb), y = imdb) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Temporada", y = "Nota IMDB") +
  theme_estat()
ggsave(filename = file.path(caminho_resultados, "boxplot-notaimbd-temporada.pdf"), width = 158, height = 93, units = "mm")

#medidas trocar para a padronização
mediana <- dados_filtrados %>%
  group_by(season) %>%
  summarise(mediana = median(imdb))
media <- dados_filtrados %>%
  group_by(season) %>%
  summarise(media = mean(imdb))
iqr <- dados_filtrados %>%
  group_by(season) %>%
  summarise(IQR = IQR(imdb))
min_max <- dados_filtrados %>%
  group_by(season) %>%
  summarise(minimo = min(imdb),
            maximo = max(imdb))
desvio_padrao <- dados_filtrados %>%
  group_by(season) %>%
  summarise(desvio_padrao = sd(imdb))

med_stat <- merge(merge(merge(merge(mediana, media, by = "season"), iqr, by = "season"), min_max, by = "season"), desvio_padrao, by = "season")
med_stat
med_stat_long <- pivot_longer(med_stat, -season, names_to = "Estatistica", values_to = "Valor")

##TERCEIRA ANÁLISE

top_terrenos <- dados_filtrados %>%
  group_by(setting_terrain) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1:3) %>%
  pull(setting_terrain)

dados_top3 <- dados_filtrados %>%
  filter(setting_terrain %in% top_terrenos)

dados_classes_top3 <- dados_top3 %>%
  count(setting_terrain) %>%
  mutate(
    freq = n,
    relative_freq = round((freq / sum(freq)) * 100, 1),
    freq = gsub("\\.", ",", relative_freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(dados_classes_top3) +
  aes(x = fct_reorder(setting_terrain, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) +
  labs(x = "Tipo de Terreno", y = "Frequência") +
  scale_x_discrete(labels = c("Floresta", "Rural", "Urbano")) +  
  theme_estat()
ggsave(filename = file.path(caminho_resultados, "colunas-uni-freq-top3.pdf"), width = 158, height = 93, units = "mm")

library(psych)

# Estatísticas sumárias para a Floresta
summary(dados_filtrados$trap_work_first[dados_filtrados$setting_terrain == "Floresta"])

# Estatísticas sumárias para o Terreno Rural
summary(dados_filtrados$trap_work_first[dados_filtrados$setting_terrain == "Rural"])

# Estatísticas sumárias para o Terreno Urbano
summary(dados_filtrados$trap_work_first[dados_filtrados$setting_terrain == "Urbano"])

# Ou usando describe() para estatísticas mais detalhadas
describe(dados_filtrados$trap_work_first[dados_filtrados$setting_terrain == "Floresta"])
describe(dados_filtrados$trap_work_first[dados_filtrados$setting_terrain == "Rural"])
describe(dados_filtrados$trap_work_first[dados_filtrados$setting_terrain == "Urbano"])


