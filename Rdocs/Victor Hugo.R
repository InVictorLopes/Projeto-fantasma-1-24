#Carregando banco de dados
library(readr)
dados <- read_csv("Banco/banco_final.csv")

#Instalando pacotes
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr)

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

# medidas trocar para a padronização
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

# Merge das estatísticas
med_stat <- merge(merge(merge(merge(mediana, media, by = "season"), iqr, by = "season"), min_max, by = "season"), desvio_padrao, by = "season")

# Converter para formato longo
med_stat_long <- med_stat %>%
  pivot_longer(cols = -season, names_to = "Estatistica", values_to = "Valor")

# Pivot_wider para trocar linhas e colunas
med_stat_wide <- med_stat_long %>%
  pivot_wider(names_from = season, values_from = Valor)

# Visualizar a tabela com linhas e colunas trocadas
med_stat_wide

#Análise 3


dados <- dados %>%
  filter(!is.na(setting_terrain) & !is.na(trap_work_first))

# Tradução dos tipos de terrenos
traducao_terrenos <- c(
  "Urban" = "Urbano",
  "Rural" = "Rural",
  "Forest" = "Floresta"
  # Adicione mais traduções conforme necessário
)

dados <- dados %>%
  mutate(setting_terrain = recode(setting_terrain, !!!traducao_terrenos))

# Traduzindo e ordenando a variável trap_work_first
dados <- dados %>%
  mutate(trap_work_first = ifelse(trap_work_first, "SIM", "NÃO"),
         trap_work_first = factor(trap_work_first, levels = c("SIM", "NÃO")))

# Análise dos três tipos de terrenos mais frequentes
terrenos_freq <- dados %>%
  group_by(setting_terrain) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  slice(1:3)

# Filtrar os dados para os três tipos de terrenos mais frequentes
dados_terrenos <- dados %>%
  filter(setting_terrain %in% terrenos_freq$setting_terrain)

# Calcular a frequência de ativação da armadilha nesses terrenos
dados_terrenos_agregados <- dados_terrenos %>%
  group_by(setting_terrain, trap_work_first) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round(freq / sum(freq) * 100, 1))

# Criar legendas com frequência e porcentagem
dados_terrenos_agregados <- dados_terrenos_agregados %>%
  mutate(legendas = str_c(freq, " (", freq_relativa, "%)"))

# Gráfico de barras
ggplot(dados_terrenos_agregados) +
  aes(x = fct_reorder(setting_terrain, freq, .desc = TRUE), y = freq, fill = trap_work_first, label = legendas) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.2, hjust = 0.5, size = 3) +
  labs(x = "Tipo de Terreno", y = "Frequência", fill = "Armadilha Funcionou de Primeira") +
  theme_estat()
ggsave(filename = file.path(caminho_resultados, "colunas-freq-armadilha-terreno.pdf"), width = 158, height = 93, units = "mm")

#Análise 4

dados <- dados %>%
  filter(!is.na(imdb) & !is.na(engagement))

# Gráfico de dispersão com linha de tendência
ggplot(dados) +
  aes(x = imdb, y = engagement) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Nota IMDB", 
    y = "Engajamento") +
  theme_estat()
ggsave(filename = file.path(caminho_resultados, "dispersao-notas-engajamento.pdf"), width = 158, height = 93, units = "mm")

#Análise 5





