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

# Calculando as estatísticas
media_imdb <- mean(dados$imdb)
media_engagement <- mean(dados$engagement)

mediana_imdb <- median(dados$imdb)
mediana_engagement <- median(dados$engagement)

desvio_padrao_imdb <- sd(dados$imdb)
desvio_padrao_engagement <- sd(dados$engagement)

correlacao <- cor(dados$imdb, dados$engagement)

# Exibindo os resultados
cat("Média IMDB:", media_imdb, "\n")
cat("Média Engajamento:", media_engagement, "\n")
cat("Mediana IMDB:", mediana_imdb, "\n")
cat("Mediana Engajamento:", mediana_engagement, "\n")
cat("Desvio Padrão IMDB:", desvio_padrao_imdb, "\n")
cat("Desvio Padrão Engajamento:", desvio_padrao_engagement, "\n")
cat("Coeficiente de Correlação:", correlacao, "\n")

#Média IMDB: 5.130522 
#Média Engajamento: 178.1177 
#Mediana IMDB: 5.2 
#Mediana Engajamento: 177.56 
#Desvio Padrão IMDB: 0.9689286 
#Desvio Padrão Engajamento: 26.15046 
#Coeficiente de Correlação: 0.9184371 

# Gráfico de dispersão
ggplot(dados) +
  aes(x = imdb, y = engagement) +
  geom_point(colour = "#A11D21", size = 3) +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(
    x = "Nota IMDB", 
    y = "Engajamento") +
  theme_estat()
ggsave(filename = file.path(caminho_resultados, "dispersao-notas-engajamento.pdf"), width = 158, height = 93, units = "mm")

#Análise 5

# Adicionando as traduções para as colunas faltantes
traducoes <- c(
  "caught_fred" = "Fred_capturou",
  "caught_daphnie" = "Daphnie_capturou",
  "caught_velma" = "Velma_capturou",
  "caught_shaggy" = "Salsicha_capturou",
  "caught_scooby" = "Scooby_capturou",
  "caught_other" = "Outro_capturou",
  "caught_not" = "Nao_capturado"
)

# Alterar os nomes das colunas
dados <- dados %>%
  rename_with(~ traducoes[.x], starts_with("caught_"))

# Mapear as variáveis caught_* para SIM ou NÃO
dados$Fred_capturou <- ifelse(dados$Fred_capturou == TRUE, "SIM", "NÃO")
dados$Daphnie_capturou <- ifelse(dados$Daphnie_capturou == TRUE, "SIM", "NÃO")
dados$Velma_capturou <- ifelse(dados$Velma_capturou == TRUE, "SIM", "NÃO")
dados$Salsicha_capturou <- ifelse(dados$Salsicha_capturou == TRUE, "SIM", "NÃO")
dados$Scooby_capturou <- ifelse(dados$Scooby_capturou == TRUE, "SIM", "NÃO")
dados$Outro_capturou <- ifelse(dados$Outro_capturou == TRUE, "SIM", "NÃO") 


dados_analise_engajamento <- dados %>%
  select(engagement, Fred_capturou, Daphnie_capturou, Velma_capturou, Salsicha_capturou, Scooby_capturou, Outro_capturou)

dados_analise_engajamento <- dados_analise_engajamento %>%
  mutate(across(
    c("Fred_capturou", "Daphnie_capturou", "Velma_capturou", "Salsicha_capturou", "Scooby_capturou", "Outro_capturou"),
    ~ fct_relevel(., "SIM", "NÃO")
  ))
# Criando um vetor apenas com os nomes traduzidos dos personagens
traducoes_apenas_personagens <- c(
  "Fred_capturou" = "Fred",
  "Daphnie_capturou" = "Daphnie",
  "Velma_capturou" = "Velma",
  "Salsicha_capturou" = "Salsicha",
  "Scooby_capturou" = "Scooby",
  "Outro_capturou" = "Outro"
)

# Renomeando as variáveis do banco de dados usando os nomes traduzidos dos personagens
dados_analise_engajamento <- dados_analise_engajamento %>%
  rename_with(~ traducoes_apenas_personagens[.x], matches("^Fred_capturou$|^Daphnie_capturou$|^Velma_capturou$|^Salsicha_capturou$|^Scooby_capturou$|^Outro_capturou$"))

# Gráfico de colunas bivariado
grafico_colunas <- dados_analise_engajamento %>%
  pivot_longer(cols = -engagement, names_to = "Capturou") %>%
  filter(!is.na(value)) %>%
  group_by(Capturou, value) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq / sum(freq),
         label = paste0(freq, " (", scales::percent(freq_relativa), ")"))

ggplot(grafico_colunas) +
  aes(
    x = fct_reorder(Capturou, freq, .desc = TRUE),
    y = freq,
    fill = value,
    label = label
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = -0.2,
    hjust = 0.5,
    size = 3
  ) +
  scale_fill_manual(values = c("SIM" = "#003366", "NÃO" = "#A11D21"),
                    name = "Capturou") + # Definindo a legenda
  labs(x = "Personagem", y = "Frequência de Engajamento", fill = "Capturou") +
  theme_estat()

# Salvando o gráfico
ggsave(filename = file.path(caminho_resultados, "colunas-bi-freq.pdf"), width = 158, height = 93, units = "mm")


