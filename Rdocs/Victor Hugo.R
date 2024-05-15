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

##TERCEIRA ANÁLISE

# Filtrar apenas os top 3 terrenos mais frequentes
top_terrenos <- dados_filtrados %>%
  group_by(setting_terrain) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1:3) %>%
  pull(setting_terrain)

# Filtrar os dados apenas para os top 3 terrenos
dados_top3 <- dados_filtrados %>%
  filter(setting_terrain %in% top_terrenos)

# Calcular o número total de armadilhas colocadas em cada tipo de terreno dos top 3
total_armadilhas <- dados_top3 %>%
  group_by(setting_terrain) %>%
  summarise(total = n())

# Calcular o número de armadilhas que foram ativadas primeiro em cada tipo de terreno dos top 3
ativadas_primeiro <- dados_top3 %>%
  filter(trap_work_first) %>%
  group_by(setting_terrain) %>%
  summarise(ativadas = n())

# Combinar os dados em um único conjunto de dados
dados_plot <- merge(total_armadilhas, ativadas_primeiro, by = "setting_terrain", all.x = TRUE)

# Preencher valores NA com 0
dados_plot[is.na(dados_plot)] <- 0

# Calcular a frequência relativa das armadilhas ativadas primeiro e não ativadas primeiro
dados_plot$frequencia_ativadas <- dados_plot$ativadas / dados_plot$total
dados_plot$frequencia_nao_ativadas <- 1 - dados_plot$frequencia_ativadas

# Reorganizar os dados para o gráfico de barras empilhado
dados_plot_long <- dados_plot %>%
  pivot_longer(cols = c(frequencia_ativadas, frequencia_nao_ativadas),
               names_to = "Status da Ativação",
               values_to = "Frequencia")

# Calcular as frequências absolutas para os top 3 terrenos
frequencias <- dados_filtrados %>%
  filter(setting_terrain %in% top_terrenos) %>%
  count(setting_terrain, trap_work_first) %>%
  group_by(setting_terrain) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(proporcao = n / total,
         label_porcentagem = paste0(format(proporcao * 100, digits = 1), "%"),
         label_absoluto = n) 

# Plot
ggplot(frequencias) +
  aes(x = setting_terrain, y = proporcao, fill = trap_work_first) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(aes(label = paste(label_porcentagem, " (", label_absoluto, ")", sep = "")),
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  labs(x = "Tipo de Terreno", y = "Proporção de Armadilhas", fill = "Status da Ativação") +
  scale_fill_manual(values = c("#FF7F0E", "#1f77b4"), labels = c("Ativadas Primeiro", "Não Ativadas Primeiro")) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(filename = file.path(caminho_resultados, "colunas-uni-freq-emp-top3.pdf"), width = 158, height = 93, units = "mm")

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