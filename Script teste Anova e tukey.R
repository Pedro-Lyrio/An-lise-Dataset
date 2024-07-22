library(psych)
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(multcompView)
# Definir o caminho para o arquivo Excel
file_path <- "Dataset Grupo.xlsx"

# Ler a planilha do Excel
dados <- read_excel(file_path, sheet = 1)

# Verificar os primeiros dados para entender a estrutura
head(dados)

colunas_desejadas <- c('Tempo/Esforço', 'Survey/Dificuldade', 'Produção/Qualidade','Tipo')

# Filtrar o dataframe para incluir apenas as colunas desejadas
dados_filtrados <- dados%>% select(all_of(colunas_desejadas))

# Verificar os dados filtrados
head(dados_filtrados)

# Usar a função describe do pacote psych para fazer a análise descritiva
resultado_completo <- describe(dados_filtrados)

# Calcular a variância para cada coluna e adicionar ao resultado
variancias <- apply(dados_filtrados, 2, var, na.rm=TRUE)
resultado_completo$var <- variancias

# Selecionar apenas as colunas desejadas (max, min, mad, sd, var, vars)
resultado_selecionado <- resultado_completo %>%
  select(max, min, mad, sd, var)

# Exibir o resultado
print(resultado_selecionado)

colnames(resultado_selecionado) <- c("Max", "Min", "Mediana", "Desvio Padrão", "Variância")

# Transforme o dataframe em um formato adequado para o ggplot2
resultado_melted <- melt(resultado_selecionado)

# Crie o gráfico de barras
ggplot(resultado_melted, aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Estatísticas Descritivas",
       x = "Variável",
       y = "Valor") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Resumo da ANOVA tempo/esforço
modelo_anova <- aov(`Tempo/Esforço` ~ Tipo, data = dados, na.action = na.exclude)
anova_table <- summary(modelo_anova)
valor_p <- anova_table[[1]][1, "Pr(>F)"]

# Exibir o valor p
print(valor_p)

#Resumo da ANOVA 
modelo_anova2 <- aov(`Survey/Dificuldade` ~ Tipo, data = dados, na.action = na.exclude)
anova_table2 <- summary(modelo_anova2)
valor_p2 <- anova_table2[[1]][1, "Pr(>F)"]

# Exibir o valor p2
print(valor_p2)

#Resumo da ANOVA 
modelo_anova3 <- aov(`Produção/Qualidade` ~ Tipo, data = dados, na.action = na.exclude)
anova_table3 <- summary(modelo_anova3)
valor_p3 <- anova_table3[[1]][1, "Pr(>F)"]

# Exibir o valor p
print(valor_p3)

# Rodar o Teste de Tukey para 'Tempo/Esforço'
tukey_resultado1 <- TukeyHSD(modelo_anova)
print(tukey_resultado1)

# Rodar o Teste de Tukey para 'Survey/Dificuldade'
tukey_resultado2 <- TukeyHSD(modelo_anova2)
print(tukey_resultado2)

# Ajustar as margens para evitar o erro de margens muito grandes
par(mar = c(5, 5, 4, 4))  # margens: bottom, left, top, right

# Configurar os parâmetros gráficos para 1 linha e 2 colunas
par(mfrow = c(1, 2))

# Plotar o resultado do Teste de Tukey para 'Tempo/Esforço'
plot(tukey_resultado1, cex = 0.7)  # ajuste o tamanho do texto da legenda com cex
mtext("Teste de Tukey para Tempo/Esforço", side = 3, line = 1, cex = 0.8)

# Plotar o resultado do Teste de Tukey para 'Survey/Dificuldade'
plot(tukey_resultado2, cex = 0.7)  # ajuste o tamanho do texto da legenda com cex
mtext("Teste de Tukey para Survey/Dificuldade", side = 3, line = 1, cex = 0.8)


