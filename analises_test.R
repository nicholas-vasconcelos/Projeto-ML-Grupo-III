
# Carregar pacotes necessários
library(tidyverse)
library(plotly)
library(knitr)


# 0. Processamento de Dados

# Carregar dados usando o parâmetro do arquivo CSV
clima <- read_csv(params$arquivo_csv)

# Renomear colunas e tratar valores faltantes (-999)
clima <- clima |>
  rename(
    data        = YYYYMMDD,
    temperatura = T2M,
    temp_max    = T2M_MAX,
    temp_min    = T2M_MIN,
    chuva       = PRECTOTCORR,
    umidade     = RH2M,
    radiacao    = ALLSKY_SFC_SW_DWN,
    vento       = WS2M
  ) |>
  mutate(across(c(temperatura, temp_max, temp_min, chuva,
                  umidade, radiacao, vento),
                ~ ifelse(.x == -999, NA, .x)))

# Constantes de Magnus e cálculos de VPD e Ponto de Orvalho
mag_b <- 17.27
mag_c <- 237.3

clima <- clima |>
  mutate(
    svp  = 0.6108 * exp((mag_b * temperatura) / (temperatura + mag_c)),
    avp  = svp * (umidade / 100),
    vpd  = svp - avp,
    gama          = log(umidade / 100) + (mag_b * temperatura) / (mag_c + temperatura),
    temp_orvalho  = (mag_c * gama) / (mag_b - gama),
    mes = factor(MM, levels = 1:12,
                 labels = c("Jan","Fev","Mar","Abr","Mai","Jun",
                            "Jul","Ago","Set","Out","Nov","Dez"))
  ) |>
  select(-svp, -avp, -gama)

# Variáveis numéricas e nomes
variaveis <- c("temperatura", "temp_max", "temp_min", "temp_orvalho",
               "chuva", "umidade", "radiacao", "vento", "vpd")

nomes <- c(temperatura  = "Temperatura média (°C)",
           temp_max     = "Temperatura máxima (°C)",
           temp_min     = "Temperatura mínima (°C)",
           temp_orvalho = "Ponto de orvalho (°C)",
           chuva        = "Chuva (mm/dia)",
           umidade      = "Umidade (%)",
           radiacao     = "Radiação (MJ/m²/dia)",
           vento        = "Vento a 2 m (m/s)",
           vpd          = "DPV (kPa)")

nomes_curtos <- c(temperatura  = "Temp. média",
                  temp_max     = "Temp. máx.",
                  temp_min     = "Temp. mín.",
                  temp_orvalho = "P. orvalho",
                  chuva        = "Chuva",
                  umidade      = "Umidade",
                  radiacao     = "Radiação",
                  vento        = "Vento",
                  vpd          = "DPV")

n_dias <- nrow(clima)

# Função de classificação da correlação
classifica <- function(r) {
  r <- abs(r)
  case_when(r < 0.20 ~ "Muito fraca",
            r < 0.40 ~ "Fraca",
            r < 0.70 ~ "Moderada",
            r < 0.90 ~ "Forte",
            TRUE     ~ "Muito forte")
}

# 1. Estatística Descritiva

moda <- function(x) {
  x  <- round(na.omit(x), 1)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

cv <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100

descritiva <- clima |>
  select(all_of(variaveis)) |>
  pivot_longer(everything(), names_to = "variavel", values_to = "valor") |>
  group_by(variavel) |>
  summarise(
    media      = mean(valor, na.rm = TRUE),
    mediana    = median(valor, na.rm = TRUE),
    moda       = moda(valor),
    minimo     = min(valor, na.rm = TRUE),
    maximo     = max(valor, na.rm = TRUE),
    amplitude  = maximo - minimo,
    variancia  = var(valor, na.rm = TRUE),
    desvio_pad = sd(valor, na.rm = TRUE),
    cv_pct     = cv(valor),
    q1         = quantile(valor, 0.25, na.rm = TRUE),
    q3         = quantile(valor, 0.75, na.rm = TRUE),
    iqr        = IQR(valor, na.rm = TRUE)
  ) |>
  arrange(match(variavel, variaveis)) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

kable(descritiva, caption = "Estatísticas Descritivas das Variáveis Climáticas")

# 2. Gráficos
# 2.1 Distribuição da Temperatura

media_temp <- mean(clima$temperatura, na.rm = TRUE)

g1 <- ggplot(clima, aes(temperatura)) +
  geom_histogram(binwidth = 1, fill = "tomato", color = "white") +
  geom_density(aes(y = after_stat(count)), linewidth = 1) +
  geom_vline(xintercept = media_temp, linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(breaks = seq(0, 34, 2)) +
  labs(title = "Distribuição da temperatura média diária",
       x = "Temperatura média diária (°C)",
       y = "Número de dias")

ggplotly(g1)

# 2.2 Classificação da Chuva

g2 <- clima |>
  mutate(classe = cut(chuva,
                      breaks = c(0, 1, 5, 25, 50, Inf),
                      labels = c("Quase sem chuva (< 1 mm)",
                                 "Fraca (1 a 5 mm)",
                                 "Moderada (5 a 25 mm)",
                                 "Forte (25 a 50 mm)",
                                 "Muito forte (> 50 mm)"),
                      right = FALSE, include.lowest = TRUE)) |>
  count(classe) |>
  mutate(pct = n / sum(n) * 100) |>
  ggplot(aes(classe, n, text = paste0(round(pct, 1), "%"))) +
  geom_col(fill = "steelblue") +
  labs(title = "Dias de cada tipo de chuva",
       x = NULL, y = "Número de dias") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(g2, tooltip = c("y", "text"))

# 2.3 Temperatura Média por Mês

g3 <- ggplot(clima, aes(mes, temperatura)) +
  geom_boxplot(fill = "orange", alpha = 0.6) +
  scale_y_continuous(breaks = seq(0, 34, 2)) +
  labs(title = "Temperatura média diária por mês",
       x = "Mês", y = "Temperatura (°C)")

ggplotly(g3)

# 2.4 Série Temporal (2020-2024)

clima_mensal <- clima |>
  mutate(ano_mes = floor_date(data, "month")) |>
  group_by(ano_mes) |>
  summarise(temperatura = mean(temperatura, na.rm = TRUE))

g4 <- ggplot() +
  geom_line(data = clima, aes(data, temperatura),
            color = "navyblue", linewidth = 0.3, alpha = 0.5) +
  geom_line(data = clima_mensal, aes(ano_mes + 14, temperatura),
            color = "red", linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 34, 2)) +
  labs(title = "Temperatura média diária e mensal",
       x = NULL, y = "Temperatura (°C)")

ggplotly(g4)

# 2.5 Radiação vs Temperatura

rho <- cor(clima$radiacao, clima$temperatura,
           method = "spearman", use = "complete.obs")

g5 <- ggplot(clima, aes(radiacao, temperatura)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red") +
  scale_y_continuous(breaks = seq(0, 34, 2)) +
  labs(title = paste0("Radiação solar x Temperatura (Rho = ", round(rho, 2), ")"),
       x = "Radiação solar (MJ/m²/dia)",
       y = "Temperatura média (°C)")

ggplotly(g5)

# 2.6 Temperaturas Máxima, Média e Mínima

g6 <- clima |>
  mutate(ano_mes = floor_date(data, "month")) |>
  group_by(ano_mes) |>
  summarise(Máxima = mean(temp_max, na.rm = TRUE),
            Média  = mean(temperatura, na.rm = TRUE),
            Mínima = mean(temp_min, na.rm = TRUE)) |>
  pivot_longer(-ano_mes, names_to = "tipo", values_to = "temp") |>
  mutate(tipo = factor(tipo, levels = c("Máxima", "Média", "Mínima"))) |>
  ggplot(aes(ano_mes + 14, temp, color = tipo)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("Máxima" = "tomato",
                                "Média"  = "grey30",
                                "Mínima" = "steelblue"),
                     name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Temperaturas máxima, média e mínima mensais",
       x = NULL, y = "Temperatura (°C)")

ggplotly(g6)

# 3. Teste de Normalidade

normalidade <- clima |>
  select(all_of(variaveis)) |>
  pivot_longer(everything(), names_to = "variavel", values_to = "valor") |>
  group_by(variavel) |>
  summarise(W = unname(shapiro.test(valor)$statistic),
            p = shapiro.test(valor)$p.value,
            .groups = "drop") |>
  arrange(match(variavel, variaveis)) |>
  mutate(
    Variável  = unname(nomes[variavel]),
    W         = round(W, 3),
    `p-valor` = ifelse(p < 0.001, "< 0,001",
                       format(round(p, 3), decimal.mark = ",")),
    Decisão   = ifelse(p >= 0.05, "Normal (não rejeita H0)",
                       "Não normal (rejeita H0)")
  ) |>
  select(Variável, W, `p-valor`, Decisão)

kable(normalidade, caption = "Teste de Shapiro-Wilk para Normalidade")

# Q-Q plot é mantido estático pois ggplotly pode ter problemas de renderização com stat_qq facetado
clima |>
  select(all_of(variaveis)) |>
  pivot_longer(everything(), names_to = "variavel", values_to = "valor") |>
  mutate(variavel = factor(unname(nomes[variavel]), levels = unname(nomes))) |>
  ggplot(aes(sample = valor)) +
  stat_qq(alpha = 0.4) +
  stat_qq_line(color = "red", linewidth = 0.8) +
  facet_wrap(~ variavel, scales = "free_y") +
  labs(title = "Q-Q Plots: Verificação Visual de Normalidade",
       x = "Quantis teóricos (normal)", y = "Quantis observados")

# 4. Teste de Correlação

matriz <- cor(clima[variaveis], method = "spearman", use = "complete.obs")

pares <- as.data.frame(t(combn(variaveis, 2)))
names(pares) <- c("v1", "v2")

correlacoes <- pares |>
  mutate(
    teste = map2(v1, v2, ~ cor.test(clima[[.x]], clima[[.y]],
                                    method = "spearman", exact = FALSE)),
    rho   = map_dbl(teste, ~ unname(.x$estimate)),
    p     = map_dbl(teste, ~ .x$p.value)
  ) |>
  transmute(
    Par       = paste(unname(nomes_curtos[v1]), "×", unname(nomes_curtos[v2])),
    rho       = round(rho, 2),
    `p-valor` = ifelse(p < 0.001, "< 0,001",
                       format(round(p, 3), decimal.mark = ",")),
    Força     = classifica(rho),
    Sentido   = ifelse(rho > 0, "Positiva", "Negativa"),
    Decisão   = ifelse(p < 0.05, "Significativa", "Sem evidência")
  )

kable(correlacoes, caption = "Testes de Correlação de Spearman entre os Pares")

# 5. Mapa de Calor da Correlação

g_calor <- matriz |>
  as.data.frame() |>
  rownames_to_column("v1") |>
  pivot_longer(-v1, names_to = "v2", values_to = "rho") |>
  mutate(v1 = factor(unname(nomes_curtos[v1]), levels = unname(nomes_curtos)),
         v2 = factor(unname(nomes_curtos[v2]), levels = unname(nomes_curtos))) |>
  ggplot(aes(v1, v2, fill = rho, text = paste("Rho:", round(rho, 2)))) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "tomato",
                       limits = c(-1, 1), name = "Correlação") +
  labs(title = "Matriz de Correlação de Spearman",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(g_calor, tooltip = "text")