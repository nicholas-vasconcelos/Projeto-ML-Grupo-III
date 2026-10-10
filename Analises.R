# =============================================================
# Projeto NASA POWER - Clima do Paraná (2020-2024)
# Estatística descritiva, gráficos, normalidade e correlação
# =============================================================

# ---- 0. Pacotes e dados -------------------------------------
library(tidyverse)
clima <- read_csv("clima_parana.csv")

# Renomear colunas para nomes mais fáceis
clima <- clima |>
  rename(
    data        = YYYYMMDD,
    temperatura = T2M,                # °C
    temp_max    = T2M_MAX,            # °C
    temp_min    = T2M_MIN,            # °C
    chuva       = PRECTOTCORR,        # mm/dia
    umidade     = RH2M,               # %
    radiacao    = ALLSKY_SFC_SW_DWN,  # MJ/m²/dia
    vento       = WS2M                # m/s (a 2 m de altura)
  )

# Conferir valores faltantes (NASA às vezes usa -999)
# Isso é feito ANTES dos cálculos abaixo, para que um -999 não
# contamine o VPD e o ponto de orvalho.
colSums(is.na(clima))
clima <- clima |>
  mutate(across(c(temperatura, temp_max, temp_min, chuva,
                  umidade, radiacao, vento),
                ~ ifelse(.x == -999, NA, .x)))

# Variáveis calculadas a partir de temperatura (T) e umidade relativa (UR)
# Constantes de Magnus (as mesmas para o VPD e o ponto de orvalho)
mag_b <- 17.27
mag_c <- 237.3

clima <- clima |>
  mutate(
    # VPD (kPa)
    svp  = 0.6108 * exp((mag_b * temperatura) / (temperatura + mag_c)),
    avp  = svp * (umidade / 100),
    vpd  = svp - avp,
    # Ponto de orvalho (°C) - fórmula de Magnus
    gama          = log(umidade / 100) + (mag_b * temperatura) / (mag_c + temperatura),
    temp_orvalho  = (mag_c * gama) / (mag_b - gama),
    # Coluna de mês
    mes = factor(MM, levels = 1:12,
                 labels = c("Jan","Fev","Mar","Abr","Mai","Jun",
                            "Jul","Ago","Set","Out","Nov","Dez"))
  ) |>
  # Remover colunas intermediárias
  select(-svp, -avp, -gama)

# Todas as variáveis numéricas analisadas
variaveis <- c("temperatura", "temp_max", "temp_min", "temp_orvalho",
               "chuva", "umidade", "radiacao", "vento", "vpd")

colSums(is.na(clima[variaveis]))

# Nomes completos, usados nas tabelas e nos gráficos
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

# Classificação da força da correlação (|rho|)
classifica <- function(r) {
  r <- abs(r)
  case_when(r < 0.20 ~ "Muito fraca",
            r < 0.40 ~ "Fraca",
            r < 0.70 ~ "Moderada",
            r < 0.90 ~ "Forte",
            TRUE     ~ "Muito forte")
}

# ---- 1. ESTATÍSTICA DESCRITIVA ------------------------------

# O R não tem função de moda, então criamos uma.
# Como os dados são contínuos, arredondamos antes (1 casa decimal,
# pois variáveis como vento e DPV têm valores pequenos).
moda <- function(x) {
  x  <- round(na.omit(x), 1)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Coeficiente de variação (%)
cv <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100

descritiva <- clima |>
  select(all_of(variaveis)) |>
  pivot_longer(everything(), names_to = "variavel", values_to = "valor") |>
  group_by(variavel) |>
  summarise(
    # Tendência central
    media      = mean(valor, na.rm = TRUE),
    mediana    = median(valor, na.rm = TRUE),
    moda       = moda(valor),
    # Dispersão
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
print(descritiva, width = Inf)
View(descritiva)
# Salvar a tabela (útil para colocar no relatório)
write_csv(descritiva, "tabela_descritiva.csv")

# ---- 2. GRÁFICOS --------------------------------------------
# 2.1 Histograma da temperatura com curva de densidade
media_temp <- mean(clima$temperatura, na.rm = TRUE)

ggplot(clima, aes(temperatura)) +
  geom_histogram(binwidth = 1, fill = "tomato", color = "white") +
  geom_density(aes(y = after_stat(count)), linewidth = 1) +
  geom_vline(xintercept = media_temp, linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = media_temp + 0.3, y = Inf,
           label = paste0("Média = ", round(media_temp, 1), " °C"),
           hjust = 0, vjust = 2) +
  scale_x_continuous(breaks = seq(0, 34, 2)) +
  labs(title = "Distribuição da temperatura média diária no Paraná",
       subtitle = "Barras = número de dias a cada 1 °C | Linha = formato suavizado da distribuição",
       x = "Temperatura média diária (°C)",
       y = "Número de dias")

# 2.2 Histograma da chuva (note a assimetria)
clima |>
  mutate(classe = cut(chuva,
                      breaks = c(0, 1, 5, 25, 50, Inf),
                      labels = c("Quase sem chuva\n(< 1 mm)",
                                 "Fraca\n(1 a 5 mm)",
                                 "Moderada\n(5 a 25 mm)",
                                 "Forte\n(25 a 50 mm)",
                                 "Muito forte\n(> 50 mm)"),
                      right = FALSE, include.lowest = TRUE)) |>
  count(classe) |>
  mutate(pct = n / sum(n) * 100) |>
  ggplot(aes(classe, n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(n, " dias\n(", round(pct, 1), "%)")),
            vjust = -0.3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Paraná: quantos dias de cada tipo de chuva?",
       subtitle = paste0(n_dias, " dias (2020-2024) classificados pela chuva acumulada no dia"),
       x = NULL, y = "Número de dias")

# 2.3 Boxplot da temperatura média por mês
ggplot(clima, aes(mes, temperatura)) +
  geom_boxplot(fill = "orange", alpha = 0.6) +
  scale_y_continuous(breaks = seq(0, 34, 2)) +
  labs(title = "Paraná: temperatura média diária por mês",
       subtitle = "Caixa = 50% centrais dos dias | Traço = mediana | Pontos = dias atípicos",
       x = "Mês", y = "Temperatura (°C)")

# 2.4 Série temporal da temperatura
# Média de cada mês (ponto colocado no meio do mês)
clima_mensal <- clima |>
  mutate(ano_mes = floor_date(data, "month")) |>
  group_by(ano_mes) |>
  summarise(temperatura = mean(temperatura, na.rm = TRUE))

ggplot() +
  geom_line(data = clima, aes(data, temperatura),
            color = "navyblue", linewidth = 0.3) +
  geom_line(data = clima_mensal, aes(ano_mes + 14, temperatura),
            color = "red", linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 34, 2)) +
  labs(title = "Temperatura média no Paraná (2020-2024)",
       subtitle = "Cinza = temperatura de cada dia | Vermelho = média de cada mês",
       x = NULL, y = "Temperatura (°C)")

# 2.5 Dispersão: radiação x temperatura (base da correlação)
rho <- cor(clima$radiacao, clima$temperatura,
           method = "spearman", use = "complete.obs")

ggplot(clima, aes(radiacao, temperatura)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red") +
  scale_y_continuous(breaks = seq(0, 34, 2)) +
  labs(title = "Paraná: radiação solar x temperatura média",
       subtitle = paste0("Cada ponto = 1 dia | Correlação de Spearman = ",
                         round(rho, 2), " (", tolower(classifica(rho)), ")"),
       x = "Radiação solar (MJ/m²/dia)",
       y = "Temperatura média (°C)")

# Para salvar o último gráfico como imagem:
# ggsave("grafico_dispersao.png", width = 8, height = 5)

# 2.6 NOVO: temperaturas máxima, média e mínima por mês
clima |>
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
  labs(title = "Paraná: temperaturas máxima, média e mínima (2020-2024)",
       subtitle = "Cada ponto = média mensal da temperatura diária",
       x = NULL, y = "Temperatura (°C)")

# ---- 3. TESTE DE NORMALIDADE --------------------------------
# H0: os dados seguem distribuição normal
# H1: os dados NÃO seguem distribuição normal
# Critério: p-valor < 0,05 -> rejeita H0 (não é normal)
# Obs.: shapiro.test() aceita no máximo 5000 observações (aqui são ~1827).

# 3.1 Tabela de resultados (Shapiro-Wilk)
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

print(normalidade)
View(normalidade)
write_csv(normalidade, "tabela_normalidade.csv")

# 3.2 Q-Q plots das variáveis
# Se os pontos seguem a linha vermelha, a distribuição é próxima da normal
clima |>
  select(all_of(variaveis)) |>
  pivot_longer(everything(), names_to = "variavel", values_to = "valor") |>
  mutate(variavel = factor(unname(nomes[variavel]), levels = unname(nomes))) |>
  ggplot(aes(sample = valor)) +
  stat_qq(alpha = 0.4) +
  stat_qq_line(color = "red", linewidth = 0.8) +
  facet_wrap(~ variavel, scales = "free_y") +
  labs(title = "Paraná: as variáveis climáticas seguem uma distribuição normal?",
       subtitle = "Pontos sobre a linha vermelha = comportamento normal",
       x = "Quantis teóricos (normal)", y = "Quantis observados")

# ---- 4. TESTE DE CORRELAÇÃO ---------------------------------
# H0: não há correlação (rho = 0)
# H1: há correlação (rho ≠ 0)
# Pearson  -> variáveis aproximadamente normais
# Spearman -> variáveis NÃO normais (caso mais provável aqui)

# 4.1 Matriz de correlação (Spearman)
matriz <- cor(clima[variaveis], method = "spearman", use = "complete.obs")
round(matriz, 2)

# 4.2 Teste para todos os pares de variáveis (36 pares com 9 variáveis)
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
    Decisão   = ifelse(p < 0.05, "Correlação significativa",
                       "Sem evidência de correlação")
  )

print(correlacoes)
View(correlacoes)
write_csv(correlacoes, "tabela_correlacao.csv")

# 4.3 Mapa de calor da matriz de correlação
matriz |>
  as.data.frame() |>
  rownames_to_column("v1") |>
  pivot_longer(-v1, names_to = "v2", values_to = "rho") |>
  mutate(v1 = factor(unname(nomes_curtos[v1]), levels = unname(nomes_curtos)),
         v2 = factor(unname(nomes_curtos[v2]), levels = unname(nomes_curtos))) |>
  ggplot(aes(v1, v2, fill = rho)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(rho, 2)), size = 3.5) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "tomato",
                       limits = c(-1, 1), name = "Correlação") +
  labs(title = "Paraná: correlação de Spearman entre as variáveis climáticas",
       subtitle = "Vermelho = positiva | Azul = negativa | Branco = sem relação",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4.4 Pearson, apenas para comparação (par radiação x temperatura)
cor.test(clima$radiacao, clima$temperatura, method = "pearson")

# Escala usada na classificação (|rho|):
# 0,00-0,19 muito fraca | 0,20-0,39 fraca | 0,40-0,69 moderada
# 0,70-0,89 forte       | 0,90-1,00 muito forte