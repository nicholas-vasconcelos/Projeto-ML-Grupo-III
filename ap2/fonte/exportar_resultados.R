# Roda o notebook da AP2 (modo rápido) e exporta todos os resultados para o artigo.
# Uso: Rscript exportar_resultados.R <pasta_saida>
args  <- commandArgs(trailingOnly = TRUE)
SAIDA <- if (length(args)) args[1] else "ap2_resultados"
FONTE <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE))))

# 2º argumento "completo" = roda o notebook com PULAR_PREPROCESSAMENTO <- FALSE (refaz a base a partir das APIs/cache)
codigo <- readLines(file.path(FONTE, "ap2_preparacao.R"), encoding = "UTF-8")
if (length(args) >= 2 && args[2] == "completo")
  codigo <- sub("^PULAR_PREPROCESSAMENTO <- TRUE", "PULAR_PREPROCESSAMENTO <- FALSE", codigo)
eval(parse(text = codigo, encoding = "UTF-8"), envir = globalenv())

dir.create(file.path(SAIDA, "figuras"), recursive = TRUE, showWarnings = FALSE)
salva <- function(df, nome) write_csv(df, file.path(SAIDA, nome))
r3 <- function(df) mutate(df, across(where(is.double), ~ signif(.x, 4)))

# ---- 1. Desempenho dos modelos ---------------------------------------------
salva(r3(todos_cv |> relocate(modelo, conjunto, safra_validacao)), "metricas_cv_por_safra.csv")
salva(r3(comparacao), "metricas_cv_media.csv")
salva(r3(res_teste), "metricas_teste_2025_26.csv")

hiper <- bind_rows(
  tibble(modelo = "Logística EN", conjunto = names(CONJUNTOS), parametros = "alpha = 0.5; lambda = lambda.1se (CV interna por safra)"),
  bind_rows(lapply(names(melhor_rf), function(n) tibble(modelo = "Floresta aleatória", conjunto = n,
    parametros = sprintf("num.trees = 500; mtry = %d; min.node.size = %d (grade mtry 3/6/12 x min.node.size 5/20)",
                         melhor_rf[[n]]$mtry, melhor_rf[[n]]$min_no)))),
  bind_rows(lapply(names(melhor_xgb), function(n) tibble(modelo = "XGBoost", conjunto = n,
    parametros = sprintf("max_depth = %d; nrounds = %d; eta = 0.05; subsample = 0.8; colsample_bytree = 0.8; min_child_weight = 3; scale_pos_weight = neg/pos (grade max_depth 2/3 x nrounds 100/300)",
                         melhor_xgb[[n]]$max_depth, melhor_xgb[[n]]$nrounds))))
)
salva(hiper, "hiperparametros_escolhidos.csv")

wt <- wilcox.test(a$auc_pr, b$auc_pr, paired = TRUE, exact = TRUE)
salva(tibble(comparacao = paste0(melhor$modelo, " (", melhor$conjunto, ") vs Linha de base"),
             metrica = "AUC-PR por safra (n = 5 pares)", V = unname(wt$statistic), p_valor = wt$p.value,
             mediana_diferenca = median(a$auc_pr - b$auc_pr)), "teste_wilcoxon_modelos.csv")

# Matrizes de confusão no teste (limiar escolhido no treino)
salva(bind_rows(lapply(names(prob_teste), function(n) {
  p <- as.integer(prob_teste[[n]] >= limiares_teste[[n]]); y <- teste$ocorrencia
  tibble(modelo = n, limiar = limiares_teste[[n]], VP = sum(p == 1 & y == 1), FP = sum(p == 1 & y == 0),
         FN = sum(p == 0 & y == 1), VN = sum(p == 0 & y == 0))
})), "matriz_confusao_teste.csv")

# ---- 2. Interpretação dos modelos -------------------------------------------
salva(r3(tibble(atributo = rownames(coefs), coef = coefs[, 1]) |>
           filter(atributo != "(Intercept)") |>
           mutate(razao_chances = exp(coef), selecionado = coef != 0) |>
           arrange(desc(abs(coef)))), "coeficientes_logistica_en.csv")
salva(r3(imp_rf), "importancia_floresta.csv")

dx <- xgb.DMatrix(matriz_x(treino, CONJUNTOS[["clima + contexto"]]), label = treino$ocorrencia)
set.seed(42)
xgb_fin <- xgb.train(params = list(objective = "binary:logistic", max_depth = melhor_xgb[["clima + contexto"]]$max_depth,
                                   eta = 0.05, subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 3,
                                   scale_pos_weight = sum(treino$ocorrencia == 0) / sum(treino$ocorrencia == 1), nthread = 2),
                     data = dx, nrounds = melhor_xgb[["clima + contexto"]]$nrounds, verbose = 0)
salva(r3(as_tibble(xgb.importance(model = xgb_fin))), "importancia_xgboost.csv")

# ---- 3. Descrição da base ---------------------------------------------------
salva(tabela |> group_by(safra) |>
        summarise(semanas_ponto = n(), semanas_com_caso = sum(ocorrencia), casos = sum(n_casos),
                  pct_positivas = round(100 * mean(ocorrencia), 1), conjunto = first(ifelse(safra == SAFRA_TESTE, "teste", "treino"))),
      "base_por_safra.csv")
salva(tabela |> group_by(ponto, mesorregiao) |>
        summarise(semanas_com_caso = sum(ocorrencia), casos = sum(n_casos), pct_positivas = round(100 * mean(ocorrencia), 1)) |>
        arrange(desc(casos)), "base_por_ponto.csv")

# ---- 4. Estatística inferencial (treino, 2020/21-2024/25) --------------------
# 4.1 Mann-Whitney: clima defasado em semanas com e sem caso
mw <- bind_rows(lapply(ATR_CLIMA, function(v) {
  x1 <- treino[[v]][treino$ocorrencia == 1]; x0 <- treino[[v]][treino$ocorrencia == 0]
  t <- suppressWarnings(wilcox.test(x1, x0, exact = FALSE))
  tibble(atributo = v, mediana_com_caso = median(x1), mediana_sem_caso = median(x0),
         W = unname(t$statistic), p_valor = t$p.value,
         r_bisserial = 2 * unname(t$statistic) / (length(x1) * length(x0)) - 1)   # efeito: -1 a 1
})) |> mutate(p_ajustado_BH = p.adjust(p_valor, "BH")) |> arrange(p_valor)
salva(r3(mw), "inferencial_mann_whitney.csv")

# 4.2 Regressão logística clássica (razões de chance por 1 desvio-padrão, IC 95% Wald)
vars_or <- c("horas_ur90_14d", "chuva_total_14d", "temp_media_14d", "dpv_medio_7d")
tr_z <- treino |> mutate(across(all_of(vars_or), ~ as.numeric(scale(.x))))
glm_or <- glm(reformulate(c(vars_or, "poly(semana, 2)", "ponto"), "ocorrencia"), data = tr_z, family = binomial)
ci <- confint.default(glm_or)
salva(r3(tibble(termo = names(coef(glm_or)), coef = coef(glm_or), ep = sqrt(diag(vcov(glm_or))),
                razao_chances = exp(coef(glm_or)), ic95_inf = exp(ci[, 1]), ic95_sup = exp(ci[, 2]),
                p_valor = summary(glm_or)$coefficients[, 4]) |> filter(termo != "(Intercept)")),
      "inferencial_logistica_razoes_chance.csv")

# 4.3 Hipóteses H1-H3 por comparação de modelos (todos com calendário + ponto)
base_f <- "ocorrencia ~ poly(semana, 2) + ponto"
m0 <- glm(as.formula(base_f), data = treino, family = binomial)
aic_de <- function(v) AIC(glm(as.formula(paste(base_f, "+", v)), data = treino, family = binomial))
hip <- tribble(
  ~hipotese, ~modelo,
  "H1", "chuva_total_7d", "H1", "chuva_total_14d", "H1", "horas_ur90_7d", "H1", "horas_ur90_14d",
  "H1", "horas_infeccao_7d", "H1", "horas_infeccao_14d", "H1", "horas_infeccao_21d",
  "H2", "umidade_media_14d", "H2", "dpv_medio_14d", "H2", "horas_ur90_14d", "H2", "dias_molhamento_14d",
  "H2", "horas_infeccao_14d"
) |> mutate(aic = map_dbl(modelo, aic_de), aic_so_calendario = AIC(m0), delta_aic = aic - AIC(m0))
m_h3a <- glm(as.formula(paste(base_f, "+ horas_ur90_14d")), data = treino, family = binomial)
m_h3b <- glm(as.formula(paste(base_f, "+ horas_ur90_14d * ponto")), data = treino, family = binomial)
lrt <- anova(m_h3a, m_h3b, test = "LRT")
hip <- bind_rows(hip, tibble(hipotese = "H3", modelo = "horas_ur90_14d x ponto (interação)",
                             aic = AIC(m_h3b), aic_so_calendario = AIC(m_h3a), delta_aic = AIC(m_h3b) - AIC(m_h3a))) |>
  mutate(lrt_p_valor = c(rep(NA, n() - 1), lrt$`Pr(>Chi)`[2]))
salva(r3(hip), "inferencial_hipoteses_aic.csv")

# ---- 5. Figuras --------------------------------------------------------------
file.copy(list.files(PASTA_FIG, full.names = TRUE), file.path(SAIDA, "figuras"), overwrite = TRUE)

g_or <- read_csv(file.path(SAIDA, "inferencial_logistica_razoes_chance.csv")) |>
  filter(termo %in% vars_or) |>
  ggplot(aes(razao_chances, reorder(termo, razao_chances))) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbar(aes(xmin = ic95_inf, xmax = ic95_sup), width = 0.2, orientation = "y") +
  geom_point(size = 3, colour = "#e76f51") +
  scale_x_log10() +
  labs(x = "Razão de chances por 1 desvio-padrão (IC 95%, escala log)", y = NULL,
       title = "Clima defasado e chance de caso na semana",
       subtitle = "Regressão logística ajustada por semana da safra e ponto (treino 2020/21-2024/25)") + tema
ggsave(file.path(SAIDA, "figuras", "fig_razoes_chance.png"), g_or, width = 8, height = 4.5, dpi = 200)

g_mw <- mw |> mutate(sig = p_ajustado_BH < 0.05) |>
  ggplot(aes(r_bisserial, reorder(atributo, r_bisserial), fill = sig)) +
  geom_col() + geom_vline(xintercept = 0) +
  scale_fill_manual(values = c(`TRUE` = "#2a9d8f", `FALSE` = "grey70"), labels = c(`TRUE` = "p ajust. < 0,05", `FALSE` = "n.s.")) +
  labs(x = "Correlação rank-bisserial (com caso vs sem caso)", y = NULL, fill = NULL,
       title = "Mann-Whitney: clima defasado em semanas com e sem caso") + tema
ggsave(file.path(SAIDA, "figuras", "fig_mann_whitney.png"), g_mw, width = 8, height = 7, dpi = 200)

# ---- 6. Ablação: efeito de cada mudança da versão 2 ---------------------------
# A = versão 1 (atributos originais, limiar 0,5); B = + limiar ótimo (F1);
# C = + horas de infecção; D = + esporos/unidades de alerta (versão 2 completa).
atr_infec <- grep("horas_infeccao", ATR_CLIMA, value = TRUE)
atr_esp   <- c("esporos_raio_2sem", "esporos_estado_2sem")
V1 <- c(setdiff(ATR_CLIMA, atr_infec), setdiff(ATR_CONTEXTO, atr_esp))
variantes <- list(
  "A. Versão 1 (limiar 0,5)"     = list(atr = V1, lim = 0.5),
  "B. + limiar ótimo (F1)"       = list(atr = V1, lim = "otimo"),
  "C. + horas de infecção"       = list(atr = c(V1, atr_infec), lim = "otimo"),
  "D. + esporos (versão 2)"      = list(atr = c(V1, atr_infec, atr_esp), lim = "otimo")
)
melhor_por_auc_pr <- function(grade, construtor) {
  g <- grade |> mutate(auc_pr = pmap_dbl(grade, function(...)
    mean(validacao_por_safra(treino, construtor(...), mostrar = FALSE)$auc_pr, na.rm = TRUE)))
  do.call(construtor, as.list(select(slice_max(g, auc_pr, n = 1, with_ties = FALSE), -auc_pr)))
}
modelos_abl <- list(
  "Linha de base"      = function(atr) baseline_calendario,
  "Logística EN"       = function(atr) ajusta_logistica(atr),
  "Floresta aleatória" = function(atr) melhor_por_auc_pr(grade_rf,  function(mtry, min_no) ajusta_floresta(atr, mtry, min_no)),
  "XGBoost"            = function(atr) melhor_por_auc_pr(grade_xgb, function(max_depth, nrounds) ajusta_xgb(atr, max_depth, nrounds))
)
abl <- list()
for (v in names(variantes)) for (m in names(modelos_abl)) {
  if (m == "Linha de base" && !startsWith(v, "A") && !startsWith(v, "B")) next   # linha de base não usa atributos novos
  cat("Ablação:", v, "|", m, "\n")
  f   <- modelos_abl[[m]](variantes[[v]]$atr)
  lim <- variantes[[v]]$lim
  cv  <- validacao_por_safra(treino, f, limiar = lim, mostrar = FALSE)
  lim_t <- if (identical(lim, "otimo")) limiar_otimo(treino, f) else lim
  te  <- metricas(f(treino, teste), teste$ocorrencia, lim_t)
  abl[[length(abl) + 1]] <- tibble(
    variante = v, modelo = m,
    cv_auc_pr = mean(cv$auc_pr, na.rm = TRUE), cv_f1 = mean(cv$f1), cv_precisao = mean(cv$precisao),
    cv_revocacao = mean(cv$revocacao), cv_limiar_medio = mean(cv$limiar),
    teste_auc_pr = te$auc_pr, teste_f1 = te$f1, teste_precisao = te$precisao,
    teste_revocacao = te$revocacao, teste_limiar = lim_t)
}
ablacao <- bind_rows(abl)
salva(r3(ablacao), "ablacao_versoes.csv")

g_abl <- ablacao |>
  select(variante, modelo, `Validação cruzada (média 5 safras)` = cv_f1, `Teste 2025/26` = teste_f1) |>
  tidyr::pivot_longer(-c(variante, modelo), names_to = "avaliacao", values_to = "f1") |>
  ggplot(aes(variante, f1, colour = modelo, group = modelo)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2.5) +
  facet_wrap(~ avaliacao) +
  scale_x_discrete(labels = function(x) sub("\\. ", ".\n", x)) +
  labs(x = NULL, y = "F1", colour = NULL, title = "Efeito de cada mudança no F1 (ablação)") +
  tema + theme(legend.position = "bottom", axis.text.x = element_text(size = 8))
ggsave(file.path(SAIDA, "figuras", "fig_ablacao_f1.png"), g_abl, width = 10, height = 5.5, dpi = 200)

# ---- 6b. Alvo secundário (versão 1: 75 km, só a semana) -----------------------
# Mesmos modelos e protocolo, trocando o alvo, para mostrar o efeito da definição do alvo
sec <- list()
for (m in names(modelos_abl)) {
  tr2 <- treino |> mutate(ocorrencia = ocorrencia_75km_1sem)
  te2 <- teste  |> mutate(ocorrencia = ocorrencia_75km_1sem)
  atr <- c(ATR_CLIMA, ATR_CONTEXTO)
  treino_bak <- treino; treino <- tr2           # melhor_por_auc_pr usa `treino`
  f <- modelos_abl[[m]](atr)
  cv <- validacao_por_safra(tr2, f, limiar = "otimo", mostrar = FALSE)
  lt <- limiar_otimo(tr2, f)
  te <- metricas(f(tr2, te2), te2$ocorrencia, lt)
  treino <- treino_bak
  sec[[m]] <- tibble(alvo = "75 km, só a semana (secundário)", modelo = m,
                     cv_auc_pr = mean(cv$auc_pr, na.rm = TRUE), cv_f1 = mean(cv$f1),
                     teste_auc_pr = te$auc_pr, teste_f1 = te$f1, teste_precisao = te$precisao,
                     teste_revocacao = te$revocacao, teste_limiar = lt)
}
principal <- bind_rows(
  res_teste |> transmute(modelo = sub(" —.*| \\(.*", "", modelo), teste_auc_pr = auc_pr, teste_f1 = f1,
                         teste_precisao = precisao, teste_revocacao = revocacao, teste_limiar = limiar) |>
    mutate(modelo = recode(modelo, "Logística EN" = "Logística EN")) |>
    left_join(comparacao |> filter(conjunto != "clima") |>
                transmute(modelo = recode(modelo, "Linha de base" = "Linha de base"), cv_auc_pr = auc_pr, cv_f1 = f1),
              by = "modelo") |>
    mutate(alvo = "150 km, 2 semanas (principal)"))
salva(r3(bind_rows(principal, bind_rows(sec)) |> relocate(alvo, modelo, cv_auc_pr, cv_f1)), "comparacao_alvos.csv")

# ---- 7. Versões ---------------------------------------------------------------
writeLines(capture.output(sessionInfo()), file.path(SAIDA, "sessionInfo.txt"))
cat("\nExportado para", normalizePath(SAIDA), "\n")
