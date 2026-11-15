# # AP2 — Ferrugem-asiática da soja no Paraná: coleta e preparação dos dados
# 
# **Grupo III — Aprendizado de Máquina**
# 
# Este notebook reproduz, do zero, toda a base de dados da AP2 e para **logo antes do treinamento**.
# Cada membro do grupo deve **fazer uma cópia** dele (`Arquivo → Salvar uma cópia no Drive`), rodar as Seções 0 a 7
# e então treinar o seu próprio modelo na Seção 8 (regressão logística, floresta aleatória, etc.).
# 
# **Pergunta de pesquisa:** dado o clima das semanas anteriores, haverá pelo menos um caso de ferrugem-asiática
# (ocorrência comercial) a até 75 km de um ponto climático em uma determinada semana da safra?
# 
# | Seção | O que faz | Tempo aprox. |
# |---|---|---|
# | 0 | Configuração e pacotes | 1–2 min |
# | 1 | Coleta das ocorrências (API do Consórcio Antiferrugem) | 3–6 min |
# | 2 | Pontos climáticos (os 7 pontos da AP1) | instantâneo |
# | 3 | Coleta do clima horário (API NASA POWER) | 5–10 min |
# | 4 | Agregação do clima para a escala diária | segundos |
# | 5 | Associação dos casos aos pontos e às semanas | segundos |
# | 6 | Tabela ponto × semana com atributos defasados | segundos |
# | 7 | Verificações, gráficos e divisão treino/teste | segundos |
# | 8 | **Área de treinamento** (cada membro) | — |
# 
# > **Runtime:** este notebook é em **R**. No Colab, confira em `Ambiente de execução → Alterar o tipo de ambiente de execução` que a linguagem é **R**.
# >
# > **Modo rápido (padrão):** com `PULAR_PREPROCESSAMENTO <- TRUE` (Seção 0), a tabela pronta é baixada do GitHub e as
# > Seções 1–6 são puladas — tudo até a Seção 7 roda em segundos.
# >
# > **Arquivos:** tudo é salvo em `/content/dados_ap2/` (painel de arquivos à esquerda). O Colab apaga essa pasta quando a sessão termina — baixe o `tabela_ponto_semana.csv` se quiser guardar.

# ## 0. Configuração
# 
# Instala os pacotes que faltam e define as opções gerais. **Só altere esta célula se souber o que está fazendo.**

# =============================================================================
#  CONFIGURAÇÃO PRINCIPAL
# =============================================================================
# TRUE  = MODO RÁPIDO: baixa do GitHub a tabela ponto × semana já pronta e PULA
#         as Seções 1 a 6 (coleta e pré-processamento). Leva poucos segundos.
#         Use este modo para treinar modelos.
# FALSE = MODO COMPLETO: refaz tudo a partir das APIs (Antiferrugem + NASA POWER).
#         Leva ~15 min. Use só se mudar o pré-processamento ou quiser reproduzir a base.
PULAR_PREPROCESSAMENTO <- TRUE

# Repositório de onde vêm os arquivos prontos (modo rápido)
REPO_GITHUB <- "nicholas-vasconcelos/Projeto-ML-Grupo-III"
RAMO_GITHUB <- "ap2"
URL_DADOS   <- sprintf("https://raw.githubusercontent.com/%s/%s/ap2/dados", REPO_GITHUB, RAMO_GITHUB)

REBAIXAR    <- FALSE   # (modo completo) TRUE = ignora arquivos já baixados e consulta as APIs de novo

# ---- Pacotes ---------------------------------------------------------------
pacotes <- c("dplyr", "tidyr", "readr", "purrr", "stringr", "lubridate", "ggplot2")
if (!PULAR_PREPROCESSAMENTO) pacotes <- c(pacotes, "jsonlite", "nasapower")
faltando <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltando) > 0) install.packages(faltando)
invisible(lapply(pacotes, library, character.only = TRUE))

# ---- Opções gerais ---------------------------------------------------------
PASTA      <- if (dir.exists("/content")) "/content/dados_ap2" else "dados_ap2"
PASTA_FIG  <- file.path(PASTA, "figuras")
dir.create(PASTA_FIG, recursive = TRUE, showWarnings = FALSE)

# Safras estudadas (nome -> id interno da API do Consórcio Antiferrugem)
SAFRAS <- c("2020/2021" = 33, "2021/2022" = 34, "2022/2023" = 35,
            "2023/2024" = 36, "2024/2025" = 38, "2025/2026" = 40)

RAIO_KM        <- 75      # raio de associação caso -> ponto (igual à AP1)
JANELAS_DIAS   <- c(7, 14) # janelas de clima ANTES de cada semana (atributos defasados)
SAFRA_TESTE    <- "2025/2026"  # safra separada como teste final (ver Seção 7)
TIPOS_ALVO     <- "comercial"  # tipo de ocorrência usado como alvo

options(dplyr.summarise.inform = FALSE, readr.show_col_types = FALSE)
cat("Pasta de trabalho:", normalizePath(PASTA), "\n")
cat("Modo:", if (PULAR_PREPROCESSAMENTO) "RÁPIDO (dados prontos do GitHub; Seções 1-6 serão puladas)"
             else "COMPLETO (coleta pelas APIs)", "\n")

# ### 0.1 Modo rápido — baixa os arquivos prontos do GitHub
# 
# Só faz algo se `PULAR_PREPROCESSAMENTO = TRUE`. Baixa para `/content/dados_ap2/`:
# 
# | Arquivo | Conteúdo |
# |---|---|
# | `tabela_ponto_semana.csv` | **a base de treinamento** (resultado da Seção 6) |
# | `ocorrencias_pr_2020_2026.csv` | casos de ferrugem (resultado da Seção 1) |
# | `clima_diario_7_pontos.csv` | clima diário dos 7 pontos (resultado da Seção 4) |
# 
# Depois desta célula pode rodar tudo em ordem: as células das Seções 1 a 6 vão só avisar que foram puladas.

if (PULAR_PREPROCESSAMENTO) {
  for (arq in c("tabela_ponto_semana.csv", "ocorrencias_pr_2020_2026.csv", "clima_diario_7_pontos.csv")) {
    download.file(file.path(URL_DADOS, arq), file.path(PASTA, arq), quiet = TRUE, mode = "wb")
    cat("Baixado:", arq, "\n")
  }
  tabela       <- read_csv(file.path(PASTA, "tabela_ponto_semana.csv"))
  ocorrencias  <- read_csv(file.path(PASTA, "ocorrencias_pr_2020_2026.csv"))
  clima_diario <- read_csv(file.path(PASTA, "clima_diario_7_pontos.csv"))
  cat(sprintf("\nTabela ponto × semana: %d linhas x %d colunas. Pode seguir para a Seção 7.\n",
              nrow(tabela), ncol(tabela)))
} else {
  cat("Modo completo: nada a baixar aqui; rode as Seções 1 a 6.\n")
}

# ## 1. Coleta das ocorrências — Consórcio Antiferrugem
# 
# Os registros vêm da API pública que alimenta o mapa do site [consorcioantiferrugem.net](http://www.consorcioantiferrugem.net)
# (é a mesma consulta do botão **APLICAR** em "Filtros avançados"). É a versão em R do script `baixar_antiferrugem.py` da AP1.
# 
# **Endpoints usados**
# 
# - `rest/ocorrencias/all/safra/{safra}/{estado}/{dataIni}/{dataFim}/{tipos}/{agrupa}` — ocorrências da safra.
#   `estado` = 41 (Paraná); `tipos` = `(3,1)` (3 = soja voluntária, 1 = ocorrência comercial);
#   `agrupa` = 0 (por município) ou 2 (por coordenada própria).
# - `rest/ocorrencias/gdia/{safra}/{estado}/{municipio}/0/0/0` — número de ocorrências por dia (o município 0 significa "todos").
# 
# **Como a data de cada caso é recuperada** (a API não informa a data do registro):
# 
# - *comerciais*: o total por município vem do campo `qtd`, e as datas vêm da contagem diária (`gdia`) filtrada por município;
# - *soja voluntária*: o período é dividido ao meio repetidamente (bissecção) até chegar a dias individuais.
# 
# Quando o registro tem coordenada da lavoura (`agrupa = 2` no mesmo dia), ela é usada; senão, usa-se a do município.

if (PULAR_PREPROCESSAMENTO) {
  cat("Pulado (modo rápido): dados já baixados do GitHub na Seção 0.1.\n")
} else {

# ---- 1.1 Funções de acesso à API ------------------------------------------
BASE_AF   <- "http://www.consorcioantiferrugem.net/rest/ocorrencias"
ESTADO_PR <- 41
NOME_TIPO <- c("1" = "comercial", "3" = "voluntaria")
nz <- function(x) if (is.null(x)) 0 else x   # NULL do JSON -> 0

get_json <- function(url) {
  for (tentativa in 1:5) {
    res <- tryCatch({
      tmp <- tempfile()
      download.file(url, tmp, quiet = TRUE, mode = "wb")
      raw <- readBin(tmp, "raw", file.size(tmp))
      unlink(tmp)
      txt <- rawToChar(raw)
      # alguns endpoints mandam UTF-8 e outros Latin-1
      if (!validUTF8(txt)) txt <- iconv(txt, "latin1", "UTF-8")
      Sys.sleep(0.15)
      jsonlite::fromJSON(txt, simplifyVector = FALSE)
    }, error = function(e) e)
    if (!inherits(res, "error")) return(res)
    message("  erro (", conditionMessage(res), "), tentando de novo: ", url)
    Sys.sleep(2 * tentativa)
  }
  stop("falhou: ", url)
}

consulta <- function(safra_id, ini, fim, agrupa, tipos = "(3,1)") {
  ini <- if (is.null(ini)) "0" else format(ini)
  fim <- if (is.null(fim)) "0" else format(fim)
  get_json(sprintf("%s/all/safra/%s/%s/%s/%s/%s/%s",
                   BASE_AF, safra_id, ESTADO_PR, ini, fim, tipos, agrupa))
}

# Linhas do Paraná com pelo menos 1 registro (qtdgeral = casos de qualquer tipo)
linhas_pr <- function(resp) {
  out <- list()
  for (est in resp$data) {
    if (identical(est$sigla, "PR")) {
      out <- c(out, Filter(function(o) nz(o$qtdgeral) > 0, est$ocorrencias))
    }
  }
  out
}

# Bissecção: guarda em `dias` (environment) a resposta agrupa=0 de cada dia com registro
bisseccao <- function(safra_id, ini, fim, dias) {
  resp <- consulta(safra_id, ini, fim, 0)
  if (length(linhas_pr(resp)) == 0) return(invisible())
  if (ini == fim) { assign(format(ini), resp, envir = dias); return(invisible()) }
  meio <- ini + floor(as.numeric(fim - ini) / 2)
  bisseccao(safra_id, ini, meio, dias)
  bisseccao(safra_id, meio + 1, fim, dias)
}

}

if (PULAR_PREPROCESSAMENTO) {
  cat("Pulado (modo rápido): dados já baixados do GitHub na Seção 0.1.\n")
} else {

# ---- 1.2 Download (ou leitura do arquivo já baixado) -----------------------
ARQ_OCORR <- file.path(PASTA, "ocorrencias_pr_2020_2026.csv")

baixar_ocorrencias <- function() {
  registros <- list()

  # n casos do município `cid` no dia d: primeiro os pontos com coordenada própria
  # (agrupa=2) daquele dia; o restante vai para a coordenada do município
  adiciona <- function(safra, d, cid, tipo, n, por_dia) {
    base <- list(safra = safra, data = if (is.null(d)) NA_character_ else d,
                 municipio = cid$nome, codigo_ibge = cid$id, tipo = NOME_TIPO[[as.character(tipo)]])
    pts <- list()
    if (!is.null(d) && !is.null(por_dia[[d]])) {
      pts <- Filter(function(p) p$id == cid$id && p$tipo == tipo,
                    linhas_pr(por_dia[[d]]$agrupa2))
    }
    for (pt in pts) {
      k <- min(pt$qtdgeral, n)
      if (k > 0) {
        registros[[length(registros) + 1]] <<- c(base, list(
          latitude = pt$latitude, longitude = pt$longitude, n_casos = k,
          origem_coord = "propria", localidade = if (is.null(pt$localidade)) NA_character_ else pt$localidade))
        n <- n - k
      }
    }
    if (n > 0) {
      registros[[length(registros) + 1]] <<- c(base, list(
        latitude = cid$clatitude, longitude = cid$clongitude, n_casos = n,
        origem_coord = "municipio", localidade = NA_character_))
    }
  }

  for (safra in names(SAFRAS)) {
    sid <- SAFRAS[[safra]]
    anos <- as.integer(strsplit(safra, "/")[[1]])
    ini <- as.Date(sprintf("%d-07-01", anos[1])); fim <- as.Date(sprintf("%d-06-30", anos[2]))
    cat(sprintf("Safra %s (id %d): %s a %s\n", safra, sid, ini, fim))

    sem_datas <- consulta(sid, NULL, NULL, 0)

    # datas por bissecção (necessária para a soja voluntária e para as coordenadas próprias)
    dias <- new.env()
    bisseccao(sid, ini, fim, dias)
    por_dia <- list()
    for (d in sort(ls(dias))) {
      por_dia[[d]] <- list(agrupa0 = get(d, envir = dias),
                           agrupa2 = consulta(sid, as.Date(d), as.Date(d), 2))
    }
    cat(sprintf("  %d dias com registro\n", length(por_dia)))

    # Comerciais: total oficial = "qtd"; datas = gdia filtrado pelo município
    for (cid in linhas_pr(sem_datas)) {
      if (cid$tipo != 1 || nz(cid$qtd) == 0) next
      resp <- get_json(sprintf("%s/gdia/%s/%s/%s/0/0/0", BASE_AF, sid, ESTADO_PR, cid$id))
      datados <- 0
      for (x in resp$data) {
        if (x$dia >= format(ini) && x$dia <= format(fim)) {
          adiciona(safra, x$dia, cid, 1, x$qtd, por_dia)
          datados <- datados + x$qtd
        } else {
          cat(sprintf("  fora do período: %s %s (%d)\n", cid$nome, x$dia, x$qtd))
        }
      }
      if (cid$qtd > datados) {
        cat(sprintf("  %d registro(s) comerciais sem data: %s\n", cid$qtd - datados, cid$nome))
        adiciona(safra, NULL, cid, 1, cid$qtd - datados, por_dia)
      }
    }

    # Soja voluntária: "qtd" vem 0; a contagem está em "qtdgeral" e a data vem da bissecção
    for (d in names(por_dia)) {
      for (cid in linhas_pr(por_dia[[d]]$agrupa0)) {
        if (cid$tipo == 3) adiciona(safra, d, cid, 3, cid$qtdgeral, por_dia)
      }
    }
  }

  bind_rows(lapply(registros, as_tibble)) |>
    mutate(data = as.Date(data)) |>
    select(safra, data, municipio, codigo_ibge, latitude, longitude,
           tipo, n_casos, origem_coord, localidade) |>
    arrange(safra, data, municipio)
}

if (file.exists(ARQ_OCORR) && !REBAIXAR) {
  ocorrencias <- read_csv(ARQ_OCORR)
  cat("Lido do arquivo já baixado:", ARQ_OCORR, "\n")
} else {
  ocorrencias <- baixar_ocorrencias()
  write_csv(ocorrencias, ARQ_OCORR)
}

print(ocorrencias |>
  group_by(safra, tipo) |>
  summarise(casos = sum(n_casos), sem_data = sum(n_casos[is.na(data)])) |>
  tidyr::pivot_wider(names_from = tipo, values_from = c(casos, sem_data), values_fill = 0))

}

# **Conferência:** na AP1 foram 577 casos nas seis safras: **553 comerciais** (551 com data; os 2 de Pitanga em 2022/23
# não têm data) e 24 de soja voluntária; 150 casos têm coordenada própria. Se o total abaixo for diferente,
# o Consórcio pode ter atualizado a base — anote isso na seção de Ameaças à Validade.

if (PULAR_PREPROCESSAMENTO) {
  cat("Pulado (modo rápido): dados já baixados do GitHub na Seção 0.1.\n")
} else {

print(ocorrencias |> filter(tipo == "comercial") |>
  summarise(casos = sum(n_casos), com_data = sum(n_casos[!is.na(data)]),
            coord_propria = sum(n_casos[origem_coord == "propria"])))

}

# ## 2. Pontos climáticos
# 
# Os sete pontos são os mesmos da AP1. Eles foram escolhidos resolvendo o **problema de máxima cobertura**
# (programação linear inteira sobre uma grade de 0,05°): os 7 pontos que deixam o maior número de casos a até 75 km
# de algum deles (93,8% dos casos). Aqui eles são usados como uma tabela fixa, para que todos os membros trabalhem
# com exatamente os mesmos pontos.

pontos <- tribble(
  ~ponto,          ~lat,   ~lon,   ~mesorregiao,
  "turvo",         -24.95, -51.30, "Centro-Sul",
  "tupassi",       -24.55, -53.50, "Oeste",
  "chopinzinho",   -25.90, -52.35, "Sudoeste",
  "quinta_do_sol", -23.95, -52.15, "Centro Ocidental",
  "capanema",      -25.60, -53.75, "Sudoeste",
  "tamarana",      -23.60, -51.05, "Norte Central",
  "pirai_do_sul",  -24.45, -50.05, "Centro Oriental"
)
pontos

# ## 3. Coleta do clima — NASA POWER (horário)
# 
# Para cada ponto são baixados dados **horários** (hora solar local) da comunidade agrícola (`ag`) do
# [NASA POWER](https://power.larc.nasa.gov), com o pacote `nasapower`, um ano por requisição.
# 
# | Parâmetro POWER | Variável | Unidade (horária) |
# |---|---|---|
# | `T2M` | temperatura do ar a 2 m | °C |
# | `RH2M` | umidade relativa a 2 m | % |
# | `PRECTOTCORR` | chuva corrigida | mm/h |
# | `ALLSKY_SFC_SW_DWN` | radiação solar incidente | MJ/m² por hora |
# 
# O **vento** (`WS2M`) não é baixado: a AP1 mostrou valores suspeitos em 4 dos 7 pontos.
# Os dados horários são necessários para a hipótese **H2** da AP1 (horas com umidade ≥ 90% por dia, uma aproximação
# do molhamento foliar).

if (PULAR_PREPROCESSAMENTO) {
  cat("Pulado (modo rápido): dados já baixados do GitHub na Seção 0.1.\n")
} else {

PARAMETROS   <- c("T2M", "RH2M", "PRECTOTCORR", "ALLSKY_SFC_SW_DWN")
CLIMA_INICIO <- as.Date("2020-07-01")   # cobre as janelas defasadas antes de out/2020
CLIMA_FIM    <- as.Date("2026-04-30")   # fim da safra 2025/26
PASTA_CLIMA  <- file.path(PASTA, "clima_horario")
dir.create(PASTA_CLIMA, showWarnings = FALSE)

baixar_clima_ponto <- function(ponto, lat, lon) {
  arq <- file.path(PASTA_CLIMA, paste0("clima_", ponto, "_horario.csv"))
  if (file.exists(arq) && !REBAIXAR) return(read_csv(arq))

  anos <- seq(year(CLIMA_INICIO), year(CLIMA_FIM))
  h <- map_dfr(anos, function(a) {
    ini <- max(CLIMA_INICIO, as.Date(sprintf("%d-01-01", a)))
    fim <- min(CLIMA_FIM,    as.Date(sprintf("%d-12-31", a)))
    cat(sprintf("  %s: %s a %s\n", ponto, ini, fim))
    for (tentativa in 1:4) {
      r <- tryCatch(get_power(community = "ag", pars = PARAMETROS, temporal_api = "hourly",
                              lonlat = c(lon, lat), dates = c(ini, fim), time_standard = "LST"),
                    error = function(e) e)
      if (!inherits(r, "error")) return(as_tibble(r))
      message("    erro (", conditionMessage(r), "), tentando de novo")
      Sys.sleep(5 * tentativa)
    }
    stop("NASA POWER falhou para ", ponto, " ", a)
  })

  h <- h |>
    transmute(ponto = ponto,
              data  = make_date(YEAR, MO, DY), hora = HR,
              T2M, RH2M, PRECTOTCORR, ALLSKY_SFC_SW_DWN) |>
    mutate(across(c(T2M, RH2M, PRECTOTCORR, ALLSKY_SFC_SW_DWN), ~ na_if(.x, -999)))
  write_csv(h, arq)
  h
}

clima_horario <- pmap_dfr(pontos[, c("ponto", "lat", "lon")], baixar_clima_ponto)
cat(nrow(clima_horario), "linhas horárias,", n_distinct(clima_horario$ponto), "pontos\n")

}

# ## 4. Agregação para a escala diária
# 
# Igual à AP1, o **DPV** e o **ponto de orvalho** são calculados a cada hora (FAO-56) e depois agregados, o que evita o
# viés da não linearidade da exponencial:
# 
# $$e_s(T) = 0{,}6108\,\exp\!\left(\frac{17{,}27\,T}{T+237{,}3}\right), \qquad \mathrm{DPV} = e_s(T)\,(1 - UR/100)$$
# 
# $$\gamma = \ln(UR/100) + \frac{17{,}27\,T}{237{,}3+T}, \qquad T_d = \frac{237{,}3\,\gamma}{17{,}27-\gamma}$$
# 
# Nova variável em relação à AP1 (para a H2): `horas_ur90` (horas do dia com UR ≥ 90%). Só são mantidos dias com as
# 24 horas completas. Os valores diários conferem exatamente com os arquivos `clima_*_diario.csv` da AP1.

if (PULAR_PREPROCESSAMENTO) {
  cat("Pulado (modo rápido): dados já baixados do GitHub na Seção 0.1.\n")
} else {

clima_diario <- clima_horario |>
  mutate(es    = 0.6108 * exp(17.27 * T2M / (T2M + 237.3)),
         vpd   = es * (1 - RH2M / 100),
         gamma = log(pmax(RH2M, 0.1) / 100) + 17.27 * T2M / (237.3 + T2M),
         td    = 237.3 * gamma / (17.27 - gamma)) |>
  group_by(ponto, data) |>
  summarise(n_horas         = sum(!is.na(T2M) & !is.na(RH2M)),
            temperatura     = mean(T2M),
            temp_max        = max(T2M),
            temp_min        = min(T2M),
            temp_orvalho    = mean(td),
            umidade         = mean(RH2M),
            vpd             = mean(vpd),
            chuva           = sum(PRECTOTCORR),
            radiacao        = sum(ALLSKY_SFC_SW_DWN),   # MJ/m²/dia
            horas_ur90      = sum(RH2M >= 90)) |>
  ungroup() |>
  filter(n_horas == 24)

write_csv(clima_diario, file.path(PASTA, "clima_diario_7_pontos.csv"))
print(clima_diario |> count(ponto, name = "dias"))
print(summary(select(clima_diario, -ponto, -data, -n_horas)))

}

# ## 5. Associação dos casos aos pontos e às semanas da safra
# 
# - **Casos usados no alvo:** ocorrências **comerciais** com data (a soja voluntária fica de fora, como na AP1).
# - **Ponto:** cada caso vai para o ponto mais próximo (distância de Haversine), se estiver a até 75 km.
# - **Semana da safra:** a janela de cada safra é de 1º de outubro a 31 de março (≈ 99,7% das detecções), dividida em
#   **26 semanas** de 7 dias contadas a partir de 1º de outubro (semana 1 = 1–7/out).

if (PULAR_PREPROCESSAMENTO) {
  cat("Pulado (modo rápido): dados já baixados do GitHub na Seção 0.1.\n")
} else {

haversine_km <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 +
       cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2 * 6371 * asin(sqrt(a))
}

inicio_safra <- function(safra) as.Date(paste0(substr(safra, 1, 4), "-10-01"))
N_SEMANAS <- 26

casos <- ocorrencias |>
  filter(tipo %in% TIPOS_ALVO, !is.na(data)) |>
  mutate(caso_id = row_number())

# distância de cada caso a cada ponto -> ponto mais próximo
dist <- tidyr::crossing(select(casos, caso_id, latitude, longitude),
                        select(pontos, ponto, lat, lon)) |>
  mutate(dist_km = haversine_km(latitude, longitude, lat, lon)) |>
  group_by(caso_id) |> slice_min(dist_km, n = 1, with_ties = FALSE) |> ungroup() |>
  select(caso_id, ponto, dist_km)

casos <- casos |>
  left_join(dist, by = "caso_id") |>
  mutate(semana = as.integer(floor(as.numeric(data - inicio_safra(safra)) / 7)) + 1L,
         no_raio   = dist_km <= RAIO_KM,
         na_janela = semana >= 1 & semana <= N_SEMANAS)

print(casos |> summarise(casos_com_data   = sum(n_casos),
                   casos_no_raio    = sum(n_casos[no_raio]),
                   casos_usados     = sum(n_casos[no_raio & na_janela]),   # no raio e entre out e mar
                   pct_usado        = round(100 * casos_usados / casos_com_data, 1)))

}

# ## 6. Tabela ponto × semana (base para o treinamento)
# 
# Cada linha é **um ponto em uma semana de uma safra**: 7 pontos × 6 safras × 26 semanas = **1.092 linhas**.
# 
# **Alvos**
# 
# - `ocorrencia` — 1 se houve ≥ 1 caso comercial no raio do ponto naquela semana, 0 caso contrário (**classificação**);
# - `n_casos` — número de casos (para quem quiser comparar com um modelo de contagem, ex.: Poisson).
# 
# **Atributos climáticos defasados** — calculados nos **7 e 14 dias ANTES** do início da semana (sufixos `_7d`, `_14d`).
# Usar só o clima anterior evita vazamento de informação: na prática, a previsão seria feita no início da semana.
# O período latente da ferrugem (infecção → sintomas) é de cerca de 6 a 10 dias, então a infecção que vira detecção
# numa semana acontece, em boa parte, nessas janelas.
# 
# | Atributo | Descrição |
# |---|---|
# | `temp_media`, `temp_min`, `temp_max` | médias da temperatura média, mínima e máxima diária (°C) |
# | `orvalho_medio`, `umidade_media`, `dpv_medio` | médias diárias (°C, %, kPa) |
# | `chuva_total`, `dias_chuva` | chuva acumulada (mm) e dias com chuva ≥ 1 mm |
# | `horas_ur90` | total de horas com UR ≥ 90% (proxy de molhamento foliar — H2) |
# | `dias_molhamento` | dias com ≥ 10 h de UR ≥ 90% |
# | `dias_temp_fav` | dias com temperatura média entre 15 e 28 °C (faixa favorável à doença) |
# | `radiacao_media` | radiação média diária (MJ/m²) |
# 
# **Atributos de contexto** (use, ou não, conforme o modelo):
# 
# | Atributo | Descrição |
# |---|---|
# | `ponto`, `mesorregiao` | região (hipótese H3: regiões respondem diferente) |
# | `semana`, `mes` | calendário — também servem para o **modelo de referência** (só sazonalidade) |
# | `casos_antes_ponto` | casos no raio do ponto **antes** desta semana, na mesma safra (fonte de inóculo) |
# | `casos_estado_sem_ant` | casos comerciais no Paraná inteiro na semana anterior |

if (PULAR_PREPROCESSAMENTO) {
  cat("Pulado (modo rápido): dados já baixados do GitHub na Seção 0.1.\n")
} else {

# ---- 6.1 Grade ponto × safra × semana --------------------------------------
grade <- tidyr::crossing(pontos |> select(ponto, mesorregiao),
                         safra = names(SAFRAS),
                         semana = seq_len(N_SEMANAS)) |>
  mutate(inicio_semana = inicio_safra(safra) + 7 * (semana - 1),
         fim_semana    = inicio_semana + 6,
         mes           = month(inicio_semana + 3))   # mês do meio da semana

# ---- 6.2 Alvos --------------------------------------------------------------
alvo <- casos |>
  filter(no_raio, na_janela) |>
  group_by(ponto, safra, semana) |>
  summarise(n_casos = sum(n_casos), .groups = "drop")

# casos do estado (todos os casos comerciais com data, dentro ou fora do raio) por safra/semana
estado <- casos |>
  filter(na_janela) |>
  group_by(safra, semana) |>
  summarise(casos_estado = sum(n_casos), .groups = "drop")

# ---- 6.3 Atributos climáticos defasados ------------------------------------
resumo_janela <- function(d) {
  tibble(temp_media      = mean(d$temperatura),
         temp_min        = mean(d$temp_min),
         temp_max        = mean(d$temp_max),
         orvalho_medio   = mean(d$temp_orvalho),
         umidade_media   = mean(d$umidade),
         dpv_medio       = mean(d$vpd),
         chuva_total     = sum(d$chuva),
         dias_chuva      = sum(d$chuva >= 1),
         horas_ur90      = sum(d$horas_ur90),
         dias_molhamento = sum(d$horas_ur90 >= 10),
         dias_temp_fav   = sum(d$temperatura >= 15 & d$temperatura <= 28),
         radiacao_media  = mean(d$radiacao),
         n_dias          = nrow(d))
}

clima_por_ponto <- split(clima_diario, clima_diario$ponto)

atributos_janela <- function(dias) {
  grade |>
    select(ponto, safra, semana, inicio_semana) |>
    mutate(res = map2(ponto, inicio_semana, function(p, ini) {
      d <- clima_por_ponto[[p]]
      resumo_janela(d[d$data >= ini - dias & d$data <= ini - 1, ])
    })) |>
    tidyr::unnest(res) |>
    select(-inicio_semana) |>
    rename_with(~ paste0(.x, "_", dias, "d"), -c(ponto, safra, semana))
}

clima_defasado <- reduce(lapply(JANELAS_DIAS, atributos_janela),
                         left_join, by = c("ponto", "safra", "semana"))

# ---- 6.4 Junta tudo ---------------------------------------------------------
tabela <- grade |>
  left_join(alvo, by = c("ponto", "safra", "semana")) |>
  mutate(n_casos = coalesce(n_casos, 0L),
         ocorrencia = as.integer(n_casos > 0)) |>
  arrange(ponto, safra, semana) |>
  group_by(ponto, safra) |>
  mutate(casos_antes_ponto = cumsum(n_casos) - n_casos) |>   # só semanas anteriores
  ungroup() |>
  left_join(estado |> mutate(semana = semana + 1L) |> rename(casos_estado_sem_ant = casos_estado),
            by = c("safra", "semana")) |>
  mutate(casos_estado_sem_ant = coalesce(casos_estado_sem_ant, 0L)) |>
  left_join(clima_defasado, by = c("ponto", "safra", "semana")) |>
  relocate(ocorrencia, n_casos, .after = semana)

# janelas incompletas não deveriam acontecer (o clima começa em jul/2020)
stopifnot(all(tabela[[paste0("n_dias_", max(JANELAS_DIAS), "d")]] == max(JANELAS_DIAS)))
tabela <- select(tabela, -starts_with("n_dias_"))

write_csv(tabela, file.path(PASTA, "tabela_ponto_semana.csv"))
cat(sprintf("Tabela: %d linhas x %d colunas -> %s\n", nrow(tabela), ncol(tabela),
            file.path(PASTA, "tabela_ponto_semana.csv")))
glimpse(tabela)

}

# ## 7. Verificações, gráficos e divisão treino/teste
# 
# ### 7.1 Balanço das classes
# 
# Espera-se **desbalanceamento**: a maioria das semanas não tem caso. Por isso, **acurácia não serve** como métrica
# principal — use AUC-ROC, AUC-PR (precisão-revocação), F1 e revocação (Seção 8).

tabela |> count(ocorrencia) |> mutate(pct = round(100 * n / sum(n), 1))
tabela |> group_by(safra) |>
  summarise(semanas_com_caso = sum(ocorrencia), casos = sum(n_casos),
            pct_positivas = round(100 * mean(ocorrencia), 1))

tema <- theme_minimal(base_size = 12)

g1 <- tabela |>
  group_by(safra, semana) |> summarise(prop = mean(ocorrencia)) |>
  ggplot(aes(semana, prop, colour = safra)) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Semana da safra (1 = 1ª semana de outubro)",
       y = "Pontos com caso na semana", colour = "Safra",
       title = "Sazonalidade das detecções") + tema
g1
ggsave(file.path(PASTA_FIG, "fig_sazonalidade_semanas.png"), g1, width = 8, height = 4.5, dpi = 200)

g2 <- tabela |>
  select(ocorrencia, horas_ur90_14d, chuva_total_14d, temp_media_14d, dpv_medio_14d) |>
  tidyr::pivot_longer(-ocorrencia) |>
  mutate(ocorrencia = factor(ocorrencia, labels = c("Sem caso", "Com caso"))) |>
  ggplot(aes(ocorrencia, value, fill = ocorrencia)) +
  geom_boxplot(outlier.alpha = 0.3, show.legend = FALSE) +
  facet_wrap(~ name, scales = "free_y") +
  labs(x = NULL, y = NULL, title = "Clima dos 14 dias anteriores × ocorrência na semana") + tema
g2
ggsave(file.path(PASTA_FIG, "fig_clima_vs_ocorrencia.png"), g2, width = 8, height = 6, dpi = 200)

# ### 7.2 Divisão treino/teste (igual para todos os membros)
# 
# - **Teste final:** a safra **2025/2026** inteira (`SAFRA_TESTE`). Ela só deve ser usada **uma vez**, no fim, com o
#   modelo já escolhido. Avaliar uma safra que o modelo nunca viu é o que mede a capacidade de prever um ano novo.
# - **Treino:** as outras 5 safras. Para escolher hiperparâmetros e comparar modelos, use **validação cruzada
#   deixando uma safra de fora** (`fold = safra`): treina em 4 safras e valida na 5ª, cinco vezes.
# 
# Nunca use divisão aleatória de linhas: semanas vizinhas da mesma safra são muito parecidas, e o resultado ficaria
# otimista demais (vazamento temporal).

treino <- tabela |> filter(safra != SAFRA_TESTE)
teste  <- tabela |> filter(safra == SAFRA_TESTE)

treino |> count(safra, ocorrencia) |> tidyr::pivot_wider(names_from = ocorrencia, values_from = n,
                                                         names_prefix = "ocorrencia_")
cat(sprintf("Treino: %d linhas (%d positivas) | Teste: %d linhas (%d positivas)\n",
            nrow(treino), sum(treino$ocorrencia), nrow(teste), sum(teste$ocorrencia)))

# Listas de atributos prontas para usar nas fórmulas / receitas
ATR_CLIMA    <- grep("_(7|14)d$", names(tabela), value = TRUE)
ATR_CONTEXTO <- c("ponto", "semana", "casos_antes_ponto", "casos_estado_sem_ant")
ALVO         <- "ocorrencia"
ATR_CLIMA

# ### 7.3 Funções de avaliação comuns
# 
# Para que os resultados de todos os membros sejam comparáveis, **todos devem avaliar com estas funções**.
# 
# - `metricas(prob, y)` — AUC-ROC, AUC-PR (precisão média), F1, precisão, revocação e Brier, com limiar 0,5 (ou outro).
# - `validacao_por_safra(dados, ajusta_e_preve)` — validação cruzada deixando uma safra de fora. Você só precisa
#   escrever uma função `ajusta_e_preve(treino, validacao)` que **treina** no primeiro data frame e **devolve as
#   probabilidades** de `ocorrencia = 1` para o segundo.
# - `registra_resultado(nome, ...)` — guarda os resultados em `resultados_modelos.csv` para juntar no artigo.

auc_roc <- function(prob, y) {
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  (sum(rank(prob)[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)   # Mann-Whitney
}

auc_pr <- function(prob, y) {                                 # precisão média (average precision)
  if (sum(y == 1) == 0) return(NA_real_)
  o <- order(prob, decreasing = TRUE); y <- y[o]
  precisao <- cumsum(y) / seq_along(y)
  sum(precisao[y == 1]) / sum(y == 1)
}

metricas <- function(prob, y, limiar = 0.5) {
  pred <- as.integer(prob >= limiar)
  vp <- sum(pred == 1 & y == 1); fp <- sum(pred == 1 & y == 0); fn <- sum(pred == 0 & y == 1)
  precisao  <- if (vp + fp == 0) 0 else vp / (vp + fp)
  revocacao <- if (vp + fn == 0) 0 else vp / (vp + fn)
  tibble(auc_roc = auc_roc(prob, y), auc_pr = auc_pr(prob, y),
         f1 = if (precisao + revocacao == 0) 0 else 2 * precisao * revocacao / (precisao + revocacao),
         precisao = precisao, revocacao = revocacao,
         brier = mean((prob - y)^2), prevalencia = mean(y))
}

validacao_por_safra <- function(dados, ajusta_e_preve, limiar = 0.5) {
  safras <- sort(unique(dados$safra))
  por_fold <- map_dfr(safras, function(s) {
    tr <- dados[dados$safra != s, ]; va <- dados[dados$safra == s, ]
    prob <- ajusta_e_preve(tr, va)
    metricas(prob, va$ocorrencia, limiar) |> mutate(safra_validacao = s, .before = 1)
  })
  print(por_fold)
  cat("\nMédia das safras:\n")
  print(por_fold |> summarise(across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE), 3))))
  invisible(por_fold)
}

ARQ_RESULTADOS <- file.path(PASTA, "resultados_modelos.csv")
registra_resultado <- function(modelo, autor, etapa, tabela_metricas) {
  linha <- tabela_metricas |>
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) |>
    mutate(modelo = modelo, autor = autor, etapa = etapa, data_hora = format(Sys.time()), .before = 1)
  write_csv(linha, ARQ_RESULTADOS, append = file.exists(ARQ_RESULTADOS))
  linha
}

# ### 7.4 Modelo de referência (linha de base) — só calendário e região
# 
# Antes de qualquer modelo com clima, é preciso saber **quanto se acerta só com a época do ano e o local**.
# Uma regressão logística com `semana` (curva quadrática) e `ponto`, **sem nenhuma variável climática**, é a
# linha de base. Um modelo com clima só é útil se **superar** esta referência.

baseline_calendario <- function(tr, va) {
  m <- glm(ocorrencia ~ poly(semana, 2) + ponto, data = tr, family = binomial)
  predict(m, newdata = va, type = "response")
}

cv_baseline <- validacao_por_safra(treino, baseline_calendario)
registra_resultado("Baseline: calendário + ponto (logística)", "grupo", "cv_treino", cv_baseline)

# ---
# ## 8. Área de treinamento — cada membro
# 
# **Antes de começar:** `Arquivo → Salvar uma cópia no Drive` e renomeie (ex.: `AP2_floresta_aleatoria_Joao`).
# Deixe `PULAR_PREPROCESSAMENTO <- TRUE` (Seção 0) e rode até a Seção 7 (`Ambiente de execução → Executar antes` a partir daqui) e escreva o seu modelo abaixo.
# 
# **Regras para manter os resultados comparáveis**
# 
# 1. Use só o data frame `treino` para treinar e ajustar; a avaliação é com `validacao_por_safra()`.
# 2. Escolha os atributos a partir de `ATR_CLIMA`, `ATR_CONTEXTO` (ou um subconjunto). Lembre-se da AP1: há forte
#    colinearidade entre temperatura média/mín./máx./orvalho e entre umidade e DPV — modelos lineares sofrem com isso.
# 3. Trate o desbalanceamento (pesos de classe, limiar diferente de 0,5, reamostragem **só dentro do treino**).
# 4. Compare com a **linha de base** da Seção 7.4.
# 5. Só no fim, com o modelo escolhido, avalie **uma vez** no `teste` (safra 2025/26).
# 6. Registre os resultados com `registra_resultado()` e baixe o `resultados_modelos.csv`.
# 
# **Sugestões de modelos e pacotes** (instale com `install.packages()` se faltar)
# 
# | Modelo | Pacote / função |
# |---|---|
# | Regressão logística (com razões de chance e IC 95%) | `glm(family = binomial)`, `confint()` |
# | Logística regularizada (Lasso/Ridge) | `glmnet` |
# | Árvore de decisão | `rpart` |
# | Floresta aleatória | `ranger` ou `randomForest` |
# | Gradient boosting | `xgboost` |
# | Modelo de contagem (comparação com regressão) | `glm(family = poisson)`, `MASS::glm.nb` com alvo `n_casos` |

# ===== MODELO DO MEMBRO ======================================================
# Exemplo de estrutura (troque pelo seu modelo). Este exemplo é uma logística
# simples com 3 variáveis climáticas, só para mostrar como usar as funções.

MEU_NOME   <- "seu_nome"
MEU_MODELO <- "Logística: exemplo com 3 atributos"

meu_modelo <- function(tr, va) {
  m <- glm(ocorrencia ~ horas_ur90_14d + chuva_total_14d + temp_media_14d + semana,
           data = tr, family = binomial)
  predict(m, newdata = va, type = "response")
}

cv_meu <- validacao_por_safra(treino, meu_modelo)
registra_resultado(MEU_MODELO, MEU_NOME, "cv_treino", cv_meu)

# ===== AVALIAÇÃO FINAL NO TESTE (rode só uma vez, com o modelo escolhido) =====
# prob_teste <- meu_modelo(treino, teste)
# res_teste  <- metricas(prob_teste, teste$ocorrencia)
# res_teste
# registra_resultado(MEU_MODELO, MEU_NOME, "teste_2025_26", res_teste)

# ---
# ## 9. Os três modelos escolhidos
# 
# | # | Modelo | Por que foi escolhido |
# |---|---|---|
# | 1 | **Regressão logística regularizada (elastic net)** | Interpretável (razões de chance), lida com a forte colinearidade vista na AP1 (temperaturas; umidade × DPV) e liga o ML à parte de estatística inferencial. |
# | 2 | **Floresta aleatória** (`ranger`) | Captura relações não lineares e interações (ex.: chuva só importa com temperatura favorável), é robusta com poucos dados e quase não precisa de ajuste. |
# | 3 | **Gradient boosting** (`xgboost`) | Em geral é o mais preciso em dados tabulares; serve para medir o "teto" de desempenho com estes atributos. |
# 
# **Regras comuns aos três**
# 
# - Mesmos dados (`treino`), mesma validação (deixando uma safra de fora) e mesmas métricas (Seção 7.3).
# - **Desbalanceamento:** os casos positivos recebem peso maior (peso = nº negativos / nº positivos), para que o
#   limiar de 0,5 faça sentido.
# - **Dois conjuntos de atributos** para cada modelo:
#   - `clima` — só os atributos climáticos defasados (`ATR_CLIMA`);
#   - `clima + contexto` — clima + ponto, semana da safra e casos anteriores (`ATR_CONTEXTO`).
# 
#   Comparar os dois com a linha de base (só calendário + ponto) responde à pergunta central do artigo:
#   **o clima acrescenta informação além da época do ano e da região?**

# ---- 9.0 Pacotes e funções auxiliares dos modelos -------------------------
for (p in c("glmnet", "ranger", "xgboost")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(glmnet); library(ranger); library(xgboost)

set.seed(42)
NIVEIS_PONTO <- sort(unique(tabela$ponto))

# Matriz numérica de atributos (ponto vira variáveis indicadoras com níveis fixos)
matriz_x <- function(d, atributos) {
  d <- as.data.frame(d[, atributos, drop = FALSE])
  if ("ponto" %in% atributos) d$ponto <- factor(d$ponto, levels = NIVEIS_PONTO)
  model.matrix(~ . , data = d)[, -1, drop = FALSE]
}

# Peso de cada linha: positivos pesam (nº negativos / nº positivos)
pesos_classe <- function(y) ifelse(y == 1, sum(y == 0) / sum(y == 1), 1)

CONJUNTOS <- list("clima" = ATR_CLIMA, "clima + contexto" = c(ATR_CLIMA, ATR_CONTEXTO))

# ### 9.1 Regressão logística regularizada (elastic net)
# 
# A penalização *elastic net* mistura Lasso (zera coeficientes de atributos inúteis) e Ridge (divide o peso entre
# atributos correlacionados). Os atributos são padronizados automaticamente pelo `glmnet`.
# 
# - `alpha = 0.5` — metade Lasso, metade Ridge;
# - `lambda` (força da penalização) é escolhido **dentro de cada fold**, por validação cruzada interna também
#   deixando uma safra de fora (`foldid = safra`). Assim, a safra de validação nunca participa da escolha.

ajusta_logistica <- function(atributos, alpha = 0.5) {
  function(tr, va) {
    x  <- matriz_x(tr, atributos)
    fold_interno <- as.integer(factor(tr$safra))
    cv <- cv.glmnet(x, tr$ocorrencia, family = "binomial", alpha = alpha,
                    weights = pesos_classe(tr$ocorrencia), foldid = fold_interno,
                    type.measure = "deviance")
    as.numeric(predict(cv, newx = matriz_x(va, atributos), s = "lambda.1se", type = "response"))
  }
}

cv_log <- list()
for (nome in names(CONJUNTOS)) {
  cat("\n=== Logística elastic net —", nome, "===\n")
  cv_log[[nome]] <- validacao_por_safra(treino, ajusta_logistica(CONJUNTOS[[nome]]))
  registra_resultado(paste("Logística elastic net —", nome), "grupo", "cv_treino", cv_log[[nome]])
}

# **Coeficientes (modelo ajustado em todo o treino).** Como os atributos são padronizados internamente, os
# coeficientes são devolvidos na escala original; `exp(coef)` é a **razão de chances** por unidade do atributo.
# Coeficiente 0 = atributo descartado pela penalização. Para intervalos de confiança e *p*-valores (estatística
# inferencial), ajuste um `glm()` comum só com os atributos que sobraram.

x_tr   <- matriz_x(treino, CONJUNTOS[["clima + contexto"]])
cv_fin <- cv.glmnet(x_tr, treino$ocorrencia, family = "binomial", alpha = 0.5,
                    weights = pesos_classe(treino$ocorrencia),
                    foldid = as.integer(factor(treino$safra)))
coefs <- as.matrix(coef(cv_fin, s = "lambda.1se"))
tibble(atributo = rownames(coefs), coef = coefs[, 1]) |>
  filter(atributo != "(Intercept)", coef != 0) |>
  mutate(razao_chances = exp(coef)) |>
  arrange(desc(abs(coef)))

# ### 9.2 Floresta aleatória (`ranger`)
# 
# Cada árvore é treinada numa amostra *bootstrap* e, em cada divisão, só considera `mtry` atributos sorteados.
# A probabilidade final é a média das árvores. Uma pequena grade de hiperparâmetros é testada com a mesma validação:
# 
# - `mtry` — atributos sorteados por divisão (menor = árvores mais diferentes entre si);
# - `min.node.size` — tamanho mínimo das folhas (maior = árvores mais rasas, menos sobreajuste).
# 
# > Escolher a melhor combinação pela mesma validação cruzada deixa a métrica um pouco otimista — por isso a
# > avaliação final é no `teste` (safra 2025/26).

ajusta_floresta <- function(atributos, mtry = 5, min_no = 10, arvores = 500) {
  function(tr, va) {
    d <- as.data.frame(tr[, atributos]); d$y <- factor(tr$ocorrencia, levels = c(0, 1))
    if ("ponto" %in% atributos) d$ponto <- factor(d$ponto, levels = NIVEIS_PONTO)
    nv <- as.data.frame(va[, atributos])
    if ("ponto" %in% atributos) nv$ponto <- factor(nv$ponto, levels = NIVEIS_PONTO)
    m <- ranger(y ~ ., data = d, probability = TRUE, num.trees = arvores,
                mtry = min(mtry, length(atributos)), min.node.size = min_no,
                case.weights = pesos_classe(tr$ocorrencia), seed = 42)
    predict(m, data = nv)$predictions[, "1"]
  }
}

# Grade pequena (≈ 1 min). Métrica de escolha: AUC-PR média nas safras.
grade_rf <- tidyr::crossing(mtry = c(3, 6, 12), min_no = c(5, 20))
cv_rf <- list(); melhor_rf <- list()
for (nome in names(CONJUNTOS)) {
  res <- grade_rf |> mutate(auc_pr = pmap_dbl(list(mtry, min_no), function(mt, mn) {
    r <- suppressWarnings(capture.output(
      v <- validacao_por_safra(treino, ajusta_floresta(CONJUNTOS[[nome]], mt, mn))))
    mean(v$auc_pr, na.rm = TRUE)
  }))
  melhor_rf[[nome]] <- slice_max(res, auc_pr, n = 1, with_ties = FALSE)
  cat("\n=== Floresta aleatória —", nome, "| melhor:", "mtry =", melhor_rf[[nome]]$mtry,
      "min.node.size =", melhor_rf[[nome]]$min_no, "===\n")
  cv_rf[[nome]] <- validacao_por_safra(
    treino, ajusta_floresta(CONJUNTOS[[nome]], melhor_rf[[nome]]$mtry, melhor_rf[[nome]]$min_no))
  registra_resultado(paste("Floresta aleatória —", nome), "grupo", "cv_treino", cv_rf[[nome]])
}

# **Importância dos atributos (permutação):** quanto o desempenho cai quando os valores de um atributo são
# embaralhados. Útil para discutir as hipóteses H1 (janela de 7 × 14 dias) e H2 (`horas_ur90` × umidade média).

atr <- CONJUNTOS[["clima + contexto"]]
d <- as.data.frame(treino[, atr]); d$ponto <- factor(d$ponto, levels = NIVEIS_PONTO)
d$y <- factor(treino$ocorrencia)
rf_final <- ranger(y ~ ., data = d, probability = TRUE, num.trees = 500,
                   mtry = melhor_rf[["clima + contexto"]]$mtry,
                   min.node.size = melhor_rf[["clima + contexto"]]$min_no,
                   case.weights = pesos_classe(treino$ocorrencia),
                   importance = "permutation", seed = 42)

imp_rf <- tibble(atributo = names(rf_final$variable.importance),
                 importancia = rf_final$variable.importance) |>
  arrange(desc(importancia))

g_imp <- imp_rf |> slice_head(n = 15) |>
  ggplot(aes(importancia, reorder(atributo, importancia))) +
  geom_col(fill = "#2a9d8f") +
  labs(x = "Importância por permutação", y = NULL,
       title = "Floresta aleatória — 15 atributos mais importantes") + tema
g_imp
ggsave(file.path(PASTA_FIG, "fig_importancia_rf.png"), g_imp, width = 8, height = 5, dpi = 200)

# ### 9.3 Gradient boosting (`xgboost`)
# 
# As árvores são construídas **em sequência**, cada uma corrigindo os erros das anteriores. Com poucos dados
# (910 linhas de treino), o risco de sobreajuste é alto, então a grade usa árvores rasas e aprendizado lento:
# 
# - `max_depth` — profundidade máxima (2 ou 3);
# - `nrounds` — número de árvores (100 ou 300), com `eta = 0.05` (taxa de aprendizado);
# - `subsample` e `colsample_bytree = 0.8` — cada árvore vê 80% das linhas e dos atributos;
# - `scale_pos_weight` — o peso dos positivos (desbalanceamento).

ajusta_xgb <- function(atributos, max_depth = 2, nrounds = 200, eta = 0.05) {
  function(tr, va) {
    dtr <- xgb.DMatrix(matriz_x(tr, atributos), label = tr$ocorrencia)
    params <- list(objective = "binary:logistic", eval_metric = "aucpr",
                   max_depth = max_depth, eta = eta, subsample = 0.8, colsample_bytree = 0.8,
                   min_child_weight = 3,
                   scale_pos_weight = sum(tr$ocorrencia == 0) / sum(tr$ocorrencia == 1),
                   nthread = 2)
    set.seed(42)
    m <- xgb.train(params = params, data = dtr, nrounds = nrounds, verbose = 0)
    predict(m, xgb.DMatrix(matriz_x(va, atributos)))
  }
}

grade_xgb <- tidyr::crossing(max_depth = c(2, 3), nrounds = c(100, 300))
cv_xgb <- list(); melhor_xgb <- list()
for (nome in names(CONJUNTOS)) {
  res <- grade_xgb |> mutate(auc_pr = pmap_dbl(list(max_depth, nrounds), function(md, nr) {
    r <- capture.output(v <- validacao_por_safra(treino, ajusta_xgb(CONJUNTOS[[nome]], md, nr)))
    mean(v$auc_pr, na.rm = TRUE)
  }))
  melhor_xgb[[nome]] <- slice_max(res, auc_pr, n = 1, with_ties = FALSE)
  cat("\n=== XGBoost —", nome, "| melhor: max_depth =", melhor_xgb[[nome]]$max_depth,
      "nrounds =", melhor_xgb[[nome]]$nrounds, "===\n")
  cv_xgb[[nome]] <- validacao_por_safra(
    treino, ajusta_xgb(CONJUNTOS[[nome]], melhor_xgb[[nome]]$max_depth, melhor_xgb[[nome]]$nrounds))
  registra_resultado(paste("XGBoost —", nome), "grupo", "cv_treino", cv_xgb[[nome]])
}

# ## 10. Comparação dos modelos (validação cruzada no treino)
# 
# A tabela e o gráfico mostram a **média nas 5 safras de validação**. O ponto de referência é a linha de base
# (só calendário + ponto, Seção 7.4).

todos_cv <- bind_rows(
  cv_baseline |> mutate(modelo = "Linha de base", conjunto = "calendário + ponto"),
  bind_rows(lapply(names(cv_log), function(n) cv_log[[n]] |> mutate(modelo = "Logística EN", conjunto = n))),
  bind_rows(lapply(names(cv_rf),  function(n) cv_rf[[n]]  |> mutate(modelo = "Floresta aleatória", conjunto = n))),
  bind_rows(lapply(names(cv_xgb), function(n) cv_xgb[[n]] |> mutate(modelo = "XGBoost", conjunto = n)))
)

comparacao <- todos_cv |>
  group_by(modelo, conjunto) |>
  summarise(across(c(auc_roc, auc_pr, f1, precisao, revocacao, brier), ~ round(mean(.x, na.rm = TRUE), 3)),
            .groups = "drop") |>
  arrange(desc(auc_pr))
comparacao
write_csv(comparacao, file.path(PASTA, "comparacao_modelos_cv.csv"))

g_cmp <- todos_cv |>
  mutate(rotulo = paste0(modelo, "\n(", conjunto, ")")) |>
  ggplot(aes(reorder(rotulo, auc_pr, FUN = mean), auc_pr)) +
  geom_boxplot(fill = "#e9c46a", outlier.shape = NA) +
  geom_jitter(aes(colour = safra_validacao), width = 0.1, size = 2) +
  geom_hline(yintercept = mean(treino$ocorrencia), linetype = "dashed") +
  coord_flip() +
  labs(x = NULL, y = "AUC-PR por safra de validação", colour = "Safra",
       title = "Comparação dos modelos (linha tracejada = modelo aleatório)") + tema
g_cmp
ggsave(file.path(PASTA_FIG, "fig_comparacao_modelos.png"), g_cmp, width = 9, height = 6, dpi = 200)

# **Teste estatístico entre modelos.** Com só 5 safras, compare o melhor modelo com a linha de base usando o
# **teste de Wilcoxon pareado** nas AUC-PR por safra (mesma safra = par). Com *n* = 5, o menor *p* possível é
# 0,0625 — reporte isso como limitação em Ameaças à Validade, e apoie a discussão também no teste final.

melhor <- comparacao |> filter(modelo != "Linha de base") |> slice_max(auc_pr, n = 1, with_ties = FALSE)
cat("Melhor modelo na validação:", melhor$modelo, "—", melhor$conjunto, "\n")
a <- todos_cv |> filter(modelo == melhor$modelo, conjunto == melhor$conjunto) |> arrange(safra_validacao)
b <- todos_cv |> filter(modelo == "Linha de base") |> arrange(safra_validacao)
wilcox.test(a$auc_pr, b$auc_pr, paired = TRUE, exact = TRUE)

# ## 11. Avaliação final no teste (safra 2025/26)
# 
# Cada modelo é treinado com **todo o treino** (2020/21–2024/25), com os hiperparâmetros escolhidos acima, e avaliado
# **uma única vez** na safra 2025/26. Lembre que 2025/26 foi a safra mais seca e com mais casos da série (AP1) — é um
# teste difícil e honesto de generalização.

ajustadores_finais <- list(
  "Linha de base (calendário + ponto)" = baseline_calendario,
  "Logística EN — clima + contexto"    = ajusta_logistica(CONJUNTOS[["clima + contexto"]]),
  "Floresta aleatória — clima + contexto" = ajusta_floresta(CONJUNTOS[["clima + contexto"]],
      melhor_rf[["clima + contexto"]]$mtry, melhor_rf[["clima + contexto"]]$min_no),
  "XGBoost — clima + contexto" = ajusta_xgb(CONJUNTOS[["clima + contexto"]],
      melhor_xgb[["clima + contexto"]]$max_depth, melhor_xgb[["clima + contexto"]]$nrounds)
)

prob_teste <- lapply(ajustadores_finais, function(f) f(treino, teste))
res_teste <- bind_rows(lapply(names(prob_teste), function(n)
  metricas(prob_teste[[n]], teste$ocorrencia) |> mutate(modelo = n, .before = 1)))
res_teste |> mutate(across(where(is.numeric), ~ round(.x, 3)))

for (n in names(prob_teste)) registra_resultado(n, "grupo", "teste_2025_26", res_teste |> filter(modelo == n))
write_csv(res_teste, file.path(PASTA, "resultados_teste_2025_26.csv"))

# Curvas precisão-revocação no teste
curva_pr <- function(prob, y) {
  o <- order(prob, decreasing = TRUE); y <- y[o]
  tibble(revocacao = cumsum(y) / sum(y), precisao = cumsum(y) / seq_along(y))
}
g_pr <- bind_rows(lapply(names(prob_teste), function(n)
  curva_pr(prob_teste[[n]], teste$ocorrencia) |> mutate(modelo = n))) |>
  ggplot(aes(revocacao, precisao, colour = modelo)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = mean(teste$ocorrencia), linetype = "dashed") +
  labs(x = "Revocação", y = "Precisão", colour = NULL,
       title = "Curvas precisão-revocação — teste (safra 2025/26)") +
  tema + theme(legend.position = "bottom", legend.direction = "vertical")
g_pr
ggsave(file.path(PASTA_FIG, "fig_curva_pr_teste.png"), g_pr, width = 8, height = 6, dpi = 200)
