# =============================================================
# Gera um relatório HTML com todos os gráficos para
# cada local listado abaixo. Basta editar a tabela "locais".
# Requer: relatorio_clima.Rmd na mesma pasta.
# =============================================================
# install.packages(c("tidyverse", "nasapower", "rmarkdown", "knitr", "lubridate"))
library(tidyverse)
library(rmarkdown)

# ---- EDITE AQUI: um local por linha -------------------------
locais <- tribble(
  ~local,         ~lon,    ~lat,
  "Curitiba",     -49.27,  -25.43,
  "Londrina",     -51.16,  -23.31,
  "Tibagi",       -50.41,  -24.50,
  "Guarapuava",   -51.45,  -25.39,
  "Cascavel",     -53.46,  -24.96,
  "Ponta Grossa", -50.16,  -25.09
)

# Início do período; o fim é sempre "hoje" (menos ~7 dias de latência da NASA)
inicio <- "2020-01-01"

dir.create("relatorios", showWarnings = FALSE)

# "Ponta Grossa" -> "ponta_grossa" (sem acentos, para nome de arquivo)
slug <- function(x) {
  x |> iconv(to = "ASCII//TRANSLIT") |> tolower() |>
    str_replace_all("[^a-z0-9]+", "_") |> str_remove_all("^_|_$")
}

pwalk(locais, function(local, lon, lat) {
  s <- slug(local)
  render(
    input       = "relatorio_clima.Rmd",
    output_file = paste0("relatorio_", s, ".html"),
    output_dir  = "relatorios",
    params      = list(local = local, lon = lon, lat = lat,
                       slug = s,
                       pasta = file.path("dados", s),   # dados/<local>/{horario,diario,graficos}
                       inicio = inicio),
    envir       = new.env(),
    quiet       = TRUE
  )
  message("Pronto: relatorios/relatorio_", s, ".html")
})