# Clima do Paraná: NASA POWER

Projeto do **Grupo III** de Aprendizado de Máquina. O objetivo é treinar um modelo capaz de **prever surtos de ferrugem-asiática da soja no Paraná**, cruzando o clima do [NASA POWER](https://power.larc.nasa.gov/) com um conjunto de dados de ocorrências da ferrugem no estado.

Este repositório contém a etapa de **coleta e análise dos dados climáticos**: para cada local, o código baixa os dados horários do NASA POWER (de 2020 até a data mais recente disponível), agrega por dia e gera um relatório HTML com estatística descritiva, gráficos, teste de normalidade e teste de correlação, tudo em R.

*Última atualização deste README: 10 de outubro de 2026.*

## Integrantes

- Nicholas Borges de Vasconcelos
- Mateus Coelho Sachino
- Luca Borges Confente
- Bernardo Ottan Procópio

## Estado atual do projeto

O projeto está na **fase de coleta de dados**. Ainda não há modelo treinado.

## Estrutura de arquivos

```
.
├── README.md
├── gerar_relatorios.R          # script que você executa (edite a lista de locais aqui)
├── relatorio_clima.Rmd         # template do relatório (recebe local, lon, lat)
├── dados/
│   └── <local>/                # um por local, ex.: curitiba/, londrina/
│       ├── horario/            # clima_<local>_horario.csv  (dados brutos da NASA, cache)
│       ├── diario/             # clima_<local>_diario.csv   (dados agregados por dia)
│       └── graficos/           # <local>_01_histograma_temperatura.png, ... (9 imagens)
├── relatorios/
│   └── relatorio_<local>.html  # relatório final de cada local
└── (arquivos antigos, ver abaixo)
```

As pastas `dados/` e `relatorios/` são criadas automaticamente na primeira execução.

### Gráficos gerados por local (`dados/<local>/graficos/`)

| Arquivo | Conteúdo |
| --- | --- |
| `<local>_01_histograma_temperatura.png` | Distribuição da temperatura média diária |
| `<local>_02_dias_por_intensidade_chuva.png` | Dias por classe de chuva |
| `<local>_03_boxplot_temperatura_mensal.png` | Temperatura média por mês |
| `<local>_04_serie_temporal_temperatura.png` | Temperatura diária e média mensal ao longo do tempo |
| `<local>_05_dispersao_radiacao_temperatura.png` | Radiação solar × temperatura |
| `<local>_06_temperaturas_max_media_min.png` | Temperaturas máxima, média e mínima por mês |
| `<local>_07_vento_mensal.png` | Velocidade do vento por mês (média e maior valor horário do dia) |
| `<local>_08_qqplots_normalidade.png` | Q-Q plots das 9 variáveis |
| `<local>_09_mapa_calor_correlacao.png` | Mapa de calor da correlação de Spearman |

### Arquivos antigos (fluxo anterior)

`baixar_dados.R` e `Analises.R` fazem o mesmo trabalho só para Curitiba, com dados diários de 2020 a 2024, e foram substituídos por `gerar_relatorios.R` + `relatorio_clima.Rmd`. `dataset_firstLook.R` é a primeira exploração, feita com o Rio de Janeiro.

## Como gerar os relatórios

### 1. Pré-requisitos

R (versão 4.1 ou superior, por causa do operador `|>`) e, de preferência, RStudio. Instale os pacotes uma vez:

```r
install.packages(c("tidyverse", "nasapower", "rmarkdown", "knitr", "lubridate"))
```

Para gerar HTML autocontido o `rmarkdown` precisa do [Pandoc](https://pandoc.org/), que já vem com o RStudio. É necessário acesso à internet na primeira execução de cada local, para baixar os dados.

### 2. Escolher os locais

Abra `gerar_relatorios.R` e edite a tabela `locais` (uma linha por local, na ordem nome, **longitude**, **latitude**):

```r
locais <- tribble(
  ~local,         ~lon,    ~lat,
  "Curitiba",     -49.27,  -25.43,
  "Londrina",     -51.16,  -23.31,
  "Maringá",      -51.94,  -23.42
)
```

Para mudar o início do período, edite `inicio <- "2020-01-01"` no mesmo arquivo. O nome do local vira o nome das pastas e arquivos, sem acentos e em minúsculas ("Ponta Grossa" vira `ponta_grossa`).

### 3. Executar

Com o diretório de trabalho na pasta do projeto, rode no RStudio (abrir `gerar_relatorios.R` e clicar em *Source*) ou no terminal:

```bash
Rscript gerar_relatorios.R
```

O script gera, para cada local, o relatório `relatorios/relatorio_<local>.html` e preenche `dados/<local>/`. Abra o HTML em qualquer navegador; ele traz os gráficos embutidos, então pode ser enviado como um arquivo só.

### 4. Atualizar depois

Rode o script de novo: o template baixa só os dias novos e refaz o relatório. Para forçar um novo download completo de um local, apague `dados/<local>/horario/`.

### Gerar um relatório avulso

```r
rmarkdown::render(
  "relatorio_clima.Rmd",
  output_file = "relatorio_maringa.html",
  output_dir  = "relatorios",
  params = list(local = "Maringá", lon = -51.94, lat = -23.42,
                slug = "maringa", pasta = "dados/maringa",
                inicio = "2020-01-01")
)
```

## Como as variáveis são calculadas

| Variável | Origem (dados horários) | Agregação diária |
| --- | --- | --- |
| Temperatura média / máxima / mínima | `T2M` | média / máximo / mínimo das 24 horas |
| Chuva | `PRECTOTCORR` (mm/h) | soma (mm/dia) |
| Umidade | `RH2M` | média |
| Radiação solar | `ALLSKY_SFC_SW_DWN` (MJ/m²/h) | soma (MJ/m²/dia) |
| Vento a 2 m | `WS2M` | média e maior valor horário do dia |
| DPV | calculado de `T2M` e `RH2M` a cada hora | média |
| Ponto de orvalho | calculado de `T2M` e `RH2M` a cada hora | média |

Só entram dias com as 24 horas disponíveis e sem o valor de "sem dado" da NASA (-999). O relatório informa quantos dias foram descartados.

## Problemas comuns

- **"Radiação diária fora do esperado":** o template confere se o máximo diário de radiação está entre 15 e 45 MJ/m²/dia. Se falhar, a unidade horária da NASA pode ter mudado.
- **Erro de coluna ausente no download:** o código aceita as colunas de mês e dia como `MO`/`DY` ou `MM`/`DD`. Se a NASA renomear outras colunas, ajuste a função `padroniza()` no template.
- **Download lento ou recusado:** a API da NASA limita requisições. O template pede um ano por vez com pausa de 2 segundos; se falhar, rode de novo (o que já foi baixado fica no cache só depois de uma execução completa).