install.packages("nasapower")
install.packages("tidyverse")
library(nasapower)
library(tidyverse)

# Local: Paraná (ponto de referência = Curitiba, capital do estado)
# O NASA POWER entrega dados de uma grade (~0,5° x 0,5°), então precisamos
# escolher UM ponto. Para outra cidade do Paraná, troque as coordenadas:
#   Londrina  c(-51.16, -23.31) | Maringá c(-51.94, -23.42)
#   Cascavel  c(-53.46, -24.96) | Ponta Grossa c(-50.16, -25.09)
local_parana <- c(-49.27, -25.43)   # c(longitude, latitude)

clima <- get_power(
  community    = "ag",
  lonlat       = local_parana,
  pars         = c("T2M",                 # temperatura média (°C)
                   "T2M_MAX",             # temperatura máxima (°C)
                   "T2M_MIN",             # temperatura mínima (°C)
                   "PRECTOTCORR",         # chuva (mm/dia)
                   "RH2M",                # umidade relativa (%)
                   "ALLSKY_SFC_SW_DWN",   # radiação solar (MJ/m²/dia)
                   "WS2M"),               # vento a 2 m (m/s)
  dates        = c("2020-01-01", "2024-12-31"),
  temporal_api = "daily"
)

write_csv(clima, "clima_parana.csv")
