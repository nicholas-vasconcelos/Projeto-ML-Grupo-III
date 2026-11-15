# AP2 — Previsão semanal da ferrugem-asiática da soja no Paraná

[![Abrir no Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/nicholas-vasconcelos/Projeto-ML-Grupo-III/blob/master/ap2/AP2_preparacao_dados.ipynb)

Notebook em **R** (Google Colab) que monta a base da AP2 e treina os modelos de classificação:
dado o clima das semanas anteriores, haverá caso de ferrugem-asiática a até 75 km de um ponto climático naquela semana?

## Como usar

1. Clique em **Abrir no Colab** acima e depois em `Arquivo → Salvar uma cópia no Drive`.
2. Confira que o ambiente de execução é **R**.
3. Rode as células em ordem.
   - `PULAR_PREPROCESSAMENTO <- TRUE` (padrão): baixa a tabela pronta desta pasta e pula a coleta — tudo roda em segundos.
   - `PULAR_PREPROCESSAMENTO <- FALSE`: refaz a coleta pelas APIs do Consórcio Antiferrugem e do NASA POWER (~15 min).

## Conteúdo

| Caminho | Descrição |
|---|---|
| `AP2_preparacao_dados.ipynb` | o notebook (coleta → tabela ponto × semana → 3 modelos → teste final) |
| `dados/tabela_ponto_semana.csv` | base de treinamento: 7 pontos × 6 safras × 26 semanas = 1.092 linhas |
| `dados/ocorrencias_pr_2020_2026.csv` | casos de ferrugem do Consórcio Antiferrugem, safras 2020/21 a 2025/26 |
| `dados/clima_diario_7_pontos.csv` | clima diário (NASA POWER, agregado de dados horários) dos 7 pontos |
| `fonte/` | arquivos-fonte do notebook; edite os `.Rnb.txt` e rode `python construir_notebook.py` para gerar o `.ipynb` e o `.R` |

## Modelos

Regressão logística elastic net (`glmnet`), floresta aleatória (`ranger`) e XGBoost (`xgboost`), comparados com uma
linha de base que usa só calendário e região. Validação cruzada deixando uma safra de fora (2020/21–2024/25) e
teste final na safra 2025/26.
