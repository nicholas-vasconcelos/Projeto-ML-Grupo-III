"""Gera AP2_preparacao_dados.ipynb (Colab, kernel R) e ap2_preparacao.R
a partir de ap2_preparacao.Rnb.txt + ap2_modelos.Rnb.txt.

Marcadores de célula nos arquivos-fonte:
  #%% md        -> célula de texto (markdown)
  #%% code      -> célula de código
  #%% code pre   -> célula de pré-processamento: é envolvida em
                    if (PULAR_PREPROCESSAMENTO) {...} else {...} para ser pulada no modo rápido
"""
import json
import re
from pathlib import Path

aqui = Path(__file__).parent
FONTES = ["ap2_preparacao.Rnb.txt", "ap2_modelos.Rnb.txt"]
src = "\n".join((aqui / f).read_text(encoding="utf-8") for f in FONTES)

GUARDA_INICIO = (
    "if (PULAR_PREPROCESSAMENTO) {\n"
    '  cat("Pulado (modo rápido): dados já baixados do GitHub na Seção 0.1.\\n")\n'
    "} else {\n\n"
)
GUARDA_FIM = "\n\n}"


def linhas_de(txt):
    linhas = [l + "\n" for l in txt.split("\n")]
    linhas[-1] = linhas[-1].rstrip("\n")
    return linhas


partes = re.split(r"^#%% (md|code pre|code)\n", src, flags=re.M)[1:]
celulas, rscript = [], []
for tipo, txt in zip(partes[::2], partes[1::2]):
    txt = txt.strip("\n")
    if tipo == "md":
        celulas.append({"cell_type": "markdown", "metadata": {}, "source": linhas_de(txt)})
        rscript.append("\n".join("# " + l for l in txt.split("\n")))
        continue
    if tipo == "code pre":
        txt = GUARDA_INICIO + txt + GUARDA_FIM
    celulas.append({"cell_type": "code", "metadata": {}, "execution_count": None,
                    "outputs": [], "source": linhas_de(txt)})
    rscript.append(txt)

nb = {"cells": celulas, "nbformat": 4, "nbformat_minor": 0,
      "metadata": {"kernelspec": {"name": "ir", "display_name": "R", "language": "R"},
                   "language_info": {"name": "R"}, "colab": {"provenance": []}}}
(aqui.parent / "AP2_preparacao_dados.ipynb").write_text(
    json.dumps(nb, ensure_ascii=False, indent=1), encoding="utf-8")
(aqui / "ap2_preparacao.R").write_text("\n\n".join(rscript) + "\n", encoding="utf-8")
print(len(celulas), "celulas")
