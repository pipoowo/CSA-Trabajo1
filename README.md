# Reporte de reproducibilidad

Link al reporte [**AQUÍ**](https://pipoowo.github.io/CSA-trabajo1/index.html)

Este repositorio contiene una plantilla para el reporte de reproducibilidad del Trabajo 1 del curso [Investigación Social Abierta](https://cienciasocialabierta.cl/2026/). La plantilla está diseñada para ser clonada y modificada por cada estudiante, siguiendo el protocolo [IPO](https://lisacoes.com/protocolos/a-ipo-rep/) (IInput-Processing-Output) y utilizando el formato Quarto.

<img src="https://lisacoes.com/protocolos/a-ipo-rep/ipo-hex.png" alt="IPO" width="220" />

## Working tree del proyecto

Este proyecto se organiza de la siguiente manera: 

<!-- WORKING_TREE_START -->
```text
CSA-Trabajo1/
 |- .vscode/
 |  |- settings.json
 |- README.md
 |- anidados/
 |  |- 1-seleccion-articulo.qmd
 |  |- 2-eval-reprod.qmd
 |  |- 3-analisis-resultado-reproducir.qmd
 |  |- 4-reprod-procesamiento.qmd
 |  |- 5-reprod-fig.qmd
 |  |- 6-conclusiones.qmd
 |  |- 7-recomendaciones.qmd
 |  |- 8-referencias.qmd
 |  |- 9-apendice.qmd
 |- index.html
 |- index.pdf
 |- index.qmd
 |- input/
 |  |- bib/
 |  |- data/
 |  |  |- original/
 |  |  |- proc/
 |  |- images/
 |  |- original-code/
 |- libs/
 |  |- ocs.scss
 |- output/
 |  |- graphs/
 |  |- tables/
 |- processing/
 |  |- README-prod.md
 |  |- prod_analysis.Rmd
 |  |- prod_analysis.html
 |  |- prod_prep.Rmd
 |  |- prod_prep.html
 |- scripts/
 |  |- update-working-tree.sh
```
<!-- WORKING_TREE_END -->

Este working tree incorpora las carpetas y archivos principales relevantes del repo (omite algunas) y se actualiza automáticamente al hacer commit mediante una github action que se encuentra definida en el archivo `.github/workflows/update-working-tree.yml`. El propósito de esta acción es mantener un registro actualizado de la estructura del proyecto, lo que facilita la navegación y organización de los archivos para los estudiantes.


