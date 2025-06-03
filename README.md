# 📊 Análise de Estoque de Medicamentos — Shiny App

Este aplicativo interativo desenvolvido com **Shiny** permite a análise e visualização dos estoques de medicamentos em unidades de saúde de um determinado distrito, com base em dados provenientes do arquivo `medicamentos_distrito03.csv`.

## 🚀 Funcionalidades

- **Dashboard interativo** com:
  - Filtros por unidade, classe, apresentação, tipo de produto, quantidade e nome do produto.
  - Indicadores resumidos:
    - Total de medicamentos listados.
    - Total de unidades de saúde.
    - Produto com maior quantidade em estoque.
  - Visualizações gráficas:
    - Gráfico de barras com os 10 produtos com maior quantidade.
    - Gráfico de pizza com proporção por apresentação.
    - Gráfico condicional mostrando quantidade por unidade ou top 10 produtos.

- **Tabela de dados filtrados** com possibilidade de download em CSV.
- **Design responsivo** utilizando `shinydashboard`.

## 🗃️ Estrutura dos Dados

O arquivo CSV é reestruturado para conter as seguintes colunas:

- `unidade`: Nome da unidade de saúde  
- `classe`: Classificação da unidade  
- `apresentacao`: Apresentação do medicamento  
- `tipo_produto`: Tipo do produto  
- `codigo_produto`: Código identificador do produto  
- `produto`: Nome do produto  
- `cadum`: Código adicional  
- `quantidade`: Quantidade disponível (numérica)

> Linhas com valores ausentes na quantidade são automaticamente excluídas.

## 🧰 Tecnologias e Pacotes Utilizados

- [Shiny](https://shiny.rstudio.com/)
- [shinydashboard](https://rstudio.github.io/shinydashboard/)
- [dplyr](https://dplyr.tidyverse.org/)
- [ggplot2](https://ggplot2.tidyverse.org/)
- [DT](https://rstudio.github.io/DT/)
- [scales](https://scales.r-lib.org/)

## 📂 Como Executar

1. Certifique-se de ter o R e o RStudio instalados.
2. Instale os pacotes necessários (caso ainda não estejam instalados):
   ```R
   install.packages(c("shiny", "shinydashboard", "dplyr", "ggplot2", "DT", "scales"))
3. Rode o arquivo (versão beta.R)
