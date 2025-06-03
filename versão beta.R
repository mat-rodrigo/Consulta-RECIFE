# app.R

# Carregar as bibliotecas necessárias
library(shiny)
library(dplyr)       # Para manipulação de dados
library(ggplot2)     # Para criação de gráficos
library(DT)          # Para tabelas interativas
library(shinydashboard) # Para um layout de dashboard profissional
library(scales)      # Para formatação de porcentagens no gráfico de pizza

# --- CORREÇÃO NA LEITURA E MAPEAMENTO DOS DADOS ---
# 1. Carrega o arquivo CSV como ele é lido inicialmente pelo R
initial_dados <- read.csv("medicamentos_distrito03.csv", encoding = "UTF-8", stringsAsFactors = FALSE, row.names = NULL)

# 2. Cria um novo dataframe 'dados' mapeando o conteúdo real para os nomes de coluna desejados.
dados <- data.frame(
  unidade = initial_dados$distrito,
  classe = initial_dados$unidade,
  apresentacao = initial_dados$classe,
  tipo_produto = initial_dados$apresentacao,
  codigo_produto = initial_dados$tipo_produto,
  produto = initial_dados$codigo_produto,
  cadum = initial_dados$produto,
  quantidade = initial_dados$cadum,
  stringsAsFactors = FALSE
)

# 3. Garante que a coluna 'quantidade' seja numérica e remove linhas com NA.
dados$quantidade <- as.numeric(dados$quantidade)
dados <- dados %>% filter(!is.na(quantidade))

# --- Interface do Usuário (UI) ---
ui <- dashboardPage(
  # Cabeçalho do Dashboard
  dashboardHeader(title = "Análise de Estoque de Medicamentos"),
  
  # Barra Lateral do Dashboard
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Tabela de Dados", tabName = "tabela", icon = icon("table"))
    )
  ),
  
  # Corpo do Dashboard
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow( # Esta linha contém os filtros e os cards
                # Caixa para os controles de filtro
                box(width = 4, title = "Filtros de Dados", status = "primary", solidHeader = TRUE,
                    selectInput("unidade_selecionada", "Selecionar Unidade(s):",
                                choices = unique(dados$unidade), multiple = TRUE,
                                selected = unique(dados$unidade)),
                    selectInput("classe_selecionada", "Filtrar por Classe:",
                                choices = c("Todas", unique(dados$classe)),
                                selected = "Todas"),
                    selectInput("apresentacao_selecionada", "Filtrar por Apresentação:",
                                choices = c("Todas", unique(dados$apresentacao)),
                                selected = "Todas"),
                    selectInput("tipo_produto_selecionado", "Filtrar por Tipo de Produto:",
                                choices = c("Todos", unique(dados$tipo_produto)),
                                selected = "Todos"),
                    textInput("busca_produto", "Buscar por Produto:", value = ""),
                    sliderInput("quantidade_range", "Filtrar por Quantidade:",
                                min = min(dados$quantidade, na.rm = TRUE),
                                max = max(dados$quantidade, na.rm = TRUE),
                                value = c(min(dados$quantidade, na.rm = TRUE), max(dados$quantidade, na.rm = TRUE)))
                ),
                # Indicadores (Cards) - Value Boxes
                valueBoxOutput("total_medicamentos_card", width = 2),
                valueBoxOutput("total_unidades_card", width = 2),
                valueBoxOutput("produto_maior_quantidade_card", width = 4)
              ),
              fluidRow( # NOVA POSIÇÃO: Gráfico de barras com a quantidade total por Produto
                box(width = 12, title = "Quantidade Total por Produto (Top 10)", status = "info", solidHeader = TRUE,
                    plotOutput("grafico_quantidade_por_produto_total", height = "300px"))
              ),
              fluidRow( # Esta linha contém os gráficos de pizza e o condicional
                # Gráfico de Pizza com proporção por apresentação (Título da caixa REINSERIDO)
                box(width = 6, title = "Proporção por Apresentação", status = "info", solidHeader = TRUE, # TÍTULO DA CAIXA REINSERIDO AQUI
                    plotOutput("grafico_pizza_apresentacao")),
                box(width = 6, title = "Quantidade de Produto por Unidade (ou Top 10 Produtos)", status = "info", solidHeader = TRUE,
                    plotOutput("grafico_barras_quantidade_produto"))
              )
      ),
      tabItem(tabName = "tabela",
              fluidRow(
                box(width = 12, title = "Dados Filtrados", status = "primary", solidHeader = TRUE,
                    downloadButton("download_dados", "Download Dados Filtrados (CSV)"),
                    DTOutput("tabela_dados_filtrados"))
              )
      )
    )
  )
)

# --- Lógica do Servidor (Server) ---
server <- function(input, output, session) {
  
  dados_filtrados <- reactive({
    df <- dados
    
    if (!is.null(input$unidade_selecionada) && length(input$unidade_selecionada) > 0) {
      df <- df %>% filter(unidade %in% input$unidade_selecionada)
    }
    
    if (input$classe_selecionada != "Todas") {
      df <- df %>% filter(classe == input$classe_selecionada)
    }
    
    if (input$apresentacao_selecionada != "Todas") {
      df <- df %>% filter(apresentacao == input$apresentacao_selecionada)
    }
    
    if (input$tipo_produto_selecionado != "Todos") {
      df <- df %>% filter(tipo_produto == input$tipo_produto_selecionado)
    }
    
    df <- df %>% filter(quantidade >= input$quantidade_range[1] & quantidade <= input$quantidade_range[2])
    
    df
  })
  
  # --- Saídas dos Indicadores (Cards) ---
  output$total_medicamentos_card <- renderValueBox({
    num_medicamentos <- nrow(dados_filtrados())
    valueBox(
      num_medicamentos, "Total de Medicamentos Listados", icon = icon("pills"),
      color = "purple"
    )
  })
  
  output$total_unidades_card <- renderValueBox({
    num_unidades <- dados_filtrados() %>% distinct(unidade) %>% nrow()
    valueBox(
      num_unidades, "Total de Unidades de Saúde", icon = icon("hospital"),
      color = "green"
    )
  })
  
  output$produto_maior_quantidade_card <- renderValueBox({
    produto_max <- dados_filtrados() %>%
      group_by(produto) %>%
      summarise(total_quantidade = sum(quantidade, na.rm = TRUE)) %>%
      arrange(desc(total_quantidade)) %>%
      head(1)
    
    if (nrow(produto_max) > 0) {
      valueBox(
        paste(produto_max$produto, "(", format(produto_max$total_quantidade, big.mark = "."), ")"),
        "Produto com Maior Quantidade em Estoque", icon = icon("prescription-bottle-alt"),
        color = "aqua"
      )
    } else {
      valueBox(
        "N/A", "Produto com Maior Quantidade em Estoque", icon = icon("prescription-bottle-alt"),
        color = "aqua"
      )
    }
  })
  
  # --- GRÁFICO MODIFICADO: Quantidade Total por Produto (Top 10) ---
  output$grafico_quantidade_por_produto_total <- renderPlot({
    dados_produto_total <- dados_filtrados() %>%
      group_by(produto) %>%
      summarise(total_quantidade = sum(quantidade, na.rm = TRUE)) %>%
      arrange(desc(total_quantidade)) %>%
      head(10) # Mostrar os 10 produtos com maior quantidade
    
    if (nrow(dados_produto_total) == 0) {
      return(NULL)
    }
    
    # Gráfico de COLUNAS NA VERTICAL
    ggplot(dados_produto_total, aes(x = reorder(produto, total_quantidade), y = total_quantidade)) +
      geom_bar(stat = "identity", fill = "darkblue") +
      labs(title = "Quantidade Total por Produto (Top 10)", x = "Produto", y = "Quantidade Total") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 10), # Rotaciona rótulos para melhor leitura
            axis.text.y = element_text(size = 12))
  })
  
  
  # --- Saídas dos Gráficos (Demais) ---
  
  # Gráfico de Pizza com Proporção por Apresentação
  output$grafico_pizza_apresentacao <- renderPlot({
    dados_pizza <- dados_filtrados() %>%
      group_by(apresentacao) %>%
      summarise(count = n()) %>%
      mutate(percentage = count / sum(count))
    
    if (nrow(dados_pizza) == 0) {
      return(NULL)
    }
    
    ggplot(dados_pizza, aes(x = "", y = percentage, fill = apresentacao)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(fill = "Apresentação") + # Apenas a legenda, sem título interno do gráfico
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            legend.position = "right") # Posiciona a legenda à direita
    # geom_text com os números e símbolos já foi removido na iteração anterior
  })
  
  # Gráfico de Barras CONDICIONAL: Quantidade de Produto por Unidade (ou Top 10 Produtos)
  output$grafico_barras_quantidade_produto <- renderPlot({
    if (input$busca_produto != "") {
      dados_por_unidade <- dados_filtrados() %>%
        filter(grepl(input$busca_produto, produto, ignore.case = TRUE)) %>%
        group_by(unidade) %>%
        summarise(total_quantidade = sum(quantidade, na.rm = TRUE)) %>%
        arrange(desc(total_quantidade))
      
      if (nrow(dados_por_unidade) == 0) {
        ggplot() + geom_text(aes(x=0.5, y=0.5, label=paste0("Produto '", input$busca_produto, "' não encontrado\nnesta(s) unidade(s) ou sem dados.")),
                             hjust=0.5, vjust=0.5, size=6) + theme_void()
      } else {
        ggplot(dados_por_unidade, aes(x = reorder(unidade, total_quantidade), y = total_quantidade)) +
          geom_bar(stat = "identity", fill = "lightcoral") +
          coord_flip() +
          labs(title = paste0("Quantidade de '", input$busca_produto, "' por Unidade"),
               x = "Unidade de Saúde", y = "Quantidade Total") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                axis.text.y = element_text(size = 10),
                axis.text.x = element_text(size = 10))
      }
    } else {
      dados_barras <- dados_filtrados() %>%
        group_by(produto) %>%
        summarise(total_quantidade = sum(quantidade, na.rm = TRUE)) %>%
        arrange(desc(total_quantidade)) %>%
        head(10)
      
      if (nrow(dados_barras) == 0) {
        return(NULL)
      }
      
      ggplot(dados_barras, aes(x = reorder(produto, total_quantidade), y = total_quantidade)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(title = "Top 10 Produtos por Quantidade", x = "Produto", y = "Quantidade Total") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.text.y = element_text(size = 12),
              axis.text.x = element_text(size = 12))
    }
  })
  
  # --- Saída da Tabela de Dados Filtrados ---
  output$tabela_dados_filtrados <- renderDT({
    datatable(dados_filtrados(),
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')
              ),
              filter = 'top',
              rownames = FALSE
    )
  })
  
  # --- Funcionalidade de Download ---
  output$download_dados <- downloadHandler(
    filename = function() {
      "dados_medicamentos_filtrados.csv"
    },
    content = function(file) {
      write.csv(dados_filtrados(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)