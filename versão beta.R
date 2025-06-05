# app.R

# Carregar as bibliotecas necessárias
library(shiny)
library(dplyr)        # Para manipulação de dados
library(ggplot2)      # Para criação de gráficos
library(DT)           # Para tabelas interativas
library(shinydashboard) # Para um layout de dashboard profissional
library(scales)       # Para formatação de números e porcentagens

# --- CORREÇÃO NA LEITURA E MAPEAMENTO DOS DADOS ---
# 1. Carrega o arquivo CSV como ele é lido inicialmente pelo R
initial_dados <- read.csv("medicamentos_distrito03.csv", encoding = "UTF-8", stringsAsFactors = FALSE, row.names = NULL)

# 2. Cria um novo dataframe 'dados' mapeando o conteúdo real para os nomes de coluna desejados.
# ATENÇÃO: Revise este mapeamento para garantir que as colunas do CSV (à direita do '$')
#          correspondem semanticamente às colunas do dataframe 'dados' (à esquerda do '=')
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
#    Também garante que 'produto' e 'unidade' sejam character para evitar problemas com fatores
dados$quantidade <- as.numeric(dados$quantidade)
dados$produto <- as.character(dados$produto)
dados$unidade <- as.character(dados$unidade) 
dados <- dados %>% filter(!is.na(quantidade))

# --- Interface do Usuário (UI) ---
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
    tags$head(
      tags$style(HTML("
        .small-box p { 
          font-size: 13px !important;
        }
        .small-box h3 { 
          font-size: 20px !important; 
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "dashboard",
              # PRIMEIRA LINHA: Value Boxes
              fluidRow(
                valueBoxOutput("total_medicamentos_card", width = 3),
                valueBoxOutput("total_unidades_card", width = 3),
                valueBoxOutput("produto_maior_quantidade_card", width = 3)
              ),
              
              # SEGUNDA LINHA: Caixa de Filtros
              fluidRow(
                box(width = 12, title = "Filtros de Dados", status = "primary", solidHeader = TRUE,
                    # Linha 1 de Filtros Internos
                    fluidRow(
                      column(width = 3, 
                             selectInput("unidade_selecionada", "Selecionar Unidade(s):",
                                         choices = NULL, multiple = TRUE)
                      ),
                      column(width = 3,
                             # CORRIGIDO PARA O FILTRO DE CLASSE
                             selectInput("classe_selecionada", "Filtrar por Classe:", 
                                         choices = NULL, selected = "Todas") 
                      ),
                      column(width = 3,
                             selectInput("apresentacao_selecionada", "Filtrar por Apresentação:",
                                         choices = NULL, selected = "Todas")
                      ),
                      column(width = 3,
                             selectInput("tipo_produto_selecionado", "Filtrar por Tipo de Produto:",
                                         choices = NULL, selected = "Todos")
                      )
                    ),
                    # Linha 2 de Filtros Internos
                    fluidRow(
                      column(width = 6,
                             selectInput("produtos_selecionados", "Selecionar Produto(s):",
                                         choices = NULL, multiple = TRUE)
                      ),
                      column(width = 6,
                             sliderInput("quantidade_range", "Filtrar por Quantidade:",
                                         min = 0, max = 1, value = c(0, 1))
                      )
                    )
                ) # Fim da Box de Filtros
              ), # Fim da FluidRow da Box de Filtros
              
              # TERCEIRA LINHA: Gráficos
              fluidRow( 
                box(width = 6, title = "Proporção por Apresentação", status = "info", solidHeader = TRUE,
                    plotOutput("grafico_pizza_apresentacao")),
                box(width = 6, title = "Distribuição por Unidade", status = "info", solidHeader = TRUE,
                    plotOutput("grafico_barras_quantidade_produto"))
              )
      ), # Fim do tabItem 'dashboard'
      
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
  
  observe({
    unidades_disponiveis <- sort(unique(dados$unidade)) # Ordena alfabeticamente
    # MODIFICAÇÃO AQUI: Selecionar apenas a primeira unidade por padrão
    primeira_unidade_selecionada <- if (length(unidades_disponiveis) > 0) {
      unidades_disponiveis[1] # Pega a primeira unidade da lista ordenada
    } else {
      character(0) 
    }
    updateSelectInput(session, "unidade_selecionada",
                      choices = unidades_disponiveis,
                      selected = primeira_unidade_selecionada)
    classes_disponiveis <- c("Todas", sort(unique(dados$classe)))
    updateSelectInput(session, "classe_selecionada", choices = classes_disponiveis, selected = "Todas")
    apresentacoes_disponiveis <- c("Todas", sort(unique(dados$apresentacao)))
    updateSelectInput(session, "apresentacao_selecionada", choices = apresentacoes_disponiveis, selected = "Todas")
    tipos_produto_disponiveis <- c("Todos", sort(unique(dados$tipo_produto)))
    updateSelectInput(session, "tipo_produto_selecionado", choices = tipos_produto_disponiveis, selected = "Todos")
    produtos_disponiveis <- sort(unique(dados$produto))
    updateSelectInput(session, "produtos_selecionados", choices = produtos_disponiveis, selected = character(0))
    
    min_qty <- min(dados$quantidade, na.rm = TRUE)
    max_qty <- max(dados$quantidade, na.rm = TRUE)
    if (is.finite(min_qty) && is.finite(max_qty) && min_qty <= max_qty) {
      updateSliderInput(session, "quantidade_range", min = min_qty, max = max_qty, value = c(min_qty, max_qty))
    } else {
      updateSliderInput(session, "quantidade_range", min = 0, max = 1, value = c(0,1))
    }
  })
  
  dados_filtrados <- reactive({
    df <- dados
    if (!is.null(input$unidade_selecionada) && length(input$unidade_selecionada) > 0) {
      df <- df %>% filter(unidade %in% input$unidade_selecionada)
    }
    if (!is.null(input$classe_selecionada) && input$classe_selecionada != "Todas") {
      df <- df %>% filter(classe == input$classe_selecionada)
    }
    if (!is.null(input$apresentacao_selecionada) && input$apresentacao_selecionada != "Todas") {
      df <- df %>% filter(apresentacao == input$apresentacao_selecionada)
    }
    if (!is.null(input$tipo_produto_selecionado) && input$tipo_produto_selecionado != "Todos") {
      df <- df %>% filter(tipo_produto == input$tipo_produto_selecionado)
    }
    if (!is.null(input$produtos_selecionados) && length(input$produtos_selecionados) > 0) {
      df <- df %>% filter(produto %in% input$produtos_selecionados)
    }
    if (!is.null(input$quantidade_range)) {
      df <- df %>% filter(quantidade >= input$quantidade_range[1] & quantidade <= input$quantidade_range[2])
    }
    df
  })
  
  output$total_medicamentos_card <- renderValueBox({
    num_medicamentos <- nrow(dados_filtrados())
    valueBox(num_medicamentos, "Itens de Medicamentos", icon = icon("pills"), color = "purple")
  })
  
  output$total_unidades_card <- renderValueBox({
    num_unidades <- dados_filtrados() %>% distinct(unidade) %>% nrow()
    valueBox(num_unidades, "Unidades de Saúde", icon = icon("hospital"), color = "green")
  })
  
  output$produto_maior_quantidade_card <- renderValueBox({
    df_filtrado_local <- dados_filtrados()
    if (nrow(df_filtrado_local) == 0) {
      return(valueBox("N/A", "Produto com Maior Quantidade", icon = icon("prescription-bottle-alt"), color = "aqua"))
    }
    produto_max <- df_filtrado_local %>%
      group_by(produto) %>%
      summarise(total_quantidade = sum(quantidade, na.rm = TRUE)) %>%
      arrange(desc(total_quantidade)) %>% head(1)
    if (nrow(produto_max) > 0 && produto_max$total_quantidade > 0) {
      valueBox(
        produto_max$produto, # <--- Alterado de: paste(produto_max$produto, "(", format(produto_max$total_quantidade, big.mark = "."), ")")
        "Produto com Maior Quantidade", 
        icon = icon("prescription-bottle-alt"),
        color = "aqua"
      )
    } else {
      valueBox(
        "N/A", 
        "Produto com Maior Quantidade", 
        icon = icon("prescription-bottle-alt"),
        color = "aqua"
      )
    }
  })
  
  output$grafico_pizza_apresentacao <- renderPlot({
    df_f <- dados_filtrados()
    if (nrow(df_f) == 0 || !("apresentacao" %in% names(df_f)) || all(is.na(df_f$apresentacao))) { 
      return(ggplot() + labs(title = "Sem dados válidos de apresentação para os filtros atuais.", x="", y="") + 
               theme_void() + theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))
    }
    
    dados_pizza <- df_f %>%
      filter(!is.na(apresentacao) & apresentacao != "") %>%
      group_by(apresentacao) %>%
      summarise(count = n(), .groups = 'drop') %>%
      filter(count > 0) %>%
      mutate(
        percentage = count / sum(count),
        # MODIFICAÇÃO 1: Adicionar '\n' para nova linha na legenda
        apresentacao_com_percentual = paste0(apresentacao, "\n(", scales::percent(percentage, accuracy = 0.1), ")")
      ) %>%
      # Ordenar para consistência da legenda
      arrange(apresentacao) 
    
    if (nrow(dados_pizza) == 0) {
      return(ggplot() + labs(title = "Nenhuma apresentação encontrada com os filtros atuais.", x="", y="") + 
               theme_void() + theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))
    }
    
    ggplot(dados_pizza, aes(x = "", y = percentage, fill = apresentacao_com_percentual)) + 
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(fill = "Apresentação e Percentual") + 
      theme_void() + 
      theme(legend.title = element_text(face="bold"), 
            legend.position = "right")
  })
  
  output$grafico_barras_quantidade_produto <- renderPlot({
    df_filtrado_local <- dados_filtrados()
    
    if (!is.null(input$produtos_selecionados) && length(input$produtos_selecionados) > 0) {
      dados_por_unidade <- df_filtrado_local %>%
        group_by(unidade) %>%
        summarise(total_quantidade = sum(quantidade, na.rm = TRUE)) %>%
        filter(total_quantidade > 0) %>% arrange(desc(total_quantidade))
      
      if(nrow(dados_por_unidade) > 0) {
        dados_por_unidade <- dados_por_unidade %>%
          mutate(
            unidade = case_when(
              grepl("^(Us|US) \\d{3}", unidade) ~ sub("^(Us|US) (\\d{3}).*", "US \\2", unidade),
              grepl("^HOSPITAL \\w+", unidade) ~ sub("^(HOSPITAL )(\\w+).*", "HOSP \\2", unidade),
              TRUE ~ as.character(unidade) 
            )
          )
      }
      
      if (nrow(dados_por_unidade) == 0) {
        ggplot() + geom_text(aes(x=0.5, y=0.5, label=paste0("Os produtos selecionados não foram encontrados\n",
                                                            "ou não possuem estoque nas unidades filtradas.")),
                             hjust=0.5, vjust=0.5, size=5) + theme_void() +
          labs(title = "Produtos Selecionados por Unidade") +
          theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
      } else {
        ggplot(dados_por_unidade, aes(x = reorder(unidade, total_quantidade), y = total_quantidade)) +
          geom_bar(stat = "identity", fill = "lightcoral") +
          # >>> ADICIONADO geom_text PARA MOSTRAR VALORES NAS BARRAS <<<
          geom_text(aes(label = scales::label_number(accuracy = 1, big.mark = ".")(total_quantidade)), 
                    hjust = -0.2, size = 3, fontface = "bold") + # Ajuste hjust e size conforme necessário
          coord_flip() +
          # Aumenta o limite do eixo para dar espaço ao texto
          scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
          labs(title = "Quantidade dos Produtos Selecionados por Unidade",
               x = "Unidade de Saúde", y = "Quantidade Total Acumulada") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
                axis.text.y = element_text(size = 10),
                axis.text.x = element_text(size = 10))
      }
    } else { 
      dados_barras <- df_filtrado_local %>%
        group_by(produto) %>%
        summarise(total_quantidade = sum(quantidade, na.rm = TRUE)) %>%
        filter(total_quantidade > 0) %>% arrange(desc(total_quantidade)) %>% head(10)
      
      if (nrow(dados_barras) == 0) {
        return(ggplot() + labs(title = "Sem dados selecionados", x="", y="") + theme_void() +
                 theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")))
      }
      
      ggplot(dados_barras, aes(x = reorder(produto, total_quantidade), y = total_quantidade)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        # >>> ADICIONADO geom_text PARA MOSTRAR VALORES NAS BARRAS <<<
        geom_text(aes(label = scales::label_number(accuracy = 1, big.mark = ".")(total_quantidade)), 
                  hjust = -0.2, size = 3, fontface = "bold") + # Ajuste hjust e size conforme necessário
        coord_flip() +
        # Aumenta o limite do eixo para dar espaço ao texto
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
        labs(title = "Top 10 Produtos por Quantidade (nos filtros)", x = "Produto", y = "Quantidade Total") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.text.y = element_text(size = 12),
              axis.text.x = element_text(size = 12))
    }
  })
  
  output$tabela_dados_filtrados <- renderDT({
    datatable(dados_filtrados(),
              options = list(pageLength = 10, scrollX = TRUE,
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')),
              filter = 'top', rownames = FALSE)
  })
  
  output$download_dados <- downloadHandler(
    filename = function() { "dados_medicamentos_filtrados.csv" },
    content = function(file) { write.csv(dados_filtrados(), file, row.names = FALSE, fileEncoding = "UTF-8") }
  )
}

shinyApp(ui, server)