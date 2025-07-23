# app.R

# Carregar as bibliotecas necessárias
library(shiny)
library(dplyr)        # Para manipulação de dados
library(ggplot2)      # Para criação de gráficos
library(DT)           # Para tabelas interativas
library(shinydashboard) # Para um layout de dashboard profissional
library(scales)       # Para formatação de números e porcentagens

# --- LEITURA E MAPEAMENTO DOS DADOS ---
# 1. Lista de arquivos de distritos disponíveis
arquivos_distritos <- list.files(pattern = "medicamentos_distrito\\d+\\.csv$")

# Função para extrair o nome amigável do distrito
nome_distrito <- function(nome_arquivo) {
  num <- gsub("[^0-9]", "", nome_arquivo)
  paste("Distrito", num)
}

# UI: Adicionar tela inicial de seleção de distrito
ui <- dashboardPage(
  dashboardHeader(title = "ConsultaRecife"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Selecionar Distrito", tabName = "selecao_distrito", icon = icon("map-marker-alt")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Tabela de Dados", tabName = "tabela", icon = icon("table")),
      hr(),
      menuItem("Redes Sociais", tabName = "redes_sociais", icon = icon("share-alt")),
      hr()
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .small-box p { font-size: 13px !important; }
        .small-box h3 { font-size: 20px !important; }
        .selectize-input { min-height: 38px; }
        .selectize-dropdown { z-index: 9999; }
        .btn-social { 
          transition: all 0.3s ease;
          border-radius: 5px;
          font-weight: bold;
        }
        .btn-social:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "selecao_distrito",
        fluidRow(
          box(width = 6, title = "Escolha o Distrito para Análise", status = "primary", solidHeader = TRUE,
            selectInput("distrito_escolhido", "Distrito:",
              choices = setNames(arquivos_distritos, sapply(arquivos_distritos, nome_distrito)),
              selected = arquivos_distritos[1]
            ),
            actionButton("confirmar_distrito", "Confirmar", icon = icon("check"))
          )
        )
      ),
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_medicamentos_card", width = 3),
                valueBoxOutput("total_unidades_card", width = 3),
                valueBoxOutput("produto_maior_quantidade_card", width = 3)
              ),
              
              fluidRow(
                box(width = 12, title = "Filtros de Dados", status = "primary", solidHeader = TRUE,
                    fluidRow(
                      column(width = 4,
                             selectizeInput("unidade_selecionada", "Selecionar Unidade(s):",
                                           choices = NULL, multiple = TRUE, 
                                           options = list(placeholder = "Digite para buscar unidades...",
                                                        maxItems = NULL,
                                                        create = FALSE))
                      ),
                      column(width = 4,
                             selectizeInput("apresentacao_selecionada", "Filtrar por Apresentação:",
                                           choices = NULL, selected = "Todas",
                                           options = list(placeholder = "Digite para buscar apresentações...",
                                                        maxItems = 1,
                                                        create = FALSE))
                      ),
                      column(width = 4,
                             selectizeInput("tipo_produto_selecionado", "Filtrar por Tipo de Produto:",
                                           choices = NULL, selected = "Todos",
                                           options = list(placeholder = "Digite para buscar tipos...",
                                                        maxItems = 1,
                                                        create = FALSE))
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             selectizeInput("produtos_selecionados", "Selecionar Produto(s):",
                                           choices = NULL, multiple = TRUE,
                                           options = list(placeholder = "Digite para buscar produtos...",
                                                        maxItems = NULL,
                                                        create = FALSE))
                      ),
                      column(width = 6,
                             sliderInput("quantidade_range", "Filtrar por Quantidade (Individual):",
                                         min = 0, max = 1, value = c(0, 1))
                      )
                    )
                ) 
              ), 
              
              fluidRow( 
                box(width = 6, title = "Proporção por Apresentação", status = "info", solidHeader = TRUE,
                    plotOutput("grafico_pizza_apresentacao")),
                box(width = 6, title = "Distribuição por Unidade", status = "info", solidHeader = TRUE,
                    plotOutput("grafico_barras_quantidade_produto"))
              )
      ), 
      
      tabItem(tabName = "tabela",
              fluidRow(
                box(width = 12, title = "Dados Filtrados", status = "primary", solidHeader = TRUE,
                    downloadButton("download_dados", "Download Dados Filtrados (CSV)"),
                    DTOutput("tabela_dados_filtrados"))
              )
      ),
      
      tabItem(tabName = "redes_sociais",
              fluidRow(
                box(width = 12, title = "Conecte-se Conosco", status = "primary", solidHeader = TRUE,
                    fluidRow(
                      column(width = 12,
                             div(style = "text-align: center; padding: 20px;",
                                 icon("instagram", "fa-3x", style = "color: #E4405F; margin-bottom: 15px;"),
                                 h3("Instagram"),
                                 p("Siga-nos para novidades e atualizações"),
                                 actionButton("instagram_page_btn", "Visitar Instagram", 
                                             icon = icon("external-link-alt"),
                                             style = "background-color: #E4405F; color: white; border: none;",
                                             onclick = "window.open('https://www.instagram.com/consultarecife/profilecard/?igsh=amdjN3k4c2pkM2dm', '_blank')")
                             )
                      )
                    )
                )
              )
      )
    )
  ) 
) 

# --- Lógica do Servidor (Server) ---
server <- function(input, output, session) {
  # Estado reativo para armazenar o nome do arquivo selecionado
  distrito_arquivo <- reactiveVal(arquivos_distritos[1])

  # Quando o usuário clicar em confirmar, atualiza o arquivo do distrito
  observeEvent(input$confirmar_distrito, {
    distrito_arquivo(input$distrito_escolhido)
    updateTabItems(session, "tabs", selected = "dashboard")
  })

  # Dados reativos baseados no distrito selecionado
  initial_dados <- reactive({
    arquivo <- distrito_arquivo()
    if (!is.null(arquivo) && file.exists(arquivo)) {
      read.csv(arquivo, encoding = "UTF-8", stringsAsFactors = FALSE, row.names = NULL)
    } else {
      warning(paste("Arquivo", arquivo, "não encontrado. Usando dados de exemplo."))
      data.frame(
        distrito = rep(paste("Unidade Exemplo", 1:2), each = 50),
        unidade = rep(paste("Classe Exemplo", LETTERS[1:5]), each = 10, length.out = 100), 
        classe = rep(paste("Apresentacao Exemplo", c("AMP", "COMPR", "FRASCO", "SACHE", "BISNAGA")), each = 20, length.out = 100), 
        apresentacao = rep(c("Tipo A", "Tipo B", "Tipo C"), length.out = 100), 
        tipo_produto = sample(paste0("Cod ", 1001:1020), 100, replace = TRUE), 
        codigo_produto = paste("Produto Exemplo", sample(LETTERS, 100, replace = TRUE), sprintf("%03d", sample(1:50, 100, replace = TRUE))), 
        produto = sample(1:200, 100, replace = TRUE), 
        cadum = sample(c(1:250, 280:500, 1000:2000), 100, replace = TRUE), 
        stringsAsFactors = FALSE
      )
    }
  })

  dados <- reactive({
    df <- initial_dados()
    data.frame(
      unidade = df$distrito,
      classe = df$unidade, 
      apresentacao = df$classe, 
      tipo_produto = df$apresentacao, 
      codigo_produto = df$tipo_produto, 
      produto = df$codigo_produto, 
      cadum = df$produto, 
      quantidade = df$cadum, 
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        quantidade = as.numeric(quantidade),
        produto = as.character(produto),
        unidade = as.character(unidade),
        classe = as.character(classe),
        apresentacao = as.character(apresentacao),
        tipo_produto = as.character(tipo_produto)
      ) %>%
      filter(!is.na(quantidade)) %>%
      filter(!is.na(unidade) & unidade != "") %>%
      filter(!is.na(produto) & produto != "") %>%
      filter(!is.na(apresentacao) & apresentacao != "") %>%
      filter(!is.na(tipo_produto) & tipo_produto != "")
  })
  
  observe({
    if(nrow(dados()) == 0) {
      warning("O dataframe 'dados' está vazio ou não contém dados válidos após a filtragem inicial.")
      return()
    }
    
    unidades_disponiveis <- sort(unique(dados()$unidade)) 
    primeira_unidade_selecionada <- if (length(unidades_disponiveis) > 0) {
      unidades_disponiveis[1] 
    } else {
      character(0) 
    }
    updateSelectizeInput(session, "unidade_selecionada",
                        choices = unidades_disponiveis,
                        selected = primeira_unidade_selecionada,
                        options = list(placeholder = "Digite para buscar unidades...",
                                     maxItems = NULL,
                                     create = FALSE))
    
    apresentacoes_validas <- dados() %>% filter(!is.na(apresentacao) & apresentacao != "") %>% distinct(apresentacao) %>% pull()
    apresentacoes_disponiveis <- c("Todas", sort(apresentacoes_validas))
    updateSelectizeInput(session, "apresentacao_selecionada", 
                        choices = apresentacoes_disponiveis, 
                        selected = "Todas",
                        options = list(placeholder = "Digite para buscar apresentações...",
                                     maxItems = 1,
                                     create = FALSE))
    
    tipos_produto_disponiveis <- c("Todos", sort(unique(dados()$tipo_produto)))
    updateSelectizeInput(session, "tipo_produto_selecionado", 
                        choices = tipos_produto_disponiveis, 
                        selected = "Todos",
                        options = list(placeholder = "Digite para buscar tipos...",
                                     maxItems = 1,
                                     create = FALSE))
    
    produtos_disponiveis <- sort(unique(dados()$produto))
    updateSelectizeInput(session, "produtos_selecionados", 
                        choices = produtos_disponiveis, 
                        selected = character(0),
                        options = list(placeholder = "Digite para buscar produtos...",
                                     maxItems = NULL,
                                     create = FALSE))
    
    min_qty <- min(dados()$quantidade, na.rm = TRUE)
    max_qty <- max(dados()$quantidade, na.rm = TRUE)
    if (is.finite(min_qty) && is.finite(max_qty) && min_qty <= max_qty) {
      updateSliderInput(session, "quantidade_range", min = min_qty, max = max_qty, value = c(min_qty, max_qty))
    }  else if (is.finite(min_qty) && !is.finite(max_qty)) { 
      updateSliderInput(session, "quantidade_range", min = min_qty, max = min_qty + 1000, value = c(min_qty, min_qty + 1000)) 
    } else if (!is.finite(min_qty) && is.finite(max_qty)) { 
      updateSliderInput(session, "quantidade_range", min = max_qty - 1000, max = max_qty, value = c(max_qty - 1000, max_qty)) 
    } else {
      updateSliderInput(session, "quantidade_range", min = 0, max = 1, value = c(0,1))
    }
  })
  
  dados_filtrados <- reactive({
    req(input$apresentacao_selecionada, input$tipo_produto_selecionado, input$quantidade_range)
    
    # Se nenhuma unidade for selecionada, não continue
    if (is.null(input$unidade_selecionada) || length(input$unidade_selecionada) == 0) {
      return(data.frame()) # Retorna um dataframe vazio explicitamente
    }
    
    df <- dados()
    
    df <- df %>% filter(unidade %in% input$unidade_selecionada)
    
    if (input$apresentacao_selecionada != "Todas") {
      df <- df %>% filter(apresentacao == input$apresentacao_selecionada)
    }
    if (input$tipo_produto_selecionado != "Todos") {
      df <- df %>% filter(tipo_produto == input$tipo_produto_selecionado)
    }
    if (!is.null(input$produtos_selecionados) && length(input$produtos_selecionados) > 0) {
      df <- df %>% filter(produto %in% input$produtos_selecionados)
    }
    if (length(input$quantidade_range) == 2 && all(is.finite(input$quantidade_range))) {
      df <- df %>% filter(quantidade >= input$quantidade_range[1] & quantidade <= input$quantidade_range[2])
    }
    df
  })
  
  # Cards (Value Boxes)
  output$total_medicamentos_card <- renderValueBox({
    #Checar se o input de unidade está vazio antes de tentar filtrar
    if (is.null(input$unidade_selecionada) || length(input$unidade_selecionada) == 0) {
      return(valueBox("N/A", "Itens de Medicamentos", icon = icon("pills"), color = "purple"))
    }
    valueBox(format(nrow(dados_filtrados()), big.mark = "."), "Itens de Medicamentos", icon = icon("pills"), color = "purple")
  })
  
  output$total_unidades_card <- renderValueBox({
    num_unidades <- length(input$unidade_selecionada)
    valueBox(num_unidades, "Unidades Selecionadas", icon = icon("hospital"), color = "green")
  })
  
  output$produto_maior_quantidade_card <- renderValueBox({
    if (is.null(input$unidade_selecionada) || length(input$unidade_selecionada) == 0) {
      return(valueBox("N/A", "Produto com Maior Quantidade", icon = icon("prescription-bottle-alt"), color = "aqua"))
    }
    df_filtrado_local <- dados_filtrados()
    if (nrow(df_filtrado_local) == 0) {
      return(valueBox("N/A", "Produto com Maior Quantidade", icon = icon("prescription-bottle-alt"), color = "aqua"))
    }
    produto_max <- df_filtrado_local %>%
      group_by(produto) %>%
      summarise(total_quantidade = sum(quantidade, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(total_quantidade)) %>% head(1)
    if (nrow(produto_max) > 0 && produto_max$total_quantidade > 0) {
      valueBox(
        produto_max$produto, 
        paste0("Produto Maior Qtd: ", format(produto_max$total_quantidade, big.mark = ".")), 
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
  
  # Gráfico de Pizza
  output$grafico_pizza_apresentacao <- renderPlot({
    #Checagem no início para 'unidade_selecionada'
    if (is.null(input$unidade_selecionada) || length(input$unidade_selecionada) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Selecione uma ou mais Unidades de Saúde", 
                        size = 5, hjust = 0.5, fontface="italic", color="black") +
               theme_void())
    }
    
    df_f <- dados_filtrados()
    if (nrow(df_f) == 0 || !("apresentacao" %in% names(df_f)) || all(is.na(df_f$apresentacao) | df_f$apresentacao == "")) { 
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
        apresentacao_com_percentual = paste0(apresentacao, "\n(", scales::percent(percentage, accuracy = 0.1), ")")
      ) %>%
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
  
  # Gráfico de Barras
  output$grafico_barras_quantidade_produto <- renderPlot({
    # Checagem no início para 'unidade_selecionada'
    if (is.null(input$unidade_selecionada) || length(input$unidade_selecionada) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Selecione uma ou mais unidades de Saúde", 
                        size = 5, hjust = 0.5, fontface="italic", color="Black") +
               theme_void())
    }
    
    df_filtrado_local <- dados_filtrados()
    
    if (!is.null(input$produtos_selecionados) && length(input$produtos_selecionados) > 0) {
      dados_por_unidade <- df_filtrado_local %>%
        group_by(unidade) %>%
        summarise(total_quantidade = sum(quantidade, na.rm = TRUE), .groups = 'drop') %>%
        filter(total_quantidade > 0) %>% 
        arrange(desc(total_quantidade))
      
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
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste0("Os produtos selecionados não foram encontrados\n",
                                  "ou não possuem estoque nas unidades filtradas."), 
                   size = 5, hjust = 0.5, vjust = 0.5, lineheight = 0.9) + 
          theme_void()
      } else {
        ggplot(dados_por_unidade, aes(x = reorder(unidade, total_quantidade), y = total_quantidade)) +
          geom_bar(stat = "identity", fill = "lightcoral") +
          geom_text(aes(label = scales::label_number(accuracy = 1, big.mark = ".")(total_quantidade)), 
                    hjust = -0.2, size = 3, fontface = "bold") + 
          coord_flip() +
          scale_y_continuous(expand = expansion(mult = c(0.01, 0.15))) + 
          labs(title = NULL,
               x = "Unidade de Saúde", y = "Quantidade Total Acumulada") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
                axis.text.y = element_text(size = 10),
                axis.text.x = element_text(size = 10))
      }
    } else { 
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Selecione um ou mais produtos\npara visualizar a distribuição por unidade.", 
                 size = 5, hjust = 0.5, vjust = 0.5, lineheight = 0.9, fontface="italic") +
        theme_void()
    }
  })
  
  # Tabela de Dados
  output$tabela_dados_filtrados <- renderDT({
    #Checar se o input de unidade está vazio antes de tentar renderizar a tabela
    if (is.null(input$unidade_selecionada) || length(input$unidade_selecionada) == 0) {
      # Retorna uma tabela vazia com uma mensagem
      return(datatable(data.frame(Mensagem = "Selecione uma Unidade de Saúde para ver os dados."), 
                       options = list(dom = 't', language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')), 
                       rownames=FALSE))
    }
    datatable(dados_filtrados(),
              options = list(pageLength = 10, scrollX = TRUE,
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')),
              filter = 'top', rownames = FALSE)
  })
  
  # Download
  output$download_dados <- downloadHandler(
    filename = function() { paste0("dados_medicamentos_filtrados_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(dados_filtrados(), file, row.names = FALSE, fileEncoding = "UTF-8") }
  )
}

shinyApp(ui, server)