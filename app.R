# app.R - Simulación de contagio en redes financieras basada en Gai & Kapadia (2011)
# Desarrollado para clase de finanzas computacionales

# Cargar librerías necesarias
library(shiny)
library(igraph)
library(visNetwork)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(shinycssloaders)

# UI de la aplicación
ui <- dashboardPage(
  dashboardHeader(title = "Simulador de Contagio Financiero"),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Introducción", tabName = "intro", icon = icon("info-circle")),
      menuItem("Simulador", tabName = "simulator", icon = icon("play")),
      menuItem("Sobre el modelo", tabName = "about", icon = icon("book"))
    ),
    h4("Configuración de la Red", style = "padding-left: 15px;"),
    sliderInput("num_banks", "Número de bancos:", min = 10, max = 100, value = 25),
    selectInput("network_topology", "Topología de la red:",
                choices = c("Aleatoria" = "random", 
                            "Escala libre" = "scale_free", 
                            "Centralizada" = "centralized",
                            "Anillo" = "ring",
                            "Core-Periphery" = "core_periphery",
                            "Small-World" = "small_world",
                            "Completa" = "complete")),
    sliderInput("connection_prob", "Probabilidad de conexión:", min = 0.05, max = 0.5, value = 0.15, step = 0.01),
    hr(),
    h4("Parámetros Financieros", style = "padding-left: 15px;"),
    sliderInput("capital_ratio", "Ratio de capital (%):", min = 1, max = 20, value = 4),
    sliderInput("interbank_exposure", "Exposición interbancaria (%):", min = 0, max = 80, value = 20),
    sliderInput("illiquid_assets", "Activos ilíquidos (%):", min = 0, max = 80, value = 50),
    hr(),
    h4("Simulación de Shock", style = "padding-left: 15px;"),
    sliderInput("shock_size", "Magnitud del shock (%):", min = 0, max = 100, value = 35),
    sliderInput("initial_defaults", "Bancos inicialmente afectados:", min = 1, max = 10, value = 1),
    selectInput("shock_type", "Tipo de shock:",
                choices = c("Aleatorio" = "random", 
                            "Bancos más grandes" = "largest", 
                            "Bancos más conectados" = "connected")),
    hr(),
    actionButton("run_simulation", "Ejecutar Simulación", class = "btn-primary btn-lg"),
    actionButton("reset", "Reiniciar", class = "btn-warning")
  ),
  dashboardBody(
    tabItems(
      # Pestaña de introducción
      tabItem(tabName = "intro",
              fluidRow(
                box(
                  title = "Introducción al modelo de contagio financiero", status = "primary", solidHeader = TRUE,
                  width = 12,
                  h3("Modelo de Gai & Kapadia (2011)"),
                  p("Esta aplicación simula el modelo de contagio financiero desarrollado por Prasanna Gai y Sujit Kapadia en su artículo 'Contagion in Financial Networks' (2011). El modelo explora cómo la estructura de las redes interbancarias influye en la propagación del riesgo sistémico."),
                  p("El modelo demuestra el fenómeno de 'robusto pero frágil', donde las redes financieras pueden absorber muchos pequeños shocks pero son vulnerables a grandes perturbaciones iniciales que desencadenan efectos de contagio significativos."),
                  h3("Instrucciones de uso"),
                  p("1. Configure los parámetros de la red y financieros utilizando los controles del panel izquierdo"),
                  p("2. Especifique el tipo y magnitud del shock inicial"),
                  p("3. Haga clic en 'Ejecutar Simulación' para ver cómo se propaga el contagio"),
                  p("4. Analice los resultados y experimente con diferentes configuraciones")
                )
              ),
              fluidRow(
                box(
                  title = "Descripción de los parámetros", status = "info", solidHeader = TRUE,
                  width = 12,
                  h4("Parámetros de la red:"),
                  tags$ul(
                    tags$li(strong("Número de bancos:"), "La cantidad total de instituciones financieras en la red."),
                    tags$li(strong("Topología de la red:"), HTML("La estructura de conexiones entre los bancos:<br>
                - <b>Aleatoria:</b> Conexiones distribuidas al azar (Modelo Erdos-Renyi)<br>
                - <b>Escala libre:</b> Algunos bancos tienen muchas más conexiones que otros (Modelo Barabasi-Albert)<br>
                - <b>Centralizada:</b> Un banco central conectado a todos los demás<br>
                - <b>Anillo:</b> Cada banco conectado solo con sus vecinos inmediatos<br>
                - <b>Core-Periphery:</b> Un núcleo densamente conectado y una periferia escasamente conectada<br>
                - <b>Small-World:</b> Mayoría de conexiones locales con algunos atajos (Modelo Watts-Strogatz)<br>
                - <b>Completa:</b> Todos los bancos están conectados entre sí")),
                    tags$li(strong("Probabilidad de conexión:"), "La probabilidad de que exista una conexión entre dos bancos cualesquiera.")
                  ),
                  h4("Parámetros financieros:"),
                  tags$ul(
                    tags$li(strong("Ratio de capital (%):"), "El porcentaje de los activos totales que se mantiene como capital propio (buffer contra pérdidas)."),
                    tags$li(strong("Exposición interbancaria (%):"), "El porcentaje de activos que consiste en préstamos a otros bancos."),
                    tags$li(strong("Activos ilíquidos (%):"), "El porcentaje de activos que no pueden convertirse fácilmente en efectivo.")
                  ),
                  h4("Parámetros de shock:"),
                  tags$ul(
                    tags$li(strong("Magnitud del shock (%):"), "El porcentaje de activos ilíquidos que pierden valor en los bancos inicialmente afectados."),
                    tags$li(strong("Bancos inicialmente afectados:"), "El número de bancos que reciben el shock inicial."),
                    tags$li(strong("Tipo de shock:"), "Determina qué bancos reciben el shock inicial:<br>
                - Aleatorio: Bancos seleccionados al azar<br>
                - Bancos más grandes: Los bancos con mayores activos<br>
                - Bancos más conectados: Los bancos con más conexiones")
                  )
                )
              )
      ),
      
      # Pestaña del simulador
      tabItem(tabName = "simulator",
              fluidRow(
                box(
                  title = "Red Financiera", status = "primary", solidHeader = TRUE,
                  width = 8, height = 550,
                  withSpinner(visNetworkOutput("network_graph", height = 480))
                ),
                column(
                  width = 4,
                  box(
                    title = "Resultados del Contagio", status = "warning", solidHeader = TRUE,
                    width = NULL, height = 250,
                    valueBoxOutput("default_box", width = 6),
                    valueBoxOutput("assets_lost_box", width = 6),
                    plotlyOutput("contagion_plot", height = 150)
                  ),
                  box(
                    title = "Bancos Sistémicamente Importantes", status = "danger", solidHeader = TRUE,
                    width = NULL, height = 280,
                    div(style = "overflow-x: auto; overflow-y: auto; max-height: 230px;",
                        DT::dataTableOutput("systemic_banks")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Análisis de Robustez", status = "info", solidHeader = TRUE,
                  width = 12, height = 300,
                  plotlyOutput("robustness_plot")
                )
              )
      ),
      
      # Pestaña sobre el modelo
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "Acerca del modelo de Gai & Kapadia", status = "primary", solidHeader = TRUE,
                  width = 12,
                  h3("Principales hallazgos del artículo original"),
                  p("El artículo 'Contagion in Financial Networks' (2011) de Prasanna Gai y Sujit Kapadia ha sido fundamental para entender la propagación del riesgo sistémico. Sus principales conclusiones son:"),
                  tags$ul(
                    tags$li("Los sistemas financieros pueden ser simultáneamente 'robustos' y 'frágiles'"),
                    tags$li("La probabilidad de contagio puede ser baja, pero su impacto puede ser extremadamente amplio cuando ocurre"),
                    tags$li("Mayor conectividad reduce la probabilidad de contagio pero aumenta su alcance cuando sucede"),
                    tags$li("Concentración en ciertos bancos ('too interconnected to fail') aumenta significativamente el riesgo sistémico"),
                    tags$li("La opacidad de las exposiciones entre bancos puede exacerbar el contagio")
                  ),
                  h3("Aplicaciones prácticas"),
                  p("Este modelo tiene importantes aplicaciones para la regulación financiera:"),
                  tags$ul(
                    tags$li("Identificación de instituciones sistémicamente importantes"),
                    tags$li("Diseño de requisitos de capital basados en la conectividad"),
                    tags$li("Evaluación de la resiliencia del sistema financiero"),
                    tags$li("Pruebas de estrés del sistema bancario")
                  ),
                  h3("Referencias"),
                  p("Gai, P., & Kapadia, S. (2011). Contagion in financial networks. Proceedings of the Royal Society A: Mathematical, Physical and Engineering Sciences, 466(2120), 2401-2423."),
                  p("LeBaron, B. (Adaptación en NetLogo). Financial Contagion Model. https://people.brandeis.edu/~blebaron/classes/agentfin/GaiKapadia.html")
                )
              )
      )
    )
  )
)

# Lógica del servidor
server <- function(input, output, session) {
  
  # Almacenamiento reactivo para los datos de la red
  network_data <- reactiveVal(NULL)
  simulation_results <- reactiveVal(NULL)
  simulation_history <- reactiveVal(data.frame())
  
  # Función para crear la red bancaria
  create_network <- function(n, topology, p) {
    if (topology == "random") {
      g <- sample_gnp(n, p)
    } else if (topology == "scale_free") {
      g <- sample_pa(n, power = 1, m = ceiling(p * n))
    } else if (topology == "centralized") {
      # Crear una topología de estrella (un nodo central conectado a todos los demás)
      g <- make_empty_graph(n, directed = FALSE)
      center_node <- 1
      for (i in 2:n) {
        g <- add_edges(g, c(center_node, i))
      }
      # Añadir algunas conexiones adicionales aleatorias
      possible_edges <- combn(2:n, 2) # Todas las posibles conexiones entre nodos que no son el centro
      num_additional_edges <- ceiling(p * n * (n-1)/2)
      if (ncol(possible_edges) > 0 && num_additional_edges > 0) { # Verificar que hay suficientes posibles conexiones
        selected_edges <- sample(1:ncol(possible_edges), min(num_additional_edges, ncol(possible_edges)))
        additional_edges <- possible_edges[, selected_edges, drop = FALSE]
        g <- add_edges(g, as.vector(additional_edges))
      }
    } else if (topology == "ring") {
      g <- make_ring(n) %>% add_edges(sample(combn(1:n, 2), ceiling(p * n * (n-1)/10)))
    } else if (topology == "core_periphery") {
      # Crear una red core-periphery: 20% core, 80% periphery
      core_size <- max(2, ceiling(n * 0.2))
      periphery_size <- n - core_size
      
      # Crear grafo vacío
      g <- make_empty_graph(n, directed = FALSE)
      
      # Conectar completamente el core
      core_nodes <- 1:core_size
      core_edges <- combn(core_nodes, 2)
      g <- add_edges(g, as.vector(core_edges))
      
      # Conectar cada nodo de la periferia con al menos un nodo del core
      periphery_nodes <- (core_size + 1):n
      for (node in periphery_nodes) {
        # Conectar con un nodo aleatorio del core
        core_connect <- sample(core_nodes, 1)
        g <- add_edges(g, c(node, core_connect))
      }
      
      # Añadir algunas conexiones aleatorias entre periferia y core
      additional_edges <- ceiling(p * periphery_size)
      for (i in 1:additional_edges) {
        periphery_node <- sample(periphery_nodes, 1)
        core_node <- sample(core_nodes, 1)
        g <- add_edges(g, c(periphery_node, core_node))
      }
    } else if (topology == "small_world") {
      # Modelo Watts-Strogatz
      g <- sample_smallworld(dim = 1, size = n, nei = 2, p = 0.1)
    } else if (topology == "complete") {
      g <- make_full_graph(n)
    }
    
    # Asignar tamaños de banco (basado en activos)
    bank_sizes <- data.frame(
      id = 1:n,
      assets = rlnorm(n, meanlog = 12, sdlog = 1.2)  # Distribución realista de tamaños bancarios con valores mayores
    )
    
    # Ordenar por tamaño para identificación
    bank_sizes <- bank_sizes %>% arrange(desc(assets))
    
    # Asignar exposiciones interbancarias
    E(g)$weight <- runif(ecount(g), min = 0.5, max = 5)
    
    # Asignar atributos a bancos
    V(g)$size <- bank_sizes$assets / max(bank_sizes$assets) * 30 + 5  # Para visualización
    V(g)$assets <- bank_sizes$assets
    V(g)$capital <- V(g)$assets * (input$capital_ratio / 100)
    V(g)$interbank_assets <- V(g)$assets * (input$interbank_exposure / 100)
    V(g)$illiquid_assets <- V(g)$assets * (input$illiquid_assets / 100)
    V(g)$liquid_assets <- V(g)$assets - V(g)$interbank_assets - V(g)$illiquid_assets
    V(g)$status <- "safe"  # Estado inicial
    V(g)$bank_id <- 1:n
    
    # Calcular métricas de centralidad
    V(g)$degree <- degree(g)
    V(g)$betweenness <- betweenness(g)
    V(g)$systemic_importance <- scale(V(g)$assets) * 0.5 + scale(V(g)$degree) * 0.3 + scale(V(g)$betweenness) * 0.2
    
    return(g)
  }
  
  # Función para aplicar shock y calcular contagio
  simulate_contagion <- function(g, shock_size, num_initial, shock_type) {
    # Copiar el grafo
    g_sim <- g
    n <- vcount(g_sim)
    
    # Seleccionar bancos iniciales para el shock
    if (shock_type == "random") {
      initial_banks <- sample(1:n, num_initial)
    } else if (shock_type == "largest") {
      initial_banks <- order(V(g_sim)$assets, decreasing = TRUE)[1:num_initial]
    } else if (shock_type == "connected") {
      initial_banks <- order(V(g_sim)$degree, decreasing = TRUE)[1:num_initial]
    }
    
    # Aplicar shock inicial
    for (bank in initial_banks) {
      V(g_sim)$illiquid_assets[bank] <- V(g_sim)$illiquid_assets[bank] * (1 - shock_size/100)
      # Comprobar si el banco es insolvente
      if (V(g_sim)$illiquid_assets[bank] < V(g_sim)$assets[bank] - V(g_sim)$capital[bank]) {
        V(g_sim)$status[bank] <- "defaulted"
      } else {
        V(g_sim)$status[bank] <- "shocked"
      }
    }
    
    # Registrar estado inicial para la animación
    history <- data.frame(
      step = 0,
      defaulted = sum(V(g_sim)$status == "defaulted"),
      shocked = sum(V(g_sim)$status == "shocked"),
      safe = sum(V(g_sim)$status == "safe")
    )
    
    # Proceso de contagio
    step <- 1
    changed <- TRUE
    
    while (changed && step <= 100) {  # Evitar bucles infinitos
      changed <- FALSE
      
      # Identificar bancos que han quebrado en este paso
      defaulted_banks <- which(V(g_sim)$status == "defaulted")
      
      # Para cada banco quebrado, propagar pérdidas a sus acreedores
      for (bank in defaulted_banks) {
        # Obtener vecinos (acreedores)
        creditors <- neighbors(g_sim, bank, mode = "in")
        
        # Si hay acreedores
        if (length(creditors) > 0) {
          # Calcular pérdida por acreedor
          loss_per_creditor <- V(g_sim)$interbank_assets[bank] / length(creditors)
          
          # Aplicar pérdidas a cada acreedor
          for (creditor in creditors) {
            if (V(g_sim)$status[creditor] == "safe") {
              # Reducir activos interbancarios
              V(g_sim)$interbank_assets[creditor] <- V(g_sim)$interbank_assets[creditor] - loss_per_creditor
              
              # Comprobar si el acreedor quiebra
              if (V(g_sim)$interbank_assets[creditor] < 0 || 
                  (V(g_sim)$assets[creditor] - V(g_sim)$capital[creditor]) > 
                  (V(g_sim)$liquid_assets[creditor] + V(g_sim)$interbank_assets[creditor] + V(g_sim)$illiquid_assets[creditor])) {
                V(g_sim)$status[creditor] <- "defaulted"
                changed <- TRUE
              } else {
                V(g_sim)$status[creditor] <- "shocked"
                changed <- TRUE
              }
            }
          }
        }
      }
      
      # Registrar estado para este paso
      if (changed) {
        history <- rbind(history, data.frame(
          step = step,
          defaulted = sum(V(g_sim)$status == "defaulted"),
          shocked = sum(V(g_sim)$status == "shocked"),
          safe = sum(V(g_sim)$status == "safe")
        ))
      }
      
      step <- step + 1
    }
    
    # Calcular métricas finales
    total_banks <- vcount(g_sim)
    defaulted_banks <- sum(V(g_sim)$status == "defaulted")
    defaulted_pct <- defaulted_banks / total_banks * 100
    
    initial_assets <- sum(V(g)$assets)
    remaining_assets <- sum(V(g_sim)$assets[V(g_sim)$status != "defaulted"])
    assets_lost_pct <- (initial_assets - remaining_assets) / initial_assets * 100
    
    # Identificar bancos sistémicamente importantes que quebraron
    systemic_banks <- data.frame(
      bank_id = V(g_sim)$bank_id,
      assets = V(g_sim)$assets,
      connections = degree(g_sim),
      systemic_importance = V(g_sim)$systemic_importance,
      status = V(g_sim)$status
    )
    
    # Devolver resultados
    return(list(
      graph = g_sim,
      defaulted_pct = defaulted_pct,
      assets_lost_pct = assets_lost_pct,
      history = history,
      systemic_banks = systemic_banks
    ))
  }
  
  # Función para preparar datos para visNetwork
  prepare_vis_data <- function(g) {
    # Preparar nodos
    nodes <- data.frame(
      id = 1:vcount(g),
      label = paste0("B", 1:vcount(g)),
      value = V(g)$assets / 1e9,  # Para tamaño de nodo
      title = paste0("Banco ", 1:vcount(g), "<br>",
                     "Activos: $", round(V(g)$assets / 1e9, 1), " MM<br>",
                     "Capital: $", round(V(g)$capital / 1e9, 1), " MM<br>",
                     "Conexiones: ", degree(g)),
      group = V(g)$status
    )
    
    # Preparar enlaces
    edges <- data.frame(
      from = get.edgelist(g)[,1],
      to = get.edgelist(g)[,2],
      width = E(g)$weight,
      title = paste0("Exposición: $", round(E(g)$weight, 2), " MM")
    )
    
    return(list(nodes = nodes, edges = edges))
  }
  
  # Evento: Botón "Ejecutar Simulación"
  observeEvent(input$run_simulation, {
    # Crear red
    g <- create_network(input$num_banks, input$network_topology, input$connection_prob)
    network_data(g)
    
    # Simular contagio
    results <- simulate_contagion(g, input$shock_size, input$initial_defaults, input$shock_type)
    simulation_results(results)
    
    # Actualizar historial de simulaciones
    history_df <- simulation_history()
    new_row <- data.frame(
      sim_id = nrow(history_df) + 1,
      topology = input$network_topology,
      num_banks = input$num_banks,
      connection_prob = input$connection_prob,
      capital_ratio = input$capital_ratio,
      shock_size = input$shock_size,
      initial_defaults = input$initial_defaults,
      shock_type = input$shock_type,
      defaulted_pct = results$defaulted_pct,
      assets_lost_pct = results$assets_lost_pct
    )
    simulation_history(rbind(history_df, new_row))
  })
  
  # Evento: Botón "Reiniciar"
  observeEvent(input$reset, {
    network_data(NULL)
    simulation_results(NULL)
  })
  
  # Salida: Gráfico de la red
  output$network_graph <- renderVisNetwork({
    req(network_data())
    g <- network_data()
    
    # Si hay resultados de simulación, usar el grafo con contagio
    if (!is.null(simulation_results())) {
      g <- simulation_results()$graph
    }
    
    # Preparar datos para visNetwork
    vis_data <- prepare_vis_data(g)
    
    # Crear gráfico interactivo
    visNetwork(vis_data$nodes, vis_data$edges, width = "100%", height = "100%") %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visGroups(groupname = "safe", color = "#4CAF50") %>%
      visGroups(groupname = "shocked", color = "#FF9800") %>%
      visGroups(groupname = "defaulted", color = "#F44336") %>%
      visOptions(highlightNearest = TRUE, selectedBy = "group") %>%
      visPhysics(stabilization = TRUE)
  })
  
  # Salida: Cajas de valores para resultados
  output$default_box <- renderValueBox({
    req(simulation_results())
    valueBox(
      paste0(round(simulation_results()$defaulted_pct, 1), "%"),
      "Bancos en quiebra",
      icon = icon("bank"),
      color = if(simulation_results()$defaulted_pct > 50) "red" else "orange"
    )
  })
  
  output$assets_lost_box <- renderValueBox({
    req(simulation_results())
    valueBox(
      paste0(round(simulation_results()$assets_lost_pct, 1), "%"),
      "Activos perdidos",
      icon = icon("chart-line"),
      color = if(simulation_results()$assets_lost_pct > 30) "red" else "orange"
    )
  })
  
  # Salida: Gráfico de evolución del contagio
  output$contagion_plot <- renderPlotly({
    req(simulation_results())
    history <- simulation_results()$history
    
    p <- ggplot(history %>% pivot_longer(cols = c(defaulted, shocked, safe), 
                                         names_to = "status", values_to = "count")) +
      geom_line(aes(x = step, y = count, color = status), size = 1) +
      scale_color_manual(values = c("defaulted" = "#F44336", "shocked" = "#FF9800", "safe" = "#4CAF50")) +
      labs(x = "Paso", y = "Número de bancos", color = "Estado") +
      theme_minimal()
    
    ggplotly(p) %>% layout(legend = list(orientation = "h", y = 1.1))
  })
  
  # Salida: Tabla de bancos sistémicamente importantes
  output$systemic_banks <- DT::renderDataTable({
    req(simulation_results())
    banks_df <- simulation_results()$systemic_banks %>%
      arrange(desc(systemic_importance)) %>%
      mutate(
        assets_display = paste0("$", round(assets / 1e9, 1), " MM"),
        systemic_importance = round(systemic_importance, 2)
      ) %>%
      select(bank_id, assets_display, connections, systemic_importance, status)
    
    DT::datatable(banks_df, 
                  colnames = c("Banco ID", "Activos", "Conexiones", "Importancia Sistémica", "Estado"),
                  options = list(pageLength = 5, scrollX = TRUE, autoWidth = TRUE)) %>%
      DT::formatStyle("status",
                      backgroundColor = DT::styleEqual(
                        c("safe", "shocked", "defaulted"),
                        c("#4CAF50", "#FF9800", "#F44336")
                      ))
  })
  
  # Salida: Gráfico de análisis de robustez
  output$robustness_plot <- renderPlotly({
    req(nrow(simulation_history()) > 0)
    
    p <- ggplot(simulation_history()) +
      geom_point(aes(x = shock_size, y = defaulted_pct, color = topology, size = num_banks)) +
      facet_wrap(~ capital_ratio, labeller = label_both) +
      labs(x = "Magnitud del shock (%)", y = "Bancos en quiebra (%)", 
           color = "Topología", size = "Número de bancos") +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Ejecutar aplicación
shinyApp(ui = ui, server = server)