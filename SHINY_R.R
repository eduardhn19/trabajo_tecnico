library(shiny)
library(tidyverse)
library(lubridate)

# Cargar datos
resultados_df <- read.csv("data/resultados_tasas_ciep.csv", header = T, sep = ",", dec = ".")
resultados_df$Periodo <- factor(resultados_df$Periodo, levels = c("Abr2023", "Sep2023", "Nov2023", "Abr2024", "Sep2024", "Nov2024", "Abr2025"))

# Función para convertir "Abr2023", etc., en fechas válidas
convertir_a_fecha <- function(periodo_vector) {
  periodo_vector <- gsub("Abr", "Apr", periodo_vector)
  periodo_vector <- gsub("Ago", "Aug", periodo_vector)
  periodo_vector <- gsub("Dic", "Dec", periodo_vector)
  return(dmy(paste0("01-", periodo_vector)))
}

# Limpiar y procesar los datos (enfocándonos en respuesta y rechazo)
crear_datos_largos <- function(df, tipo = c("respuesta", "rechazo")) {
  tipo <- match.arg(tipo)
  
  if (tipo == "respuesta") {
    datos <- df %>%
      select(Periodo, TasaExito, RR1, RR3) %>%
      pivot_longer(
        cols = -Periodo,
        names_to = "Tipo",
        values_to = "Porcentaje"
      ) %>%
      mutate(
        Tipo = recode(Tipo,
                      "TasaExito" = "Tasa de éxito (antigua)",
                      "RR1" = "RR1: respuesta mínima",
                      "RR3" = "RR3: proporción de elegibilidad"),
        Fecha = convertir_a_fecha(Periodo)
      )
  } else {
    datos <- df %>%
      select(Periodo, TasaRechazo, REF1, REF2) %>%
      pivot_longer(
        cols = -Periodo,
        names_to = "Tipo",
        values_to = "Porcentaje"
      ) %>%
      mutate(
        Tipo = recode(Tipo,
                      "TasaRechazo" = "Tasa de rechazo (antigua)",
                      "REF1" = "REF1: rechazo mínimo",
                      "REF2" = "REF2: proporción de elegibilidad"),
        Fecha = convertir_a_fecha(Periodo)
      )
  }
  
  return(datos)
}

# Función para crear el gráfico de intentos con etiquetas personalizadas
crear_grafico <- function(df, tipo) {
  if (tipo == "intentos") {
    df <- resultados_df %>%
      mutate(
        Fecha = convertir_a_fecha(Periodo),
        posicion_vjust = case_when(
          Periodo == "Abr2023" ~ 1.5,
          Periodo == "Sep2023" ~ -1.5,
          Periodo == "Nov2023" ~ 1.5,
          Periodo == "Abr2024" ~ 1.5,
          Periodo == "Sep2024" ~ -1.5,
          Periodo == "Nov2024" ~ 1.5,
          Periodo == "Abr2025" ~ -1.5,
          TRUE ~ 1.5
        )
      )
    
    ggplot(df, aes(x = Fecha, y = Intentos)) +
      geom_line(size = 1.2, alpha = 0.9, color = "#2E86AB") +
      geom_point(size = 2.2, shape = 21, fill = "white", stroke = 1.2, color = "#2E86AB") +
      geom_text(
        aes(label = Intentos, vjust = posicion_vjust),
        size = 5, show.legend = FALSE
      ) +
      scale_y_continuous(limits = c(20000, 30000), expand = c(0, 0)) +
      scale_x_date(
        breaks = convertir_a_fecha(levels(resultados_df$Periodo)),
        date_labels = "%b %Y"
      ) +
      labs(x = "Fecha", y = "Cantidad de intentos") +
      theme_minimal(base_size = 16) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = 0.5),
        axis.line.y = element_line(color = "black", size = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "bottom"
      )
  } else {
    y_limite <- if (tipo == "respuesta") c(0, 25) else c(0, 40)
    df$Porcentaje <- pmax(df$Porcentaje, 0)
    
    ggplot(df, aes(x = Fecha, y = Porcentaje, color = Tipo)) +
      geom_line(size = 1.5, alpha = 0.95) +
      geom_point(size = 3, shape = 21, fill = "white", stroke = 1.2) +
      geom_text(aes(label = sprintf("%.1f", Porcentaje),
                    vjust = case_when(
                      Tipo == "Tasa de rechazo (antigua)" ~ 1.5,
                      Tipo == "REF1: rechazo mínimo" ~ -1.5,
                      Tipo == "REF2: proporción de elegibilidad" ~ -1.5,
                      Tipo == "Tasa de éxito (antigua)" ~ 1.5,
                      Tipo == "RR1: respuesta mínima" ~ -1.5,
                      Tipo == "RR3: proporción de elegibilidad" ~ -1.5,
                      TRUE ~ 1.5)),
                size = 5, show.legend = FALSE) +
      scale_color_brewer(palette = "Set1") +
      scale_y_continuous(limits = y_limite, expand = c(0, 0)) +
      scale_x_date(
        breaks = convertir_a_fecha(levels(resultados_df$Periodo)),
        date_labels = "%b %Y"
      ) +
      labs(x = "Fecha", y = "Porcentaje", color = "Indicador") +
      theme_minimal(base_size = 16) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = 0.5),
        axis.line.y = element_line(color = "black", size = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
  }
}

# Interfaz de usuario (UI)
ui <- fluidPage(
  titlePanel("Revisión de tasas (2023-2025)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,  # o incluso 1
      selectInput("tipo_tasa", "Seleccione el tipo de gráfico:",
                  choices = c("Tasas de respuesta" = "respuesta", "Tasas de rechazo" = "rechazo", "Intentos de contacto" = "intentos")),
      selectInput("periodo_select", "Tabule según periodo:",
                  choices = unique(resultados_df$Periodo))
    ),
    
    mainPanel(
      h3("Visualización de tasas o intentos a lo largo del tiempo"),
      plotOutput("grafico_tasas"),
      br(),
      h3("Tabulación datallada por periodo seleccionado"),
      tableOutput("tabla_tasas")
    )
  )
)

# Función servidor (server)
server <- function(input, output, session) {
  
  datos_reactivos <- reactive({
    req(input$tipo_tasa)
    
    if (input$tipo_tasa == "intentos") {
      return(NULL)
    } else {
      datos <- crear_datos_largos(resultados_df, tipo = input$tipo_tasa)
      return(datos)
    }
  })
  
  output$grafico_tasas <- renderPlot({
    if (input$tipo_tasa == "intentos") {
      crear_grafico(NULL, tipo = "intentos")
    } else {
      datos <- datos_reactivos()
      if (nrow(datos) == 0) {
        plot(NA, xlab = "Fecha", ylab = "Porcentaje", main = "No hay datos disponibles para este tipo de tasa.")
      } else {
        crear_grafico(datos, tipo = input$tipo_tasa)
      }
    }
  })
  
  output$tabla_tasas <- renderTable({
    req(input$periodo_select)
    resultados_df %>%
      filter(Periodo == input$periodo_select)
  })
}

shinyApp(ui = ui, server = server)
