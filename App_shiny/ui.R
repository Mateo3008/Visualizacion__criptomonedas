# ============================================================
# Archivo: ui.R
# Proyecto: EDA de Criptomonedas con API de Mercado
# ============================================================

library(shinydashboard)

ui <- dashboardPage(
  skin = "black",

  # ── 1. Cabecera ──────────────────────────────────────────
  dashboardHeader(
    title = tagList(
      tags$img(src = "crypto.svg", height = "38", style = "padding-right:8px;"),
      "Crypto EDA Dashboard"
    ),
    titleWidth = 340
  ),

  # ── 2. Barra lateral ─────────────────────────────────────
  dashboardSidebar(
    width = 240,
    sidebarMenu(
      menuItem("Introducción",      tabName = "overview",    icon = icon("gauge")),
      menuItem("Visión General",      tabName = "overview",    icon = icon("gauge")),
      menuItem("Precios & Tendencia", tabName = "precios",     icon = icon("chart-line")),
      menuItem("Retornos & Riesgo",   tabName = "retornos",    icon = icon("percent")),
      menuItem("Correlaciones",       tabName = "correlacion", icon = icon("table-cells")),
      menuItem("Comparador",          tabName = "comparador",  icon = icon("sliders"))
    )
  ),

  # ── 3. Cuerpo ─────────────────────────────────────────────
  dashboardBody(

    # CSS personalizado
    tags$head(tags$style(HTML("
      .skin-black .main-header .logo { background-color: #1a1a2e; }
      .skin-black .main-header .navbar { background-color: #16213e; }
      .skin-black .main-sidebar { background-color: #0f3460; }
      .skin-black .sidebar-menu > li.active > a,
      .skin-black .sidebar-menu > li:hover > a {
        background-color: #e94560;
        border-left: 3px solid #f0a500;
      }
      .small-box .inner { text-align: center; }
      .small-box h3, .small-box p { text-align: center; }
      .value-box-positive { color: #2ecc71 !important; }
      .value-box-negative { color: #e74c3c !important; }
      .content-wrapper, .right-side { background-color: #f4f6f9; }
    "))),

    tabItems(

      # ══════════════════════════════════════════════════════
      # TAB 1 — VISIÓN GENERAL
      # ══════════════════════════════════════════════════════
      tabItem(tabName = "overview",
        h2("📊 Visión General del Mercado"),
        p("Snapshot en tiempo real de las principales criptomonedas analizadas en este estudio."),

        fluidRow(
          box(
            title = "Resumen del Dataset", status = "primary",
            solidHeader = TRUE, collapsible = TRUE, width = 12,
            fluidRow(
              column(3, valueBoxOutput("vbox_monedas",   width = 12)),
              column(3, valueBoxOutput("vbox_registros", width = 12)),
              column(3, valueBoxOutput("vbox_periodo",   width = 12)),
              column(3, valueBoxOutput("vbox_missing",   width = 12))
            )
          )
        ),

        fluidRow(
          box(
            title = "Precios Actuales y Métricas de Mercado",
            status = "warning", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("tabla_overview")
          )
        ),

        fluidRow(
          box(
            title = "Capitalización de Mercado (USD)",
            status = "success", solidHeader = TRUE, width = 6,
            plotlyOutput("plot_market_cap", height = 320)
          ),
          box(
            title = "Volumen de Negociación 24h (USD)",
            status = "info", solidHeader = TRUE, width = 6,
            plotlyOutput("plot_volume", height = 320)
          )
        )
      ),

      # ══════════════════════════════════════════════════════
      # TAB 2 — PRECIOS & TENDENCIA
      # ══════════════════════════════════════════════════════
      tabItem(tabName = "precios",
        h2("📈 Análisis de Precios y Tendencia"),
        p("Serie de tiempo del precio de cierre con medias móviles para identificar tendencias."),

        fluidRow(
          box(
            title = "Configuración", status = "primary",
            solidHeader = TRUE, width = 3,
            selectInput("sel_crypto_precio", "Criptomoneda:",
                        choices = CRYPTOS, selected = "BTC"),
            sliderInput("sel_dias", "Últimos N días:",
                        min = 30, max = 365, value = 180, step = 30),
            checkboxInput("chk_ma7",  "Media Móvil 7 días",  value = TRUE),
            checkboxInput("chk_ma30", "Media Móvil 30 días", value = TRUE),
            checkboxInput("chk_bb",   "Bandas de Bollinger",  value = FALSE),
            hr(),
            selectInput("tipo_precio", "Mostrar precio:",
                        choices = c("Cierre" = "close",
                                    "Apertura" = "open",
                                    "Máximo" = "high",
                                    "Mínimo" = "low"))
          ),
          box(
            title = "Serie de Tiempo del Precio",
            status = "success", solidHeader = TRUE, width = 9,
            plotlyOutput("plot_precio_serie", height = 380)
          )
        ),

        fluidRow(
          box(
            title = "Gráfico de Velas (Candlestick) — Últimos 60 días",
            status = "warning", solidHeader = TRUE, width = 12,
            plotlyOutput("plot_candlestick", height = 380)
          )
        )
      ),

      # ══════════════════════════════════════════════════════
      # TAB 3 — RETORNOS & RIESGO
      # ══════════════════════════════════════════════════════
      tabItem(tabName = "retornos",
        h2("💹 Retornos Diarios y Análisis de Riesgo"),
        p("Distribución de los retornos diarios, volatilidad histórica y métricas de riesgo."),

        fluidRow(
          box(
            title = "Configuración", status = "primary",
            solidHeader = TRUE, width = 3,
            selectInput("sel_crypto_ret", "Criptomoneda:",
                        choices = CRYPTOS, selected = "BTC"),
            sliderInput("sel_dias_ret", "Período (días):",
                        min = 30, max = 365, value = 365, step = 30),
            selectInput("tipo_grafico_ret", "Tipo de gráfico:",
                        choices = c(
                          "Histograma de Retornos"    = "hist",
                          "Retornos en el Tiempo"     = "serie",
                          "Box Plot por Mes"          = "boxplot",
                          "Volatilidad Rodante 30d"   = "vol_rodante"
                        ))
          ),
          box(
            title = "Visualización de Retornos",
            status = "success", solidHeader = TRUE, width = 9,
            plotlyOutput("plot_retornos", height = 380)
          )
        ),

        fluidRow(
          box(
            title = "Métricas de Riesgo",
            status = "danger", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("tabla_riesgo")
          )
        )
      ),

      # ══════════════════════════════════════════════════════
      # TAB 4 — CORRELACIONES
      # ══════════════════════════════════════════════════════
      tabItem(tabName = "correlacion",
        h2("🔗 Análisis de Correlaciones"),
        p("Exploración de la correlación entre los retornos diarios de las distintas criptomonedas."),

        fluidRow(
          box(
            title = "Configuración", status = "primary",
            solidHeader = TRUE, width = 3,
            sliderInput("sel_dias_corr", "Período (días):",
                        min = 30, max = 365, value = 365, step = 30),
            selectInput("metodo_corr", "Método de correlación:",
                        choices = c("Pearson" = "pearson",
                                    "Spearman" = "spearman")),
            checkboxGroupInput("sel_cryptos_corr", "Criptomonedas:",
                               choices = CRYPTOS,
                               selected = CRYPTOS)
          ),
          box(
            title = "Mapa de Calor — Correlación de Retornos",
            status = "warning", solidHeader = TRUE, width = 9,
            plotlyOutput("plot_heatmap_corr", height = 420)
          )
        ),

        fluidRow(
          box(
            title = "Dispersión de Retornos entre Dos Monedas",
            status = "info", solidHeader = TRUE, width = 12,
            fluidRow(
              column(4,
                selectInput("corr_x", "Eje X:", choices = CRYPTOS, selected = "BTC"),
                selectInput("corr_y", "Eje Y:", choices = CRYPTOS, selected = "ETH")
              ),
              column(8, plotlyOutput("plot_scatter_corr", height = 340))
            )
          )
        )
      ),

      # ══════════════════════════════════════════════════════
      # TAB 5 — COMPARADOR
      # ══════════════════════════════════════════════════════
      tabItem(tabName = "comparador",
        h2("⚖️ Comparador de Rendimiento"),
        p("Compara el rendimiento acumulado normalizado de múltiples criptomonedas en el mismo período."),

        fluidRow(
          box(
            title = "Configuración", status = "primary",
            solidHeader = TRUE, width = 3,
            checkboxGroupInput("sel_cryptos_comp", "Selecciona monedas:",
                               choices = CRYPTOS,
                               selected = c("BTC", "ETH", "SOL")),
            sliderInput("sel_dias_comp", "Período (días):",
                        min = 30, max = 365, value = 365, step = 30),
            radioButtons("tipo_norm", "Normalización:",
                         choices = c("Base 100 (inicio = 100)" = "base100",
                                     "Retorno acumulado (%)"   = "pct"),
                         selected = "base100")
          ),
          box(
            title = "Rendimiento Acumulado Comparado",
            status = "success", solidHeader = TRUE, width = 9,
            plotlyOutput("plot_comparador", height = 380)
          )
        ),

        fluidRow(
          box(
            title = "Resumen Estadístico Comparativo",
            status = "info", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("tabla_comparador")
          )
        )
      )

    ) # fin tabItems
  )   # fin dashboardBody
)     # fin dashboardPage
