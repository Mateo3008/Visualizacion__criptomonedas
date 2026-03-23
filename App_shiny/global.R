# ============================================================
# Archivo: global.R
# Proyecto: EDA de Criptomonedas con API de Mercado
# Descripción: Carga de librerías, conexión a API y
#              preparación de datos para la app Shiny.
# ============================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(lubridate)
library(scales)
library(jsonlite)
library(httr)

# ---- Configuración de la API ----
API_KEY    <- "ce6e922820dabbb917d5f6fd82b867726fbf320cf3f7414b33748c19e9514aae"
BASE_URL   <- "https://min-api.cryptocompare.com/data"

# ---- Lista de criptomonedas disponibles en la app ----
CRYPTOS <- c(
  "Bitcoin"  = "BTC",
  "Ethereum" = "ETH",
  "BNB"      = "BNB",
  "Solana"   = "SOL",
  "XRP"      = "XRP"
)

# ---- Función: obtener datos históricos diarios (OHLCV) ----
get_historical_daily <- function(fsym, tsym = "USD", limit = 365) {
  url <- paste0(
    BASE_URL, "/v2/histoday",
    "?fsym=", fsym,
    "&tsym=", tsym,
    "&limit=", limit,
    "&api_key=", API_KEY
  )
  resp <- GET(url)
  if (http_error(resp)) {
    warning("Error en la solicitud a la API para: ", fsym)
    return(NULL)
  }
  data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  if (data$Response != "Success") {
    warning("La API devolvió un error para: ", fsym)
    return(NULL)
  }
  df <- as.data.frame(data$Data$Data)
  df <- df %>%
    mutate(
      fecha      = as.Date(as.POSIXct(time, origin = "1970-01-01")),
      simbolo    = fsym,
      retorno    = (close - lag(close)) / lag(close) * 100,
      volatilidad = abs(high - low) / open * 100,
      rango      = high - low
    ) %>%
    filter(!is.na(retorno))
  return(df)
}

# ---- Función: obtener precio actual y métricas de mercado ----
get_price_overview <- function(fsyms, tsym = "USD") {
  url <- paste0(
    BASE_URL, "/pricemultifull",
    "?fsyms=", paste(fsyms, collapse = ","),
    "&tsyms=", tsym,
    "&api_key=", API_KEY
  )
  resp  <- GET(url)
  if (http_error(resp)) return(NULL)
  data  <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  raw   <- data$RAW

  rows <- lapply(fsyms, function(sym) {
    d <- raw[[sym]][[tsym]]
    data.frame(
      simbolo          = sym,
      precio           = d$PRICE,
      cambio_24h_pct   = d$CHANGEPCT24HOUR,
      volumen_24h      = d$VOLUME24HOURTO,
      cap_mercado      = d$MKTCAP,
      high_24h         = d$HIGH24HOUR,
      low_24h          = d$LOW24HOUR,
      stringsAsFactors = FALSE
    )
  })
  bind_rows(rows)
}

# ---- Carga inicial de datos al arrancar la app ----
# Datos históricos de todas las monedas (carga en arranque)
cat("Cargando datos históricos desde la API...\n")
hist_data <- bind_rows(lapply(CRYPTOS, get_historical_daily))
cat("Datos históricos cargados:", nrow(hist_data), "filas.\n")

# Resumen de precios actuales
cat("Obteniendo precios actuales...\n")
prices_overview <- get_price_overview(CRYPTOS)
cat("Precios actuales obtenidos para:", paste(prices_overview$simbolo, collapse = ", "), "\n")
