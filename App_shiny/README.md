# 📊 Crypto EDA Dashboard — Proyecto Shiny

## 🎯 Objetivo del Proyecto

Construir un dashboard interactivo en R/Shiny que realice un **Análisis Exploratorio de Datos (EDA)** completo sobre el mercado de criptomonedas, consumiendo datos en tiempo real desde la API de CryptoCompare. El dashboard permite a analistas e inversores explorar el comportamiento histórico de los precios, los patrones de retornos y las relaciones entre activos digitales de forma visual e interactiva.

---

## 🔍 Problema que Resuelve

El mercado de criptomonedas es altamente volátil y difícil de interpretar sin herramientas adecuadas. Los inversores y analistas necesitan:

1. **Visualizar tendencias** de precios con indicadores técnicos (medias móviles, Bandas de Bollinger).
2. **Cuantificar el riesgo** a través de métricas estadísticas (VaR, Sharpe, volatilidad).
3. **Entender las correlaciones** entre distintos activos para tomar decisiones de portafolio.
4. **Comparar el rendimiento** acumulado entre criptomonedas en el mismo horizonte temporal.

---

## 🗂️ Estructura del Proyecto

```
Crypto_EDA_App/
├── global.R      # Librerías, configuración API, carga de datos
├── ui.R          # Interfaz de usuario (shinydashboard)
├── server.R      # Lógica del servidor y gráficos reactivos
└── www/
    └── crypto.svg  # Logo de la aplicación
```

---

## 📦 Dependencias (R)

Instalar antes de ejecutar:

```r
install.packages(c(
  "shiny", "shinydashboard", "tidyverse",
  "plotly", "DT", "lubridate", "scales",
  "jsonlite", "httr", "zoo"
))
```

---

## 🚀 Cómo Ejecutar

```r
# En RStudio, desde el directorio del proyecto:
shiny::runApp("Crypto_EDA_App")
```

---

## 🖥️ Pestañas del Dashboard

| Pestaña | Descripción |
|---|---|
| **Visión General** | Precios actuales, capitalización de mercado y volumen de negociación 24h |
| **Precios & Tendencia** | Serie temporal con MA7, MA30, Bandas de Bollinger y gráfico de velas |
| **Retornos & Riesgo** | Histograma de retornos, box plots por mes, volatilidad rodante 30d |
| **Correlaciones** | Heatmap de correlación entre retornos y scatter plot interactivo |
| **Comparador** | Rendimiento acumulado normalizado (base 100 o %) entre múltiples activos |

---

## 🔑 Configuración de la API

La app usa **CryptoCompare API** (https://min-api.cryptocompare.com).

La clave API está definida en `global.R`:
```r
API_KEY <- "tu_clave_aqui"
```

**Endpoints utilizados:**
- `GET /data/v2/histoday` — OHLCV histórico diario
- `GET /data/pricemultifull` — Precio y métricas en tiempo real

---

## 📈 Criptomonedas Analizadas

| Símbolo | Nombre    |
|---------|-----------|
| BTC     | Bitcoin   |
| ETH     | Ethereum  |
| BNB     | BNB       |
| SOL     | Solana    |
| XRP     | XRP       |

---

## 📐 Métricas de Riesgo Calculadas

- **Retorno medio diario** — promedio aritmético de retornos
- **Desviación estándar** — medida de dispersión/volatilidad
- **VaR 95%** — Valor en Riesgo: pérdida máxima esperada el 5% del tiempo
- **Sharpe aproximado** — retorno por unidad de riesgo (sin tasa libre de riesgo)
- **% Días positivos** — proporción de sesiones con retorno positivo
- **Volatilidad rodante 30d** — desviación estándar móvil de 30 días

---

*Desarrollado como proyecto de EDA con R/Shiny · API: CryptoCompare*
