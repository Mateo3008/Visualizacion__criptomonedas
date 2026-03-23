# ============================================================
# Archivo: server.R
# Proyecto: EDA de Criptomonedas con API de Mercado
# ============================================================

server <- function(input, output, session) {

  # ── Reactivos globales ────────────────────────────────────

  # Filtra hist_data para el período seleccionado (genérico por ndays)
  datos_periodo <- function(ndays, simbolo = NULL) {
    cutoff <- Sys.Date() - ndays
    d <- hist_data %>% filter(fecha >= cutoff)
    if (!is.null(simbolo)) d <- d %>% filter(simbolo == !!simbolo)
    d
  }

  # ══════════════════════════════════════════════════════════
  # TAB 1 — VISIÓN GENERAL
  # ══════════════════════════════════════════════════════════

  output$vbox_monedas <- renderValueBox({
    valueBox(length(CRYPTOS), "Criptomonedas", icon = icon("coins"), color = "yellow")
  })
  output$vbox_registros <- renderValueBox({
    valueBox(nrow(hist_data), "Registros Históricos", icon = icon("database"), color = "blue")
  })
  output$vbox_periodo <- renderValueBox({
    rango <- paste0(
      format(min(hist_data$fecha), "%d/%m/%Y"), " — ",
      format(max(hist_data$fecha), "%d/%m/%Y")
    )
    valueBox(rango, "Período de Datos", icon = icon("calendar"), color = "green")
  })
  output$vbox_missing <- renderValueBox({
    valueBox(sum(is.na(hist_data)), "Valores Faltantes", icon = icon("triangle-exclamation"), color = "red")
  })

  output$tabla_overview <- DT::renderDataTable({
    prices_overview %>%
      mutate(
        precio         = dollar(precio,       accuracy = 0.01),
        cambio_24h_pct = paste0(round(cambio_24h_pct, 2), "%"),
        volumen_24h    = dollar(volumen_24h,   accuracy = 1, scale = 1e-6, suffix = "M"),
        cap_mercado    = dollar(cap_mercado,   accuracy = 1, scale = 1e-9, suffix = "B"),
        high_24h       = dollar(high_24h,      accuracy = 0.01),
        low_24h        = dollar(low_24h,       accuracy = 0.01)
      ) %>%
      rename(
        Símbolo         = simbolo,
        `Precio (USD)`  = precio,
        `Cambio 24h`    = cambio_24h_pct,
        `Volumen 24h`   = volumen_24h,
        `Cap. Mercado`  = cap_mercado,
        `Máx. 24h`      = high_24h,
        `Mín. 24h`      = low_24h
      )
  }, options = list(pageLength = 10, dom = "t"), rownames = FALSE)

  output$plot_market_cap <- renderPlotly({
    df_mc <- prices_overview %>%
      mutate(
        simbolo    = factor(simbolo, levels = simbolo[order(cap_mercado)]),
        cap_B      = cap_mercado / 1e9
      )
    p <- ggplot(df_mc, aes(x = simbolo, y = cap_B, fill = simbolo,
                           text = paste0(simbolo, ": $", round(cap_B, 1), "B"))) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c(BTC="#F7931A", ETH="#627EEA",
                                   BNB="#F3BA2F", SOL="#9945FF", XRP="#00AAE4")) +
      labs(x = "", y = "Cap. Mercado (miles de millones USD)") +
      theme_minimal() +
      coord_flip()
    ggplotly(p, tooltip = "text")
  })

  output$plot_volume <- renderPlotly({
    df_v <- prices_overview %>%
      mutate(
        simbolo = factor(simbolo, levels = simbolo[order(volumen_24h)]),
        vol_M   = volumen_24h / 1e6
      )
    p <- ggplot(df_v, aes(x = simbolo, y = vol_M, fill = simbolo,
                          text = paste0(simbolo, ": $", round(vol_M, 0), "M"))) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c(BTC="#F7931A", ETH="#627EEA",
                                   BNB="#F3BA2F", SOL="#9945FF", XRP="#00AAE4")) +
      labs(x = "", y = "Volumen 24h (millones USD)") +
      theme_minimal() +
      coord_flip()
    ggplotly(p, tooltip = "text")
  })

  # ══════════════════════════════════════════════════════════
  # TAB 2 — PRECIOS & TENDENCIA
  # ══════════════════════════════════════════════════════════

  datos_precio_reactivo <- reactive({
    req(input$sel_crypto_precio, input$sel_dias)
    datos_periodo(input$sel_dias, input$sel_crypto_precio) %>%
      arrange(fecha)
  })

  output$plot_precio_serie <- renderPlotly({
    df   <- datos_precio_reactivo()
    col  <- input$tipo_precio

    p <- ggplot(df, aes(x = fecha)) +
      geom_line(aes(y = .data[[col]], color = "Precio"), linewidth = 0.8)

    if (input$chk_ma7) {
      df <- df %>% mutate(ma7 = zoo::rollmean(close, 7, fill = NA, align = "right"))
      p <- p + geom_line(data = df, aes(y = ma7, color = "MA 7d"), linewidth = 0.8, linetype = "dashed")
    }
    if (input$chk_ma30) {
      df <- df %>% mutate(ma30 = zoo::rollmean(close, 30, fill = NA, align = "right"))
      p <- p + geom_line(data = df, aes(y = ma30, color = "MA 30d"), linewidth = 0.8, linetype = "dotted")
    }
    if (input$chk_bb) {
      df <- df %>%
        mutate(
          ma20  = zoo::rollmean(close, 20, fill = NA, align = "right"),
          sd20  = zoo::rollapply(close, 20, sd, fill = NA, align = "right"),
          bb_up = ma20 + 2 * sd20,
          bb_lo = ma20 - 2 * sd20
        )
      p <- p +
        geom_ribbon(data = df, aes(ymin = bb_lo, ymax = bb_up), alpha = 0.15, fill = "steelblue") +
        geom_line(data = df, aes(y = bb_up), color = "steelblue", linewidth = 0.5) +
        geom_line(data = df, aes(y = bb_lo), color = "steelblue", linewidth = 0.5)
    }

    p <- p +
      scale_color_manual(values = c("Precio" = "#e94560", "MA 7d" = "#f0a500", "MA 30d" = "#2ecc71")) +
      labs(
        title  = paste("Precio de", input$sel_crypto_precio, "— últimos", input$sel_dias, "días"),
        x      = "", y = "Precio (USD)", color = ""
      ) +
      theme_minimal()

    ggplotly(p)
  })

  output$plot_candlestick <- renderPlotly({
    df <- datos_precio_reactivo() %>% tail(60)
    plot_ly(
      df, x = ~fecha, type = "candlestick",
      open = ~open, close = ~close, high = ~high, low = ~low,
      increasing = list(line = list(color = "#2ecc71")),
      decreasing = list(line = list(color = "#e74c3c"))
    ) %>%
      layout(
        title  = paste("Candlestick —", input$sel_crypto_precio, "— últimos 60 días"),
        xaxis  = list(title = ""),
        yaxis  = list(title = "Precio (USD)"),
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
  })

  # ══════════════════════════════════════════════════════════
  # TAB 3 — RETORNOS & RIESGO
  # ══════════════════════════════════════════════════════════

  datos_ret_reactivo <- reactive({
    req(input$sel_crypto_ret, input$sel_dias_ret)
    datos_periodo(input$sel_dias_ret, input$sel_crypto_ret) %>%
      arrange(fecha) %>%
      mutate(
        mes     = floor_date(fecha, "month"),
        vol30   = zoo::rollapply(retorno, 30, sd, fill = NA, align = "right"),
        anio_mes = format(fecha, "%Y-%m")
      )
  })

  output$plot_retornos <- renderPlotly({
    df   <- datos_ret_reactivo()
    tipo <- input$tipo_grafico_ret

    if (tipo == "hist") {
      p <- ggplot(df, aes(x = retorno)) +
        geom_histogram(aes(fill = ..x.. > 0), bins = 50, alpha = 0.8, show.legend = FALSE) +
        scale_fill_manual(values = c(`TRUE` = "#2ecc71", `FALSE` = "#e74c3c")) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
        geom_vline(xintercept = mean(df$retorno, na.rm = TRUE), color = "blue", linewidth = 1) +
        labs(title = "Distribución de Retornos Diarios (%)",
             x = "Retorno Diario (%)", y = "Frecuencia") +
        theme_minimal()

    } else if (tipo == "serie") {
      p <- ggplot(df, aes(x = fecha, y = retorno, fill = retorno > 0)) +
        geom_col(show.legend = FALSE, width = 1) +
        scale_fill_manual(values = c(`TRUE` = "#2ecc71", `FALSE` = "#e74c3c")) +
        labs(title = "Retornos Diarios en el Tiempo (%)",
             x = "", y = "Retorno (%)") +
        theme_minimal()

    } else if (tipo == "boxplot") {
      df_m <- df %>% mutate(mes_label = format(mes, "%b %Y"))
      p <- ggplot(df_m, aes(x = reorder(mes_label, mes), y = retorno, fill = mes_label)) +
        geom_boxplot(show.legend = FALSE) +
        labs(title = "Box Plot de Retornos por Mes",
             x = "", y = "Retorno Diario (%)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    } else { # vol_rodante
      p <- ggplot(df, aes(x = fecha, y = vol30)) +
        geom_line(color = "#e94560", linewidth = 0.8) +
        geom_area(alpha = 0.2, fill = "#e94560") +
        labs(title = "Volatilidad Rodante 30 días (desv. estándar de retornos)",
             x = "", y = "Volatilidad (%)") +
        theme_minimal()
    }

    ggplotly(p)
  })

  output$tabla_riesgo <- DT::renderDataTable({
    cutoff <- Sys.Date() - input$sel_dias_ret
    hist_data %>%
      filter(fecha >= cutoff) %>%
      group_by(simbolo) %>%
      summarise(
        `Retorno Medio (%)`    = round(mean(retorno, na.rm = TRUE), 3),
        `Desv. Estándar (%)`   = round(sd(retorno,   na.rm = TRUE), 3),
        `Retorno Máx. (%)`     = round(max(retorno,  na.rm = TRUE), 2),
        `Retorno Mín. (%)`     = round(min(retorno,  na.rm = TRUE), 2),
        `VaR 95% (%)`          = round(quantile(retorno, 0.05, na.rm = TRUE), 3),
        `Sharpe (aprox.)`      = round(mean(retorno, na.rm = TRUE) / sd(retorno, na.rm = TRUE), 3),
        `% Días Positivos`     = round(sum(retorno > 0, na.rm = TRUE) / n() * 100, 1),
        .groups = "drop"
      ) %>%
      rename(Símbolo = simbolo)
  }, options = list(pageLength = 10, dom = "t"), rownames = FALSE)

  # ══════════════════════════════════════════════════════════
  # TAB 4 — CORRELACIONES
  # ══════════════════════════════════════════════════════════

  datos_corr_wide <- reactive({
    req(input$sel_dias_corr, input$sel_cryptos_corr)
    cutoff <- Sys.Date() - input$sel_dias_corr
    hist_data %>%
      filter(fecha >= cutoff, simbolo %in% input$sel_cryptos_corr) %>%
      select(fecha, simbolo, retorno) %>%
      pivot_wider(names_from = simbolo, values_from = retorno) %>%
      select(-fecha)
  })

  output$plot_heatmap_corr <- renderPlotly({
    df_wide <- datos_corr_wide()
    req(ncol(df_wide) >= 2)
    corr_mat <- cor(df_wide, use = "complete.obs", method = input$metodo_corr)

    df_long <- as.data.frame(as.table(corr_mat)) %>%
      rename(Var1 = Var1, Var2 = Var2, Corr = Freq)

    p <- ggplot(df_long, aes(x = Var1, y = Var2, fill = Corr,
                             text = paste0(Var1, " vs ", Var2, ": ", round(Corr, 3)))) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Corr, 2)), size = 4, color = "white") +
      scale_fill_gradient2(low = "#3498db", mid = "#f4f6f9", high = "#e74c3c",
                           midpoint = 0, limits = c(-1, 1)) +
      labs(title = paste("Correlación de Retornos —", input$metodo_corr),
           x = "", y = "", fill = "r") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p, tooltip = "text")
  })

  output$plot_scatter_corr <- renderPlotly({
    req(input$corr_x, input$corr_y, input$corr_x != input$corr_y)
    cutoff <- Sys.Date() - input$sel_dias_corr
    df <- hist_data %>%
      filter(fecha >= cutoff, simbolo %in% c(input$corr_x, input$corr_y)) %>%
      select(fecha, simbolo, retorno) %>%
      pivot_wider(names_from = simbolo, values_from = retorno) %>%
      drop_na()

    cx <- input$corr_x
    cy <- input$corr_y

    p <- ggplot(df, aes(x = .data[[cx]], y = .data[[cy]])) +
      geom_point(alpha = 0.5, color = "#627EEA") +
      geom_smooth(method = "lm", se = TRUE, color = "#e94560", fill = "#e9456033") +
      labs(
        title = paste("Dispersión:", cx, "vs", cy),
        x = paste("Retorno diario", cx, "(%)"),
        y = paste("Retorno diario", cy, "(%)")
      ) +
      theme_minimal()
    ggplotly(p)
  })

  # ══════════════════════════════════════════════════════════
  # TAB 5 — COMPARADOR
  # ══════════════════════════════════════════════════════════

  datos_comp_reactivo <- reactive({
    req(input$sel_cryptos_comp, input$sel_dias_comp)
    cutoff <- Sys.Date() - input$sel_dias_comp
    hist_data %>%
      filter(fecha >= cutoff, simbolo %in% input$sel_cryptos_comp) %>%
      arrange(fecha) %>%
      group_by(simbolo) %>%
      mutate(
        precio_ini    = first(close),
        rendim_base100 = close / precio_ini * 100,
        rendim_pct     = (close - precio_ini) / precio_ini * 100
      ) %>%
      ungroup()
  })

  output$plot_comparador <- renderPlotly({
    df  <- datos_comp_reactivo()
    yvar <- if (input$tipo_norm == "base100") "rendim_base100" else "rendim_pct"
    ylab <- if (input$tipo_norm == "base100") "Rendimiento (base 100)" else "Rendimiento acumulado (%)"

    p <- ggplot(df, aes(x = fecha, y = .data[[yvar]], color = simbolo)) +
      geom_line(linewidth = 0.9) +
      scale_color_manual(values = c(BTC="#F7931A", ETH="#627EEA",
                                    BNB="#F3BA2F", SOL="#9945FF", XRP="#00AAE4")) +
      {if (input$tipo_norm == "base100") geom_hline(yintercept = 100, linetype="dashed", color="grey50")} +
      {if (input$tipo_norm == "pct")     geom_hline(yintercept =   0, linetype="dashed", color="grey50")} +
      labs(
        title = paste("Rendimiento Comparado — últimos", input$sel_dias_comp, "días"),
        x = "", y = ylab, color = "Cripto"
      ) +
      theme_minimal()

    ggplotly(p)
  })

  output$tabla_comparador <- DT::renderDataTable({
    req(input$sel_cryptos_comp)
    cutoff <- Sys.Date() - input$sel_dias_comp
    hist_data %>%
      filter(fecha >= cutoff, simbolo %in% input$sel_cryptos_comp) %>%
      group_by(simbolo) %>%
      summarise(
        `Precio Inicial (USD)` = round(first(close), 2),
        `Precio Final (USD)`   = round(last(close),  2),
        `Rendimiento (%)`      = round((last(close) - first(close)) / first(close) * 100, 2),
        `Volatilidad (%)`      = round(sd(retorno, na.rm = TRUE), 3),
        `Retorno Medio (%)`    = round(mean(retorno, na.rm = TRUE), 3),
        `VaR 95% (%)`          = round(quantile(retorno, 0.05, na.rm = TRUE), 3),
        `Máx. Precio (USD)`    = round(max(high, na.rm = TRUE), 2),
        `Mín. Precio (USD)`    = round(min(low,  na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      rename(Símbolo = simbolo) %>%
      arrange(desc(`Rendimiento (%)`))
  }, options = list(pageLength = 10, dom = "t"), rownames = FALSE)

} # fin server
