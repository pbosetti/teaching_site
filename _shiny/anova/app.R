# ANOVA a un fattore: esperimento sintetico con 5 trattamenti x n ripetizioni
library(shiny)
library(bslib)
library(ggplot2)

a <- 5 # numero di trattamenti
n_max <- 25 # numero massimo di ripetizioni per trattamento
mu <- 10 # media generale

# Riquadri compatti per le statistiche
stat_box <- function(title, value, ..., theme) {
  value_box(title, value, ..., theme = theme, height = "130px",
            class = "stat-box")
}
stat_css <- "
.stat-box .value-box-area { padding: 0.5rem 1rem !important; }
.stat-box .value-box-title { margin-bottom: 0; }
.stat-box .value-box-value { font-size: 1.6rem; margin-bottom: 0; }
.stat-box .verdict { font-size: 0.8rem; }
"

ui <- page_sidebar(
  title = "ANOVA a un fattore",
  withMathJax(),
  tags$head(tags$style(HTML(stat_css))),
  sidebar = sidebar(
    width = 300,
    sliderInput("var_e", "Varianza entro i trattamenti \\(\\sigma^2\\)",
      min = 0.1, max = 10, value = 1, step = 0.1
    ),
    sliderInput("var_tr", "Varianza tra i trattamenti \\(\\sigma^2_\\tau\\)",
      min = 0, max = 10, value = 1, step = 0.1
    ),
    sliderInput("n", "Ripetizioni per trattamento \\(n\\)",
      min = 5, max = n_max, value = 5, step = 1
    ),
    sliderInput("seed", "Seme casuale",
      min = 1, max = 100, value = 1, step = 1
    ),
    hr(),
    helpText(
      "Modello: \\(y_{ij} = \\mu + \\tau_i + \\varepsilon_{ij}\\),",
      "con \\(\\tau_i \\sim \\mathcal{N}(0, \\sigma^2_\\tau)\\) e",
      "\\(\\varepsilon_{ij} \\sim \\mathcal{N}(0, \\sigma^2)\\).",
      textOutput("summary", inline = TRUE),
      "Cambiando il seme si ottiene un nuovo campione con le stesse varianze;",
      "cambiando le varianze si riscalano gli stessi numeri casuali;",
      "aumentando \\(n\\) si aggiungono osservazioni a quelle già presenti."
    )
  ),
  layout_columns(
    fill = FALSE,
    stat_box("\\(MS_{tr}\\)", textOutput("ms_tr"), theme = "primary"),
    stat_box("\\(MS_E\\)", textOutput("ms_e"), theme = "secondary"),
    stat_box("\\(F_0 = MS_{tr}/MS_E\\)", textOutput("f0"), theme = "info"),
    stat_box("p-value", textOutput("pval"),
      textOutput("verdict", container = tags$p, inline = FALSE) |>
        tagAppendAttributes(class = "verdict"),
      theme = "warning"
    )
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(
      card_header("Boxplot dei trattamenti"),
      plotOutput("boxplot")
    ),
    card(
      card_header(
        "Distribuzione \\(F_{a-1,N-a}\\) sotto \\(H_0\\)"
      ),
      plotOutput("fplot")
    )
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(
      card_header("Tabella ANOVA"),
      tableOutput("anova")
    ),
    card(
      max_height = 450,
      card_header("Dati"),
      tableOutput("data")
    )
  )
)

server <- function(input, output, session) {
  n <- reactive(input$n)
  N <- reactive(a * n())

  # Numeri casuali standardizzati: dipendono solo dal seme. Gli errori sono
  # generati per n_max ripetizioni (una colonna per trattamento) e se ne usano
  # le prime n, così cambiando n le osservazioni esistenti non cambiano
  z <- reactive({
    set.seed(input$seed)
    list(tau = rnorm(a), eps = matrix(rnorm(n_max * a), nrow = n_max))
  })

  df <- reactive({
    n <- n()
    tau <- sqrt(input$var_tr) * z()$tau
    eps <- sqrt(input$var_e) * as.vector(z()$eps[1:n, ])
    trt <- factor(rep(LETTERS[1:a], each = n))
    data.frame(
      Trattamento = trt,
      Ripetizione = rep(1:n, times = a),
      y = mu + tau[as.integer(trt)] + eps
    )
  })

  stats <- reactive({
    n <- n()
    N <- N()
    d <- df()
    y_bar <- mean(d$y)
    y_i <- tapply(d$y, d$Trattamento, mean)
    ss_tr <- n * sum((y_i - y_bar)^2)
    ss_e <- sum((d$y - y_i[d$Trattamento])^2)
    ms_tr <- ss_tr / (a - 1)
    ms_e <- ss_e / (N - a)
    f0 <- ms_tr / ms_e
    list(
      ss_tr = ss_tr, ss_e = ss_e, ms_tr = ms_tr, ms_e = ms_e, f0 = f0,
      p = pf(f0, a - 1, N - a, lower.tail = FALSE)
    )
  })

  output$summary <- renderText(
    sprintf("%d trattamenti, %d ripetizioni, %d osservazioni.", a, n(), N())
  )
  output$ms_tr <- renderText(format(stats()$ms_tr, digits = 4))
  output$ms_e <- renderText(format(stats()$ms_e, digits = 4))
  output$f0 <- renderText(format(stats()$f0, digits = 4))
  output$pval <- renderText(format.pval(stats()$p, digits = 3, eps = 1e-6))
  output$verdict <- renderText(
    if (stats()$p < 0.05) {
      "Si rifiuta H₀ (α = 5%)"
    } else {
      "Non si rifiuta H₀ (α = 5%)"
    }
  )

  output$boxplot <- renderPlot({
    d <- df()
    ggplot(d, aes(x = Trattamento, y = y)) +
      geom_hline(yintercept = mean(d$y), linetype = 2, color = "gray40") +
      geom_boxplot(fill = "lightblue", alpha = 0.6, outlier.shape = NA) +
      geom_point(position = position_jitter(width = 0.1, seed = 1)) +
      stat_summary(fun = mean, geom = "point", shape = 4, size = 4,
                   color = "red") +
      labs(
        y = "Resa y",
        caption = "Croce rossa: media del trattamento; tratteggio: media generale"
      ) +
      theme_bw(base_size = 14)
  })

  output$fplot <- renderPlot({
    N <- N()
    f0 <- stats()$f0
    x_max <- max(8, f0 * 1.1)
    fd <- data.frame(x = seq(0, x_max, length.out = 500))
    fd$d <- stats::df(fd$x, a - 1, N - a)
    ggplot(fd, aes(x = x, y = d)) +
      geom_area(data = subset(fd, x >= f0), fill = "orange", alpha = 0.6) +
      geom_line() +
      geom_vline(xintercept = f0, color = "red") +
      geom_vline(xintercept = qf(0.95, a - 1, N - a), linetype = 2) +
      annotate("text", x = f0, y = max(fd$d[is.finite(fd$d)]) * 0.9,
               label = "F[0]", parse = TRUE, hjust = -0.3, color = "red") +
      labs(
        x = "F", y = "Densità",
        caption = "Area arancione: p-value; tratteggio: quantile al 95%"
      ) +
      theme_bw(base_size = 14)
  })

  output$anova <- renderTable({
    s <- stats()
    N <- N()
    data.frame(
      Sorgente = c("Trattamenti", "Errore", "Totale"),
      GdL = c(a - 1, N - a, N - 1),
      SS = c(s$ss_tr, s$ss_e, s$ss_tr + s$ss_e),
      MS = c(s$ms_tr, s$ms_e, NA),
      F0 = c(s$f0, NA, NA),
      `p-value` = c(format.pval(s$p, digits = 3, eps = 1e-6), NA, NA),
      check.names = FALSE
    )
  }, digits = 3, na = "")

  output$data <- renderTable({
    n <- n()
    d <- df()
    w <- matrix(d$y, nrow = n, dimnames = list(NULL, levels(d$Trattamento)))
    cbind(Ripetizione = 1:n, as.data.frame(w))
  }, digits = 2)
}

shinyApp(ui, server)
