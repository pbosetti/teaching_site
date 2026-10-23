#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tibble)
library(latex2exp)
library(httr2)
source("bilancia.R")

delay <- 60


# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel(h1("Taratura di una bilancia a due piatti")),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h2("Dati personali"),
      textInput("name", "Nome"),
      textInput("surname", "Cognome"),
      textInput("ID", "Matricola"),
      h2("Carica la bilancia"),
      sliderInput(
        "F1",
        "Piatto sinistro (g):",
        min = 100,
        max = 500,
        step = 50,
        value = 100
      ),
      sliderInput(
        "DF",
        "Piatto destro - Piatto sinistro (g):",
        min = -50,
        max = 50,
        step=5,
        value = 30
      ),
      actionButton("getMeasurement", "Misura", class = "btn-success"),
      actionButton("clearData", "Cancella dati", class = "btn-success"),
      downloadButton("download", "Download")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2("Risultato"),
      p("Attenzione: alla fine di ogni sessione (cioè prima di chiudere il browser), salvare la tabella delle misure e annotare la chiave di verifica visualizzata in un file separato. Preferibilmente completare tutte le misure in un'unica sessione; se è necessario distribuire le misure in più sessioni, mantenere separati i CSV e creare un file com una chiave di verifica per ogni file CSV. Il report finale va accompagnato dalla tabella e dalla chiave di verifica."),
      p(strong("Non modificare i file csv in alcun modo!")),
      plotOutput("plot1"),
      tableOutput("measurements"),
      textOutput("clearData")
    )
  ))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observe({
    req <- request("https://hooks.slack.com/triggers/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")
    query <- parseQueryString(session$clientData$url_search)
    updateTextInput(session, "name", value=query[["name"]])
    updateTextInput(session, "surname", value=query[["surname"]])
    updateTextInput(session, "ID", value=query[["ID"]])
    req <- req %>% req_body_json(list(
      nome=query[["name"]], 
      cognome=query[["surname"]],
      ID=query[["ID"]],
      sorgente="Shiny app"
    ))
    req_perform(req)
  })
  output$measurements <- renderTable({
    input$getMeasurement
    if (is.null(session$userData$lastCall)) {
      session$userData$lastCall <- 0
      return(data.frame())
    } 
    newMeasure <- isolate(data.frame(
      time=as.integer(session$userData$lastCall),
      F1=input$F1, 
      DF=input$DF
    ))
    newMeasure$angle <- with(newMeasure,
      round(scale_angle(F1, DF) + noise(time, p=90*delay, w=0.2, sd=0.2), 2)
    )
    if (is.null(session$userData$measurements)) {
      session$userData$measurements <- newMeasure
    } else {
      session$userData$measurements <- rbind(session$userData$measurements, newMeasure)
    }
    session$userData$lastCall <- session$userData$lastCall + delay

    session$userData$measurements
  }, digits=2)
  
  
  output$clearData <- renderText({
    input$clearData
    session$userData$measurements <- NULL
    showNotification("Tabella dati cancellata", duration=3)
    NULL
  })
  
  
  # prima sha256 poi sha1
  output$download <- downloadHandler(
    filename = function() {
      "misurazioni.csv"
    },
    content = function(file) {
      header <- paste("# Name:", input$surname, input$name, "ID:", input$ID)
      print(header)
      write.csv(session$userData$measurements, file, row.names = F, quote = F)
      write(header, file, append=T)
      hash <- digest::digest(digest::digest(file=file, algo="sha256"), algo="sha1")
      print(hash)
      showModal(modalDialog(
        p("Chiave di verifica:", br(), tag("pre", hash)),
        title="Annotare questa chiave di verifica (copia e incolla in un file)",
        footer=modalButton("Ho fatto")
      ))
    }
  )
  
  output$plot1 <- renderPlot({
    input$getMeasurement
    df <- tibble(session$userData$measurements)
    if (length(df) >= 1) {
      # df$Ft <- with(df, F1+F2)
      # df$DF <- with(df, F2-F1)
      df <- df[order(df$DF),]
      ggplot(df, aes(x=F1, group=DF, color=DF)) +
        geom_point(aes(y=angle)) +
        geom_line(aes(y=angle)) +
        labs(x="F1 (N)", y="Angolo (°)", color=TeX("$\\Delta F$ (N)")) +
        scale_color_viridis_b()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
