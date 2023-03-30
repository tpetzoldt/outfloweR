## load packages
library(shiny)
library(deSolve)
library(dplyr, mask.ok=TRUE)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(outfloweR)
library(shinyjs, mask.ok=TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(


  uiOutput("title"),
  sidebarLayout( uiOutput("sidebar"),
                 uiOutput("Main"))


)

server <- function(input, output,session) {


observe({
  query <- parseQueryString(session$clientData$url_search)
  if (!is.null(query[['ui']]))  {
    if (query[['ui']] == "plots") {
      output$title <- renderUI(
        titlePanel("Talsperren-Volumenbilanz in 2 Schichten"))
      output$sidebar <- renderUI(sidebarPanel())
      output$Main <- renderUI(mainPanel(
        div(id = "volumes", plotOutput("plot_model")),
        div(id="flows",plotOutput("plot_flows"))))
    }
    if (query[['ui']] == "volume") {
      output$title <- renderUI(
        titlePanel("Talsperren-Massenbilanz in 2 Schichten"))
      output$sidebar <- renderUI()
      output$Main <- renderUI(mainPanel(
        div(id = "volumes", plotOutput("plot_model"))))
    }
    if (query[['ui']] == "flows") {
      output$title <- renderUI(
        titlePanel("Talsperren-Massenbilanz in 2 Schichten"))
      output$sidebar <- renderUI()
      output$Main <- renderUI(mainPanel(
        div(id = "flows", plotOutput("plot_flows"))))
    }
  }
  else {
    output$title <- renderUI(
      titlePanel("Talsperren-Massenbilanz in 2 Schichten"),

    )

    output$sidebar <- renderUI(sidebarPanel(
      selectInput("discharge", "Auswahl Zu- und Abflussmenge",
                  choices = list("Normales Jahr"  = "discharge_normal",
                                 "Nasses Jahr"    = "discharge_wet",
                                 "Trockenes Jahr" = "discharge_dry",
                                 "Sommerhochwasser (theoretisches Szenario)" = "discharge_summerflood"),
      ),
      selectInput("withdrawal", "Auswahl Managementstrategie",
                  choices = list("Standard" = "standard",
                                 "Zufluss ins Hypolimnion" = "cold_inflow",
                                 "Wildbettabgabe aus dem Epilimnion" = "wb_top")),
      sliderInput("volume", "Auswahl Startvolumen", min = 10, max = 100, value = 60),
      sliderInput("begin", "Beginn der Schichtung", min = 60, max = 121, value = 91),
      sliderInput("end", "spätestes Schichtungsende", min = 258, max = 335 , value = 305 ),
      checkboxInput("cumulative", "Zu- und Abfluss als Summen", FALSE),
      radioButtons("vol_level", "Plot als:",
                   choices = c("Volumen", "Stauspiegel"), selected = "Volumen"),
      hr(),
      p("Epilimnion: warmes Oberflächenwasser"),
      p("Hypolimnion: kaltes Tiefenwasser"),
      p("Rohwasser: Entnahme für die Trinkwasserversorgung"),
      p("Wildbett: Ablauf in den Fluss"),
      downloadButton("download", "Ergebnisse als CSV-Datei speichern")
    ))

    output$Main <- renderUI(mainPanel(
      div(id = "volumes", plotOutput("plot_model")),
      div(id="flows",plotOutput("plot_flows"))))
  }
})




  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['discharge']])) {
      updateTextInput(session, "discharge", value = query[['discharge']])
    }
    if (!is.null(query[['withdrawal']])) {
      updateTextInput(session, "withdrawal", value = query[['withdrawal']])
    }
    if (!is.null(query[['volume']])) {
      updateTextInput(session, "volume", value = query[['volume']])
    }
    if (!is.null(query[['begin']])) {
      updateTextInput(session, "begin", value = query[['begin']])
    }
    if (!is.null(query[['end']])) {
      updateTextInput(session, "end", value = query[['end']])
    }
  })

  run_scenario <- reactive({
    data(list = input$discharge, envir = environment())
    discharge <- get(input$discharge)
    scenario(option = input$withdrawal, vol = input$volume,
             t_strat = input$begin, t_autumn = input$end,
             discharge$inflow, discharge$outflow, discharge$outflow_wb
             )
  })

  output$plot_model <- renderPlot({
    out <- run_scenario()

    if(input$vol_level == "Stauspiegel") {
      data(hypso)
      hyps <- hypso_functions(hypso)
      out2 <- out
      out2[, "vol_E"] <- hyps$level(out[, "gesamt"] * 1e6) - hyps$level(out[, "vol_H"] * 1e6)
      out2[, "vol_H"] <- hyps$level(out[, "vol_H"] * 1e6)
      plot_volumes(out2, ylab="Wasserstand über Grund (m)")
    } else {
      plot_volumes(out)
    }


  })


  output$plot_flows <- renderPlot ({
    data(list = input$discharge, envir = environment())
    discharge <- get(input$discharge)

    if (input$cumulative) {
      discharge <- mutate(discharge, inflow=cumsum(inflow), outflow=cumsum(outflow), outflow_wb=cumsum(outflow_wb))
      ylab1 <- "Zufluss (Mio m3)"
      ylab2 <- "Rohwasser (Mio m3)"
      ylab3 <- "Wildbett (Mio m3)"

    } else {
      ylab1 <- "Zufluss (Mio m3/d)"
      ylab2 <- "Rohwasser (Mio m3/d)"
      ylab3 <- "Wildbett (Mio m3/d)"

    }
    ggplot(discharge, aes(x = time, y = inflow )) + geom_line() +
      ylab(ylab1) + xlab("Tag im Jahr") -> p1
    ggplot(discharge, aes(x = time, y = outflow)) + geom_line() +
      ylab(ylab2) + xlab("Tag im Jahr") -> p2
    ggplot(discharge, aes(x = time, y = outflow_wb)) + geom_line() +
      ylab(ylab3) + xlab("Tag im Jahr") -> p3
    ggarrange(p1, p2,p3, ncol=1)

  })


  output$download <- downloadHandler(
    filename = function() {
      "data.csv"
    },
    content = function(file) {
      dat <- run_scenario()
      names(dat) <- c("times", "Epilimnion", "Hypolimnion", "gesamt")

      write.csv(dat, file, quote = FALSE, row.names = FALSE)
    }
  )

}
# Run the application
shinyApp(ui = ui, server = server)
