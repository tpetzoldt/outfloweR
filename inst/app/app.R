## load packages
library(shiny)
library(deSolve)
library(dplyr, mask.ok=TRUE)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(outfloweR)
library(shinyjs, mask.ok=TRUE)
library(purrr)

library(shiny.i18n)

i18n_json_path <- system.file("extdata", "i18n", package = "outfloweR")

# Check if the path exists (for debugging)
if (!dir.exists(i18n_json_path)) {
  stop("i18n translation directory not found at: ", i18n_json_path)
} else {
  message("i18n translation directory found at: ", i18n_json_path)
}

i18n <- shiny.i18n::Translator$new(translation_json_path = paste0(i18n_json_path, "/", "i18n.json"))
i18n$set_translation_language("en")

## helper functions
translate_choices <- function(choices, keys) {
  set_names(choices,  map_chr(keys, i18n$t))
}



# Define UI for application
ui <- fluidPage(

  uiOutput("title"),

  ## tp: select language
  shiny.i18n::usei18n(i18n),
  tags$div(
    style='float: right;color: white; font-family: Open Sans;',
    selectInput(
      inputId = "selected_language",
      label = i18n$t("Sprache"),
      choices = setNames(
        c("en", "de"),
        c("🇬🇧 - en", "🇩🇪 - de")
      ),
      selected = i18n$get_key_translation()
    )
  ),
  sidebarLayout(uiOutput("sidebar"),
                uiOutput("Main")
                )
)

server <- function(input, output,session) {


observe({

  query <- parseQueryString(session$clientData$url_search)
  if (!is.null(query[['ui']]))  {
    if (query[['ui']] == "plots") {
      output$title <- renderUI(
        titlePanel(i18n$t("Talsperren-Volumenbilanz in 2 Schichten")))
      output$sidebar <- renderUI(sidebarPanel())
      output$Main <- renderUI(mainPanel(
        div(id = "volumes", plotOutput("plot_model")),
        div(id="flows",plotOutput("plot_flows"))))
    }
    if (query[['ui']] == "volume") {
      output$title <- renderUI(
        titlePanel(i18n$t("Talsperren-Volumenbilanz in 2 Schichten")))
      output$sidebar <- renderUI()
      output$Main <- renderUI(mainPanel(
        div(id = "volumes", plotOutput("plot_model"))))
    }
    if (query[['ui']] == "flows") {
      output$title <- renderUI(
        titlePanel(i18n$t("Talsperren-Volumenbilanz in 2 Schichten")))
      output$sidebar <- renderUI()
      output$Main <- renderUI(mainPanel(
        div(id = "flows", plotOutput("plot_flows"))))
    }
  }
  else {
    i18n$set_translation_language(input$selected_language)
    output$title <- renderUI(
      titlePanel(i18n$t("Talsperren-Volumenbilanz in 2 Schichten")),

    )

    translated_choices <- translate_choices(
      choices = c(
        "discharge_normal",
        "discharge_wet",
        "discharge_dry",
        "discharge_constant",
        "discharge_summerflood"
      ),
      keys = c(
        "Normales Jahr",
        "Nasses Jahr",
        "Trockenes Jahr",
        "Konstante Zu- und Abflüsse",
        "Sommerhochwasser (theoretisches Szenario)"
      )
    )

    output$sidebar <- renderUI(sidebarPanel(
      selectInput(
        "discharge",
        i18n$t("Auswahl Zu- und Abflussmenge"),
        choices = translated_choices
      ),

       conditionalPanel(
         condition = "input.discharge == 'discharge_constant'",
         fluidRow(
           numericInput("const_inflow", i18n$t("Zufluss (Mio m3/d)"), min=0, value=0.2, max=1, width=60),
           numericInput("const_outflow", i18n$t("Rohwasser (Mio m3/d)"), min=0, value=0.1, max=.2, width=60),
           numericInput("const_outflow_wb", i18n$t("Wildbett (Mio m3/d)"), min=0, value=0.1, max=0.6, width=60)
         )
      ),

      selectInput(
        "withdrawal",
        i18n$t("Auswahl Managementstrategie"),
        choices = translate_choices(
          c("standard", "cold_inflow", "wb_top"),
          c("Standard", "Zufluss ins Hypolimnion", "Wildbettabgabe aus dem Epilimnion")
        )),
      sliderInput("volume",i18n$t("Auswahl Startvolumen"), min = 10, max = 100, value = 60),
      sliderInput("begin", i18n$t("Beginn der Schichtung"), min = 60, max = 121, value = 91),
      sliderInput("end", i18n$t("spätestes Schichtungsende"), min = 258, max = 335 , value = 305 ),
      checkboxInput("cumulative", i18n$t("Zu- und Abfluss als Summen"), FALSE),
      radioButtons("vol_level", i18n$t("Plot als:"),
                   choiceValues = c("Volumen", "Stauspiegel"),
                   choiceNames = c(i18n$t("Volumen"), i18n$t("Stauspiegel"))),
      hr(),
      p(i18n$t("Epilimnion: warmes Oberflächenwasser")),
      p(i18n$t("Hypolimnion: kaltes Tiefenwasser")),
      p(i18n$t("Rohwasser: Entnahme für die Trinkwasserversorgung")),
      p(i18n$t("Wildbett: Ablauf in den Fluss")),
      downloadButton("download", i18n$t("Ergebnisse als CSV-Datei speichern"))
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

    if (input$discharge == "discharge_constant") {
      discharge <- data.frame(
        time = 1:365,
        inflow = rep(input$const_inflow, 365),
        outflow = rep(input$const_outflow, 365),
        outflow_wb = rep(input$const_outflow_wb, 365)
      )
    } else {
      data(list = input$discharge, envir = environment())
      discharge <- get(input$discharge)
    }

    #print(str(discharge))

    scenario(option = input$withdrawal,
             vol = input$volume,
             t_strat = input$begin,
             t_autumn = input$end,
             discharge$inflow,
             discharge$outflow,
             discharge$outflow_wb
             )
  })

  output$plot_model <- renderPlot({
    out <- run_scenario()

    i18n$set_translation_language(input$selected_language)

    if(input$vol_level == "Stauspiegel") {
      data(hypso)
      hyps <- hypso_functions(hypso)
      out2 <- out
      out2[, "vol_E"] <- hyps$level(out[, "gesamt"] * 1e6) - hyps$level(out[, "vol_H"] * 1e6)
      out2[, "vol_H"] <- hyps$level(out[, "vol_H"] * 1e6)
      plot_volumes(out2,
                   xlab = i18n$t("Tag im Jahr"),
                   ylab = i18n$t("Wasserstand über Grund (m)"),
                   legend_title = i18n$t("Teilraum")#,
                   #translated_legend = c("Epilimnion", "Hypolimnion") # redundant
                   )
    } else {
      plot_volumes(out,
                   xlab = i18n$t("Tag im Jahr"),
                   ylab = i18n$t("Volumen (Mio m3)"),
                   legend_title = i18n$t("Teilraum"))
    }
  })


  output$plot_flows <- renderPlot ({

    i18n$set_translation_language(input$selected_language)

    if (input$discharge == "discharge_constant") {
      print(input$const_inflow)
      discharge <- data.frame(
        time = 1:365,
        inflow = rep(input$const_inflow, 365),
        outflow = rep(input$const_outflow, 365),
        outflow_wb = rep(input$const_outflow_wb, 365)
      )
    } else {
      data(list = input$discharge, envir = environment())
      discharge <- get(input$discharge)
    }

    print(str(discharge))

    if (input$cumulative) {
      discharge <- mutate(discharge, inflow=cumsum(inflow), outflow=cumsum(outflow), outflow_wb=cumsum(outflow_wb))
      ylab1 <- i18n$t("Zufluss (Mio m3)")
      ylab2 <- i18n$t("Rohwasser (Mio m3)")
      ylab3 <- i18n$t("Wildbett (Mio m3)")

    } else {
      ylab1 <- i18n$t("Zufluss (Mio m3/d)")
      ylab2 <- i18n$t("Rohwasser (Mio m3/d)")
      ylab3 <- i18n$t("Wildbett (Mio m3/d)")

    }

    xlab1 <- i18n$t("Tag im Jahr")

    ggplot(discharge, aes(x = time, y = inflow )) + geom_line() +
      ylab(ylab1) + xlab(xlab1) -> p1
    ggplot(discharge, aes(x = time, y = outflow)) + geom_line() +
      ylab(ylab2) + xlab(i18n$t(xlab1)) -> p2
    ggplot(discharge, aes(x = time, y = outflow_wb)) + geom_line() +
      ylab(ylab3) + xlab(i18n$t(xlab1)) -> p3
    ggarrange(p1, p2,p3, ncol=1)

  })


  output$download <- downloadHandler(
    filename = function() {
      "data.csv"
    },
    content = function(file) {
      dat <- run_scenario()
      names(dat) <- c("times", "Epilimnion", "Hypolimnion", i18n$t("gesamt"))

      write.csv(dat, file, quote = FALSE, row.names = FALSE)
    }
  )

}
# Run the application
shinyApp(ui = ui, server = server)
