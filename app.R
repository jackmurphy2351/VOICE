library(shiny)

voice_framework <- data.frame(
  domain = c(
    "Vision and values",
    "Opportunities for engagement",
    "Inclusion in design and execution",
    "Context and implementation",
    "Equity evaluation and accountability"
  ),
  question = c(
    "Are equity goals explicitly defined with measurable outcomes?",
    "Have affected communities and patient partners been engaged early?",
    "Are recruitment, eligibility, and analysis plans inclusive and bias-aware?",
    "Are social determinants, access barriers, and local context addressed?",
    "Will outcomes be monitored for differential impact across groups?"
  ),
  stringsAsFactors = FALSE
)

score_levels <- c(
  "Not addressed" = 0,
  "Partially addressed" = 1,
  "Addressed" = 2
)

ui <- fluidPage(
  titlePanel("VOICE Health Equity Assessment Tool"),
  p(
    "Point-and-click support for applying VOICE framework principles in health research.",
    a("Framework poster", href = "https://www.ohdsi.org/2026showcase-50/", target = "_blank")
  ),
  sidebarLayout(
    sidebarPanel(
      textInput("study_name", "Study name"),
      textAreaInput("study_notes", "Study context/notes", rows = 5),
      actionButton("reset", "Reset form")
    ),
    mainPanel(
      lapply(seq_len(nrow(voice_framework)), function(i) {
        tagList(
          h4(paste0(i, ". ", voice_framework$domain[i])),
          p(voice_framework$question[i]),
          radioButtons(
            inputId = paste0("criterion_", i),
            label = NULL,
            choices = names(score_levels),
            selected = "Not addressed",
            inline = TRUE
          ),
          tags$hr()
        )
      }),
      h3("Summary"),
      textOutput("score_text"),
      tableOutput("results_table")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$reset, {
    updateTextInput(session, "study_name", value = "")
    updateTextAreaInput(session, "study_notes", value = "")
    for (i in seq_len(nrow(voice_framework))) {
      updateRadioButtons(
        session,
        inputId = paste0("criterion_", i),
        selected = "Not addressed"
      )
    }
  })

  scores <- reactive({
    sapply(seq_len(nrow(voice_framework)), function(i) {
      score_levels[[input[[paste0("criterion_", i)]]]]
    })
  })

  output$score_text <- renderText({
    total <- sum(scores())
    max_total <- 2 * nrow(voice_framework)
    pct <- round((total / max_total) * 100)
    paste0("Overall VOICE equity coverage: ", total, "/", max_total, " (", pct, "%).")
  })

  output$results_table <- renderTable({
    data.frame(
      Domain = voice_framework$domain,
      Question = voice_framework$question,
      Selection = sapply(seq_len(nrow(voice_framework)), function(i) {
        input[[paste0("criterion_", i)]]
      }),
      Score = scores(),
      check.names = FALSE
    )
  }, striped = TRUE, spacing = "s")
}

app <- shinyApp(ui = ui, server = server)
