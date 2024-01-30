# Load libraries
library(shiny)
library(shinyjs)
library(dplyr)
library(bslib)
library(readr)
library(reactable)
library(glue)

# Define utility functions
comb_tbl <- function(vec) {
  pairs <- combn(vec, 2)
  pairs_tbl <- tibble(item1 = pairs[1, ], item2 = pairs[2, ])
  pairs_tbl
}
# comb_tbl(c("apple", "grapes", "banana"))

unique_combo_ct <- \(n) n*(n-1) / 2
# unique_combo_ct(3)

# Read sample items
sample_items <- readr::read_lines("sample_items3.txt") %>%
  paste0(collapse = ", ")

# Helper function to parse items and URLs from the input
parseItemsAndUrls <- function(inputString) {
  splitInput <- strsplit(inputString, ";\\s+")[[1]]
  itemsAndUrls <- tibble(
    item = splitInput[seq(1, length(splitInput), 2)],
    url = splitInput[seq(2, length(splitInput), 2)]
  )
  return(itemsAndUrls)
}

# Define UI
ui <- fluidPage(
  theme = bs_theme(version = 5),
  useShinyjs(),
  titlePanel("Pairwise Comparisons"),
  textInput("topic", "Topic name"),
  textAreaInput("items", "List of comparison items", placeholder = "Enter items separated by commas"),
  actionButton("sample", "Sample"),
  actionButton("reset", "Reset"),
  actionButton("start", "Start"),
  reactableOutput("resultsTable"),
  downloadButton("downloadResults", "Download Results")
)

# Define server logic
server <- function(input, output, session) {
  observe({
    shinyjs::disable("skip") # Disable download button after reset}
  })

  results <- reactiveVal(tibble(item1 = character(), item2 = character(), winner = character()))
  comparisons <- reactiveVal(data.frame())
  currentComparisonIndex <- reactiveVal(1)
  skippedCount <- reactiveVal(0) # To keep track of how many times skip has been clicked
  items <- reactiveVal()
  scores <- reactiveVal()


  observeEvent(input$sample, {
    updateTextInput(session, "topic", value = "Apples & Oranges")
    updateTextAreaInput(session, "items", value = sample_items)
  })

  observeEvent(input$reset, {
    updateTextInput(session, "topic", value = "")
    updateTextAreaInput(session, "items", value = "")
    results(tibble(item1 = character(), item2 = character(), winner = character()))  # Reset results
    shinyjs::disable("downloadResults") # Disable download button after reset
    skippedCount(0) # Reset the skip counter
    currentComparisonIndex(1) # Reset the index for comparisons
  })

  observeEvent(input$start, {
    req(nchar(input$items) > 0)
    items(unlist(strsplit(input$items, ",\\s*")))
    comparisons(comb_tbl(items()))
    currentComparisonIndex(1)
    results(tibble(item1 = character(), item2 = character(), winner = character()))  # Reset results
    showModalForCurrentComparison()
  })

  showModalForCurrentComparison <- function() {
    cmp <- comparisons()
    index <- currentComparisonIndex()
    total <- nrow(cmp)
    if (index > total) return()

    # Parse item names and URLs
    item1Info <- parseItemsAndUrls(cmp$item1[index])
    item2Info <- parseItemsAndUrls(cmp$item2[index])

    modalUI <- modalDialog(
      title = "Pairwise Comparison",
      # Custom layout for the comparison
      fluidRow(
        column(5, offset = 1,
               div(style = "text-align: center;",
                   if(nchar(item1Info[,2]) > 0) img(src = item1Info[,2], height = "100px"),
                   h3(item1Info[,1]),
                   actionButton("choose1", label = "Select", class = "btn-primary")
               )
        ),
        column(1,
               div(style = "text-align: center; padding-top: 50px;",
                   h4("OR")
               )
        ),
        column(5,
               div(style = "text-align: center;",
                   if(nchar(item2Info[,2]) > 0) img(src = item2Info[,2], height = "100px"),
                   h3(item2Info[,1]),
                   actionButton("choose2", label = "Select", class = "btn-primary")
               )
        )
      ),
      footer = div(
        class = "container-fluid",
        div(
          style = "position: relative; bottom: 0; width: 100%;",
          div(
            class = "col text-center",
            style = "padding: 10px;",
            span("Skipped: ", textOutput("skipCount")),
            actionButton("skip", "SKIP", class = "btn-secondary")
          )
        )
      )
    )

    output$skipCount <- renderText({
      skippedCount()
    })

    showModal(modalUI)
  }

  # Observe skip button click
  observeEvent(input$skip, {
    skippedCount(skippedCount() + 1) # Increment the skip counter
    currentComparisonIndex(currentComparisonIndex() + 1) # Move to the next comparison
    removeModal()
    showModalForCurrentComparison()
  })

  observeEvent(input$choose1, {
    cmp <- comparisons()
    index <- currentComparisonIndex()
    results(bind_rows(results(), tibble(item1 = cmp$item1[index], item2 = cmp$item2[index], winner = cmp$item1[index])))
    removeModal()
    currentComparisonIndex(index + 1)
    showModalForCurrentComparison()
  })

  observeEvent(input$choose2, {
    cmp <- comparisons()
    index <- currentComparisonIndex()
    results(bind_rows(results(), tibble(item1 = cmp$item1[index], item2 = cmp$item2[index], winner = cmp$item2[index])))
    removeModal()
    currentComparisonIndex(index + 1)
    showModalForCurrentComparison()
  })

  # Display the final results in a reactable table
  output$resultsTable <- renderReactable({
    req(nrow(results()) > 0) # Ensure there are results to display
    # require that comparisons are done
    req(currentComparisonIndex() > nrow(comparisons()))
    # Calculate the ranking scores
    ranking_scores <- results() %>%
      count(winner, name = "N") %>%
      rename(item = winner) %>%
      mutate(score = round(N / length(items()), 2) * 100 ) %>%
      arrange(desc(score))
    # include counts that didnt win
    ranking_scores <- ranking_scores %>%
      bind_rows(tibble(item = setdiff(items(), ranking_scores$item), score = 0, N = 0))
    scores(ranking_scores)
    reactable(scores(), columns = list(
      item = colDef(name = "Name"),
      score = colDef(name = "Score")
    ))
  })

  # Enable/disable download button based on the results
  observe({
    if (nrow(results()) > 0 && currentComparisonIndex() > nrow(comparisons())) {
      shinyjs::enable("downloadResults")
    } else {
      shinyjs::disable("downloadResults")
    }
  })

  output$downloadResults <- downloadHandler(
    filename = function() {
      glue::glue("{input$topic}_Results_{Sys.Date()}.csv")
    },
    content = function(file) {
      # Write the results to the specified file path
      readr::write_csv(scores(), file)
    }
  )
}

# Run the app
shinyApp(ui, server)
