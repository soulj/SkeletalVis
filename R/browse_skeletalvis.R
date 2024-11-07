
#' Browse Skeletal Visualization Table
#'
#' This function opens an interactive data table from a file located in the specified folder.
#' It allows the user to browse and select a row, then returns the selected row ID.
#'
#' @param skeletalvis The directory path where the 'exptable.txt' and "comparisons.txt" file is located.
#' @return The gene expression profile ID of the selected row from the interactive table.
#'
#' @examples
#' # Assuming 'skeletalvis' is the directory containing 'exptable.txt'
#' # selected_id <- browse_skeletalvis("path/to/skeletalvis")
#'
#' @export
browse_skeletalvis <- function(skeletalvis) {
  exp_filepath <- file.path(skeletalvis, "expTable.txt")
  comp_filepath <- file.path(skeletalvis, "comparisons.txt")

  if (!file.exists(exp_filepath)) stop("The file 'expTable.txt' does not exist in the specified directory.")
  if (!file.exists(comp_filepath)) stop("The file 'comparisons.txt' does not exist in the specified directory.")

  exptable <- read.csv(exp_filepath, header = TRUE, stringsAsFactors = FALSE)
  comparisons <- read.delim(comp_filepath, header = TRUE, stringsAsFactors = FALSE)

  shiny::runGadget(
    miniUI::miniPage(
      miniUI::miniTitleBar("Select a row from the table"),
      miniUI::miniContentPanel(
        DT::dataTableOutput("table")
      )
    ),
    server = function(input, output, session) {
      browse_skeletalvis_server(input, output, session, exptable, comparisons)
    }
  )
}



browse_skeletalvis_server <- function(input, output, session, exptable, comparisons) {
  # Server logic
  output$table <- DT::renderDataTable({
    DT::datatable(
      exptable,
      selection = "single",  # Allow single row selection
      options = list(pageLength = 10, info = FALSE, paging = FALSE)
    )
  })

  # Reactive value to store selected row ID in modal
  selected_modal_row <- shiny::reactiveVal(NULL)

  # Observe the row selection in the main table
  observeEvent(input$table_rows_selected, {
    selected_accession <- exptable[input$table_rows_selected, 1]
    matched_rows <- comparisons[comparisons$accession == selected_accession, ]

    showModal(shiny::modalDialog(
      title = paste("Comparisons for Accession:", selected_accession),
      shiny::fluidRow(
        shiny::column(6, shiny::actionButton("confirm_selection", label = "Confirm", style = "background-color: green; color: white;")),
        shiny::column(6, shiny::actionButton("close_modal", label = "Close", style = "background-color: red; color: white; float: right;"))
      ),
      DT::dataTableOutput("modal_table"),
      easyClose = TRUE,
      footer = NULL
    ))

    output$modal_table <- DT::renderDataTable({
      DT::datatable(
        matched_rows,
        selection = "single",
        options = list(pageLength = 10, info = FALSE, paging = FALSE)
      )
    })

    observeEvent(input$modal_table_rows_selected, {
      selected_id <- matched_rows[input$modal_table_rows_selected, 1]
      selected_modal_row(selected_id)
    })

    observeEvent(input$confirm_selection, {
      shiny::stopApp(selected_modal_row())
    })

    observeEvent(input$close_modal, {
      removeModal()
    })
  })
}
