#' Browse the skeletalvis database metadata
#'
#' Opens an interactive table showing the experiments available for analysis.
#' Row can be selected to return the datasetID for the comparison of interest within that experiment.
#'
#' @param skeletalvis The path to the SkeletalVis data folder.
#' @return The datasetID of the selected gene expression profile from the interactive table.
#'
#' @examples
#' if(interactive()){
#' skeletalvis <- load_skeletalvis(demo=TRUE)
#' selected_id <- browse_skeletalvis(skeletalvis)
#' }
#'
#' @export
browse_skeletalvis <- function(skeletalvis) {
  exp_filepath <- file.path(skeletalvis, "expTable.txt")
  comp_filepath <- file.path(skeletalvis, "accessions.txt")

  if (!file.exists(exp_filepath)) stop("The file 'expTable.txt' does not exist in the specified directory.")
  if (!file.exists(comp_filepath)) stop("The file 'accessions.txt' does not exist in the specified directory.")

  exptable <- utils::read.csv(exp_filepath, header = TRUE, stringsAsFactors = FALSE)
  comparisons <- utils::read.delim(comp_filepath, header = TRUE, stringsAsFactors = FALSE)
  comparisons$ID <- paste(comparisons$accession, comparisons$comparison, sep="_")

  shiny::runGadget(
    miniUI::miniPage(
      miniUI::miniTitleBar("Select an experiment from the table"),
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
      options = list(pageLength = 20, info = FALSE, paging = FALSE)
    )
  })

  # Reactive value to store selected row ID in modal
  selected_modal_row <- shiny::reactiveVal(NULL)

  # Observe the row selection in the main table
  shiny::observeEvent(input$table_rows_selected, {
    selected_accession <- exptable[input$table_rows_selected, 1]
    matched_rows <- comparisons[comparisons$accession == selected_accession, ]

    shiny::showModal(shiny::modalDialog(
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
        selection = list(mode = "single", selected = 1),
                options = list(pageLength = 10, info = FALSE, paging = FALSE)
      )
    })

    shiny::observeEvent(input$modal_table_rows_selected, {
      selected_id <- matched_rows[input$modal_table_rows_selected, "ID"]
      selected_modal_row(selected_id)
    })

    shiny::observeEvent(input$confirm_selection, {
      shiny::stopApp(selected_modal_row())
    })

    shiny::observeEvent(input$close_modal, {
      shiny::removeModal()
    })
  })
}
