#' Creates a single row for matrixInput
#'
#' @param rowID character string. Unique ID for the row. It should be unique within
#'   a given 'matrixInput', since it is used when identifying the value user
#'   has selected. It will be put into the \code{name} attribute of the
#'   corresponding \code{<tr>} tag, as well as in the \code{name} attributes of
#'   the button inputs in this row.
#' @param rowLLabel character string. A label displayed in the leftmost point of the row.
#' @param rowRLabel character string. A label displayed in the rightmost point of the row.
#' @param choiceNames,choiceValues List of names and values, respectively, that
#'   are displayed to the user in the app and correspond to the each choice (for
#'   this reason, the objects 'choiceNames' and 'choiceValues' must have the
#'   same length). If either of these arguments is provided, then the other must
#'   be provided and choices must not be provided. The advantage of using both of
#'   these over a named list for choices is that the object 'choiceNames' allows
#'   any type of UI object to be passed through (tag objects, icons, HTML code,
#'   ...), instead of just simple text.
#' @param selected The initially selected values (if not specified then defaults
#'   to \code{NULL}).
#' @param labelsWidth List of two valid values of CSS length unit. Each element
#'   has to be a properly formatted CSS unit of length (e.g., (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}), specifying the minimum (first value) and
#'   maximum (second value) width of the labels columns. The valid elements will
#'   be written to the \code{style} attribute of the labels \code{td} tags.
#' @param inputType character string with a value of either 'checkbox' or 'radio'.
#'   If 'checkbox', then the user can select multiple values. If 'radio', then the
#'   user can select only one value.
#' @return HTML markup for a table row with select radio or checkbox inputs inside each
#'   cell
#'
#' @keywords internal
#'
#' @noRd
#'

generateMatrixRow <- function(rowID, rowLLabel, rowRLabel, choiceNames, choiceValues, selected = NULL, labelsWidth = list(NULL, NULL), inputType = "checkbox") {
  row_dat <- mapply(choiceNames, choiceValues, FUN = function(name, value) {
    inputTag <- if (inputType == "checkbox") {
      shiny::tags$input(
        type = "checkbox", name = rowID,
        title = value, # to provide tooltips with the value
        value = value
      )
    } else {
      shiny::tags$input(
        type = "radio", name = rowID,
        title = value, # to provide tooltips with the value
        value = value
      )
    }

    if (value %in% selected) {
      inputTag$attribs$checked <- "checked"
    }
    shiny::tags$td(inputTag)
  }, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  style <- NULL
  if (!is.null(labelsWidth[[1]])) {
    style <- paste0(style, "min-width:", labelsWidth[[1]], ";")
  }
  if (!is.null(labelsWidth[[2]])) {
    style <- paste0(style, "max-width:", labelsWidth[[2]], ";")
  }

  row_dat <- list(
    shiny::tags$td(rowID),
    if (is.null(style)) shiny::tags$td(rowLLabel) else shiny::tags$td(rowLLabel, style = style),
    row_dat,
    if (!is.null(rowRLabel)) if (is.null(style)) shiny::tags$td(rowRLabel) else shiny::tags$td(rowRLabel, style = style)
  )

  shiny::tags$tr(
    name = rowID,
    class = "shiny-matrix-row", # used for CSS styling
    row_dat
  )
}

#' Generate the header row of matrixInput
#'
#' @param question character. The question to appear at the top of the table.
#' @param choiceNames character. Names displayed on top of the assignment matrix.
#' @param rowLLabels character. Vector (or a matrix with one column) of labels that
#'   displayed in the leftmost point of each row. The column name of the matrix
#'   could be displayed in the header of the assignment matrix.
#' @param rowRLabels character. Vector (or a matrix with one column) of labels that
#'   displayed in the rightmost point of each row. The column name of the matrix
#'   could be displayed in the header of the assignment matrix. Using this argument
#'   is optional. But it allows to create Likert scales, potentially with several
#'   scales arranged in a matrix.
#' @param rowIDsName single character that defines the header of the ID column in the
#'   input matrix.
#'
#' @return HTML markup for the header table row
#'
#' @keywords internal
#'
#' @noRd

generateMatrixHeader <- function(question, choiceNames, rowLLabels, rowRLabels, rowIDsName) {
  # First row for the question
  questionRow <- shiny::tags$tr(
    shiny::tags$td(colspan = length(choiceNames) + 2 + if (!is.null(rowRLabels)) 1 else 0, question)
  )

  # Second row for rowIDsName and choice names
  rRName <- ifelse(!is.null(rowRLabels) && is.matrix(rowRLabels), colnames(rowRLabels), "")
  rLName <- ifelse(is.matrix(rowLLabels), colnames(rowLLabels), "")

  header <- lapply(
    c(rowIDsName, rLName, choiceNames, rRName),
    function(n) {
      shiny::tags$td(n)
    }
  )

  headerRow <- shiny::tags$tr(header)

  # Combine the question and header rows
  list(questionRow, headerRow)
}

#' Generate complete HTML markup for matrixInput
#'
#' @param inputId The input slot that will be used to access the value.
#' @param question character. The question to appear at the top of the table.
#' @param rowIDs character. Vector of row identifiers that will be used to find
#'   values that the user has selected. In the output, the component will return
#'   a named list of values, each name corresponding to the row id, and the
#'   value - to the value user has selected in this row.
#' @param rowLLabels character. Vector (or a matrix with one column) of labels that
#'   displayed in the leftmost point of each row. The column name of the matrix
#'   could be displayed in the header of the assignment matrix.
#' @param rowRLabels character. Vector (or a matrix with one column) of labels that
#'   displayed in the rightmost point of each row. The column name of the matrix
#'   could be displayed in the header of the assignment matrix. Using this argument
#'   is optional. But it allows to create Likert scales, potentially with several
#'   scales arranged in a matrix.
#' @param selected Vector of the initially selected values (if not specified then
#'   defaults to \code{NULL}).
#' @param rowIDsName single character that defines the header of the ID column in the
#'   input matrix.
#' @param choiceNames,choiceValues List of names and values, respectively, that
#'   are displayed to the user in the app and correspond to the each choice (for
#'   this reason, the objects 'choiceNames' and 'choiceValues' must have the
#'   same length). If either of these arguments is provided, then the other must
#'   be provided and choices must not be provided. The advantage of using both of
#'   these over a named list for choices is that the object 'choiceNames' allows
#'   any type of UI object to be passed through (tag objects, icons, HTML code,
#'   ...), instead of just simple text.
#' @param labelsWidth List of two valid values of CSS length unit. Each element
#'   has to be a properly formatted CSS unit of length (e.g., (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}), specifying the minimum (first value) and
#'   maximum (second value) width of the labels columns. The valid elements will
#'   be written to the \code{style} attribute of the labels \code{td} tags.
#' @param inputType character string with a value of either 'checkbox' or 'radio'.
#'   If 'checkbox', then the user can select multiple values. If 'radio', then the
#'   user can select only one value.
#' @param session copied from \code{shiny:::generateOptions}
#'
#' @keywords internal
#'
#' @noRd

generateMatrix <- function(inputId,
                           question,
                           rowIDs,
                           rowLLabels,
                           rowRLabels = NULL,
                           choiceNames = NULL,
                           choiceValues = NULL,
                           selected = NULL,
                           rowIDsName = "ID",
                           labelsWidth = list(NULL, NULL),
                           inputType = "checkbox",
                           session = shiny::getDefaultReactiveDomain()) {
  # Generate the header with inputType parameter
  header <- generateMatrixHeader(question, choiceNames, rowLLabels, rowRLabels, rowIDsName)

  # Generate the rows with inputType parameter
  rows <- lapply(1:length(rowIDs), function(i) {
    generateMatrixRow(
      rowID = rowIDs[[i]],
      rowLLabel = rowLLabels[[i]],
      rowRLabel = rowRLabels[[i]],
      choiceNames = choiceNames,
      choiceValues = choiceValues,
      selected = if (is.null(selected)) selected else selected[[i]],
      labelsWidth = labelsWidth,
      inputType = inputType
    )
  })

  table <- shiny::tags$table(header, rows)

  shiny::div(class = "shiny-matrix", table)
}

#' @param inputId The input slot that will be used to access the value.
#' @param rowIDs character. Vector of row identifiers that will be used to find
#'   values that the user has selected. In the output, the component will return
#'   a named list of values, each name corresponding to the row id, and the
#'   value - to the value user has selected in this row.
#' @param rowLLabels character. Vector (or a matrix with one column) of labels that
#'   displayed in the leftmost point of each row. The column name of the matrix
#'   could be displayed in the header of the assignment matrix.
#' @param rowRLabels character. Vector (or a matrix with one column) of labels that
#'   displayed in the rightmost point of each row. The column name of the matrix
#'   could be displayed in the header of the assignment matrix. Using this argument
#'   is optional. But it allows to create Likert scales, potentially with several
#'   scales arranged in a matrix.
#' @param choices List of values to select from (if elements of the list are
#'   named then that name rather than the value is displayed to the user). If
#'   this argument is provided, then choiceNames and choiceValues must not be
#'   provided, and vice-versa. The values should be strings; other types (such
#'   as logicals and numbers) will be coerced to strings.
#' @param selected Vector of the initially selected values (if not specified then
#'   defaults to \code{NULL}).
#' @param choiceNames,choiceValues List of names and values, respectively, that
#'   are displayed to the user in the app and correspond to the each choice (for
#'   this reason, the objects 'choiceNames' and 'choiceValues' must have the
#'   same length). If either of these arguments is provided, then the other must
#'   be provided and choices must not be provided. The advantage of using both of
#'   these over a named list for choices is that the object 'choiceNames' allows
#'   any type of UI object to be passed through (tag objects, icons, HTML code,
#'   ...), instead of just simple text.
#' @param rowIDsName single character that defines the header of the ID column in the
#'   input matrix.
#' @param labelsWidth List of two valid values of CSS length unit. Each element
#'   has to be a properly formatted CSS unit of length (e.g., (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}), specifying the minimum (first value) and
#'   maximum (second value) width of the labels columns. The valid elements will
#'   be written to the \code{style} attribute of the labels \code{td} tags.
#'
#' @keywords internal
#'
#' @noRd
#'
validateParams <- function(rowIDs,
                           rowLLabels,
                           rowRLabels,
                           selected,
                           choiceNames,
                           rowIDsName,
                           labelsWidth) {
  # Check if rowLLabels and rowRLabels are vectors or matrices with at least one column
  validateLabel <- function(label, labelName) {
    if (!any(
      length(label) >= 1 && !is.list(label),
      length(dim(label)) == 2 && dim(label)[2L] == 1
    )) {
      stop(sprintf("'%s' must be a vector or a matrix with at least one column.", labelName))
    }
  }

  validateLabel(rowLLabels, "rowLLabels")
  if (!is.null(rowRLabels)) validateLabel(rowRLabels, "rowRLabels")

  # Convert to array if they are 2D with one column
  if (length(dim(rowLLabels)) == 2 && dim(rowLLabels)[2L] == 1) {
    rowLLabels <- array(t(rowLLabels))
  }
  if (!is.null(rowRLabels) && length(dim(rowRLabels)) == 2 && dim(rowRLabels)[2L] == 1) {
    rowRLabels <- array(t(rowRLabels))
  }

  # Check lengths of rowIDs, rowLLabels, rowRLabels, and selected
  checks <- list(rowIDs, rowLLabels)
  if (!is.null(rowRLabels)) checks <- c(checks, rowRLabels)
  if (!is.null(selected)) checks <- c(checks, selected)

  lengths <- sapply(checks, length)
  if (length(unique(lengths)) > 1) {
    stop("All elements of 'rowIDs', 'rowLabels', and 'selected' must be of the same length!")
  }

  # Ensure rowIDs have at least one element and are unique
  if (length(rowIDs) < 1) {
    stop("The assignment matrix should contain at least one row. 'rowIDs' must be a vector with at least one element.")
  }
  if (length(unique(rowIDs)) < length(rowIDs)) {
    duplicated_ids <- rowIDs[duplicated(rowIDs)]
    stop(sprintf("Some elements of 'rowIDs' are not unique. The following values are duplicated: %s.", paste(duplicated_ids, collapse = ", ")))
  }

  # Ensure choiceNames have at least one element
  if (length(choiceNames) < 1) {
    stop("There should be at least one column in the assignment matrix. 'choiceNames' must be a vector with at least one element.")
  }

  # Validate labelsWidth
  if (!(is.list(labelsWidth) && length(labelsWidth) == 2)) {
    stop("The object 'labelsWidth' must be a list with two elements.")
  }

  # Validate rowIDsName
  if (!(is.character(rowIDsName) && length(rowIDsName) == 1)) {
    stop("The object 'rowIDsName' must be a character with a single element.")
  }

  # Validate CSS units for labelsWidth
  pattern <- "^(auto|inherit|calc\\(.*\\)|((\\.\\d+)|(\\d+(\\.\\d+)?))(%|in|cm|mm|ch|em|ex|rem|pt|pc|px|vh|vw|vmin|vmax))$"
  is.cssu <- function(x) (is.character(x) && grepl(pattern, x))
  lwNull <- sapply(labelsWidth, is.null)
  lwCssU <- sapply(labelsWidth, is.cssu)
  lwTest <- !(lwNull | lwCssU)
  if (any(lwTest)) {
    stop(
      "The object 'labelsWidth' can only contain NULLs or ",
      "properly formatted CSS units of length!"
    )
  }
}


#' Create matrixInput
#'
#' @param inputId The input slot that will be used to access the value.
#' @param question character. The question to appear at the top of the table.
#' @param rowIDs character. Vector of row identifiers that will be used to find
#'   values that the user has selected. In the output, the component will return
#'   a named list of values, each name corresponding to the row id, and the
#'   value - to the value user has selected in this row.
#' @param rowLLabels character. Vector (or a matrix with one column) of labels that
#'   displayed in the leftmost point of each row. The column name of the matrix
#'   could be displayed in the header of the assignment matrix.
#' @param rowRLabels character. Vector (or a matrix with one column) of labels that
#'   displayed in the rightmost point of each row. The column name of the matrix
#'   could be displayed in the header of the assignment matrix. Using this argument
#'   is optional. But it allows to create Likert scales, potentially with several
#'   scales arranged in a matrix.
#' @param choices List of values to select from (if elements of the list are
#'   named then that name rather than the value is displayed to the user). If
#'   this argument is provided, then choiceNames and choiceValues must not be
#'   provided, and vice-versa. The values should be strings; other types (such
#'   as logicals and numbers) will be coerced to strings.
#' @param selected Vector of the initially selected values (if not specified then
#'   defaults to \code{NULL}).
#' @param choiceNames,choiceValues List of names and values, respectively, that
#'   are displayed to the user in the app and correspond to the each choice (for
#'   this reason, the objects 'choiceNames' and 'choiceValues' must have the
#'   same length). If either of these arguments is provided, then the other must
#'   be provided and choices must not be provided. The advantage of using both of
#'   these over a named list for choices is that the object 'choiceNames' allows
#'   any type of UI object to be passed through (tag objects, icons, HTML code,
#'   ...), instead of just simple text.
#' @param rowIDsName single character that defines the header of the ID column in the
#'   input matrix.
#' @param labelsWidth List of two valid values of CSS length unit. Each element
#'   has to be a properly formatted CSS unit of length (e.g., \code{'10\%'},
#'   \code{'40px'}, \code{'auto'}), specifying the minimum (first value) and
#'   maximum (second value) width of the labels columns. The valid elements will
#'   be written to the \code{style} attribute of the labels \code{td} tags.
#' @param inputType character string with a value of either 'checkbox' or 'radio'.
#'   If 'checkbox', then the user can select multiple values. If 'radio', then the
#'   user can select only one value.
#'
#' @return HTML markup for matrixInput
#'
#' @examples
#' library(shiny)
#' library(shinyMatrix)
#'
#'
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'   data(exTaxonList)
#'   data(exPftList)
#'
#'   ui <- fluidPage(
#'     CheckboxMatrixInput(
#'       inputId = "rmi01", rowIDs = head(exTaxonList$Var),
#'       rowLLabels = head(as.matrix(subset(exTaxonList, select = "VarName"))),
#'       choices = exPftList$ID,
#'       selected = head(exTaxonList$DefPFT)
#'     ),
#'     verbatimTextOutput("debug01")
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$debug01 <- renderPrint({
#'       input$rmi01
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' if (interactive()) {
#'   ui <- fluidPage(
#'     checkboxMatrixInput(
#'       inputId = "rmi02", rowIDs = c("Performance", "Statement A"),
#'       rowLLabels = c("Poor", "Agree"),
#'       rowRLabels = c("Excellent", "Disagree"),
#'       choices = 1:5,
#'       selected = rep(3, 2),
#'       rowIDsName = "Grade",
#'       labelsWidth = list("100px", "100px")
#'     ),
#'     verbatimTextOutput("debug02")
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$debug02 <- renderPrint({
#'       input$rmi02
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @export
#'
matrixInput <- function(inputId,
                        question,
                        rowIDs,
                        rowLLabels,
                        rowRLabels = NULL,
                        choices = NULL,
                        selected = NULL,
                        choiceNames = NULL,
                        choiceValues = NULL,
                        rowIDsName = "ID",
                        labelsWidth = list(NULL, NULL),
                        inputType = "checkbox") {
  # Check and normalize the inputs
  args <- shiny:::normalizeChoicesArgs(choices, choiceNames, choiceValues)
  selected <- shiny::restoreInput(id = inputId, default = selected)
  validateParams(rowIDs, rowLLabels, rowRLabels, selected, args$choiceNames, rowIDsName, labelsWidth)

  # Generate the HTML for the input matrix
  inputMatrix <- generateMatrix(
    inputId = inputId,
    question = question,
    rowIDs = rowIDs,
    rowLLabels = rowLLabels,
    rowRLabels = rowRLabels,
    selected = selected,
    choiceNames = args$choiceNames,
    choiceValues = args$choiceValues,
    rowIDsName = rowIDsName,
    labelsWidth = labelsWidth,
      inputType = inputType
    )

  divClass <- "form-group shiny-inputmatrix-container dataTable-container"

  # Ensure that the JavaScript and CSS files are locatable
  shiny::addResourcePath("inputmatrix", system.file(package = "shinyMatrix"))

  shiny::tagList(
    shiny::tags$head(
      shiny::singleton(shiny::tags$script(src = "inputmatrix/inputMatrixBinding.js")),
      shiny::singleton(shiny::tags$link(
        rel = "stylesheet", type = "text/css",
        href = "inputmatrix/inputMatrixCss.css"
      ))
    ),
    shiny::tags$div(
      id = inputId,
      class = divClass,
      inputMatrix
    )
  )
}
