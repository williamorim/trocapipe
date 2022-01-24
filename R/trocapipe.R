#' Troca pipe
#'
#' Troca o pipe tradicional pelo novo ou vice-versa.
#'
#' @param file String com o caminho ate o arquivo com o codigo.
#' @param from String indicando se eh o pipe tradicional "%>%" ou o
#' pipe novo "|>". O padrao eh o tradicional "%>%.
#' @param to String indicando se eh o pipe tradicional "%>%" ou o
#' pipe novo "|>". O padrao eh o novo "|>".
#' @param overwrite Se TRUE, sobresve o arquivo de entrada. Se FALSE
#' (padrao), coloca o codigo novo na tela.
#'
#' @return Retorna um TRUE invisivel caso ela funcione.
#' @export
#'
#' @examples
#'
#' trocapipe(
#'   system.file("exemplos/exemplo_pipe_tradicional.R", package = "trocapipe")
#' )
#'
#' \dontrun{
#'   trocapipe("file.R", "%>%", "|>")
#' }
#'
trocapipe <- function(file, from = "%>%", to = "|>", overwrite = FALSE) {

  lines <- readLines(file) %>%
    stringr::str_replace_all(stringr::fixed(from), stringr::fixed(to))

  if(overwrite) {
    writeLines(lines, file)
  } else {
    lines %>%
      paste(collapse = "\n") %>%
      cat()
  }

  invisible(TRUE)
}


#' Troca pipe
#'
#' Troca o pipe tradicional pelo novo.
#'
#' @param file String com o caminho ate o arquivo com o codigo.
#'
#' @return Retorna um TRUE invisivel caso ela funcione.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'   trocapipe("file.R")
#' }
#'
trocapipe_tradicional <- function(file) {
  trocapipe(file, "%>%", "|>")
}

#' Troca pipe
#'
#' Troca o pipe tradicional pelo novo.
#'
#' @param file String com o caminho ate o arquivo com o codigo.
#'
#' @return Retorna um TRUE invisivel caso ela funcione.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'   trocapipe("file.R")
#' }
#'
trocapipe_novo <- function(file) {
  trocapipe(file, "|>", "%>%")
}


