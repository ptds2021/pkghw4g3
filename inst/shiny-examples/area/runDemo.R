#' @export
runDemo <- function() {
    appDir <- system.file("shiny-examples", "area", package = "pkghw4g3")
    if (appDir == "") {
        stop(
            "Could not find example directory. Try re-installing pkghw4g3.",
            call. = FALSE
        )
    }

    shiny::runApp(appDir, display.mode = "normal")

}
