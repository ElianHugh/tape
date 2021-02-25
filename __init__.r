#' @export
box::use(
    . / add[add],
    . / init[init],
    . / install[install],
    . / remove[remove],
    . / upgrade[upgrade],
    . / tape[tape]
)

# Internal variables
#' @export
#' @keywords internal
.tape_env <- rlang::env(
    # * TODO, don't use globalenv here
    rlang::current_env(),
    module_lib = path.expand("~/R/tape/modules/"),
    pkg_lib = path.expand("~/R/tape/packages/"),
    cache_lib = path.expand("~/R/tape/cache/"),
    module_json = glue::glue("{here::here()}/tape.json"),
    lock_json = glue::glue("{here::here()}/tape.lock")
)

.on_load <- function(ns) {
    box::use(
        cli[cli_h1, cli_div],
        here[here],
        glue[glue]
    )

    module_name <- box::name()
    cli_div(theme = list(span.emph = list(color = "orange")))
    cli_h1(
        'Loading module "{.emph {module_name}}"'
    )
    cli::cli_alert_info(
        "{.emph {module_name}} exposes the following functions: add, grab, init, install, remove, upgrade"
    )

}
