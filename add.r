#' @export
add <- function(repo, lib = NULL, force = FALSE) {
    box::use(
        . / tape[tape_audit],
        . / crawl[drill_packages],
        purrr[walk]
    )

    walk(repo, ~ add_helper(.x, lib, force))
    drill_packages()
}

add_helper <- function(repo, lib = NULL, force = FALSE) {
    box::use(
        jsonlite[write_json, unbox],
        cli[cli_h1, cli_alert, cli_alert_warning, cli_alert_success],
        rlang[interrupt],
        glue[glue],
        . / util[scalar, is_blank],
        . / tape[get_tape_file, tape_audit, add_to_tape],
        . / SHA[grab_SHA],
        . / install[inst = install]
    )

    input <- as.list(unlist(strsplit(repo, "[/@]")))

    # Check if input is valid
    # before continuing
    if (length(input) > 1 && length(input) < 4) {
        details <- list(
        moduleName = ifelse(!is_blank(input[2]), scalar(unlist(input[2])), NA),
        author = ifelse(!is_blank(input[1]), scalar(unlist(input[1])), NA),
        repo = ifelse(!is_blank(input[1]) && !is_blank(input[2]), scalar(glue("https://github.com/{input[1]}/{input[2]}/")), NA),
        version = ifelse(!is_blank(input[1]) && !is_blank(input[2]), scalar(grab_SHA(repo)), NA)
        )

        # If tape does contain the module, add it to tape
        add_to_tape(details)

    } else {
         cli_alert_warning(
             "Invalid repo input. An add call must refer to a Github repository. E.g. grab$add('User/Repo')"
         )
    }

}
