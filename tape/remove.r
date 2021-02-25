#' @export
remove <- function(repo, lib) {
    box::use(
        purrr[walk],
        . / tape[tape_audit]
    )

    walk(repo, ~ remove_helper(.x, lib))

    tape_audit()
}

remove_helper <- function(repo, lib) {
    box::use(
        jsonlite[write_json],
        cli[cli_h1, cli_alert, cli_alert_warning, cli_alert_success],
        glue[glue],
        fs[dir_delete, dir_exists],
        here[here],
        . / tape[get_tape_file],
        . / lock[get_lock_file],
        . / crawl[drill_packages]
    )

     input <- as.list(unlist(strsplit(repo, "[/@]")))
     tape <- get_tape_file()
     lock <- get_lock_file()

     # * TODO, when versioning is implemented properly, check for version
    remove_from_tape(repo)
    remove_from_lock(repo)
    drill_packages(clean = TRUE)

    if (dir_exists(glue("{here()}/modules/{input[2]}"))) {
        dir_delete(glue("{here()}/modules/{input[2]}"))
    }
}