#' @export
upgrade <- function(repo = NULL) {
    box::use(
        . / tape[get_tape_file, tape_audit],
        . / util[is_blank],
        glue[glue],
        purrr[walk]
    )

    # if no repo specified, upgrade all
    if (is_blank(repo)) {
        package <- get_tape_file()
        package <- glue("{package$modules$author}/{package$modules$moduleName}@HEAD")
        walk(package, upgrade_helper)
    } else {
        walk(repo, upgrade_helper)
    }

    tape_audit()
}

upgrade_helper <- function(x) {
    box::use(
        . / SHA[compare_SHA],
        . / add[add],
        . / remove[rmv = remove],
        . / install[inst = install],
        . / tape[get_tape_file],
        glue[glue]
    )

    if (compare_SHA(x) == FALSE) {
        suppressMessages(rmv(x))
        add(x)
        inst(x)
    }
}