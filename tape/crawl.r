# find used packages by loading .r files into a temporary
# environment and assessing environment's session info
#' @export
#' @keywords internal
drill_packages <- function(clean = FALSE) {
    box::use(
        glue[glue],
        here[here],
        jsonlite[write_json],
        stringi[stri_reverse, stri_split_fixed],
        purrr[walk],
        tools[file_path_sans_ext],
        . / util[scalar, exists_in],
        . / tape[get_tape_file]
    )

    modFiles <- file_path_sans_ext(list.files(
        path = glue("{here()}/modules"),
        recursive = TRUE,
        ignore.case = TRUE,
        pattern = {
            ".r$"
        }
    ))

    # Clean module dependencies if called from remove
    if (as.character(sys.call(-1))[[1]] == "remove_helper") {
        tape_file <- get_tape_file()
        tape_file$packages <- list()
        write_json(tape_file, "tape.json", auto_unbox = TRUE, pretty = TRUE)
    }


    # box can only use standard r identifiers
    # so we have to ensure that the symbols passed
    # are usable
    screen_names <- function(x) {
        box::use(
            rlang[parse_expr, env_names]
        )
        splitName <- stri_reverse(stri_split_fixed(stri_reverse(x), "/", n = 2)[[1]])[2:1]
        moduleString <- glue(".. / modules / {splitName[1]} /`{splitName[2]}`")
        eval({
                parse_expr(glue("box::use({moduleString})"))
            }
        )
    }

    walk(modFiles, ~ suppressMessages(screen_names(.x)))
    si <- utils::sessionInfo()$loadedOnly

    append_to_tape <- function(pkg, tape) {
         box::use(
             cli[cli_alert_success, cli_alert_warning],
             . / util[is_blank]
         )

         tape <- get_tape_file()

         x <- list(
             package = scalar(pkg$Package),
             version = ifelse(!is_blank(pkg$Version), scalar(pkg$Version), "None"),
             maintainer = ifelse(!is_blank(pkg$Maintainer), scalar(pkg$Maintainer), "None"),
             repo = get_repo(pkg)
         )

         # Check if the package exists in the tape file & then add to tape

         if (!exists_in(x, tape$packages, package) && !(exists_in(x, tape$packages, maintainer))) {
             if (NROW(tape$packages) > 0) {
                 tape$packages[nrow(tape$packages) + 1, ] <- x
             } else {
                 tape$packages[1] <- list(x)
             }
             write_json(tape, "tape.json", auto_unbox = TRUE, pretty = TRUE)

         } else {
             # check the function caller
             if (as.character(sys.call(-1))[[1]] == "add") {
                 cli_alert_warning(
                     "tape.json already contains {x$package}."
                 )
             }
         }
    }

    get_repo <- function(x) {
        box::use(glue[glue], ./util[is_blank])
        if (!is_blank(x$Repository)) {
            return(scalar(x$Repository))
        } else if (!is_blank(x$GithubRef)) {
            return(
                scalar(
                    glue("{x$GithubUsername}/{x$GithubRepo}")
                )
            )
        } else {
            return("Unlisted")
        }
    }

    walk(si, ~append_to_tape(.x))

}


# get the dependencies of the installed module
# and install them if necessary
#' @export
drill_dependencies <- function(tapeFile, module, lib) {
    box::use(
        jsonlite[read_json],
        glue[glue],
        cli[cli_alert, cli_alert_info, cli_div],
        . / add[add],
        purrr[walk]
    )
    cli_div(theme = list(span.emph = list(color = "orange")))

    if (file.exists(tapeFile)) {
        cli_alert("Accessing modules for listed dependencies...")

        deps <- read_json(tapeFile, simplifyVector = TRUE)
        mods <- glue("{deps$modules$author}/{deps$modules$moduleName}@{deps$modules$version}")
        pkgs <- deps$packages$package

        if (length(mods) > 0) {
            cli_alert_info("{module} has the following exposed module dependencies: {.emph {mods} }", wrap = TRUE)
        }
        if (length(pkgs) > 0) {
            cli_alert_info("{module} has the following exposed package dependencies: {pkgs}", wrap = TRUE)
        }

        walk(mods, ~ add(.x, lib, force = FALSE))
    } else {
        cli_alert("{module} does not have a tape.json file.")
    }
}