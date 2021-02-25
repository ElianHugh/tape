#' @export
tape <- function() {
    box::use(
        glue[glue],
        here[here],
        . / install[install],
        . / init[init],
        . / crawl[drill_packages]
    )

    if (!file.exists(glue("{here()}/tape.json"))) init()
    install()
    drill_packages()
}

#' @export
#' @keywords internal
get_tape_file <- function() {
    box::use(
        jsonlite[read_json],
        cli[cli_alert, cli_alert_danger],
        glue[glue],
        here[here],
        rlang[interrupt]
    )

    module_json <- get("module_json", .GlobalEnv$tape$.tape_env)
    if (file.exists(module_json)) {
        return(read_json(module_json, simplifyVector = TRUE))
    } else {
        cli_alert_danger(
            glue("tape.json is missing at '{here()}'")
        )
        cli_alert(
            "Ensure you have run tape$init() or tape$tape() before trying to retrieve modules."
        )
        interrupt()
    }
}

#' @export
add_to_tape <- function(input) {
    box::use(
        jsonlite[write_json],
        cli[cli_alert_success, cli_alert_warning],
        glue[glue],
        . / install[inst = install],
        . / util[exists_in]
    )

    tape <- get_tape_file()

    if (!typeof(input) == "list") {
        input <- as.list(unlist(strsplit(input, "[/@]")))
        input <- list(
            author = unlist(input[1]),
            moduleName = unlist(input[2]),
            version = unlist(input[3]),
            repo = glue("https://github.com/{unlist(input[1])}/{unlist(input[2])}/")
        )
    }

    if (!exists_in(input, tape$modules, moduleName) && !exists_in(input, tape$modules, author)) {

        if (NROW(tape$modules) > 0) {
            tape$modules[nrow(tape$modules) + 1, ] <- input
        } else {
            tape$modules[1] <- list(input)
        }

        write_json(tape, "tape.json", auto_unbox = TRUE, pretty = TRUE)
        cli_alert_success(
            glue("'{input$moduleName}' added to tape.json")
        )

        ## Call install
        module_lib <- get("module_lib", .GlobalEnv$tape$.tape_env)
        libLoc <- glue("{module_lib}/{input$moduleName}/{input$version}")
        if (!dir.exists(libLoc)) {
            inst(repo, force = TRUE)
        }
    } else {
        # check the function caller
        # if called from add, show...
        if (unlist(sys.call(-5))[[1]] == "tape$add") {
            cli_alert_warning(
                "tape.json already contains '{input$moduleName}'. Run tape$upgrade to check for updates."
            )
        }
    }
}

#' @export
remove_from_tape <- function(repo) {
    box::use(
        jsonlite[write_json],
        cli[cli_alert_warning, cli_alert_success],
        . / util[is_blank]
    )

    tape <- get_tape_file()
    record_to_remove <- get_tape_record(repo, invert = TRUE)

    if (is_blank(record_to_remove)) {
        tape$modules <- record_to_remove
        write_json(tape, "tape.json", auto_unbox = TRUE, pretty = TRUE)
        cli_alert_success(
            glue("'{repo}' removed from tape.lock.")
        )
    } else {
        cli_alert_warning(
            "tape.json does not contain {repo}."
        )
    }
}

#' @export
get_tape_record <- function(repo, invert = FALSE, approx = FALSE) {
    box::use(./util[exists_in])

    input <- as.list(unlist(strsplit(repo, "[/@]")))
    tape <- get_tape_file()

    if (NROW(tape$modules) > 0) {
        if (approx == FALSE) {
            if (invert == TRUE) {
                return(tape$modules[
                    which(tape$modules$moduleName != unlist(input[2])
                    && tape$modules$author != unlist(input[1])),
                ])
            } else if (exists_in(unlist(input[2]), tape$modules, moduleName) &
                       exists_in(unlist(input[1]), tape$modules, author)) {
                    return(tape$modules[
                        which(tape$modules$moduleName == unlist(input[2])
                        | tape$modules$author == unlist(input[1])),
                    ])
            } else {
                return(NULL)
            }
        } else {
            if (invert == TRUE) {
                return(tape$modules[
                    which(tape$modules$moduleName != input),
                ])
            } else if (input %in% tape$modules$moduleName) {
                return(tape$modules[
                    which(tape$modules$moduleName == input),
                ])
            } else {
                return(NULL)
            }
        }
    } else {
        return(NULL)
    }
}

#' @export
#' @keywords internal
tape_audit <- function() {
    box::use(
        cli[cli_h1, cli_alert, cli_alert_warning, cli_alert_success],
        utils[installed.packages],
        purrr[walk, pwalk],
        here[here],
        glue[glue],
        fs[dir_copy, dir_exists, file_exists],
        jsonlite[read_json],
        tools[file_path_sans_ext],
        . / lock[get_lock_file, get_lock_record],
        . / util[is_blank]
    )

    tape <- get_tape_file()
    lock <- get_lock_file()

    # Check tape structure
    if (length(tape) != 5) {
        cli_alert_warning(
            "tape.json field length must equal 5. Check tape.json for inconsistencies."
        )
    }

    if (is_blank(tape$name) |
        is_blank(tape$author) |
        is_blank(tape$version) |
        is_blank(names(tape)[4]) |
        is_blank(names(tape)[5])) {
            blankVars <- which(
                lapply(tape, is_blank) == TRUE
            )
            cli_alert_warning(
                "tape.json structure is invalid. Missing values for: '{names(blankVars)}'."
            )
    }

    if (is_blank(names(lock)[1]) |
        is_blank(names(lock)[2])) {
        blankVars <- which(
            lapply(lock, is_blank) == TRUE
        )
        cli_alert_warning(
            "tape.lock structure is invalid. Missing values for: '{names(blankVars)}'."
        )
    }

    if (length(tape$modules) > 0 && length(tape$modules) != 4) {
        cli_alert_warning(
            "tape.json 'modules' field is invalid length. Check tape.json for errors."
        )
    }

    if (length(tape$packages) > 0 && length(tape$packages) != 4) {
        cli_alert_warning(
            "tape.json 'packages' field is invalid length. Check tape.json for errors."
        )
    }

    # Check if modules are properly installed
    pwalk(list(tape$modules$moduleName, tape$modules$version), ~ (
        if (!file.exists(
            glue("{here()}/modules/{.x}")
        )) {
            cli_alert_warning("'{.x}' is missing from the local module library")

            lib <- glue("{here()}/modules")
            module_lib <- get("module_lib", .GlobalEnv$tape$.tape_env)
            loc <- glue("{lib}/{.x}")
            moduleCacheLoc <- glue("{module_lib}/{.x}/{.y}")

            if (dir_exists(moduleCacheLoc)) {
                dir_copy(moduleCacheLoc, loc)
                cli_alert("{.x} copied from cache to local module library")
            }
        }))

    # Check if packages are properly installed
    walk(tape$packages$package, ~ (
        if (!(.x %in% installed.packages())) {
            cli_alert_warning("'{.x}' is missing from the package library")
        }))

    # Check for unlisted modules
    installedMods <- list.files(path = glue("{here()}/modules/"))
    walk(installedMods, ~ (
        if (!any(.x %in% tape$modules$moduleName)) {
            cli_alert_warning("'{.x}' is missing from tape.json")

            if (.x %in% lock$modules$moduleName) {
                approxRecord <- get_lock_record(.x, approx = TRUE)
                cli_alert_warning(
                    "'{.x}' approximated to '{approxRecord$author}/{.x}' from tape.lock"
                )
                add_to_tape(
                    glue("{approxRecord$author}/{.x}/{approxRecord$version}")
                )
            } else if (file_exists(glue("{here()}/modules/{.x}/tape.json"))) {
                remoteTape <- read_json(
                    glue("{here()}/modules/{.x}/tape.json"),
                    simplifyVector = TRUE
                )
                remoteSHA <- list.files(
                    glue("{here()}/modules/{.x}/"),
                    pattern = "*.sha"
                )[1]
                if (!is_blank(remoteSHA)) {
                    modSHA <- file_path_sans_ext(remoteSHA)
                } else {
                    modSHA <- "HEAD"
                }

                add_to_tape(glue("{remoteTape$author}/{.x}/{modSHA}"))
            } else {
                 cli_alert_warning(
                     "'{.x}' has no reference to add to tape.json"
                 )
            }
        }))
}
