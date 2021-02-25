#' @export
create_lock_file <- function() {
    box::use(
        cli[cli_alert_success],
        jsonlite[write_json],
        fs[file_exists],
        here[here]
    )

    lock_json <- get("lock_json", .GlobalEnv$tape$.tape_env)
    if (!file_exists(lock_json)) {
        cli_alert_success("Creating tape.lock at '{here()}'.")
        temp <- list(
            modules = list(),
            packages = list()
        )
        write_json(temp, lock_json, pretty = TRUE)
    }
}

#' @export
add_to_lock <- function(repo, overwrite = FALSE) {
    box::use(
        . / util[scalar, is_blank],
        . / SHA[grab_SHA],
        rlang[interrupt],
        glue[glue],
        jsonlite[write_json]
    )

    input <- as.list(unlist(strsplit(repo, "[/@]")))
    lock <- get_lock_file()

    if (length(input) > 1 && length(input) < 4) {
        details <- list(
            moduleName = ifelse(!is_blank(input[2]), scalar(unlist(input[2])), NA),
            version = ifelse(!is_blank(input[3]), scalar(input[3]), NA),
            author = ifelse(!is_blank(input[1]), scalar(unlist(input[1])), NA),
            resolved = ifelse(!is_blank(input[1]) && !is_blank(input[2]), scalar(glue(
                "https://api.github.com/repos/{input[1]}/{input[2]}/tarball/{input[3]}")), NA)
        )

        if (overwrite == TRUE) {
            if (NROW(lock$modules) > 0) {
                lock$modules <- lock$modules[
                    which(lock$modules$moduleName != details$moduleName),
                ]
                lock$modules[nrow(lock$modules) + 1, ] <- details
            } else {
                lock$modules[1] <- list(details)
            }
        } else {
            if (!any(details$moduleName %in% lock$modules$moduleName)) {
                if (NROW(lock$modules) > 0) {
                    lock$modules[nrow(lock$modules) + 1, ] <- details
                } else {
                    lock$modules[1] <- list(details)
                }
            }
        }
        write_json(lock, "tape.lock", auto_unbox = TRUE, pretty = TRUE)
    }


}

#' @export
remove_from_lock <- function(repo) {
    box::use(
        jsonlite[write_json],
        cli[cli_alert_warning, cli_alert_success],
        . / util[is_blank]
    )

    lock <- get_lock_file()()
    record_to_remove <- get_lock_file(repo, invert = TRUE)

    if (!is_blank(record_to_remove)) {
        lock$modules <- record_to_remove
        write_json(lock, "tape.lock", auto_unbox = TRUE, pretty = TRUE)
        cli_alert_success(
            glue("'{repo}' removed from tape.lock.")
        )
    } else {
        cli_alert_warning(
            "tape.lock does not contain {repo}."
        )
    }

}

#' @export
#' @keywords internal
get_lock_file <- function() {
    box::use(jsonlite[read_json])

    lock_json <- get("lock_json", .GlobalEnv$tape$.tape_env)
    if (file.exists(lock_json)) {
        return(read_json(lock_json, simplifyVector = TRUE))
    } else {
        cli_alert_danger(
            glue("tape.lock is missing at '{here()}'")
        )
        cli_alert(
            "Ensure you have run tape$init() or tape$tape() before trying to retrieve modules."
        )
    }
}

#' @export
get_lock_record <- function(repo, invert = FALSE, approx = FALSE) {

    lock <- get_lock_file()
    input <- as.list(unlist(strsplit(repo, "[/@]")))

    if (NROW(lock$modules) > 0) {
        if (approx == FALSE) {
            if (invert == TRUE) {
                return(lock$modules[
                    which(lock$modules$moduleName != unlist(input[2])
                    && lock$modules$author != unlist(input[1])),
                ])
            } else if (any(unlist(input[2]) %in% lock$modules$moduleName & unlist(input[1]) %in% lock$modules$author)) {
                return(lock$modules[
                    which(lock$modules$moduleName == unlist(input[2])
                    | lock$modules$author == unlist(input[1])),
                ])
            } else {
                return(NULL)
            }
        } else {
            if (invert == TRUE) {
                return(lock$modules[
                    which(lock$modules$moduleName != input),
                ])
            } else if (input %in% lock$modules$moduleName) {
                return(lock$modules[
                    which(lock$modules$moduleName == input),
                ])
            } else {
                return(NULL)
            }
        }
    } else {
        return(NULL)
    }
}
