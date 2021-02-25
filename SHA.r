#' @title Grab module SHA
#' @description Returns the commit SHA of a module's git repo
#' @param repo A repo, such as "ElianHugh/boxingtape@master"
#' if the branch isn't specified, grabs @HEAD branch
#' @export
#' @import box
#' @keywords internal
grab_SHA <- function(repo) {
    box::use(
        jsonlite[read_json],
        cli[cli_h1, cli_alert, cli_alert_warning, cli_alert_success],
        glue[glue],
        utils[download.file, URLencode, untar],
        . / util[is_blank]
    )

    git_template <- "https://api.github.com/repos/%s/%s/commits/%s"
    input <- as.list(unlist(strsplit(repo, "[/@]")))

    if (is_blank(unlist(input[3]))) {
        git_request <- URLencode(
            sprintf(
                git_template,
                input[1],
                input[2],
                "HEAD"
            )
        )
    } else {
        git_request <- URLencode(
            sprintf(
                git_template,
                input[1],
                input[2],
                input[3]
            )
        )
    }

    SHA <- read_json(git_request)$sha
    return(SHA)
}

#' Compare SHA
#' @param repo the examined repo
#' @param oldSHA optional value, compared to the repoSHA for equality check
#' @return logical
#' @export
#' @keywords internal
compare_SHA <- function(repo, oldSHA = NULL) {
    box::use(
        . / tape[get_tape_file],
        . / util[is_blank],
        cli[cli_alert_success, cli_alert_warning, cli_alert_danger]
    )

    repoSHA <- grab_SHA(repo)
    input <- as.list(unlist(strsplit(repo, "[/@]")))

    if (is_blank(oldSHA)) {
        package <- get_tape_file()
        storedSHA <- package$modules[
            which(package$modules$moduleName == input[2] &&
                package$modules$author == input[1]),
        ]$version
    } else {
        storedSHA <- oldSHA
    }

    if (length(storedSHA)) {
        if (typeof(storedSHA) == "list") {
            if (repoSHA %in% storedSHA) {
                # SHA is up to date
                cli_alert_success("'{input[2]}' SHA is correct/up-to-date.")
                invisible(return(TRUE))
            } else {
                 # SHA is out of date
                 cli_alert_warning("'{input[2]}' SHA is incorrect/not up-to-date.")
                 invisible(return(FALSE))
            }
        } else {
            if (storedSHA == repoSHA) {
                # SHA is up to date
                cli_alert_success("'{input[2]}' SHA is correct/up-to-date.")
                invisible(return(TRUE))
            } else {
                # SHA is out of date
                cli_alert_warning("'{input[2]}' SHA is incorrect/not up-to-date.")
                invisible(return(FALSE))
            }
        }
    } else {
        # Stored SHA does not exist
        cli_alert_danger("There is no SHA stored for '{input[1]}/{input[2]}'.")
        invisible(return(FALSE))
    }
}