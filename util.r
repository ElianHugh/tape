#' @export
#' @keywords internal
scalar <- function(x) {
    class(x) <- c("scalar", class(x))
    x
}

#' @export
#' @keywords internal
is_blank <- function(x) {
    return(
        is.null(x) ||
            length(x) == 0 ||
            all(is.na(x)) ||
            all(x == "")
    )
}

#' @export
#' @keywords internal
exists_in <- function(checkList, comparedList, ...) {
    box::use(
        purrr[pmap]
    )
    comparator <- function(checkList, comparedList, by) {
        box::use(rlang[interrupt])

        by <- as.character(by)
        if (typeof(checkList) == "list" && typeof(comparedList) == "list") {
            if (any(checkList[[by]] %in% comparedList[[by]]) == TRUE) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        } else if (typeof(checkList) != "list" && typeof(comparedList) == "list") {
            if (any(checkList %in% comparedList[[by]]) == TRUE) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        } else if (typeof(checkList) == "list" && typeof(comparedList) != "list") {
            if (any(checkList[[by]] %in% comparedList) == TRUE) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        } else {
            if (any(checkList %in% comparedList) == TRUE) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        }
    }

    by <- rlang::ensyms(...)

    comparedVals <- pmap(
        .l = list(
            checkList,
            comparedList,
            by
        ),
        .f = ~comparator(checkList, comparedList, by)
    )

    if (suppressWarnings(any(comparedVals))) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}


#' @export
#' @keywords internal
construct_git_path <- function(repo) {
    box::use(
        utils[URLencode]
    )

    git_template <- "https://api.github.com/repos/%s/%s/tarball/%s"
    input <- as.list(unlist(strsplit(repo, "[/@]")))

    if (is.null(unlist(input[3]))) {
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

    return(git_request)
}