# 1. Check if module library exists
#    create if not
# 2. Check if module library contains module
#    download if not
#    2a. Check if cached SHA is up to date
#        if not, redownload file and SHA
# 3. Copy module to local directory
#' @export
#' @keywords internal
cache_module <- function(git_request, input, repo) {
    box::use(
        utils[download.file, untar],
        cli[cli_alert],
        here[here],
        glue[glue],
        fs[dir_copy, dir_create, dir_delete, file_create, dir_exists, file_delete, file_move],
        . / SHA[compare_SHA, grab_SHA],
        . / tape[tape_audit]
    )

    module_lib <- get("module_lib", .GlobalEnv$tape$.tape_env)
    tempZipFile <- tempfile(".tar.gz")
    moduleCacheLoc <- glue("{module_lib}/{input[2]}/{input[3]}")
    loc <- glue("{here()}/modules/")
    cache_lib <- get("cache_lib", .GlobalEnv$tape$.tape_env)
    grabbedSHA <- grab_SHA(repo)

    # if module isn't appropriately cached
    # download
    if (!file.exists(moduleCacheLoc)) {
        download.file(git_request, tempZipFile, "auto")
        untar(
            tempZipFile,
            files = NULL,
            list = FALSE,
            exdir = moduleCacheLoc,
            extra = "--strip-components 1"
        )

        # Create SHA cache
        if (!dir_exists(glue("{cache_lib}/{input[2]}/"))) {
            dir_create(glue("{cache_lib}/{input[2]}/"))
        }

        file_create(glue("{cache_lib}/{input[2]}/{grabbedSHA}"))
        cli_alert("{input[2]} cached at '{moduleCacheLoc}'")

    } else {
        if (length(list.files(path = glue("{cache_lib}/{input[2]}/"))) > 0) {
            cachedSHA <- list.files(path = glue("{cache_lib}/{input[2]}/"))

            if (!compare_SHA(repo, cachedSHA)) {
                # Create new SHA
                file_create(
                    glue("{cache_lib}/{input[2]}/{grabbedSHA}")
                )

                download.file(git_request, tempZipFile, "auto")
                untar(
                    tempZipFile,
                    files = NULL,
                    list = FALSE,
                    exdir = moduleCacheLoc,
                    extra = "--strip-components 1"
                )

                cli_alert("{input[2]} new cache at '{moduleCacheLoc}'")
            }
        }
    }

    newDir <- dir_copy(moduleCacheLoc, loc)
    renamedDir <- file_move(newDir, glue("{here()}/modules/{input[2]}"))
    shaFile <- file_create(glue("{renamedDir}/{grabbedSHA}.sha"))
    return(glue("{here()}/modules/{input[2]}"))
}
