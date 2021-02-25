#' @export
install <- function(lib = NULL, force = FALSE) {
    box::use(
        here[here],
        glue[glue],
        jsonlite[read_json],
        purrr[walk],
        . / tape[get_tape_file, tape_audit],
        . / crawl[drill_dependencies],
        . / lock[create_lock_file, add_to_lock, get_lock_file]
    )

    create_lock_file()

    # try to install from lock file versions
    lock <- get_lock_file()
    lockedModules <- glue("{lock$modules$author}/{lock$modules$moduleName}@{lock$modules$version}")
    walk(lockedModules, ~ install_helper(.x, lib, force))

    # try to install from tape file versions
    # will skip if already installed (^)
    tape <- get_tape_file()
    tapeModules <- glue("{tape$modules$author}/{tape$modules$moduleName}@{tape$modules$version}")
    walk(tapeModules, ~ install_helper(.x, lib, force))

    tape_audit()
}


#' @description very rudementary function to download
#' a github repo and save it into a library
install_helper <- function(repo, lib, force) {
    box::use(
        utils[installed.packages, URLencode],
        fs[dir_exists, dir_delete],
        glue[glue],
        here[here],
        pak[pkg_install],
        cli[cli_alert_warning],
        . / tape[add_to_tape, get_tape_file],
        . / util[construct_git_path],
        . / SHA[compare_SHA],
        . / cache[cache_module],
        . / crawl[drill_dependencies],
        . / lock[add_to_lock, get_lock_file]
    )

    # Need to refactor this out as it is
    # repeated in a couple places
    module_lib <- get("module_lib", .GlobalEnv$tape$.tape_env)
    cache_lib <- get("cache_lib", .GlobalEnv$tape$.tape_env)
    input <- as.list(unlist(strsplit(repo, "[/@]")))
    libLoc <- glue("{module_lib}/{input[2]}")

    if (dir_exists(glue("modules/{input[2]}"))) {
        if (is_SHA_correct(repo) == FALSE) {
            # incorrect module version is present
            dir_delete(glue("modules/{input[2]}"))
            git_path <- construct_git_path(repo)
            loc <- cache_module(git_path, input, repo)
            drill_dependencies(
                glue("{loc}/tape.json"), input[2], lib
            )
        }
    } else {
        git_path <- construct_git_path(repo)
        loc <- cache_module(git_path, input, repo)
        drill_dependencies(glue("{loc}/tape.json"), input[2], lib)
    }

    # Add to lock file without overwrite
    # i.e. if file was not present in lock file
    # add it
    add_to_lock(repo)
    add_to_tape(repo)

    # * TODO: version packages, call tape$install_pkg
    # Install any packages if not installed already
        tape <- get_tape_file()
        pkgToInstall <- setdiff(tape$packages$package, rownames(installed.packages()))

        if (length(pkgToInstall) > 0) {
            pkg_install(pkgToInstall)
        }
}

# Drill module file, search for
# either a .sha file, a tape.json,
# or other identifier to show its SHA
is_SHA_correct <- function(repo) {
    box::use(
        . / tape[get_tape_file],
        . / SHA[compare_SHA],
        . / lock[get_lock_file],
        tools[file_path_sans_ext],
        fs[file_exists, dir_ls],
        jsonlite[read_json],
        glue[glue]
    )

    input <- as.list(unlist(strsplit(repo, "[/@]")))
    modFolder <- glue("modules/{input[2]}")
    tape <- get_tape_file()
    lock <- get_lock_file()

    storedSHA <- tryCatch(
        {
            lock$modules[
            which(lock$modules$moduleName == input[2] &&
                lock$modules$author == input[1]),
            ]$version
        },
        error = function(e) {
            tryCatch({
                tape$modules[
                which(tape$modules$moduleName == input[2] &&
                    tape$modules$author == input[1]),
                ]$version
                },
                error = function(e) return (TRUE)
            )
        }
    )

    # Check for tape.lock
    if (file_exists(glue("{modFolder}/tape.lock"))) {
        lockFile <- read_json(
            glue("{modFolder}/tape.lock"),
            simplifyVector = TRUE
        )

        try(
            retrievedSHA <- lockFile$modules[
                which(lockFile$modules$moduleName == input[2] &&
                    lockFile$modules$author == input[1]),
            ]$version,
            silent = TRUE
        )

        print(retrievedSHA)

        if (!is.null(retrievedSHA)) {
            if (retrievedSHA == storedSHA) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        } else {
            return(TRUE)
        }
    }

    # Check for tape.json
    # find any
    if (file_exists(glue("{modFolder}/tape.json"))) {
        tapeFile <- read_json(
            glue("{modFolder}/tape.json"),
            simplifyVector = TRUE
        )

        SHA_attempt_1 <- try(
             tapeFile$modules[
                which(tapeFile$modules$moduleName == input[2] &&
                    tapeFile$modules$author == input[1]),
            ]$version,
            silent = TRUE
        )

        SHA_attempt_2 <- tapeFile$SHA

        if (!inherits(SHA_attempt_1,  "try-error") && !is.null(SHA_attempt_1)) {
            if (SHA_attempt_1 == storedSHA) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        } else if (!is.null(SHA_attempt_2)) {
            if (SHA_attempt_2 == storedSHA) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        }
    }

    # Check for SHA file
    if (length(dir_ls(path = modFolder))) {
        SHA_file <- file_path_sans_ext(
            basename(
                dir_ls(
                    path = modFolder,
                    glob = "*.sha"
                )[1]
            )
        )

        if(!is.na(SHA_file) && length(SHA_file)) {
            if (SHA_file == storedSHA) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        } else {
            return(TRUE)
        }

    }

    # Otherwise, return TRUE regardless
    # to prevent issues with unversioned modules
    return(TRUE)
}
