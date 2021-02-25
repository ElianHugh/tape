#' @export
init <- function() {
    create_tape_files()
    create_directories()
}

create_tape_files <- function() {
    box::use(
        here[here],
        jsonlite[write_json, read_json],
        glue[glue],
        cli[cli_h1, cli_alert, cli_alert_danger, cli_alert_success],
        . / util[scalar]
    )

    # Create module file
    module_json <- get("module_json", .GlobalEnv$tape$.tape_env)

    if (!file.exists(module_json)) {
        cli_alert_success("Creating tape.json at '{here()}'.")
        temp <- list(
            name = scalar("MyFirstModule"),
            author = scalar("User"),
            version = scalar(format(1.000, nsmall = 3)),
            modules = list(),
            packages = list()
        )
        write_json(temp, module_json, pretty = TRUE)
    } else {
        cli_alert(
            "tape.json already exists at '{module_json}'."
        )
    }

}


create_directories <- function() {
    box::use(
        here[here],
        glue[glue],
        cli[cli_alert_success, cli_alert]
    )

    module_lib <- get("module_lib", .GlobalEnv$tape$.tape_env)
    pkg_lib <- get("pkg_lib", .GlobalEnv$tape$.tape_env)
    cache_lib <- get("cache_lib", .GlobalEnv$tape$.tape_env)
    localModules <- glue("{here()}/modules/")
    localPackages <- glue("{here()}/packages/")

    # Global Directories
    if (!dir.exists(path.expand("~/R/tape/"))) {
        dir.create(path.expand("~/R/tape/"))
        cli_alert_success("tape directory created at \'{path.expand('~/R/tape/')}\'")
    }

    if (!dir.exists(module_lib)) {
        dir.create(module_lib)
        cli_alert_success("tape module library created at '{module_lib}'")
    }
    if (!dir.exists(pkg_lib)) {
        dir.create(pkg_lib)
        cli_alert_success("tape package library created at '{pkg_lib}'")
    }

    if (!dir.exists(cache_lib)) {
        dir.create(cache_lib)
        cli_alert_success("tape cache library created at '{cache_lib}'")
    }

    # Local Directories
    if (!dir.exists(localModules)) {
        dir.create(localModules)
        cli_alert_success("local module directory created at '{localModules}'")
    } else {
        cli_alert("local module library already exists at '{localModules}'")
    }

    if (!dir.exists(localPackages)) {
        dir.create(localPackages)
        cli_alert_success("local package directory created at '{localPackages}'")
    } else {
        cli_alert("local package library already exists at '{localPackages}'")
    }
}