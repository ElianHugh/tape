# Respect tape.lock -> tape.json, then latest version
#' @export
install_pkg <- function(name, version) {
    box::use(
        pak[pkg_install],
        fs[dir_exists],
        here[here],
        glue[glue]
    )

    if (dir_exists(
            glue("{here()}/packages/{name}")
        )) {
        # TODO check if right version is saved
    } else {

    }
    pkg_install(name, lib = get("", .GlobalEnv$tape$.tape_env))
}