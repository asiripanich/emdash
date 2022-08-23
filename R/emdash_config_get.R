emdash_config_get <- function(name, config_file = Sys.getenv("EMDASH_CONFIG", here::here("_emdash.yaml"))) {
    message("config_file: ", config_file)
    config::get(value = name, file = config_file)
}
