Sys.setenv(TZ = "GMT")
if (!testthat:::on_ci()) {
  cons <- connect_stage_collections()
}
