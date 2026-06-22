# Run the app in dev mode — called by golem::run_dev()
# Ensure the package is loaded first: devtools::load_all()

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

options(
  # Warn on partial matching (good discipline)
  warnPartialMatchArgs = FALSE,
  warnPartialMatchDollar = FALSE,
  warnPartialMatchAttr = FALSE
)

fin.extract::run_app()
