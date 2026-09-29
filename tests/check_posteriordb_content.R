# remotes::install_github("stan-dev/posteriordb-r")
library(posteriordb)
source("tests/check_posteriordb_content_functions.R")
pdbl <- pdb_local("posterior_database")
status_code <- check_pdb(pdbl, run_stan_code_checks = FALSE)

# A posterior with no reference_posterior_name should not have a matching
# reference-draw archive. Such archives would otherwise be present in the
# database but unreachable through the posterior metadata.
posterior_dir <- file.path("posterior_database", "posteriors")
draw_dir <- file.path("posterior_database", "reference_posteriors", "draws", "draws")
posterior_files <- list.files(
  posterior_dir,
  pattern = "\\.json$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

null_reference_draws <- lapply(posterior_files, function(path) {
  posterior <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) stop("Could not read posterior metadata: ", path, "\n", e$message)
  )

  if (!is.list(posterior) ||
      !"reference_posterior_name" %in% names(posterior) ||
      !is.null(posterior[["reference_posterior_name"]])) {
    return(NULL)
  }

  posterior_name <- posterior[["name"]]
  if (is.null(posterior_name) || length(posterior_name) != 1L) {
    posterior_name <- sub("\\.json$", "", basename(path), ignore.case = TRUE)
  }

  draw_path <- file.path(draw_dir, paste0(posterior_name, ".json.zip"))
  if (!file.exists(draw_path)) return(NULL)

  data.frame(
    posterior_name = posterior_name,
    posterior_file = path,
    reference_draw_file = draw_path,
    stringsAsFactors = FALSE
  )
})
null_reference_draws <- Filter(Negate(is.null), null_reference_draws)

if (length(null_reference_draws) > 0L) {
  null_reference_draws <- do.call(rbind, null_reference_draws)
  cat("Found reference-draw archives for posteriors with null reference_posterior_name:\n")
  print(null_reference_draws, row.names = FALSE)
  status_code <- 1L
} else {
  cat("No reference-draw archives found for posteriors with null reference_posterior_name.\n")
}

# These tests are currently skipped because a large number of model
# updates are being done to update to Stan 2.26 syntax.
# All models has been tested locally.
# In addition it is difficulties in getting changed files that needs to be fixed
if(FALSE){
  # Run Stan code for changed or added models
  added_modified_paths <- strsplit(readLines(con = "added_modified.txt"), " ")[[1]]
  posteriors_to_check <- get_posteriors_from_paths(paths = added_modified_paths, pdbl)

  # Posteriors to skip on CI (they work locally)
  posteriors_to_skip_check <- c("dogs-dogs_nonhierarchical", "wells_data-wells_dae_c_model", "seeds_data-seeds_stanified_model")


  if(length(posteriors_to_check) > 0){
    cat("Checking changed posteriors:\n")
    cat(paste(posteriors_to_check, collapse = "\n"),"\n\n")
    library(rstan)
    for(i in seq_along(posteriors_to_check)){
      if(posteriors_to_check[i] %in% posteriors_to_skip_check) next
      post <- pdb_posterior(posteriors_to_check[i], pdbl)
      status_code2 <- posteriordb:::check_pdb_posterior(post, run_stan_code_checks = TRUE)
      posteriordb:::pdb_clear_cache(pdbl)
      status_code <- max(status_code, as.integer(!status_code2))
    }
  }
}
q(status = status_code)
