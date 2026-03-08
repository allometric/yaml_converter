# convert_all.R
source("convert_to_yaml.R")

devtools::load_all("../models")
models::set_params_path("../models/parameters/")

pubs_dir <- "../models/publications"
pub_dirs <- list.dirs(pubs_dir, recursive = FALSE)

# Process all alphabetic directories
for (d in pub_dirs) {
  pub_files <- list.files(d, pattern = "\\.R$", full.names = TRUE)

  for (f in pub_files) {
    # Source directly into the global env to allow it to access allometric functions
    # including internals which might be required by the S4 object definitions
    env <- new.env()
    # attach allometric internals if needed, but devtools::load_all does it usually.
    # We will just eval the file inside our script environment.
    eval(parse(f), envir = env)

    # Extract the Publication object(s) loaded from this file
    pub_objs <- Filter(function(x) inherits(x, "Publication"), as.list(env))

    if (length(pub_objs) > 0) {
      for (pub_name in names(pub_objs)) {
        pub <- pub_objs[[pub_name]]
        alpha_name <- basename(d)
        tryCatch(
          {
            convert_publication(pub, alpha_dir = alpha_name)
          },
          error = function(e) {
            message("Error converting ", pub@id, " in ", f, ": ", e$message)
          }
        )
      }
    } else {
      message("No Publication object found in ", f)
    }
  }
}
message("Conversion sweep complete.")
