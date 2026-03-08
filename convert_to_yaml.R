library(devtools)
library(yaml)
devtools::load_all("../allometric")
devtools::load_all("../models")

extract_meta <- function(pub) {
  cit <- pub@citation

  authors_list <- if (!is.null(cit$author)) {
    as.character(cit$author)
  } else {
    list()
  }

  list(
    id = pub@id,
    source = list(
      doi = if (!is.null(cit$doi)) cit$doi else NULL,
      authors = authors_list,
      year = if (!is.null(cit$year)) as.integer(cit$year) else NULL,
      journal = if (!is.null(cit$journal)) cit$journal else if (!is.null(cit$institution)) cit$institution else NULL,
      title = if (!is.null(cit$title)) cit$title else NULL
    )
  )
}

get_units <- function(x) {
  u <- attr(x, "units")
  if (!is.null(u)) {
    return(as.character(u))
  }
  return(NA)
}

extract_taxa <- function(taxa_obj) {
  if (inherits(taxa_obj, "Taxa") && length(taxa_obj@.Data) > 0) {
    taxon <- taxa_obj@.Data[[1]]
    return(list(
      family = if (.hasSlot(taxon, "family")) taxon@family else NA,
      genus = if (.hasSlot(taxon, "genus")) taxon@genus else NA,
      species = if (.hasSlot(taxon, "species")) taxon@species else NA
    ))
  }
  return(list())
}

extract_model_set <- function(model_set, response_key, pub_id) {
  first_model <- model_set@models[[1]]

  inputs <- lapply(names(first_model@covariates), function(n) {
    list(name = n, unit = get_units(first_model@covariates[[n]]))
  })

  outputs <- lapply(names(first_model@response), function(n) {
    list(name = n, unit = get_units(first_model@response[[n]]))
  })

  all_model_defs <- list()
  for (model in model_set@models) {
    spec <- model@specification

    # Calculate unique ID uses first 8 characters
    mod_id <- substr(models:::get_model_hash(model@predict_fn_populated, model@descriptors), 1, 8)

    mod_def_items <- list()
    for (col in names(spec)) {
      if (col == "taxa") {
        mod_def_items <- c(mod_def_items, extract_taxa(spec[[col]][[1]]))
      } else {
        mod_def_items[[col]] <- spec[[col]][[1]]
      }
    }

    # Prepend id to the properties
    mod_def <- c(list(id = mod_id), mod_def_items)
    all_model_defs <- append(all_model_defs, list(mod_def))
  }

  predict_fn_str <- paste(deparse(model_set@predict_fn), collapse = "\n")

  list(
    pub_id = pub_id,
    response = response_key,
    input = inputs,
    output = outputs,
    predict_fn = predict_fn_str,
    model_definitions = all_model_defs
  )
}

format_flow_yaml <- function(model_defs) {
  # Format list elements into inline JSON-like strings
  lines <- sapply(model_defs, function(m) {
    # Remove NA values safely (handling vectors)
    m_clean <- Filter(function(x) !(is.atomic(x) && length(x) == 1 && is.na(x)), m)
    json_str <- jsonlite::toJSON(as.list(m_clean), auto_unbox = TRUE)
    # Convert JSON format to more YAML-y (optional but auto_unbox does most)
    # jsonlite gives {"a":1,"b":2}, which is valid YAML flow style
    paste0("- ", json_str)
  })
  paste(lines, collapse = "\n")
}

convert_publication <- function(pub, out_dir = "yaml_models", alpha_dir = NULL) {
  if (!is.null(alpha_dir)) {
    pub_dir <- file.path(out_dir, alpha_dir, pub@id)
  } else {
    pub_dir <- file.path(out_dir, pub@id)
  }

  if (!dir.exists(pub_dir)) dir.create(pub_dir, recursive = TRUE)

  # 1. Write _meta.yaml
  meta_list <- extract_meta(pub)
  yaml::write_yaml(meta_list, file.path(pub_dir, "_meta.yaml"))

  # 2. Iterate through response sets
  for (resp_key in names(pub@response_sets)) {
    resp_set_list <- pub@response_sets[[resp_key]]

    for (i in seq_along(resp_set_list)) {
      model_set <- resp_set_list[[i]]
      resp_list <- extract_model_set(model_set, resp_key, pub@id)

      # Separate the definitions to format them inline
      model_defs <- resp_list$model_definitions
      resp_list$model_definitions <- NULL

      inputs <- resp_list$input
      resp_list$input <- NULL

      outputs <- resp_list$output
      resp_list$output <- NULL

      # pull out predict_fn to append it as a block formatted string
      predict_fn_str <- resp_list$predict_fn
      resp_list$predict_fn <- NULL

      yaml_file <- file.path(pub_dir, paste0(resp_key, "_", i, ".yaml"))

      # Write the base structure
      yaml::write_yaml(resp_list, yaml_file)

      # Append inputs
      cat("input:\n", file = yaml_file, append = TRUE)
      cat(format_flow_yaml(inputs), file = yaml_file, append = TRUE)
      cat("\n", file = yaml_file, append = TRUE)

      # Append outputs
      cat("output:\n", file = yaml_file, append = TRUE)
      cat(format_flow_yaml(outputs), file = yaml_file, append = TRUE)
      cat("\n", file = yaml_file, append = TRUE)

      # Append predict_fn
      cat("predict_fn: |\n", file = yaml_file, append = TRUE)
      predict_lines <- strsplit(predict_fn_str, "\n")[[1]]
      for (pl in predict_lines) {
        cat(paste0("  ", pl, "\n"), file = yaml_file, append = TRUE)
      }

      # Append the inline structured model_definitions
      cat("model_definitions:\n", file = yaml_file, append = TRUE)
      cat(format_flow_yaml(model_defs), file = yaml_file, append = TRUE)
      cat("\n", file = yaml_file, append = TRUE)
    }
  }

  cat(paste("Successfully converted", pub@id, "to", pub_dir, "\n"))
}
