library(yaml)

def_dir <- "c:/Users/bfran/Programming/allometric/inst/variable_defs"
setwd(def_dir)

# Read schema
prefixes <- read.csv("prefix.csv", stringsAsFactors = FALSE, na.strings = c("NA", ""))
suffixes <- read.csv("suffix.csv", stringsAsFactors = FALSE, na.strings = c("NA", ""))
components <- read.csv("components.csv", stringsAsFactors = FALSE, na.strings = c("NA", ""))
measures_meta <- read.csv("measures.csv", stringsAsFactors = FALSE, na.strings = c("NA", ""))

prefixes <- prefixes[!is.na(prefixes$prefix) & prefixes$prefix != "", ]
suffixes <- suffixes[!is.na(suffixes$suffix) & suffixes$suffix != "", ]

schema <- list(
  prefixes = setNames(as.list(prefixes$prefix_description), prefixes$prefix),
  suffixes = setNames(as.list(suffixes$scale_description), suffixes$suffix),
  components = setNames(as.list(components$component_name), components$component)
)

measures_list <- list()

measure_files <- c("a.csv", "b.csv", "d.csv", "e.csv", "g.csv", "h.csv", "n.csv", "r.csv", "t.csv", "v.csv")

for (mfile in measure_files) {
  if (file.exists(mfile)) {
    mdata <- read.csv(mfile, stringsAsFactors = FALSE, na.strings = c("NA", ""))
    mchar <- strsplit(mfile, "\\.")[[1]][1]

    m_name <- measures_meta$measure_name[measures_meta$measure == mchar]

    measure_entry <- list(
      description = m_name,
      variables = lapply(1:nrow(mdata), function(i) {
        row <- mdata[i, ]

        # Add modifiers if present and not empty
        mods <- c()
        if ("modifier_1" %in% names(row) && !is.na(row$modifier_1) && row$modifier_1 != "") {
          mods <- c(mods, row$modifier_1)
        }
        if ("modifier_2" %in% names(row) && !is.na(row$modifier_2) && row$modifier_2 != "") {
          mods <- c(mods, row$modifier_2)
        }

        if (length(mods) > 0) {
          class(mods) <- "inline"
          list(
            component = row$component,
            modifiers = mods,
            description = row$description
          )
        } else {
          list(
            component = row$component,
            description = row$description
          )
        }
      })
    )

    measures_list[[mchar]] <- measure_entry
  }
}

model_types <- read.csv("model_types_defined.csv", stringsAsFactors = FALSE, na.strings = c("NA", ""))
model_types_list <- list()
for (i in 1:nrow(model_types)) {
  model_types_list[[i]] <- list(
    model_type = model_types$model_type[i],
    response_name_start = model_types$response_name_start[i]
  )
}


final_yaml <- list(
  schema = schema,
  model_types = model_types_list,
  measures = measures_list
)

handlers <- list(inline = function(x) {
  paste0("@@[", paste(sprintf("\"%s\"", x), collapse = ", "), "]@@")
})

yaml_txt <- as.yaml(final_yaml, handlers = handlers)
yaml_txt <- gsub("[\047\"]@@(\\[.*?\\])@@[\047\"]", "\\1", yaml_txt)

writeLines(yaml_txt, "c:/Users/bfran/Programming/models/variable_defs.yaml")
cat("Conversion complete. Wrote c:/Users/bfran/Programming/models/variable_defs.yaml\n")
