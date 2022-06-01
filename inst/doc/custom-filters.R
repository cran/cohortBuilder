## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options("tibble.print_min" = 5, "tibble.print_max" = 5)
library(magrittr)
library(dplyr, exclude = "filter")
library(cohortBuilder)

## -----------------------------------------------------------------------------
filter

## -----------------------------------------------------------------------------
cohortBuilder:::filter.discrete

## -----------------------------------------------------------------------------
spec_filter <- filter("discrete", value = "setosa", dataset = "iris", variable = "Species")
spec_filter

## -----------------------------------------------------------------------------
cb_filter.discrete

## -----------------------------------------------------------------------------
iris_source <- set_source(
  tblist(iris = iris)
)
str(
  spec_filter(iris_source),
  give.attr = FALSE
)

## -----------------------------------------------------------------------------
filter.logical <- function(type, id, name, ..., active = getOption("cb_active_filter", default = TRUE)) {
  # Skip missing parameters passed and attach `...`
  args <- append(
    environment() %>% as.list() %>% purrr::keep(~ !is.symbol(.x)),
    list(...)
  )

  # Return function of source parameter calling valid S3 method based on source type
  function(source) {
    do.call(
      cb_filter.logical,
      append(list(source = source), args)
    )
  }
}

## -----------------------------------------------------------------------------
cb_filter.logical <- function(source, ...) {
  UseMethod("cb_filter.logical", source)
}

## -----------------------------------------------------------------------------
cb_filter.logical.tblist <- function(
  source, type = "logical", id = .gen_id(), name = id, dataset, variable, 
  value = NA, keep_na = TRUE, description = NULL, ..., active = TRUE) {
  args <- list(...)

  def_filter(
    type = type,
    id = id,
    name = name,
    input_param = "value",
    filter_data = function(data_object) {

      selected_value <- value # code include
      if (keep_na && !identical(selected_value, NA)) {
        # keep_na !value_na start
        data_object[[dataset]] <- data_object[[dataset]] %>%
          dplyr::filter(!!sym(variable) == !!selected_value | is.na(variable))
        # keep_na !value_na end
      }
      if (!keep_na && identical(selected_value, NA)) {
        # !keep_na value_na start
        data_object[[dataset]] <- data_object[[dataset]] %>%
          dplyr::filter(!is.na(!!sym(variable)))
        # !keep_na value_na end
      }
      if (!keep_na && !identical(selected_value, NA)) {
        # !keep_na !value_na start
        data_object[[dataset]] <- data_object[[dataset]] %>%
          dplyr::filter(!!sym(variable) %in% !!selected_value & !is.na(variable))
        # !keep_na !value_na end
      }
      attr(data_object[[dataset]], "filtered") <- TRUE # code include
      data_object
    },
    get_stats = function(data_object, name) {
      if (missing(name)) {
        name <- c("n_data", "choices", "n_missing")
      }
      stats <- list(
        choices = if ("choices" %in% name) data_object[[dataset]][[variable]] %>% 
          stats::na.omit() %>% table() %>% as.list(),
        n_data = if ("n_data" %in% name)  data_object[[dataset]][[variable]] %>% 
          stats::na.omit() %>% 
          length(),
        n_missing = if ("n_missing" %in% name) data_object[[dataset]][[variable]] %>% is.na() %>% sum()
      )
      if (length(name) == 1) {
        return(stats[[name]])
      } else {
        return(stats[name])
      }
    },
    plot_data = function(data_object) {
      if (nrow(data_object[[dataset]])) {
        data_object[[dataset]][[variable]] %>% table %>% prop.table() %>% graphics::barplot()
      } else {
        graphics::barplot(0, ylim = c(0, 0.1), main = "No data")
      }
    },
    get_params = function(name) {
      params <- list(
        dataset = dataset,
        variable = variable,
        value = value,
        description = description,
        keep_na = keep_na,
        active = active,
        ...
      )
      if (!missing(name)) return(params[[name]])
      return(params)
    },
    get_data = function(data_object) {
      data_object[[dataset]][[variable]]
    },
    get_defaults = function(data_object, cache_object) {
      list(value = names(cache_object$choices))
    }
  )
}

## -----------------------------------------------------------------------------
iris2 <- dplyr::mutate(iris, is_setosa = Species == "setosa")
coh <- set_source(tblist(iris = iris2)) %>%
  cohort(
    filter("logical", dataset = "iris", variable = "is_setosa", value = TRUE)
  ) %>%
  run()

