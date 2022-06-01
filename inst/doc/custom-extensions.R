## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options("tibble.print_min" = 5, "tibble.print_max" = 5)
library(magrittr)
library(cohortBuilder)

## ---- eval = FALSE------------------------------------------------------------
#  set_source.tblist <- function(dtconn, primary_keys = NULL, binding_keys = NULL,
#                             source_code = NULL, description = NULL, ...) {
#    Source$new(
#      dtconn, primary_keys = primary_keys,
#      binding_keys = binding_keys, source_code = source_code,
#      ...
#    )
#  }

## ---- eval = FALSE------------------------------------------------------------
#  .init_step.tblist <- function(source, ...) {
#    source$dtconn
#  }

## ---- eval = FALSE------------------------------------------------------------
#  .init_step.db <- function(source) {
#    purrr::map(
#      stats::setNames(source$dtconn$tables, source$dtconn$tables),
#      function(table) {
#        tbl_conn <- dplyr::tbl(
#          source$dtconn$connection,
#          dbplyr::in_schema(source$dtconn$schema, table)
#        )
#        attr(tbl_conn, "tbl_name") <- table
#        tbl_conn
#      }
#    )
#  }

## ---- eval = FALSE------------------------------------------------------------
#  .pre_filtering.tblist <- function(source, data_object, step_id) {
#    for (dataset in names(data_object)) {
#      attr(data_object[[dataset]], "filtered") <- FALSE
#    }
#    return(data_object)
#  }

## ---- eval = FALSE------------------------------------------------------------
#  .pre_filtering.db <- function(source, data_object, step_id) {
#    purrr::map(
#      stats::setNames(source$dtconn$tables, source$dtconn$tables),
#      function(table) {
#        table_name <- tmp_table_name(attr(data_object[[table]], "tbl_name"), step_id)
#        DBI::dbRemoveTable(source$dtconn$connection, table_name, temporary = TRUE, fail_if_missing = FALSE)
#        data_object[[table]] <- dplyr::compute(
#          data_object[[table]],
#          name = table_name
#        )
#        attr(data_object[[table]], "filtered") <- FALSE
#        return(data_object[[table]])
#      }
#    )
#  }

## ---- eval = FALSE------------------------------------------------------------
#  .collect_data.tblist <- function(source, data_object) {
#    data_object
#  }

## ---- eval = FALSE------------------------------------------------------------
#  .collect_data.db <- function(source, data_object) {
#    purrr::map(
#      stats::setNames(source$dtconn$tables, source$dtconn$tables),
#      ~dplyr::collect(data_object[[.x]])
#    )
#  }

## ---- eval = FALSE------------------------------------------------------------
#  .get_stats.tblist <- function(source, data_object) {
#    dataset_names <- names(source$dtconn)
#    dataset_names %>%
#      purrr::map(
#        ~ list(n_rows = nrow(data_object[[.x]]))
#      ) %>%
#      stats::setNames(dataset_names)
#  }

## ---- eval = FALSE------------------------------------------------------------
#  .get_stats.db <- function(source, data_object) {
#    dataset_names <- source$dtconn$tables
#    dataset_names %>%
#      purrr::map(
#        ~ list(
#          n_rows = data_object[[.x]] %>%
#            dplyr::summarise(n = n()) %>%
#            dplyr::collect() %>%
#            dplyr::pull(n) %>%
#            as.integer()
#        )
#      ) %>%
#      stats::setNames(dataset_names)
#  }

## ---- eval = FALSE------------------------------------------------------------
#  .run_binding.tblist <- function(source, binding_key, data_object_pre, data_object_post, ...) {
#    binding_dataset <- binding_key$update$dataset
#    dependent_datasets <- names(binding_key$data_keys)
#    active_datasets <- data_object_post %>%
#      purrr::keep(~ attr(., "filtered")) %>%
#      names()
#  
#    if (!any(dependent_datasets %in% active_datasets)) {
#      return(data_object_post)
#    }
#  
#    key_values <- NULL
#    common_key_names <- paste0("key_", seq_along(binding_key$data_keys[[1]]$key))
#    for (dependent_dataset in dependent_datasets) {
#      key_names <- binding_key$data_keys[[dependent_dataset]]$key
#      tmp_key_values <- dplyr::distinct(data_object_post[[dependent_dataset]][, key_names, drop = FALSE]) %>%
#        stats::setNames(common_key_names)
#      if (is.null(key_values)) {
#        key_values <- tmp_key_values
#      } else {
#        key_values <- dplyr::inner_join(key_values, tmp_key_values, by = common_key_names)
#      }
#    }
#  
#    data_object_post[[binding_dataset]] <- dplyr::inner_join(
#      switch(
#        as.character(binding_key$post),
#        "FALSE" = data_object_pre[[binding_dataset]],
#        "TRUE" = data_object_post[[binding_dataset]]
#      ),
#      key_values,
#      by = stats::setNames(common_key_names, binding_key$update$key)
#    )
#    if (binding_key$activate) {
#      attr(data_object_post[[binding_dataset]], "filtered") <- TRUE
#    }
#  
#    return(data_object_post)
#  }

## ---- eval = FALSE------------------------------------------------------------
#  .get_attrition_count.tblist <- function(source, data_stats, dataset, ...) {
#    data_stats %>%
#      purrr::map_int(~.[[dataset]][["n_rows"]])
#  }

## ---- eval = FALSE------------------------------------------------------------
#  get_attrition_label.tblist <- function(source, step_id, step_filters, dataset, ...) {
#    pkey <- source$primary_keys
#    binding_keys <- source$binding_keys
#    if (step_id == "0") {
#      if (is.null(pkey)) {
#        return(dataset)
#      } else {
#        dataset_pkey <- .get_item(pkey, "dataset", dataset)[1][[1]]$key
#        if (is.null(dataset_pkey)) return(dataset)
#        return(glue::glue("{dataset}\n primary key: {paste(dataset_pkey, collapse = ', ')}"))
#      }
#    }
#    filters_section <- step_filters %>%
#      purrr::keep(~.$dataset == dataset) %>%
#      purrr::map(~get_attrition_filter_label(.$name, .$value_name, .$value)) %>%
#      paste(collapse = "\n")
#    bind_keys_section <- ""
#    if (!is.null(binding_keys)) {
#      dependent_datasets <- .get_item(
#        binding_keys, attribute = "update", value = dataset,
#        operator = function(value, target) {
#          value == target$dataset
#        }
#      ) %>%
#        purrr::map(~names(.[["data_keys"]])) %>%
#        unlist() %>%
#        unique()
#      if (length(dependent_datasets) > 0) {
#        bind_keys_section <- glue::glue(
#          "\nData linked with external datasets: {paste(dependent_datasets, collapse = ', ')}",
#          .trim = FALSE
#        )
#      }
#    }
#    gsub(
#      "\n$",
#      "",
#      glue::glue("Step: {step_id}\n{filters_section}{bind_keys_section}")
#    )
#  }

