## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options("tibble.print_min" = 5, "tibble.print_max" = 5)
library(magrittr)
library(cohortBuilder)

## -----------------------------------------------------------------------------
cohortBuilder::librarian

## -----------------------------------------------------------------------------
str(as.tblist(librarian), max.level = 1)

## -----------------------------------------------------------------------------
librarian_source <- set_source(
  as.tblist(librarian)
)
class(librarian_source)

## -----------------------------------------------------------------------------
librarian_cohort <- librarian_source %>% 
  cohort()
class(librarian_cohort)

## -----------------------------------------------------------------------------
get_data(librarian_cohort)

## -----------------------------------------------------------------------------
author_filter <- filter(
  "discrete",
  dataset = "books",
  variable = "author",
  value = "Dan Brown"
)

## -----------------------------------------------------------------------------
librarian_cohort <- librarian_cohort %>%
  add_filter(author_filter)

## -----------------------------------------------------------------------------
librarian_cohort <- librarian_cohort %->%
  author_filter

## -----------------------------------------------------------------------------
librarian_cohort <- librarian_source %>% 
  cohort(
    author_filter
  )

## ---- eval = FALSE------------------------------------------------------------
#  librarian_cohort %>%
#    add_filter(author_filter)

## -----------------------------------------------------------------------------
sum_up(librarian_cohort)

## -----------------------------------------------------------------------------
run(librarian_cohort)

## -----------------------------------------------------------------------------
get_data(librarian_cohort)

## -----------------------------------------------------------------------------
librarian_cohort <- librarian_source %>% 
  cohort() %>% 
  add_filter(author_filter, run_flow = TRUE)

## -----------------------------------------------------------------------------
librarian_cohort <- librarian_source %>% 
  cohort(
    author_filter,
    run_flow = TRUE
  )

## -----------------------------------------------------------------------------
get_data(librarian_cohort, state = "pre")

## -----------------------------------------------------------------------------
librarian_cohort <- librarian_source %>% 
  cohort(
    step(
      filter(
        "discrete", id = "author", dataset = "books", 
        variable = "author", value = "Dan Brown"
      ),
      filter(
        "discrete", id = "program", dataset = "borrowers", 
        variable = "program", value = "premium", keep_na = FALSE
      )
    ),
    step(
      filter(
        "range", id = "copies", dataset = "books", 
        variable = "copies", range = c(-Inf, 5)
      )
    )
  )

## -----------------------------------------------------------------------------
sum_up(librarian_cohort)

## -----------------------------------------------------------------------------
run(librarian_cohort)
get_data(librarian_cohort, step_id = 1)
get_data(librarian_cohort, step_id = 2)

## -----------------------------------------------------------------------------
identical(
  get_data(librarian_cohort, step_id = 1, state = "post"),
  get_data(librarian_cohort, step_id = 2, state = "pre")
)

## -----------------------------------------------------------------------------
stat(librarian_cohort, step_id = 1, filter_id = "program")
stat(librarian_cohort, step_id = 2, filter_id = "copies")

## -----------------------------------------------------------------------------
plot_data(librarian_cohort, step_id = 1, filter_id = "program")

## -----------------------------------------------------------------------------
plot_data(librarian_cohort, step_id = 2, filter_id = "copies")

## -----------------------------------------------------------------------------
attrition(librarian_cohort, dataset = "books")

## -----------------------------------------------------------------------------
attrition(librarian_cohort, dataset = "borrowers")

## -----------------------------------------------------------------------------
code(librarian_cohort)

## -----------------------------------------------------------------------------
librarian_source <- set_source(
  as.tblist(librarian),
  source_code = quote({
    source <- list(attributes = list(datasets = librarian))
  })
)

librarian_cohort <- librarian_source %>% 
  cohort(
    step(
      filter(
        "discrete", id = "author", dataset = "books", 
        variable = "author", value = "Dan Brown"
      ),
      filter(
        "discrete", id = "program", dataset = "borrowers", 
        variable = "program", value = "premium", keep_na = FALSE
      )
    ),
    step(
      filter(
        "range", id = "copies", dataset = "books", 
        variable = "copies", range = c(-Inf, 5)
      )
    ),
    run_flow = TRUE
  )

code(librarian_cohort)

## -----------------------------------------------------------------------------
state <- get_state(librarian_cohort, json = TRUE)
state

## -----------------------------------------------------------------------------
librarian_cohort <- librarian_source %>%
  cohort()

restore(librarian_cohort, state = state)

sum_up(librarian_cohort)

