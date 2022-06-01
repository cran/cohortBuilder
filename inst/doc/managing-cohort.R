## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options("tibble.print_min" = 5, "tibble.print_max" = 5)
library(magrittr)
library(cohortBuilder)

## -----------------------------------------------------------------------------
librarian_source <- set_source(
  as.tblist(librarian)
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

## -----------------------------------------------------------------------------
librarian_cohort %>% 
  update_filter(
    step_id = 1, filter_id = "author", value = c("Dan Brown", "Khaled Hosseini")
  )

sum_up(librarian_cohort)

## -----------------------------------------------------------------------------
librarian_cohort %>% 
  add_filter(
    filter(
      "date_range", id = "issue_date", dataset = "issues", 
      variable = "date", range = c(as.Date("2010-01-01"), Inf)
    ),
    step_id = 2
  )

sum_up(librarian_cohort)

## -----------------------------------------------------------------------------
librarian_cohort %>% 
  rm_filter(step_id = 2, filter_id = "copies")

sum_up(librarian_cohort)

## -----------------------------------------------------------------------------
run(librarian_cohort, min_step_id = 2)

get_data(librarian_cohort)

## -----------------------------------------------------------------------------
librarian_cohort %>% 
  rm_step(step_id = 1)

sum_up(librarian_cohort)

## -----------------------------------------------------------------------------
librarian_cohort %>% 
  add_step(
    step(
      filter(
        "discrete", id = "author", dataset = "books", 
        variable = "author", value = "Dan Brown"
      ),
      filter(
        "discrete", id = "program", dataset = "borrowers", 
        variable = "program", value = "premium", keep_na = FALSE
      )
    )
  )

sum_up(librarian_cohort)

## -----------------------------------------------------------------------------
code(librarian_cohort)

new_source <- set_source(
  as.tblist(librarian),
  source_code = quote({
    source <- list(attributes = list(datasets = librarian))
  })
)

update_source(librarian_cohort, new_source)
code(librarian_cohort)
sum_up(librarian_cohort)
code(librarian_cohort)

## -----------------------------------------------------------------------------
update_source(librarian_cohort, new_source, keep_steps = FALSE)
sum_up(librarian_cohort)

## -----------------------------------------------------------------------------
empty_cohort <- cohort()
update_source(librarian_cohort, new_source)
sum_up(empty_cohort)

## -----------------------------------------------------------------------------
source_one <- set_source(
  as.tblist(librarian)
) %>% 
  add_step(
    step(
      filter(
        "discrete", id = "author", dataset = "books", 
        variable = "author", value = "Dan Brown"
      ),
      filter(
        "discrete", id = "program", dataset = "borrowers", 
        variable = "program", value = "premium", keep_na = FALSE
      )
    )
  )

source_two <- set_source(
  as.tblist(librarian)
) %>% 
  add_step(
    step(
      filter(
        "range", id = "copies", dataset = "books", 
        variable = "copies", range = c(-Inf, 5)
      )
    )
  )

my_cohort <- cohort(source_one)
sum_up(my_cohort)

update_source(my_cohort, source_two)
sum_up(my_cohort)

