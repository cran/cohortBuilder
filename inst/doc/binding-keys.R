## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options("tibble.print_min" = 5, "tibble.print_max" = 5)
library(magrittr)
library(cohortBuilder)

## -----------------------------------------------------------------------------
str(librarian)

## -----------------------------------------------------------------------------
librarian_source <- set_source(
  as.tblist(librarian)
)

librarian_cohort <- librarian_source %>% 
  cohort(
    step(
      filter(
        "discrete", id = "title", dataset = "books", 
        variable = "title", value = "Birdsong"
      ),
      filter(
        "date_range", id = "issue_date", dataset = "issues", 
        variable = "date", range = c(as.Date("2016-01-01"), as.Date("2016-12-31"))
      )
    )
  )

## -----------------------------------------------------------------------------
run(librarian_cohort)
selected_isbn <- get_data(librarian_cohort)$books$isbn
librarian_cohort %->% 
  step(
    filter("discrete", id = "isbn", dataset = "issues", variable = "isbn", value = selected_isbn)  
  ) %>% 
  run(step_id = 2)

## -----------------------------------------------------------------------------
selected_borrower_id <- get_data(librarian_cohort)$issues$borrower_id
librarian_cohort %->% 
  step(
    filter("discrete", id = "borr_id", dataset = "borrowers", variable = "id", value = selected_borrower_id)  
  ) %>% 
  run(step_id = 3)


## -----------------------------------------------------------------------------
get_data(librarian_cohort)$borrowers

## -----------------------------------------------------------------------------
issue_books_bk <- bind_key(
  update = data_key(dataset = "issues", key = "isbn"),
  data_key(dataset = "books", key = "isbn")
)

## -----------------------------------------------------------------------------
case_bks <- bind_keys(
  bind_key(
    update = data_key(dataset = "issues", key = "isbn"),
    data_key(dataset = "books", key = "isbn")
  ),
  bind_key(
    update = data_key(dataset = "borrowers", key = "id"),
    data_key(dataset = "issues", key = "borrower_id")
  )
)

## -----------------------------------------------------------------------------
librarian_source <- set_source(
  as.tblist(librarian),
  binding_keys = case_bks
)

librarian_cohort <- librarian_source %>% 
  cohort(
    step(
      filter(
        "discrete", id = "title", dataset = "books", 
        variable = "title", value = "Birdsong"
      ),
      filter(
        "date_range", id = "issue_date", dataset = "issues", 
        variable = "date", range = c(as.Date("2016-01-01"), as.Date("2016-12-31"))
      )
    )
  )

## -----------------------------------------------------------------------------
run(librarian_cohort)
get_data(librarian_cohort)

