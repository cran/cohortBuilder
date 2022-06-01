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

## -----------------------------------------------------------------------------
librarian_source <- set_source(
  as.tblist(librarian)
) %->% 
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

## -----------------------------------------------------------------------------
librarian_source <- set_source(
  as.tblist(librarian)
) %>% 
  add_filter(
    filter(
      "discrete", id = "author", dataset = "books", 
      variable = "author", value = "Dan Brown"
    ),
    step_id = 1
  ) %>% 
  add_filter(
    filter(
      "discrete", id = "program", dataset = "borrowers", 
      variable = "program", value = "premium", keep_na = FALSE
    ),
    step_id = 1
  )

## -----------------------------------------------------------------------------
librarian_source <- set_source(
  as.tblist(librarian)
) %->% 
  filter(
    "discrete", id = "author", dataset = "books", 
    variable = "author", value = "Dan Brown"
  ) %->% 
  filter(
    "discrete", id = "program", dataset = "borrowers", 
    variable = "program", value = "premium", keep_na = FALSE
  )

## -----------------------------------------------------------------------------
librarian_cohort <- cohort(librarian_source)
sum_up(librarian_cohort)

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
    )
  )

## -----------------------------------------------------------------------------
librarian_cohort <- librarian_source %>% 
  cohort(
    filter(
      "discrete", id = "author", dataset = "books", 
      variable = "author", value = "Dan Brown"
    ),
    filter(
      "discrete", id = "program", dataset = "borrowers", 
      variable = "program", value = "premium", keep_na = FALSE
    )
  )

## -----------------------------------------------------------------------------
librarian_cohort <- librarian_source %>% cohort()

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
  

## -----------------------------------------------------------------------------
librarian_cohort <- librarian_source %>% cohort()

librarian_cohort %->% 
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

## -----------------------------------------------------------------------------
librarian_cohort <- librarian_source %>% cohort()

librarian_cohort %>% 
  add_filter(
    filter(
      "discrete", id = "author", dataset = "books", 
      variable = "author", value = "Dan Brown"
    )
  ) %>% 
  add_filter(
    filter(
      "discrete", id = "program", dataset = "borrowers", 
      variable = "program", value = "premium", keep_na = FALSE
    )
  )

## -----------------------------------------------------------------------------
librarian_cohort <- librarian_source %>% cohort()

librarian_cohort %->% 
  filter(
    "discrete", id = "author", dataset = "books", 
    variable = "author", value = "Dan Brown"
  ) %->% 
  filter(
    "discrete", id = "program", dataset = "borrowers", 
    variable = "program", value = "premium", keep_na = FALSE
  )

## -----------------------------------------------------------------------------
sum_up(librarian_cohort)

