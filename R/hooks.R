#' Cohort hooks.
#'
#' In order to make integration of `cohortBuilder` package with other layers/packages easier,
#' hooks system was introduced.
#'
#' Many \link{Cohort} methods allow to define `hook` parameter.
#' For such method, `hook` is a list containing two values: `pre` and `post`,
#' storing functions (hooks) executed before and after the method is run respectively.
#'
#' Each `hook` is a function of two obligatory parameters:
#' \itemize{
#'   \item{\code{public} - Cohort object.}
#'   \item{\code{private} - Private environment of Cohort object.}
#' }
#'
#' When Cohort method, for which hook is defined, allow to pass custom parameters,
#' the ones should be also available in hook definition (with some exclusions, see below).
#'
#' For example `Cohort$remove_step` has three parameters:
#' \itemize{
#'   \item{\code{step_id}}
#'   \item{\code{run_flow}}
#'   \item{\code{hook}}
#' }
#' By the implementation, the parameters that we should skip are `run_flow` and `hook`,
#' so the hook should have three parameters `public`, `private` and `step_id`.
#'
#' There are two ways of defining hooks for the specific method.
#' The first one is to define the method `hook` directly as its parameter (while calling the method).
#'
#' The second option can be achieved with usage of `add_hook` (and `get_hook`) function.
#' The default `hook` parameter for each method is constructed as below:
#' \preformatted{
#' remove_step = function(step_id, run_flow = FALSE,
#'   hook = list(
#'     pre = get_hook("pre_rm_step_hook"),
#'     post = get_hook("post_rm_step_hook")
#'   )
#' )
#' }
#'
#' 'Pre' hooks are defined with 'pre_<method_name>_hook' and 'Post' ones as 'post_<method_name>_hook'.
#' As a result calling:
#' \preformatted{
#' add_hook(
#'   "pre_remove_step_hook",
#'   function(public, private, step_id) {...}
#' )
#' }
#' will result with specifying a new pre-hook for `remove_step` method.
#'
#' You may add as many hooks as you want.
#' The order of hooks execution is followed by the order or registering process.
#' If you want to check currently registered hooks for the specific method, just use:
#' \preformatted{
#' get_hook("pre_<method_name>_hook")
#' }
#'
#' @name hooks

#' @rdname hooks
#' @param name Name of the hook. See Details section.
#' @param method Function to be assigned as hook.
#' @returns No returned value (`add_hook`) or the list of functions (`get_hook`).
#'
#' @export
add_hook <- function(name, method) {

  hooks <- getOption(name)
  if (!is.null(hooks)) {
    for (hook in hooks) {
      if (isTRUE(all.equal(method, hook, check.environment = FALSE))) {
        warning("hook already exists")
        return(invisible(FALSE))
      }
    }
  }

  do.call(
    options,
    stats::setNames(
      list(
        append(
          getOption(name),
          list(method)
        )
      ),
      name
    )
  )
}

#' @rdname hooks
#' @export
get_hook <- function(name) {
  getOption(name, default = function(...) {})
}

run_hooks <- function(hooks, ...) {
  if (is.function(hooks)) {
    return(hooks(...))
  }
  for (hook in hooks) {
    hook(...)
  }
}
