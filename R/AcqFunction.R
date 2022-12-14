#' @title Acquisition Function Base Class
#'
#' @include mlr_acqfunctions.R
#'
#' @description
#' Abstract acquisition function class.
#'
#' Based on the predictions of a [Surrogate], the acquisition function encodes the preference to evaluate a new point.
#'
#' @family Acquisition Function
#' @export
AcqFunction = R6Class("AcqFunction",
  inherit = bbotk::Objective,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' Note that the surrogate can be initialized lazy and can later be set via the active binding `$surrogate`.
    #'
    #' @param id (`character(1)`).
    #' @param constants ([paradox::ParamSet]).
    #'   Changeable constants or parameters.
    #' @param surrogate (`NULL` | [Surrogate]).
    #'   Surrogate whose predictions are used in the acquisition function.
    #' @param direction (`"same"` | `"minimize"` | `"maximize"`).
    #'   Optimization direction of the acquisition function relative to the direction of the
    #'   objective function of the [bbotk::OptimInstance].
    #'   Must be `"same"`, `"minimize"`, or `"maximize"`.
    #' @param label (`character(1)`)\cr
    #'   Label for this object.
    #' @param man (`character(1)`)\cr
    #'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
    initialize = function(id, constants = ParamSet$new(), surrogate, direction, label = NA_character_, man = NA_character_) {
      # FIXME: Should we allow alternative search_space as additional argument?
      # If we do, we need to trafo values before updating the surrogate and predicting?
      assert_string(id)
      assert_r6(surrogate, classes = "Surrogate", null.ok = TRUE)
      self$direction = assert_choice(direction, c("same", "minimize", "maximize"))
      if (is.null(surrogate)) {
        domain = ParamSet$new()
        codomain = ParamSet$new()
      } else {
        private$.surrogate = surrogate
        private$.archive = assert_r6(surrogate$archive, classes = "Archive")
        codomain = generate_acq_codomain(surrogate$archive$codomain, id = id, direction = direction)
        self$surrogate_max_to_min = surrogate_mult_max_to_min(surrogate$archive$codomain, y_cols = surrogate$y_cols)
        domain = surrogate$archive$search_space$clone(deep = TRUE)
        domain$trafo = NULL
      }
      private$.label = assert_string(label, na.ok = TRUE)
      private$.man = assert_string(man, na.ok = TRUE)
      super$initialize(id = id, domain = domain, codomain = codomain, constants = constants)
    },

    #' @description
    #' Update the acquisition function.
    #'
    #' Can be implemented by subclasses.
    update = function() {
      # FIXME: at some point we may want to make this an AB to a private$.update
    },

    #' @description
    #' Evaluates multiple input values on the objective function.
    #'
    #' @param xss (`list()`)\cr
    #'   A list of lists that contains multiple x values, e.g.
    #'   `list(list(x1 = 1, x2 = 2), list(x1 = 3, x2 = 4))`.
    #'
    #' @return data.table::data.table() that contains one y-column for
    #' single-objective functions and multiple y-columns for multi-objective functions,
    #' e.g. `data.table(y = 1:2)` or `data.table(y1 = 1:2, y2 = 3:4)`.
    eval_many = function(xss) {
      if (self$check_values) lapply(xss, self$domain$assert)
      res = invoke(private$.fun, rbindlist(xss, use.names = TRUE, fill = TRUE), .args = self$constants$values)
      if (self$check_values) self$codomain$assert_dt(res[, self$codomain$ids(), with = FALSE])
      res
    },

    #' @description
    #' Evaluates multiple input values on the objective function
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   One point per row, e.g. `data.table(x1 = c(1, 3), x2 = c(2, 4))`.
    #'
    #' @return data.table::data.table() that contains one y-column for
    #' single-objective functions and multiple y-columns for multi-objective
    #' functions, e.g. `data.table(y = 1:2)` or `data.table(y1 = 1:2, y2 = 3:4)`.
    eval_dt = function(xdt) {
      if (self$check_values) self$domain$assert_dt(xdt)
      res = invoke(private$.fun, xdt, .args = self$constants$values)
      if (self$check_values) self$codomain$assert_dt(res[, self$codomain$ids(), with = FALSE])
      res
    }
  ),

  active = list(
    #' @field direction (`"same"` | `"minimize"` | `"maximize"`)\cr
    #'   Optimization direction of the acquisition function relative to the direction of the
    #'   objective function of the [bbotk::OptimInstance].
    #'   Must be `"same"`, `"minimize"`, or `"maximize"`.
    direction = function(rhs) {
      if (missing(rhs)) {
        private$.direction
      } else {
        private$.direction = assert_choice(rhs, choices = c("same", "minimize", "maximize"))
      }
    },

    #' @field surrogate_max_to_min (`-1` | `1`)\cr
    #'   Multiplicative factor to correct for minimization or maximization of the acquisition
    #'   function.
    surrogate_max_to_min = function(rhs) {
     if (missing(rhs)) {
        private$.surrogate_max_to_min
      } else {
        private$.surrogate_max_to_min = assert_subset(rhs, choices = c(-1L, 1L))
      }
    },

    #' @field label (`character(1)`)\cr
    #'   Label for this object.
    label = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.label)) {
        stop("$label is read-only.")
      }
      private$.label
    },

    #' @field man (`character(1)`)\cr
    #'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
    man = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.man)) {
        stop("$man is read-only.")
      }
      private$.man
    },

    #' @field archive ([bbotk::Archive])\cr
    #'   Points to the [bbotk::Archive] of the surrogate.
    archive = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.archive)) stop("$archive is read-only.")
      private$.archive
    },

    #' @field fun (`function`)\cr
    #'   Pointing to the private acquistion function to be implemented by subclasses.
    fun = function(lhs) {
      if (!missing(lhs) && !identical(lhs, private$.fun)) stop("$fun is read-only.")
      private$.fun
    },

    #' @field surrogate ([Surrogate])\cr
    #'  Surrogate.
    surrogate = function(rhs) {
      if (missing(rhs)) {
        private$.surrogate
      } else {
        private$.surrogate = assert_r6(rhs, classes = "Surrogate")
        private$.archive = assert_r6(rhs$archive, classes = "Archive")
        codomain = generate_acq_codomain(rhs$archive$codomain, id = self$id, direction = self$direction)
        self$surrogate_max_to_min = surrogate_mult_max_to_min(rhs$archive$codomain, y_cols = rhs$y_cols)
        domain = rhs$archive$search_space$clone(deep = TRUE)
        domain$trafo = NULL
        self$codomain = Codomain$new(codomain$params)  # lazy initialization requires this
        self$domain = domain
      }
    }
  ),

  private = list(
    .direction = NULL,

    .surrogate_max_to_min = NULL,

    .label = NULL,

    .man = NULL,

    .archive = NULL,

    .fun = function(xdt) {
      stop("Abstract.")
    },

    .surrogate = NULL
  )
)

