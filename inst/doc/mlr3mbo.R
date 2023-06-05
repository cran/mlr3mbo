## ---- include = FALSE---------------------------------------------------------

library(knitr)
opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

update_db = function() {
  if (is.null(db$base) || is.null(db$aliases)) {
    hdb = hsearch_db(package = unique(c(db$index, db$hosted)), types = "help")
    db$base = setkeyv(as.data.table(hdb$Base), "ID")
    db$aliases = setkeyv(as.data.table(hdb$Aliases), "Alias")
  }
}

#' @title Hyperlink to Function Reference
#'
#' @description
#' Creates a markdown link to a function reference.
#'
#' @param topic Name of the topic to link against.
#' @param text Text to use for the link. Defaults to the topic name.
#' @param format Either markdown or HTML.
#'
#' @return (`character(1)`) markdown link.
ref = function(topic, text = topic, format = "markdown") {
  strip_parenthesis = function(x) sub("\\(\\)$", "", x)

  checkmate::assert_string(topic, pattern = "^[[:alnum:]._-]+(::[[:alnum:]._-]+)?(\\(\\))?$")
  checkmate::assert_string(text, min.chars = 1L)
  checkmate::assert_choice(format, c("markdown", "html"))

  topic = trimws(topic)
  text = trimws(text)

  if (stringi::stri_detect_fixed(topic, "::")) {
    parts = strsplit(topic, "::", fixed = TRUE)[[1L]]
    topic = parts[2L]
    name = strip_parenthesis(parts[2L])
    pkg = parts[1L]
  } else {
    update_db()
    matched = db$base[db$aliases[list(strip_parenthesis(topic)), c("Alias", "ID"), on = "Alias", nomatch = 0L], on = "ID", nomatch = NULL]
    if (nrow(matched) == 0L) {
      stop(sprintf("Could not find help page for topic '%s'", topic))
    }
    if (nrow(matched) >= 2L) {
      lgr$warn("Ambiguous link to '%s': %s", topic, paste0(paste(matched$Package, matched$Name, sep = "::"), collapse = " | "))
      matched = head(matched, 1L)
    }

    pkg = matched$Package
    name = matched$Name
    lgr$debug("Resolved '%s' to '%s::%s'", topic, pkg, name)
  }

  if (pkg %in% db$hosted) {
    url = sprintf("https://%s.mlr-org.com/reference/%s.html", pkg, name)
  } else {
    url = sprintf("https://www.rdocumentation.org/packages/%s/topics/%s", pkg, name)
  }

  switch(format,
    "markdown" = sprintf("[`%s`](%s)", text, url),
    "html" = sprintf("<a href=\"%s\">%s</a>", url, text)
  )
}

#' @title Hyperlink to Package
#'
#' @description
#' Links either to respective mlr3 website or to CRAN page.
#'
#' @param pkg Name of the package.
#' @inheritParams ref
#'
#' @return (`character(1)`) markdown link.
#' @export
ref_pkg = function(pkg, format = "markdown") {
  checkmate::assert_string(pkg, pattern = "^[[:alnum:]._-]+$")
  checkmate::assert_choice(format, c("markdown", "html"))
  pkg = trimws(pkg)

  if (grepl("/", pkg, fixed = TRUE)) {
    gh_pkg(pkg, format = format)
  } else if (pkg %in% db$hosted) {
    mlr_pkg(pkg, format = format)
  } else {
    cran_pkg(pkg, format = format)
  }
}

#' @title Hyperlink to CRAN Package
#'
#' @description
#' Creates a markdown link to a CRAN package.
#'
#' @inheritParams ref_pkg
#'
#' @return (`character(1)`) markdown link.
cran_pkg = function(pkg, format = "markdown") {
  checkmate::assert_string(pkg, pattern = "^[[:alnum:]._-]+$")
  checkmate::assert_choice(format, c("markdown", "html"))
  pkg = trimws(pkg)

  if (pkg %in% c("stats", "graphics", "datasets")) {
    return(pkg)
  }
  url = sprintf("https://cran.r-project.org/package=%s", pkg)
  switch(format,
    "markdown" = sprintf("[%s](%s)", pkg, url),
    "html" = sprintf("<a href = \"%s\">%s</a>", url, pkg)
  )
}

#' @title Hyperlink to mlr3 Package
#'
#' @description
#' Creates a markdown link to a mlr3 package with a "mlr-org.com" subdomain.
#'
#' @inheritParams ref_pkg
#'
#' @return (`character(1)`) markdown link.
mlr_pkg = function(pkg, format = "markdown") {
  checkmate::assert_string(pkg, pattern = "^[[:alnum:]._-]+$")
  checkmate::assert_choice(format, c("markdown", "html"))
  pkg = trimws(pkg)

  url = sprintf("https://%1$s.mlr-org.com", pkg)
  switch(format,
    "markdown" = sprintf("[%s](%s)", pkg, url),
    "html" = sprintf("<a href = \"%s\">%s</a>", url, pkg)
  )
}

#' @title Hyperlink to GitHub Repository
#'
#' @description
#' Creates a markdown link to GitHub repository.
#'
#' @param pkg Name of the repository specified as "{repo}/{name}".
#' @inheritParams ref_pkg
#'
#' @return (`character(1)`) markdown link.
gh_pkg = function(pkg, format = "markdown") {
  checkmate::assert_string(pkg, pattern = "^[[:alnum:]_-]+/[[:alnum:]._-]+$")
  checkmate::assert_choice(format, c("markdown", "html"))
  pkg = trimws(pkg)

  parts = strsplit(pkg, "/", fixed = TRUE)[[1L]]
  url = sprintf("https://github.com/%s", pkg)
  switch(format,
    "markdown" = sprintf("[%s](%s)", parts[2L], url),
    "html" = sprintf("<a href = \"%s\">%s</a>", url, parts[2L])
  )
}

db = new.env()
db$index = c("base", "utils", "datasets", "data.table", "stats")
db$hosted = c("paradox", "mlr3misc", "mlr3", "mlr3data", "mlr3db", "mlr3proba", "mlr3pipelines", "mlr3learners", "mlr3filters", "bbotk", "mlr3tuning", "mlr3viz", "mlr3fselect", "mlr3cluster", "mlr3spatiotempcv", "mlr3spatial", "mlr3extralearners", "mlr3tuningspaces", "mlr3hyperband", "mlr3mbo")

lgr = NULL

## -----------------------------------------------------------------------------
library(mlr3mbo)
library(data.table)
as.data.table(mlr_loop_functions)

## -----------------------------------------------------------------------------
library(mlr3learners)
surrogate = SurrogateLearner$new(lrn("regr.km"))

## -----------------------------------------------------------------------------
surrogate = srlrn(lrn("regr.km"))

## -----------------------------------------------------------------------------
surrogate$learner

## -----------------------------------------------------------------------------
surrogate$param_set

## ---- eval=FALSE--------------------------------------------------------------
#  library(mlr3)
#  library(mlr3learners)
#  # there are plenty of more in mlr3extralearners
#  # library(mlr3extralearners)
#  learners = as.data.table(mlr_learners)
#  learners[task_type == "regr"]

## -----------------------------------------------------------------------------
as.data.table(mlr_acqfunctions)

## -----------------------------------------------------------------------------
acq_function = AcqFunctionEI$new()

## -----------------------------------------------------------------------------
acq_function = acqf("ei")

## -----------------------------------------------------------------------------
acqf("cb") # lower / upper confidence bound with lambda hyperparameter

## -----------------------------------------------------------------------------
library(bbotk)
acq_optimizer = AcqOptimizer$new(opt("random_search"), terminator = trm("evals"))

## -----------------------------------------------------------------------------
acq_optimizer = acqo(opt("random_search"), terminator = trm("evals"))

## -----------------------------------------------------------------------------
acq_optimizer$optimizer

## -----------------------------------------------------------------------------
acq_optimizer$terminator

## -----------------------------------------------------------------------------
acq_optimizer$param_set

## ---- eval=FALSE--------------------------------------------------------------
#  as.data.table(mlr_optimizers)

## ---- eval=FALSE--------------------------------------------------------------
#  as.data.table(mlr_terminators)

## -----------------------------------------------------------------------------
optimizer = OptimizerMbo$new(bayesopt_ego,
  surrogate = surrogate,
  acq_function = acq_function,
  acq_optimizer = acq_optimizer)

## -----------------------------------------------------------------------------
optimizer = opt("mbo",
  loop_function = bayesopt_ego,
  surrogate = surrogate,
  acq_function = acq_function,
  acq_optimizer = acq_optimizer)

## -----------------------------------------------------------------------------
optimizer

## -----------------------------------------------------------------------------
as.data.table(mlr_result_assigners)

## -----------------------------------------------------------------------------
result_assigner = ResultAssignerArchive$new()

## -----------------------------------------------------------------------------
result_assigner = ras("archive")

## -----------------------------------------------------------------------------
tuner = TunerMbo$new(bayesopt_ego,
  surrogate = surrogate,
  acq_function = acq_function,
  acq_optimizer = acq_optimizer)

mlr3misc::get_private(tuner)[[".optimizer"]]

## ---- eval = FALSE------------------------------------------------------------
#  repeat {
#    xdt = tryCatch({
#      .
#      .
#      .
#      acq_function$surrogate$update()
#      acq_function$update()
#      acq_optimizer$optimize()
#    }, mbo_error = function(mbo_error_condition) {
#      lg$info(paste0(class(mbo_error_condition), collapse = " / "))
#      lg$info("Proposing a randomly sampled point")
#      SamplerUnif$new(domain)$sample(1L)$data
#    })
#    .
#    .
#    .
#  }

## -----------------------------------------------------------------------------
set.seed(2906)
domain = ps(x = p_dbl(lower = -1, upper = 1))

codomain = ps(y = p_dbl(tags = "minimize"))

objective_function = function(xs) {
  list(y = xs$x ^ 2)
}

objective = ObjectiveRFun$new(
  fun = objective_function,
  domain = domain,
  codomain = codomain)

instance = OptimInstanceSingleCrit$new(
  objective = objective,
  terminator = trm("evals", n_evals = 10))

initial_design = data.table(x = rep(1, 4))
instance$eval_batch(initial_design)

surrogate = srlrn(lrn("regr.km",
  covtype = "matern3_2",
  optim.method = "gen",
  nugget.stability = 10^-8,
  control = list(trace = FALSE)))
acq_function = acqf("ei")
acq_optimizer = acqo(opt("random_search", batch_size = 1000),
  terminator = trm("evals", n_evals = 1000))
optimizer = opt("mbo",
  loop_function = bayesopt_ego,
  surrogate = surrogate,
  acq_function = acq_function,
  acq_optimizer = acq_optimizer)

optimizer$optimize(instance)

## -----------------------------------------------------------------------------
instance$archive$data

## ---- error = TRUE------------------------------------------------------------
instance$archive$clear()
instance$eval_batch(initial_design)
optimizer$surrogate$param_set$values$catch_errors = FALSE
optimizer$optimize(instance)

## -----------------------------------------------------------------------------
bayesopt_custom = function(instance, surrogate, acq_function, acq_optimizer) {
  # typically some assertions

  # initial design handling

  # actual loop function
}

class(bayesopt_custom) = "loop_function"
attr(bayesopt_custom, "id") = "bayesopt_custom"
attr(bayesopt_custom, "label") = "My custom BO loop"
attr(bayesopt_custom, "instance") = "single-crit"
attr(bayesopt_custom, "man") = ""  # no man page
# if you want to add it to the dictionary: mlr_loop_functions$add("bayesopt_custom", bayesopt_custom)

## -----------------------------------------------------------------------------
bayesopt_custom

## ---- eval=FALSE--------------------------------------------------------------
#  objective_function = function(xs) {
#    list(y = 418.9829 * 2 - (sum(unlist(xs) * sin(sqrt(abs(unlist(xs)))))))
#  }
#  domain = ps(x1 = p_dbl(lower = -500, upper = 500),
#    x2 = p_dbl(lower = -500, upper = 500))
#  codomain = ps(y = p_dbl(tags = "minimize"))
#  
#  objective = ObjectiveRFun$new(
#    fun = objective_function,
#    domain = domain,
#    codomain = codomain)
#  
#  instance = OptimInstanceSingleCrit$new(
#    objective = objective,
#    search_space = domain,
#    terminator = trm("evals", n_evals = 60))
#  
#  # Gaussian Process, EI, DIRECT
#  surrogate = srlrn(lrn("regr.km",
#    covtype = "matern3_2",
#    optim.method = "gen",
#    nugget.stability = 10^-8, control = list(trace = FALSE)))
#  acq_function = acqf("ei")
#  acq_optimizer = acqo(opt("nloptr", algorithm = "NLOPT_GN_DIRECT_L"),
#    terminator = trm("stagnation", threshold = 1e-8))
#  optimizer = opt("mbo",
#    loop_function = bayesopt_ego,
#    surrogate = surrogate,
#    acq_function = acq_function,
#    acq_optimizer = acq_optimizer)
#  
#  set.seed(2906)
#  optimizer$optimize(instance)

## ---- eval=FALSE--------------------------------------------------------------
#  library(ggplot2)
#  
#  ggplot(aes(x = batch_nr, y = cummin(y)), data = instance$archive$data) +
#    geom_point() +
#    geom_step() +
#    labs(x = "Batch Nr.", y = "Best y") +
#    theme_minimal()

## ---- eval=FALSE--------------------------------------------------------------
#  xdt = generate_design_grid(instance$search_space, resolution = 101)$data
#  ydt = objective$eval_dt(xdt)
#  ggplot(aes(x = x1, y = x2, z = y), data = cbind(xdt, ydt)) +
#    geom_contour_filled() +
#    geom_point(aes(color = batch_nr), size = 2, data = instance$archive$data) +
#    scale_color_gradient(low = "lightgrey", high = "red") +
#    theme_minimal()

## ---- eval=FALSE--------------------------------------------------------------
#  objective_function = function(xs) {
#    list(y1 = xs$x^2, y2 = (xs$x - 2)^2)
#  }
#  domain = ps(x = p_dbl(lower = -10, upper = 10))
#  codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
#  
#  objective = ObjectiveRFun$new(
#    fun = objective_function,
#    domain = domain,
#    codomain = codomain)
#  
#  instance = OptimInstanceMultiCrit$new(
#    objective = objective,
#    search_space = domain,
#    terminator = trm("evals", n_evals = 30))
#  
#  # Gaussian Process, EI, DIRECT
#  surrogate = srlrn(lrn("regr.km",
#    covtype = "matern3_2",
#    optim.method = "gen",
#    nugget.stability = 10^-8,
#    control = list(trace = FALSE)))
#  acq_function = acqf("ei")
#  acq_optimizer = acqo(opt("nloptr", algorithm = "NLOPT_GN_DIRECT_L"),
#    terminator = trm("stagnation", threshold = 1e-8))
#  optimizer = opt("mbo",
#    loop_function = bayesopt_parego,
#    surrogate = surrogate,
#    acq_function = acq_function,
#    acq_optimizer = acq_optimizer)
#  
#  set.seed(2906)
#  optimizer$optimize(instance)

## ---- eval=FALSE--------------------------------------------------------------
#  ggplot(aes(x = y1, y = y2), data = instance$archive$best()) +
#    geom_point() +
#    theme_minimal()

## ---- eval=FALSE--------------------------------------------------------------
#  library(emoa)
#  library(mlr3misc)
#  library(data.table)
#  anytime_hypervolume = map_dtr(unique(instance$archive$data$batch_nr), function(bnr) {
#    pareto = instance$archive$best(batch = 1:bnr)[, instance$archive$cols_y, with = FALSE]
#    dhv = dominated_hypervolume(t(pareto), ref = t(t(c(100, 144))))
#    data.table(batch_nr = bnr, dhv = dhv)
#  })
#  
#  ggplot(aes(x = batch_nr, y = dhv), data = anytime_hypervolume[batch_nr > 1]) +
#    geom_point() +
#    geom_step(direction = "vh") +
#    labs(x = "Batch Nr.", y = "Dominated Hypervolume") +
#    theme_minimal()

## ---- eval=FALSE--------------------------------------------------------------
#  objective_function = function(xs) {
#    list(y1 = xs$x^2, y2 = (xs$x - 2)^2)
#  }
#  domain = ps(x = p_dbl(lower = -10, upper = 10))
#  codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
#  
#  objective = ObjectiveRFun$new(
#    fun = objective_function,
#    domain = domain,
#    codomain = codomain)
#  
#  instance = OptimInstanceMultiCrit$new(
#    objective = objective,
#    search_space = domain,
#    terminator = trm("evals", n_evals = 30))
#  
#  # Gaussian Processes, SMS-EGO, DIRECT
#  learner_y1 = lrn("regr.km",
#    covtype = "matern3_2",
#    optim.method = "gen",
#    nugget.stability = 10^-8,
#    control = list(trace = FALSE))
#  learner_y2 = learner_y1$clone(deep = TRUE)
#  surrogate = srlrn(list(learner_y1, learner_y2))
#  acq_function = acqf("smsego")
#  acq_optimizer = acqo(opt("nloptr", algorithm = "NLOPT_GN_DIRECT_L"),
#    terminator = trm("stagnation", threshold = 1e-8))
#  optimizer = opt("mbo",
#    loop_function = bayesopt_smsego,
#    surrogate = surrogate,
#    acq_function = acq_function,
#    acq_optimizer = acq_optimizer)
#  
#  set.seed(2906)
#  optimizer$optimize(instance)

## ---- eval=FALSE--------------------------------------------------------------
#  ggplot(aes(x = y1, y = y2), data = instance$archive$best()) +
#    geom_point() +
#    theme_minimal()

## ---- eval=FALSE--------------------------------------------------------------
#  anytime_hypervolume = map_dtr(unique(instance$archive$data$batch_nr), function(bnr) {
#    pareto = instance$archive$best(batch = 1:bnr)[, instance$archive$cols_y, with = FALSE]
#    dhv = dominated_hypervolume(t(pareto), ref = t(t(c(100, 144))))
#    data.table(batch_nr = bnr, dhv = dhv)
#  })
#  
#  ggplot(aes(x = batch_nr, y = dhv), data = anytime_hypervolume[batch_nr > 1]) +
#    geom_point() +
#    geom_step(direction = "vh") +
#    labs(x = "Batch Nr.", y = "Dominated Hypervolume") +
#    theme_minimal()

## ---- eval=FALSE--------------------------------------------------------------
#  library(mlr3)
#  task = tsk("wine")
#  learner = lrn("classif.rpart",
#    cp = to_tune(lower = 1e-4, upper = 1, logscale = TRUE),
#    maxdepth = to_tune(lower = 1, upper = 10),
#    minbucket = to_tune(lower = 1, upper = 10),
#    minsplit = to_tune(lower = 1, upper = 10))
#  resampling = rsmp("cv", folds = 3)
#  measure = msr("classif.acc")
#  
#  instance = TuningInstanceSingleCrit$new(
#    task = task,
#    learner = learner,
#    resampling = resampling,
#    measure = measure,
#    terminator = trm("evals", n_evals = 30))
#  
#  # Gaussian Process, EI, FocusSearch
#  surrogate = srlrn(lrn("regr.km",
#    covtype = "matern3_2",
#    optim.method = "gen",
#    nugget.estim = TRUE,
#    jitter = 1e-12,
#    control = list(trace = FALSE)))
#  acq_function = acqf("ei")
#  acq_optimizer = acqo(opt("focus_search", n_points = 100L, maxit = 9),
#    terminator = trm("evals", n_evals = 3000))
#  tuner = tnr("mbo",
#    loop_function = bayesopt_ego,
#    surrogate = surrogate,
#    acq_function = acq_function,
#    acq_optimizer = acq_optimizer)
#  
#  set.seed(2906)
#  tuner$optimize(instance)
#  instance$result

## ---- eval=FALSE--------------------------------------------------------------
#  task = tsk("wine")
#  learner = lrn("classif.rpart",
#    cp = to_tune(lower = 1e-4, upper = 1, logscale = TRUE),
#    maxdepth = to_tune(lower = 1, upper = 10),
#    minbucket = to_tune(lower = 1, upper = 10),
#    minsplit = to_tune(lower = 1, upper = 10))
#  resampling = rsmp("cv", folds = 3)
#  measures = msrs(c("classif.acc", "selected_features"))
#  
#  instance = TuningInstanceMultiCrit$new(
#    task = task,
#    learner = learner,
#    resampling = resampling,
#    measures = measures,
#    terminator = trm("evals", n_evals = 30),
#    store_models = TRUE) # required due to selected features
#  
#  # Gaussian Process, EI, FocusSearch
#  surrogate = srlrn(lrn("regr.km",
#    covtype = "matern3_2",
#    optim.method = "gen",
#    nugget.estim = TRUE,
#    jitter = 1e-12,
#    control = list(trace = FALSE)))
#  acq_function = acqf("ei")
#  acq_optimizer = acqo(opt("focus_search", n_points = 100L, maxit = 9),
#    terminator = trm("evals", n_evals = 3000))
#  tuner = tnr("mbo",
#    loop_function = bayesopt_parego,
#    surrogate = surrogate,
#    acq_function = acq_function,
#    acq_optimizer = acq_optimizer)
#  
#  set.seed(2906)
#  tuner$optimize(instance)
#  instance$result

