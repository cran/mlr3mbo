% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AcqFunction.R
\name{AcqFunction}
\alias{AcqFunction}
\title{Acquisition Function Base Class}
\description{
Abstract acquisition function class.

Based on the predictions of a \link{Surrogate}, the acquisition function encodes the preference to evaluate a new point.
}
\seealso{
Other Acquisition Function: 
\code{\link{mlr_acqfunctions}},
\code{\link{mlr_acqfunctions_aei}},
\code{\link{mlr_acqfunctions_cb}},
\code{\link{mlr_acqfunctions_ehvi}},
\code{\link{mlr_acqfunctions_ehvigh}},
\code{\link{mlr_acqfunctions_ei}},
\code{\link{mlr_acqfunctions_ei_log}},
\code{\link{mlr_acqfunctions_eips}},
\code{\link{mlr_acqfunctions_mean}},
\code{\link{mlr_acqfunctions_multi}},
\code{\link{mlr_acqfunctions_pi}},
\code{\link{mlr_acqfunctions_sd}},
\code{\link{mlr_acqfunctions_smsego}},
\code{\link{mlr_acqfunctions_stochastic_cb}},
\code{\link{mlr_acqfunctions_stochastic_ei}}
}
\concept{Acquisition Function}
\section{Super class}{
\code{\link[bbotk:Objective]{bbotk::Objective}} -> \code{AcqFunction}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{direction}}{(\code{"same"} | \code{"minimize"} | \code{"maximize"})\cr
Optimization direction of the acquisition function relative to the direction of the
objective function of the \link[bbotk:OptimInstance]{bbotk::OptimInstance} related to the passed \link[bbotk:Archive]{bbotk::Archive}.
Must be \code{"same"}, \code{"minimize"}, or \code{"maximize"}.}

\item{\code{surrogate_max_to_min}}{(\code{-1} | \code{1})\cr
Multiplicative factor to correct for minimization or maximization of the acquisition
function.}

\item{\code{label}}{(\code{character(1)})\cr
Label for this object.}

\item{\code{man}}{(\code{character(1)})\cr
String in the format \verb{[pkg]::[topic]} pointing to a manual page for this object.}

\item{\code{archive}}{(\link[bbotk:Archive]{bbotk::Archive})\cr
Points to the \link[bbotk:Archive]{bbotk::Archive} of the surrogate.}

\item{\code{fun}}{(\code{function})\cr
Points to the private acquisition function to be implemented by subclasses.}

\item{\code{surrogate}}{(\link{Surrogate})\cr
Surrogate.}

\item{\code{requires_predict_type_se}}{(\code{logical(1)})\cr
Whether the acquisition function requires the surrogate to have \code{"se"} as \verb{$predict_type}.}

\item{\code{packages}}{(\code{character()})\cr
Set of required packages.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-AcqFunction-new}{\code{AcqFunction$new()}}
\item \href{#method-AcqFunction-update}{\code{AcqFunction$update()}}
\item \href{#method-AcqFunction-reset}{\code{AcqFunction$reset()}}
\item \href{#method-AcqFunction-eval_many}{\code{AcqFunction$eval_many()}}
\item \href{#method-AcqFunction-eval_dt}{\code{AcqFunction$eval_dt()}}
\item \href{#method-AcqFunction-clone}{\code{AcqFunction$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="bbotk" data-topic="Objective" data-id="eval"><a href='../../bbotk/html/Objective.html#method-Objective-eval'><code>bbotk::Objective$eval()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bbotk" data-topic="Objective" data-id="format"><a href='../../bbotk/html/Objective.html#method-Objective-format'><code>bbotk::Objective$format()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bbotk" data-topic="Objective" data-id="help"><a href='../../bbotk/html/Objective.html#method-Objective-help'><code>bbotk::Objective$help()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bbotk" data-topic="Objective" data-id="print"><a href='../../bbotk/html/Objective.html#method-Objective-print'><code>bbotk::Objective$print()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AcqFunction-new"></a>}}
\if{latex}{\out{\hypertarget{method-AcqFunction-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.

Note that the surrogate can be initialized lazy and can later be set via the active binding \verb{$surrogate}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AcqFunction$new(
  id,
  constants = ParamSet$new(),
  surrogate,
  requires_predict_type_se,
  direction,
  packages = NULL,
  label = NA_character_,
  man = NA_character_
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{(\code{character(1)}).}

\item{\code{constants}}{(\link[paradox:ParamSet]{paradox::ParamSet}).
Changeable constants or parameters.}

\item{\code{surrogate}}{(\code{NULL} | \link{Surrogate}).
Surrogate whose predictions are used in the acquisition function.}

\item{\code{requires_predict_type_se}}{(\code{logical(1)})\cr
Whether the acquisition function requires the surrogate to have \code{"se"} as \verb{$predict_type}.}

\item{\code{direction}}{(\code{"same"} | \code{"minimize"} | \code{"maximize"}).
Optimization direction of the acquisition function relative to the direction of the
objective function of the \link[bbotk:OptimInstance]{bbotk::OptimInstance}.
Must be \code{"same"}, \code{"minimize"}, or \code{"maximize"}.}

\item{\code{packages}}{(\code{character()})\cr
Set of required packages.
A warning is signaled prior to construction if at least one of the packages is not installed, but loaded (not attached) later on-demand via \code{\link[=requireNamespace]{requireNamespace()}}.}

\item{\code{label}}{(\code{character(1)})\cr
Label for this object.}

\item{\code{man}}{(\code{character(1)})\cr
String in the format \verb{[pkg]::[topic]} pointing to a manual page for this object.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AcqFunction-update"></a>}}
\if{latex}{\out{\hypertarget{method-AcqFunction-update}{}}}
\subsection{Method \code{update()}}{
Update the acquisition function.

Can be implemented by subclasses.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AcqFunction$update()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AcqFunction-reset"></a>}}
\if{latex}{\out{\hypertarget{method-AcqFunction-reset}{}}}
\subsection{Method \code{reset()}}{
Reset the acquisition function.

Can be implemented by subclasses.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AcqFunction$reset()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AcqFunction-eval_many"></a>}}
\if{latex}{\out{\hypertarget{method-AcqFunction-eval_many}{}}}
\subsection{Method \code{eval_many()}}{
Evaluates multiple input values on the acquisition function.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AcqFunction$eval_many(xss)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{xss}}{(\code{list()})\cr
A list of lists that contains multiple x values, e.g.
\code{list(list(x1 = 1, x2 = 2), list(x1 = 3, x2 = 4))}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
data.table::data.table() that contains one y-column for
single-objective acquisition functions and multiple y-columns for multi-objective
acquisition functions, e.g. \code{data.table(y = 1:2)} or \code{data.table(y1 = 1:2, y2 = 3:4)}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AcqFunction-eval_dt"></a>}}
\if{latex}{\out{\hypertarget{method-AcqFunction-eval_dt}{}}}
\subsection{Method \code{eval_dt()}}{
Evaluates multiple input values on the objective function
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AcqFunction$eval_dt(xdt)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{xdt}}{(\code{\link[data.table:data.table]{data.table::data.table()}})\cr
One point per row, e.g. \code{data.table(x1 = c(1, 3), x2 = c(2, 4))}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
data.table::data.table() that contains one y-column for
single-objective acquisition functions and multiple y-columns for multi-objective
acquisition functions, e.g. \code{data.table(y = 1:2)} or \code{data.table(y1 = 1:2, y2 = 3:4)}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AcqFunction-clone"></a>}}
\if{latex}{\out{\hypertarget{method-AcqFunction-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AcqFunction$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
