% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interactive_point_verif.R
\name{interactive_point_verifUI}
\alias{interactive_point_verifUI}
\alias{interactive_point_verif}
\title{Shiny module for interactively showing point verification scores}
\usage{
interactive_point_verifUI(id)

interactive_point_verif(
  input,
  output,
  session,
  verif_data,
  colour_table,
  time_axis
)
}
\arguments{
\item{id}{An ID string for the module's UI}

\item{input}{input reactive}

\item{output}{output reactive}

\item{session}{Session from which to make a child scope (the default should
almost always be used).}

\item{verif_data}{verification data as a reactive value}

\item{colour_table}{A reactive data frame in a format suitable for the
\code{colour_table} argument to \code{\link{plot_point_verif}}}

\item{time_axis}{A reactive string giving the name of the time axis to
use as the x-axis in the dashboard. Mut be one of "lead_time",
"valid_dttm", or "valid_hour" and the column must exist in \code{verif_data}.}
}
\value{
An interactive list of options chosen for a plot that can be passed
to \code{\link{download_verif_plot}}
}
\description{
Shiny module for interactively showing point verification scores
}
\examples{
library(shiny)

shinyOptions(theme = "white")

ui <- fluidPage(
  fluidRow(
    column(12, interactive_point_verifUI("int"))
  ),
  fluidRow(
    column(12, verbatimTextOutput("opts"))
  )
)

server <- function(input, output, session) {

  col_tbl <- data.frame(
    fcst_model = unique(verif_data_ens$ens_summary_scores$fcst_model),
    colour     = c("red", "blue")
  )

  opts <- callModule(
    interactive_point_verif, "int", reactive(verif_data_ens),
    reactive(col_tbl),  reactive("lead_time")
  )

  output$opts <- renderPrint({
    req(opts())
  })

}

if (interactive()) {
  shinyApp(ui, server)
}
}
