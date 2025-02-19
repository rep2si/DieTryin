% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_PGG_payouts.R
\name{calculate_PGG_payouts}
\alias{calculate_PGG_payouts}
\title{A function to calculate payouts for RICH public goods games}
\usage{
calculate_PGG_payouts(
  path,
  sub_path = "/Results/",
  K = 5,
  B = 20,
  Mu = 2,
  Game = "All",
  NA_Payout = 10
)
}
\arguments{
\item{path}{Full path to main folder.}

\item{K}{The number of players in the public goods game.}

\item{B}{The endowment provided to each person in the public goods game.}

\item{Mu}{The scalar on the goods provided to the public pot.}

\item{Game}{While ame to inlcude? "All" for all games.}

\item{NA_Payout}{What value is assigned to NAs?}
}
\description{
This function allows you to quickly calculate PGG payouts. See details below.
}
\examples{
\dontrun{
  calculate_PGG_payouts(path=path, K=5, B=20, Mu=2, NA_Payout=10)
                   }
}
