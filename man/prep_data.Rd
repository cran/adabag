\name{prep_data}
\alias{prep_data}

\title{Prepare Ranking Data for Item-Weighted Ensemble Algorithm}

\description{
  The \code{prep_data} function prepares item-weighted ranking data for further analysis. It takes a ranking matrix, predictors matrix, and weighting vector or matrix, and returns a data frame suitable for item-weighted ensemble algorithms for rankings.
}

\usage{
  prep_data(y, x, iw)
}

\arguments{
  \item{y}{an N by M matrix or data frame representing the ranking responses, where N is the number of individuals and M is the number of items. Each row corresponds to a ranking, ties are allowed.}
  \item{x}{an N by K matrix or data frame containing the K predictors associated with each individual ranking. Continuous variables are allowed, while the dummy coding should be used for categorical variables.}
  \item{iw}{a vector or matrix representing the item weights or dissimilarities for the ranking data. For a vector, it should be a row vector of length M. For a matrix, it should be a symmetric M by M matrix representing item dissimilarities.}
}

\details{
  The \code{prep_data} function performs the following steps:
 Check the dimensions of the weighting vector or matrix to ensure compatibility with the ranking data.
 Adjust the ranking matrix  \emph{y} using the "min" method for ties.
Convert the ranked matrix into a data frame.
Generate the universe of rankings using the \pkg{ConsRank::}\code{univranks} function. Match the ranking matrix  \emph{y} with the whole universe of rankings to obtain a label for each ranking. Combine the Label column with the predictor matrix.
 Remove rows with missing values. The function then returns the prepared data frame for ensemble ranking. It also create the internal objects: \code{item}, \code{perm_tab_complete_up}, \code{perm}, \code{mat.dist} that are employed in the \code{Ensemble_ranking_IW} function.
}

\value{
  An N by (K+1) data frame containing the prepared item-weighted ranking data. The first column "Label" contains the transformed ranking responses, and the remaining columns contain the predictors.
}

\references{
  Albano, A., Sciandra, M., and Plaia, A. (2023): "A Weighted Distance-Based Approach with Boosted Decision Trees for Label Ranking." \emph{Expert Systems with Applications}.

  D'Ambrosio, A.[aut, cre], Amodio, S. [ctb], Mazzeo, G. [ctb], Albano, A. [ctb], Plaia, A. [ctb] (2023). "ConsRank: Compute the Median Ranking(s) According to the Kemeny's Axiomatic Approach. R package version 2.1.3", https://cran.r-project.org/package=ConsRank.

  Plaia, A., Buscemi, S., Furnkranz, J., and Mencıa, E.L. (2021): "Comparing Boosting and Bagging for Decision Trees of Rankings." \emph{Journal of Classification}, pages 1–22.
}

\examples{
  # Prepare item-weighted ranking data
  y <- matrix(c(1, 2, 3, 4, 2, 3, 1, 4, 4, 1, 3, 2, 2, 3, 1, 4), nrow = 4, ncol = 4, byrow = TRUE)
  x <- matrix(c(0.5, 0.8, 1.2, 0.7, 1.1, 0.9, 0.6, 1.3, 0.4, 1.5, 0.7, 0.9), nrow = 4, ncol = 3)
  iw <- c(2, 5, 5, 2)
  dati <- prep_data(y, x, iw)
}
