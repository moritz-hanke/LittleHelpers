#' Evaluation of binary classifiers
#'
#' \code{evaBin} returns different measures for the performance of binary classifiers.
#'
#' @param p Numeric, the number of positives.
#' @param n Numeric, the number of negatives.
#' @param tp Numeric, the number of true positives.
#' @param tn Numeric, the number of true negatives.
#' @param fp Numeric, the number of false positives.
#' @param fn Numeric, the number of false negatives.
#' @param s Numeric, true vector with values \code{TRUE} and \code{FALSE}
#' or \code{1}s and \code{0}s.
#' @param s.est Numeric, extimated vector with values \code{TRUE} and \code{FALSE}
#' or \code{1}s and \code{0}s.

#'
#' @return Returns a list with the following measures:
#'  \item{True positive (TP)}
#'  \item{False positive (FP)}
#'  \item{True negative (TN)}
#'  \item{False negative (FN)}
#'  \item{Precision}
#'  \item{F1-Score (F1)}
#'  \item{Accuracy (ACC)}
#'  \item{Balanced accuracy (BACC)}
#'  \item{Matthews correlation coefficient(MCC)}
#'  \item{True postive rate (TPR)}
#'  \item{True negative rate (TNR)}
#'  \item{False positive rate (FPR)}
#'  \item{False negative rate (FNR)}
#'  \item{False desovery rate (FDR)}
#' @export
#' @examples
#' evaBin(7, 13, 2, 10)
#'
evaBin <- function(p=NULL, n=NULL, tp=NULL, tn=NULL, fp=NULL, fn=NULL,s=NULL, s.est=NULL){
  if(all(
    c(!is.null(p),
      !is.null(n),
      !is.null(tp),
      !is.null(tn),
      !is.null(fp),
      !is.null(fn)))){
    P <- p
    N <- n
    TP <- tp
    TN <- tn
    FP <- P-TP
    FN <- (P+N) - (TP+TN+FP)
  }
  if(all(c(!is.null(s),
           !is.null(s.est)))){
    P <- length(which(s.est!=0))
    N <- length(which(s.est==0))
    TP <- sum(which(s.est!=0) %in% which(s!=0))
    FP <- sum(which(s.est!=0) %!in% which(s!=0))
    TN <- sum(which(s.est==0) %in% which(s==0))
    FN <- (P+N) - (TP+TN+FP)
  }

  Precision <- TP/(TP+FP)
  F1 <- 2*TP/(2*TP+FP+FN)
  ACC <- (TP+TN)/(P+N)
  BACC <- (TP/P+TN/N)/2
  MCC <- (TP*TN-FP*FN)/
    sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  TPR <- TP/(TP+FN)
  TNR <- TN/(TN+FP)
  FPR <- FP/(FP+TN)
  FNR <- FN/(FN+TP)
  FDR <- FP/(FP+TP)
  out <-list(c(TP=TP,
               FP=FP,
               TN=TN,
               FN=FN,
               Precision=Precision,
               F1=F1,
               ACC=ACC,
               BACC=BACC,
               MCC=MCC,
               TPR=TPR,
               TNR=TNR,
               FPR=FPR,
               FNR=FNR,
               FDR=FDR))
  return(out)
}
