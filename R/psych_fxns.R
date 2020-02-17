###################################################################
#
# https://www.rdocumentation.org/packages/psych/versions/1.9.12.31
# https://personality-project.org/r/psych/psych-manual.pdf
# https://cran.r-project.org/web/packages/psych/vignettes/intro.pdf
# Utelizing functions in psych package
#
###################################################################

#' This function describe data using psych's basic functions
#'
#' @param dataset A dataset
#' @return result table of described dataset
#' @export
desc_data <- function(dataset) {
  print(dim(dataset))
  title('Described Data')
  psych_desc <- describe(dataset)
  print(psych_desc)

  cat('\n\n')
  title('Head/Tail Data')
  print(headTail(dataset))


  # Basic histograms/dencity
  num_col <- get_num_col(dataset)
  class(num_col$bpm)
  jpeg("dencity_hist.jpg")
  par(mfrow=c(3,4))
  # draw histograms of the numerical cols
  for(name in names(num_col)){
    hist( num_col[,name], col='grey', freq=FALSE, main=paste0('Histogram of ', name), xlab = paste0(name))
    lines(density(num_col[,name])) # add density lines
  }
  dev.off() # turning off graphic device


  return(psych_desc)
}




#' This function retrives and peeks numeric columns data
#' @param dataset A dataset
#' @return dataset with numerical columns
#' @example
#' get_num_col(dataset)
#' @export
#'
get_num_col <- function(dataset){
  title('Peek Numeric Columns')
  dataset.num.col <- dataset[,sapply(dataset, is.numeric)] # Get all the numeric col for correlation calculation
  dataset.num.col <- dataset.num.col[2:12] # remove X col
  print(headTail(dataset.num.col)) # preview top/tail rows

  return(dataset.num.col)
}




#' This function prints corr test result using corr.test() to
#' Find the correlations, sample sizes,  and probability values
#' between elements of a matrix or data.frame.
#' @param dataset A dataset
#' @return results table of corr.test()
#' @example
#' corrTest(dataset)
#' @export
#'
testCorr <- function(dataset.num.col){
  title('Corr Test')
  corr_test <- corr.test(dataset.num.col)
  print(corr_test)

  return(corr_test)
}



#' This function prints corr test result using corr.test() to
#' Find and print (usinglowerMat) the lower diagonal correlation matrix
#' but returns (in-visibly) the full correlation matrix found with the use
# and method parameters.
#' @param dataset A dataset
#' @return results table of corr.test()
#' @example
#' corrTest(dataset)
#' @export
#'
lowerCorr <- function(dataset){
  cat("\n")
  title('Lower Correlation')
  lowerCor <- lowerCor(dataset, digits=3, method='pearson')

  return(lowerCor)
}



#' This function uses:
#' - paris.panels()
#' - corPlot()
#' to graph the correlations to scatter plot and heatmap
#' @return It will create 2 png files of each
#' @examples
#' draw_Corr(num_col_dataset, lowerCor_var)
#' @export
#'
draw_Corr <- function(dataset.num.col, lowerCor){

  png('pars_panels.png')
  pairs.panels(dataset.num.col) # whows scatter plot matrices and as well as histograms and the Pearson correlation

  png('corPlot.png')
  gr <- colorRampPalette(c("#B52127", "white", "#2171B5"))
  corPlot(lowerCor,numbers=TRUE,upper=FALSE,diag=FALSE,gr=gr, main="Song Properties Correlations")  # Plot heatmap
  dev.off()
}








#' This function prints divider with title
#'
#' @examples
#' top_dnce_by_year(songs, 2012)
#' @export
#'
title <- function(title){
  cat('-------------------------------------------------------------------------------------------------------------\n')
  cat('                                    ', eval(title) , '\n')
  cat('-------------------------------------------------------------------------------------------------------------\n')
}

