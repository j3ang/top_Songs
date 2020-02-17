###################################################################
#
# https://www.rdocumentation.org/packages/dplyr/versions/0.7.8
# https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
# https://newsroom.spotify.com/2019-12-03/the-top-songs-artists-playlists-and-podcasts-of-2019-and-the-last-decade/
# https://dplyr.tidyverse.org/
# Utelizing functions in dplyr package
#
###################################################################

#' ######################################################
#' POP value is the popularity of the artist.
#' The value will be between 0 and 100, with 100 being the most popular.
#' The artist’s popularity is calculated from the popularity of all the artist’s tracks.
#' With no steaming data available in this dataset.
#' The artist's popularity are deternmined by the weighted value:
#'
#' 1. POP value of the artist at that time * (20%)
#' 2. The number of songs of the artist appeared on the list of that year (80%)
#' #####################################################
#'
#' This function returns top artist of the year
#'
#' @param dataset The dataset
#' @param y The year
#' @examples
#' top_artist_yr(dataset, 2012)
#' @export
#'
get_topArtistsBy_yr <- function(dataset, y) {
  select_all(dataset) %>%
    group_by(artist) %>%
    filter(year == y) %>%
    summarise(
      max_pop = max(pop),
      min_pop = min(pop),
      sum_pop = sum(pop),
      mean_pop = mean(pop),
      year = as.character(y),
      n = n(),
      eval_pop_of_yr = (n*.8 +  mean_pop*.2)/nrow(.) ) %>%
    arrange(desc(eval_pop_of_yr)) -> top_artist
  return(top_artist)
}



#' This function returns all top 10 artists of past 10 years
#'
#' @param dataset The dataset
#' @return conbined results of top_artist_yr()
#' @examples
#' top_artist_yr_all(dataset)
#' @export
#'
get_topArtists <- function(dataset) {
  datalist= list()
  for (i in (max(dataset$year):min(dataset$year))){
    datalist[[i]] <- head(assign(paste0("top_artist_yr_", i),
                                 get_topArtistsBy_yr(dataset, as.character(i))),10)
  }
  all_top_artists_of_the_yr = do.call(rbind, datalist)  # join all the table
  return(all_top_artists_of_the_yr)
}

#' This function returns the most popular songs over the decade
#' @param dataset The dataset
#' @return songs with pop value over 80
#' @examples
#' get_popSongs(songs)
#' @export
#'
get_popSongs<- function(dataset) {
  select_all(dataset) %>%
    filter( pop > 80) %>%
    arrange(desc(pop)) -> most_pop
  return(most_pop)
}


#' This function returns top dance songs
#' evaluated by (dnce by .3 + pop by .4  + nrgy by .3)
#'
#' The dancibility are re-evaluated on top of original
#' dnce value with weight given to {nrgy} and {bpm}
#' @param dataset The dataset
#' @examples
#' top_dnce_by_year(songs, 2012)
#' @export
#'
get_topDnce <- function(dataset) {
 select_all(dataset) %>%
    group_by(title, artist, year, pop, dnce,nrgy, bpm, val,  dB, dur, acous, spch) %>%
    filter( top.genre == 'dance pop' & dnce > 75 ) %>%
    summarise(eval=(dnce*.3 + pop*.4  + nrgy*.3)) %>%
    arrange(desc(eval)) -> topDnce
  return(topDnce)
}

#' This function returns top dance songs by year
#' evaluated by (dnce by .3 + pop by .4  + nrgy by .3)
#'
#' The dancibility are re-evaluated on top of original
#' dnce value with weight given to {nrgy} and {bpm}
#' @param dataset The dataset
#' @param yr The year
#' @examples
#' top_dnce_by_year(songs, 2012)
#' @export
#'
get_topDnceBy_yr <- function(dataset,y) {
  topDnce_all <- get_topDnce(dataset)
  topDnce_all %>%
    filter(year == y) -> topDnce_Yr
  return(topDnce_Yr)
}

#' This function returns number of songs grouped by year
#' @param dataset The dataset
#' @return number of songs by year
#' @examples
#' get_number_of_songBy_yr(songs)
#' @export
#'
get_number_of_songBy_yr <- function(dataset){
  dataset %>%
    group_by(year) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) -> n_songsBy_yr

  return(n_songsBy_yr)
}





