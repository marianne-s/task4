
####--------------------------------------------------------####
####----  Exercises: Comp. Statistician / R Developper  ----####
####--------------------------------------------------------####


#' popmean.
#'
#' `popmean` calculates the national prevalence (population-weighted mean of the two zones) by years for an intervention type.
#'
#' @param dat dataset with columns "zone",	"population"	"year"	"none"	"IRS"	"ITN"	"IRS+ITN"
#' @param intervention intervention concerned ("none"	"IRS"	"ITN"	"IRS+ITN")
#' @param years years concerned
#' @return Returns the value of \code{arg1}
#' @examples {
#' dat = read.delim(text = "zone	population	year	none	IRS	ITN	IRS+ITN
#' Zone 1	692089	2005	0.41	0.40	0.39	0.40
#' Zone 1	692089	2006	0.37	0.35	0.35	0.35
#' Zone 1	692089	2007	0.31	0.29	0.29	0.29
#' Zone 2	324469	2005	0.26	0.26	0.26	0.26
#' Zone 2	324469	2006	0.21	0.20	0.20	0.20
#' Zone 2	324469	2007	0.16	0.15	0.15	0.15", sep = "\t", header = TRUE)
#' popmean( dat, intervention = "IRS", years = 2005:2006)
#' }
#'
#' @export

popmean <- function(dat, intervention, years){
  xz1 = which(subset(dat, dat$zone == "Zone 1")[,3] %in% years)
  nrowz = nrow(subset(dat, dat$zone == "Zone 1"))
  xz2 = nrowz + which(subset(dat, dat$zone == "Zone 2")[,3] %in% years)

  factorZone1 <- dat[xz1,2] / (dat[xz1,2] + dat[xz2,2])
  factorZone2 <- dat[xz2,2] / (dat[xz1,2] + dat[xz2,2])

  # Find the column corresponding of the intervention
  intervention=gsub("\\+", ".", intervention)
  col = which(names(dat)==intervention)

  #return the calculate the national prevalence
  return(dat[xz1,col]*factorZone1+dat[xz2,col]*factorZone2)
}



