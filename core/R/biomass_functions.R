# Functions for above ground biomass estimation

treeMass <- function(speciesgroup, dbh) {
  # biomass for individual tree
  # dbh in CM
  # returns in kg
  
  bparams = read.csv(file.path('src', 'csv', 'bparams.csv'))
  
  # check that species and dbh values are valid
  if (!is.na(match(tolower(speciesgroup), bparams$species.group))) {
    if (is.numeric(dbh)) {
      
      rowIndex = which(bparams$species.group == tolower(speciesgroup))
      
      # fetch contants
      b0 = bparams$b0[rowIndex]
      b1 = bparams$b1[rowIndex]
      
      # biomass equation
      biomass = exp(b0 + b1 * log(dbh))
      
      return(biomass)
    } else {
      return(NA)
    }
    
  } else {
    return(NA)
  }
  
}


plotMass <- function(datatable, speciescol, dbhcol) {
  # return biomass for entire datatable
  # takes species and dbh colum indexes as arguments
  
  datatable$biomass = mapply(treeMass, datatable[,c(speciescol)], datatable[,c(dbhcol)])
  
  return(datatable)
  
}