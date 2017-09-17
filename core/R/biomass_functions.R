# Functions for above ground biomass estimation

treeMass <- function(speciesgroup, dbh) {
  # biomass for individual tree
  # dbh in CM
  # returns in kg
  
  bparams = read.csv(file.path('..', '..', 'src', 'csv', 'bparams.csv'))
  
  rowIndex = which(bparams$species.group == tolower(speciesgroup))
  
  # fetch contants
  b0 = bparams$b0[rowIndex]
  b1 = bparams$b1[rowIndex]
  
  # biomass equation
  biomass = exp(b0 + b1 * log(dbh))
  
  return(biomass)
  
}


plotMass <- function(datatable, speciescol, dbhcol) {
  # return biomass for entire datatable
  # takes species and dbh colum indexes as arguments
  
  datatable$biomass = mapply(treeMass, datatable[,c(speciescol)], datatable[,c(dbhcol)])
  
  return(datatable)
  
}

testData = read.csv("C:\\Users\\Jamie\\Documents\\GitHub\\Biomassr\\test\\AG1 _biomass_est.csv")

testresults = plotMass(testData, 4, 5)