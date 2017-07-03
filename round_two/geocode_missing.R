#Geocoding Missing Addresses
#Philadelphia Real Estate Tax Evasion
#  Round 2
#Michael Chirico

##Packages
library(data.table)
library(ggmap)
library(funchir)
library(readxl)

##Geocode
all_prop = read_excel(
  'data/req20150709_PennLetterExperiment (September 2015 update) v2.xlsx',
  sheet = "DETAILS", skip = 8L, na = c('-', 'NULL'),
  col_names = c('x', 'opa_no', 'address', 'x', 'y', rep('x', 20L)),
  col_types = abbr_to_colClass("stsnsss", "1212991")
)

setDT(all_prop)

##two properties have missing address -- 
##  151102610 and 881577275 -- likely
##  related to change of OPA # for these properties.
##  did these two by hand
miss_x = all_prop[is.na(x) & !is.na(address)]

## most of the properties with missing x/y are hyphenated
##   (e.g., 440-42 W Manheim); split such addresses,
##   geolocate both (i.e., 440 W Manheim and 442 W Manheim),
##   then find the midpoint to assign as the actual x/y
miss_x[ , addr := gsub('\\s+([0-9]+|[A-Z]{1})$', '', address)]
miss_x[ , addr1 := gsub('-[ 0-9]+', '', addr)]
miss_x[ , addr2 := gsub('[0-9]{2}\\s-\\s', '', addr)]
## not always the case that it's hyphenated, in which case
##   don't bother geocoding the same thing twice
miss_x[addr1==addr2, addr2 := NA]

miss_x = 
  melt(miss_x, id.vars = 'opa_no',
       measure.vars = c('addr1', 'addr2'), na.rm = TRUE,
       value.name = 'address')
## direct geocoder to Philadelphia, otherwise it will
##   find the addresses all over the place
miss_x[ , address := paste(address, 'PHILADELPHIA, PENNSYLVANIA')]

miss_x[ , c('x', 'y') := geocode(address)]

miss_x = miss_x[ , .(x = mean(x), y = mean(y)), by = opa_no]
hand_geocode = data.table(
  opa_no = c('151102610', '881577275'),
  x = c(-75.1726347, .5*(-75.1754994-75.1756785)),
  y = c(39.9708001, .5*(39.9271901+39.9273799))
)
miss_x = rbind(miss_x, hand_geocode)

## these are "phantom" tracts which don't exist on a real
##   street, so geolocation placed them outside the city.
##   used official zoning maps to locate them & Google to get x/y
##   (in fact, there are likely more like this, but they at least
##    landed in the city, so ignore as noise)
miss_x[opa_no == '141184200', c('x', 'y') := .(-75.157757, 39.964109)]
miss_x[opa_no == '141184300', c('x', 'y') := .(-75.157716, 39.964100)]

#save output
fwrite(miss_x, 'round_two/geocoded_missing.csv', quote = TRUE)
