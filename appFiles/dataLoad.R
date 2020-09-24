#covid_ci = read.csv("data/covid_times_series_ci.csv",sep = ";",stringsAsFactors = F)

rap_jour = read.csv("data/rapport_journalier.csv")
rap_jour$date = as.Date(rap_jour$date)
############## Data all country -------------
coronavirus = read.csv("data/coronavirus1.csv", stringsAsFactors = F) [,-1]
coronavirus = coronavirus %>% 
  dplyr::mutate(country = dplyr::if_else(countryName == "United States", "United States of America", countryName)) %>% 
  select(-countryName)
covid_ci = coronavirus %>% 
              dplyr::filter(countryCode=="CI")
names(covid_ci) = c("date","countryCode","region","lat","lon",
                       "Confirmed","Recovered","Deaths","countryName")
covid_cities = read.csv("data/ville_touchees.csv")
names(covid_cities) = c("City","Confirmed")

covid_communes = read.csv("data/cas_commune.csv",sep = ",")
covid_communes$Commune = c("ABOBO","ATTECOUBE","ADJAME","COCODY","YOPOUGON","PLATEAU","MARCORY","KOUMASSI",
                          "TREICHVILLE","PORT-BOUET")
names(covid_communes) = c("Commune","Confirmed","Recovered","Deaths")



depistage = read.csv("data/depistage.csv",sep = ";")
cities = read.csv("data/cities.csv",sep = ";")
covid_commune = read.csv("data/communeCovid.csv",sep = ";")
pop_coummune = read.csv("data/population_commune.csv",sep=";")
#covid_ci$date = as.Date(covid_ci$date,"%d/%m/%Y")

prelev <- read.csv("data/prelev.csv",sep = ";")

depistage$date = as.Date(depistage$date,"%d/%m/%Y")



names(coronavirus) = c("date","countryCode","region","lat","lon",
                       "Confirmed","Recovered","Deaths","countryName")
###################################################################################

############## Map Abidjan ---------------------------------------
#Chargement du fichier pour tracer les contours de la carte
ab_map <- read.csv("data/abidjan_map.csv", sep = ";") %>% 
  filter(Commune!='CONTOUR ABIDJAN') %>% 
  mutate(Commune = as.character(Commune)) 
  
  

#Pour pouvoir écrire les noms des communes sur l carte au milieu de chaque commune
center <- aggregate(.~Commune, data = ab_map[,c(1,2,3)], FUN = mean)
center$Commune[center$Commune=='LAGUNE EBRIE'] <- ''
center$Commune[center$Commune=='LAGUNE EBRIE'] <- ''
center$Commune[center$Commune=='LAGUNE EBRIE'] <- ''
center$Commune[center$Commune=='LAGUNE EBRIE'] <- ''

#Chargement du jeu de données contenant les cas de covid  (confirmed, recoveredn deaths) par commune
df <- read.csv("data/communeCovid.csv",sep = ";", header = TRUE) %>% 
  mutate(Commune = as.character(Commune)) 
########----------------------------------------------------------





##### Map regions CI
##### Map regions CI
CIV <- raster::getData("GADM", country="CIV", level=4)
civ <- st_as_sf(CIV)
civ <- civ %>% 
  rename(name = "NAME_4")
civ[1,10] = "Abidjan"

tmp = read.csv("data/tmp.csv",sep = ";")
tmp$city = as.character(tmp$city)

ci_map <- left_join(
  x = civ,
  y = tmp,
  by =c("name"="city")
)

ci_map$Confirmed <- ci_map$Confirmed %>% replace_na(0)
ci_map$Recovered <- ci_map$Recovered %>% replace_na(0)
ci_map$Deaths <- ci_map$Deaths %>% replace_na(0)
write.csv(ci_map,"data/ci_map.csv")
#### Old data
# 
# <- read.csv(file = "https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv", stringsAsFactors = F)
#dd = df1 <- read.csv(file = "https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_daily_reports/04-05-2020.csv", stringsAsFactors = F)
#write.csv(df1,"data//coronavirus.csv")



##########    New data
#df1 <- read.csv(file = "https://raw.githubusercontent.com/ulklc/covid19-timeseries/master/countryReport/raw/rawReport.csv", stringsAsFactors = F)
#write.csv(df1,"data//coronavirus1.csv")
############


hideAllBut = function(divList, butNdx) {
  library("shinyjs")
  divList[-butNdx] %>% sapply(function(x) {shinyjs::hide(x)})
  shinyjs::show(divList[butNdx])
}

