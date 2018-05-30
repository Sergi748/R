
scraping = function(page, team) {
  
  #Loading the rvest package
  library(xml2)
  library(rvest)
  library(RJSONIO)
  library(tidyverse)
  library(tokenizers)
  library(qdapRegex)
  
  url <- paste0("https://www.bdfutbol.com/es", page)
  # print(url)
  #Reading the HTML code from the website
  webpage <- read_html(url)
  
  #Using CSS selectors to scrap the rankings section
  # Obtenemos la fecha del partido
  rank_data_html_info <- html_nodes(webpage,'.info')
  info = unlist(lapply(rm_between(as.character(rank_data_html_info), "info", "(", extract=TRUE), function(x) gsub("\">", "", x)))
  fecha = substr(info, nchar(info)-9, nchar(info))[1]
  
  # Obtenemos los equipos de dicho partido
  rank_data_html_teams <- html_nodes(webpage,'.nom')
  teams = unlist(lapply(rm_between(as.character(rank_data_html_teams), "html", "</a></div>", extract=TRUE), function(x) gsub("\">", "", x[])))
  
  # Equipos que jugaron el partido de titulares
  rank_team_local <- html_nodes(webpage,'.eqcasa')[2]
  rank_team_visitante <- html_nodes(webpage,'.eqfora')[2]
  players_local = unlist(lapply(rm_between(as.character(rank_team_local), "html", "</a></td>", extract=TRUE), function(x) gsub("\">", "", x)))[1:11]
  players_visitante = unlist(lapply(rm_between(as.character(rank_team_visitante), "html", "</a></td>", extract=TRUE), function(x) gsub("\">", "", x)))[1:11]
  
  # Arbitro
  rank_arbitro <- html_nodes(webpage,'.arbit')
  arbitro = unlist(lapply(rm_between(as.character(rank_arbitro), ">", "<", extract=TRUE), function(x) gsub("\">", "", x[])))[2]
  
  # Entrenadores
  rank_manager_home = html_nodes(webpage,'.eqcasa')[2]
  manager_home = unlist(lapply(rm_between(as.character(rank_manager_home), "html", "</a>", extract=TRUE), function(x) gsub("\">", "", x)))[length(rm_between(as.character(rank_manager_home), "html", "</a>", extract=TRUE)[[1]])]
  rank_manager_away = html_nodes(webpage,'.eqfora')[2]
  manager_away = unlist(lapply(rm_between(as.character(rank_manager_away), "html", "</a>", extract=TRUE), function(x) gsub("\">", "", x)))[length(rm_between(as.character(rank_manager_away), "html", "</a>", extract=TRUE)[[1]])]
  
  names_total = c("Fecha", "Arbiter", "Team_Home", "Team_Away", "Manager Home", "Manager Away", "Player_1_Home", "Player_2_Home", "Player_3_Home", "Player_4_Home", "Player_5_Home", "Player_6_Home",
                  "Player_7_Home", "Player_8_Home", "Player_9_Home", "Player_10_Home", "Player_11_Home", "Player_1_Away", "Player_2_Away", "Player_3_Away", "Player_4_Away", "Player_5_Away", "Player_6_Away",
                  "Player_7_Away", "Player_8_Away", "Player_9_Away", "Player_10_Away", "Player_11_Away")
  tablon = data.frame()
  tablon = rbind(tablon, c(fecha, arbitro, teams, manager_home, manager_away, players_local, players_visitante))
  colnames(tablon) = names_total
  tablon$Fecha = as.factor(paste0(substr(tablon$Fecha, 1, 6), substr(tablon$Fecha, 9, 10)))
  
  if (!file.exists(file.path("data/plantillas", paste0("plantilla_", team, ".rds")))) {
    saveRDS(tablon, file.path("data/plantillas", paste0("plantilla_", team, ".rds")))
  } else {
    tablon_old = readRDS(file.path("data/plantillas", paste0("plantilla_", team, ".rds")))
    tablon_old = rbind(tablon_old, tablon)
    saveRDS(tablon_old, file.path("data/plantillas", paste0("plantilla_", team, ".rds")))
  }
  
}

union = function(team) {
  plantilla = readRDS(file.path("data/plantillas", paste0("plantilla_", team, ".rds")))
  partidos = readRDS(file.path("data/datosporequipos", paste0(team, ".rds")))
  
  tablon_final = merge(partidos, plantilla, by.x = "Date", by.y = "Fecha", all.x = TRUE)
  saveRDS(tablon_final, file.path("data/datos_finales", paste0("datos_completos_", team, ".rds")))
}

library(xml2)
library(rvest)
temporadas = c("2005-06", "2006-07", "2007-08", "2008-09", "2009-10", "2010-11", "2011-12", "2012-13", "2013-14",
               "2014-15", "2015-16","2016-17", "2017-18")

# Real madrid: 2.html?tab=partits
# Valencia: 36.html?tab=partits
# Barcelona: 1.html?tab=partits
# Osasuna: 27.html?tab=partits
# Sevilla: 33.html?tab=partits
# Celta: 11.html?tab=partits
# Villareal: 38.html?tab=partits
# Deportivo de la acoruña: 13.html?tab=partits
# Getafe: 16.html?tab=partits
# Ath_Madrid: 7.html?tab=partits
# Zaragoza: 39.html?tab=partits
# Ath_Bilbao: 6.html?tab=partits
# Mallorca: 24.html?tab=partits
# Betis: 8.html?tab=partits
# Espanol: 14.html?tab=partits
# Sociedad: 30.html?tab=partits
# Santander: 29.html?tab=partits
# Alaves: 3.html?tab=partits
# Cadiz: 10.html?tab=partits
# Malaga: 23.html?tab=partits

for (temporada in temporadas) {
  
  print(temporada)
  URL <- paste0("https://www.bdfutbol.com/es/t/t", temporada, "23.html?tab=partits")
  pg <- read_html(URL)
  links = html_attr(html_nodes(pg, "a"), "href")
  pages = lapply(as.character(unique(grep("/p/p.php", links, value = TRUE))), function (x) gsub("../", "/", x))
  team = "Malaga"
  
  for (page in pages) {
    # Solo cogemos datos para partidos de la liga
    if (nchar(page) < 18) {
      scraping(page = page, team = team)
    }
  }
  
}

union(team = team)

rm(list = ls())
cat("\014")
