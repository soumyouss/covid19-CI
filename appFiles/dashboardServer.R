results = reactiveValues(
  dataframeTotal = NULL,
  dataframeTotalCI = NULL,
  dataframeCities = NULL,
  dataframeWaterFall = NULL,
  dataframeFinalWorld = NULL,
  dfDaily = NULL,
  newCases = NULL,
  dataframeTotalOldCases = NULL,
  dataframeTotalAb = NULL,
  newCasesDeath = NULL,
  dataframeFinal = NULL,
  newCasesRecovered = NULL,
  dataframeOldCases = NULL,
 dataframeAb = NULL,
  resultTable = NULL
)


output$dashboard = renderUI({
  argonTabSet(
    id = "analysisSettingsTabs",
    card_wrapper = T,
    horizontal = T,
    circle = F,
    size = "sm",
    width = 12,
    iconList = list(
      icon("home"),
      icon("tachometer-alt"), 
      icon("laptop-code"), 
      icon("chart-line"),
      argonIcon("world")
    ),
    argonTab(
      tabName = "Home",
      active = F,
      argonRow(
        argonColumn(
          width = 4,
          img(src = 'pub.gif',width = "100%")
        ),
        argonColumn(
          width = 5,
          #center("ALERTE CORONAVIRUS", style = 'color:Red;font-size:20px;text-align:Left;'),
          p("ALERTE CORONAVIRUS !",style = 'color:Red;font-size:30px;text-align:Left;'),
          tags$strong("La Cote d'Ivoire a recencé son 1er cas de Covid-19 le 11 Mars 2020 à Abidjan. "),
          
          p("Si vous pensez être malade, éviter les endroits publics. Eviter de sortir de vous.
            Respectez les mesures barrières (lavage de main au savpn, le port de masque,...). Surveillez vos
            symptomes, l'évolution de votre température corporelle et recherchez un avis medical.",style = 'text-align:justify;'),
          tags$br()
          
          
        ),
        argonColumn(
          width = 3,
          img(src = 'covid4.jpg',width = "100%")
          
        )

      ),
      tags$br(),
      argonRow(
        tags$strong("Description"),
        tags$hr(),
        p("L'application présente plusieurs onglets qui permettront aux utilisateurs de voir l'évolution
          du COVID-19 en Cote d'Ivoire et plus préçisement dans le district d'Abidjan (épicentre de l'épidemie).L'onglet Comparaison permettra
          à l'utilisateur d'établir une comparaison entre les différentes communes d'Abidjan sur l'évolution de l'épidemie.
          L'application permet également de voir d'une manière générale la situation dans le monde via l'onglet World Map."),
        tags$strong("Méthode utilisée pour la prévision"),
        tags$hr(),
        p("Le modèle utilisé pour la prévision est la modélisation des maladies infectueuses.Ces maladies peuvent se propager d'un membre d'une population à l'autre; nous essayons de comprendre à quelle vitesse ils se propagent, 
           quelle proportion d'une population elles infectent, quelle proportion meurt, etc. L'une des façons les plus 
           faciles de les modéliser (et la façon dont nous nous concentrons ici) est avec un modèle compartimenté.
          Un modèle compartimenté sépare la population en plusieurs compartiments, par exemple:"),
      tags$ul(
        tags$li(tags$strong("S"),"usceptible (ceux qui sont en bonne santé mais sensibles à la maladie (c.-à-d. À risque d'être contaminés). Au début de la pandémie, S est l'ensemble de la population car personne n'est à l'abri du virus.)"),
        tags$li(tags$strong("I"),"nfected: les personnes infectieuses (et donc infectées)"),
        tags$li(tags$strong("R"),"ecovered :individus contaminés mais qui se sont rétablis ou sont morts. Ils ne sont plus contagieux.")
      ),
      p("Ces groupes évoluent avec le temps à mesure que le virus progresse dans la population."),
      p("Les paramètres importants du modèle sont donnés ci-après"),
      tags$ul(
        tags$li(tags$strong("S"),"S diminue lorsque les individus sont contaminés et passent au groupe infectieux I."),
        tags$li("Lorsque les gens se rétablissent ou meurent, ils passent du groupe infecté I au groupe R récupéré.")
      ),
      p("Pour modéliser la dynamique de l'épidémie, nous avons besoin de trois équations différentielles pour décrire les taux 
        de changement dans chaque groupe, paramétrées par:"),
      tags$ul(
        tags$li(tags$strong("beta"),"nombre attendu de personnes qu'une personne infectée infecte par jour"),
        tags$li(tags$strong("gamma"),"proportion de personnes infectées se rétablissant par jour")
      ),
      p("Officiellement, cela donne:")
      ),
      argonRow(
        center = T,
        argonImage(src="equa.JPG")
      ),
      argonRow(
        p("La première équation (équation 1) indique que le nombre d'individus sensibles ( S ) diminue avec le nombre d'individus nouvellement infectés, où les nouveaux cas infectés 
        sont le résultat du taux d'infection (beta lié par le nombre d'individus sensibles ( S ) 
          ayant eu un contact avec des individus infectieux ( I )."),
        p("La deuxième équation (équation 2) indique que le nombre d'individus infectieux ( I ) augmente avec les individus nouvellement infectés (gamma.I.S), moins les personnes précédemment infectées qui se sont rétablies (c.-à-d. 
γI quel est le taux d'élimination gamma multiplié par les individus infectieux I )."),
        p("Enfin, la dernière équation (équation 3) indique que le groupe récupéré ( R ) augmente avec le nombre d'individus infectieux et qui ont récupéré 
          ou sont morts (gamma*I)."),
        p("Une épidémie se développe comme suit:"),
        tags$ol(
          tags$li("Avant le début de l'épidémie, S est égal à l'ensemble de la population car personne n'a d'anti-corps."),
          tags$li("Au début de l'épidémie, dès que le premier individu est infecté, S diminue de 1 et I augmente de 1 également."),
          tags$li("Ce premier individu infectieux contamine (avant de se remettre ou de mourir) d'autres individus susceptibles."),
          tags$li("La dynamique se poursuit, des individus récemment contaminés infectant à leur tour d'autres personnes sensibles avant de se remettre.")
        )
        ),
      p("Visuellement, nous avons:"),
      argonRow(
        center = T,
        argonImage(src = "modele.jpg")
      )
    ),
    # analysis setting tab -----
    argonTab(
      tabName = "Dashboard",
      active = T,
      tags$head(tags$style(type = "text/css", "
             #loadmessage {
                           position: fixed;
                           top: 150px;
                           left: 50px;
                           width: 93%;
                           padding: 5px 0px 5px 0px;
                           text-align: center;
                           font-weight: bold;
                           font-size: 100%;
                           color: #000000;
                           background-color: #CCFF66;
                           z-index: 105;
}
  ")),
      argonRow(
        argonColumn(
          width = 12,
          uiOutput("cardUI") %>% withSpinner()
        )
      ),
      tags$hr(),
      argonRow(
        argonColumn(
          width = 12,
          uiOutput("chartUI") %>% withSpinner()
        )
      ),
      tags$hr(),
      argonRow(
        argonColumn(
          width = 12,
          uiOutput("chart2UI") %>% withSpinner()
        )
      ),
      conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                       tags$div("Chargement de Page!!! veuillez patienter...",id = "loadmessage")),
      
      argonRow(
        argonColumn(
          width = 12,
          dataTableOutput("dataTableCityWise") %>% withSpinner()
        
      )
    ),
    tags$hr(),
    tags$br(),
    h5("Source des données:",style = 'color:Red;font-size:20px;text-align:Left;'),
    p("1.Ministère de la santé et de l'hygène publique .",style = 'color:blue;font-size:15px;text-align:Left;'),
    p(paste0("2. Dernière mise à jour: ",lastUpdate),style = 'color:blue;font-size:15px;text-align:Left;')
    ),
    argonTab(
      tabName = "Comparaison",
      active = F,
      uiOutput("countryComparisionChartsUI") %>% withSpinner(),
      tags$hr(),
      argonRow(
        argonColumn(
          width = 12,
          dataTableOutput("dataTableCountryCompare") %>% withSpinner()
          
        )
      ),
      tags$hr(),
      tags$br(),
      #h5("NOTE IMPORTANTE:",style = 'color:Red;font-size:20px;text-align:Left;'),
      #p("1.L.",style = 'color:blue;font-size:15px;text-align:Left;')
      #p(paste0("2. Le Tableau de bord sera mis a jour tout les jours a  GMT 00:00. DERNIERE MISE A JOUR: 05-05-2020 "),style = 'color:red;font-size:15px;text-align:Left;')
    ),
    argonTab(
      tabName = "Prévision",
      active = F,
      uiOutput("forecastUI") %>% withSpinner(),
      tags$hr(),
      tags$br(),
      #h6(HTML(paste0("Source des données: <a href='https://www.mobile.twitter.com' target = '_blank'>Shubhram Pandey</a>")), style = 'color:blue;')
      
                             

      #p(paste0("2. Le Tableau de bord sera mis a jour tout les jours a  GMT 00:00. DERNIERE MISE A JOUR: 05-05-2020 "),style = 'color:Red;font-size:15px;text-align:Left;')
    ),
     argonTab(
       tabName = "World Map",
       active = F,
       uiOutput("world") %>% withSpinner(),
       tags$hr(),
       argonRow(
         argonColumn(
           width = 12,
           dataTableOutput("dataTableCountryWise") %>% withSpinner()
           
         )
       ),
       tags$hr(),
       tags$br(),
       h5("Source des données:",style = 'color:Red;font-size:20px;text-align:Left;')
       #p("1. Université JOHN HOKPINS.",style = 'color:blue;font-size:15px;text-align:Left;'),
       #p(paste0("2. DERNIERE MISE A JOUR: 05-05-2020 "),style = 'color:blue;font-size:15px;text-align:Left;')
     )
  )
})

observe({
  waiter_show(loader)
  results$dataframeFinal = covid_ci
  results$dataframeFinalWorld = coronavirus
  
  # dataframeAb <- covid_commune %>%
  #      dplyr::group_by(Commune) %>%
  #      dplyr::summarise(Confirmed=sum(Confirmed,na.rm = TRUE),Recovered=sum(Recovered,na.rm = TRUE),
  #             Deaths=sum(Deaths,na.rm = TRUE)) %>%
  #      dplyr::mutate(Unrecovered = Confirmed - ifelse(is.na(Recovered), 0, Recovered) - ifelse(is.na(Deaths), 0, Deaths))
  #      
       

  results$dataframeTotalAb <- covid_communes
  
  covid_cities <- cities%>%
    dplyr::group_by(city) %>%
    dplyr::summarise(Confirmed=sum(Confirmed,na.rm = TRUE),Recovered=sum(Recovery,na.rm = TRUE),
                     Deaths=sum(Deaths,na.rm = TRUE)) %>%
    dplyr::mutate(Unrecovered = Confirmed - ifelse(is.na(Recovered), 0, Recovered) - ifelse(is.na(Deaths), 0, Deaths))
  results$dataframeCities <- covid_cities           
  
  
  dataframeTotalCI <- covid_ci %>%
    dplyr::group_by(countryName) %>%
    slice(n()) %>%
    ungroup() %>%
    dplyr::summarise(Confirmed=sum(Confirmed,na.rm = TRUE),Recovered=sum(Recovered,na.rm = TRUE),
                     Deaths=sum(Deaths,na.rm = TRUE)) %>%
    dplyr::mutate(Unrecovered = Confirmed - ifelse(is.na(Recovered), 0, Recovered) - ifelse(is.na(Deaths), 0, Deaths))
  results$dataframeTotalCI <- dataframeTotalCI
  
  
   dataframeTotal <- coronavirus %>%
     dplyr::group_by(countryName) %>%
    slice(n()) %>%
     ungroup() %>%
     dplyr::mutate(Unrecovered = Confirmed - ifelse(is.na(Recovered), 0, Recovered) - ifelse(is.na(Deaths), 0, Deaths)) %>%
     dplyr::arrange(-Confirmed) %>%
     dplyr::ungroup() %>%
     dplyr::select(-c(date,region,lat,lon))
   results$dataframeTotal = dataframeTotal

  # browser()
  df_daily <- covid_ci %>% 
    dplyr::group_by(date) %>%
    dplyr::summarise(totalConfirmed = sum(Confirmed, na.rm = TRUE),
                     totalRecovered = sum(Recovered,na.rm = TRUE),
                     totalDeaths = sum(Deaths,na.rm = T)
    ) %>%
    dplyr::arrange(date) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(totalUnrecovered = totalConfirmed - totalRecovered - totalDeaths)
  results$dfDaily = df_daily
  
  max_date <- as.Date(max(covid_ci$date)) 
  newCasesCI = covid_ci %>%
    dplyr::filter(date == max_date | date == max_date - 1) %>%
    dplyr::group_by(countryName) %>%
    mutate(ConfirmedNew = Confirmed - shift(Confirmed,1)) %>%
    mutate(RecoveredNew = Recovered - shift(Recovered,1)) %>%
    mutate(DeathsNew = Deaths - shift(Deaths,1)) %>%
    slice(n()) %>%
    ungroup() %>%
    select(countryName,ConfirmedNew,RecoveredNew,DeathsNew)

  results$newCases = newCasesCI
  
  dataframeOldCasesCI = covid_ci %>%
    dplyr::filter(date == max_date - 1) %>%
    dplyr::mutate(Unrecovered = Confirmed - Recovered - Deaths)
  results$dataframeOldCasesCI = dataframeOldCasesCI
  
  dataframeTotalOldCases = coronavirus %>%
    dplyr::filter(date == max_date - 1) %>%
    dplyr::mutate(Unrecovered = Confirmed - Recovered - Deaths) %>%
    summarise(totalConfirmed = sum(Confirmed,na.rm = T),
              totalDeath = sum(Deaths,na.rm = T),
              totalRecovered = sum(Recovered,na.rm = T),
              totalUnrecovered = sum(Unrecovered,na.rm = T)
    )
  results$dataframeTotalOldCases = dataframeTotalOldCases
  dataframeOldCases = coronavirus %>%
    dplyr::filter(date == max_date - 1) %>%
    dplyr::mutate(Unrecovered = Confirmed - Recovered - Deaths)
  results$dataframeOldCases = dataframeOldCases
  
  waiter_hide()
  
})



output$prelevementCount <- renderCountup({
  x = sum(rap_jour$echantillons)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Pr&eacute;l&egrave;vements  ")
  countup(
    x,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})

output$confirmedCount <- renderCountup({
  totalConfirmed = sum(results$dataframeTotalCI$Confirmed,na.rm = T)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Cas Confirm&eacute;s ")
  countup(
    totalConfirmed,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$activeCount <- renderCountup({
  totalUnrecovered = sum(results$dataframeTotalCI$Unrecovered,na.rm = T)
  totalConfirmed = sum(results$dataframeTotalCI$Confirmed,na.rm = T)
  activeCasesPer = round(((totalUnrecovered/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Cas Actifs ",
               suffix = paste0(" (",activeCasesPer,"%)")
  )
  countup(
    totalUnrecovered,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$recoveredCount <- renderCountup({
  totalRecovered = sum(results$dataframeTotalCI$Recovered,na.rm = T)
  totalConfirmed = sum(results$dataframeTotalCI$Confirmed,na.rm = T)
  totalRecoveredPer = round(((totalRecovered/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Cas Gu&eacute;ris ",
               suffix = paste0(" (",totalRecoveredPer,"%)")
  )
  countup(
    totalRecovered,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$deathCount <- renderCountup({
  totalDeath = sum(results$dataframeTotalCI$Deaths,na.rm = T)
  totalConfirmed = sum(results$dataframeTotalCI$Confirmed,na.rm = T)
  totalDeathPer = round(((totalDeath/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "D&eacute;c&egrave;s ",
               suffix = paste0(" (",totalDeathPer,"%)"))
  countup(
    totalDeath,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$countryCount <- renderCountup({
  x = covid_cities %>%
    filter(Confirmed > 0) %>%
    select(City) %>%
    unique() %>% nrow()
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "NOMBRE DE VILLES TOUCHEES: "
  )
  countup(
    x,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})

output$cardUI = renderUI({
  tagList(
    argonRow(
      argonColumn(
        width = 12,
        center = T,
        argonBadge(text = countupOutput("countryCount"), 
                   src = NULL, 
                   pill = T, 
                   status = "danger")
      )
    ),
    tags$br(),
    argonRow(
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("confirmedCount"),
          icon = icon("users"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "warning",
          gradient = T,
          width = 12
        ),
        h6(paste0("Hier: ",
                  prettyNum(results$dataframeOldCasesCI$Confirmed,big.mark = ",")
        ), 
        style = 'text-align:center;
                           font-size:15px;')
      ),
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("activeCount"),
          icon = icon("hospital"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "info",
          gradient = T,
          width = 12
        ),
        h6(paste0("Hier: ",
                  prettyNum(results$dataframeOldCasesCI$Unrecovered,big.mark = ",")
        ), 
        style = 'text-align:center;
                           font-size:15px;')
      ),
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("recoveredCount"),
          icon = icon("smile"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "success",
          gradient = T,
          width = 12
        ),
        h6(paste0("Hier: ",
                  prettyNum(results$dataframeOldCasesCI$Recovered,big.mark = ",")
        ), 
        style = 'text-align:center;
                           font-size:15px;')
      ),
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("deathCount"),
          icon = icon("heartbeat"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "danger",
          gradient = T,
          width = 12
        ),
        h6(paste0("Hier: ",
                  prettyNum(results$dataframeOldCasesCI$Death,big.mark = ",")
        ), 
        style = 'text-align:center;
                           font-size:15px;')
      )
    )
  )
})

output$chart2UI = renderUI({
  # tmp = data.frame(Commune="Total",Confirmed=sum(results$dataframeTotalAb$Confirmed),Recovered=sum(results$dataframeTotalAb$Recovered),
  #                  Deaths=sum(results$dataframeTotalAb$Deaths),Unrecovered=sum(results$dataframeTotalAb$Unrecovered))
  # results$dataframeWaterFall=rbind(results$dataframeTotalAb,tmp)
  tagList(
    argonRow(
      argonColumn(
        width = 6,
        argonRow(
          center = T,
          h3("Autre diagramme")
          # argonColumn(
          #   width = 6,
          #   pickerInput(
          #     inputId = "communeWaterfallInput",
          #     label = strong("Point par zone (choisir une zone):"),
          #     choices = as.character(results$dataframeWaterFall$Commune),
          #     selected = "Total",
          #     width = "80%",
          #     options = list(`live-search` = TRUE),
          #     inline = F
          #   )
          # )
        ),
        argonRow(
          
          highchartOutput("communeWaterfallPlot",width = "100%") %>% withSpinner()
        )
      ),
      argonColumn(
        width = 1
      ),
      argonColumn(
        width = 5,
        h4("Situation à Abidjan"),
        girafeOutput("mapAbjPlot") %>% withSpinner()
        # argonRow(
        #   center = T,
        #   argonColumn(
        #     width = 12,
        #     argonCard(
        #       status = "info",
        #       width = 12,
        #       title = "INFOS",
        #       hover_lift = T,
        #       shadow = T,
        #       icon = argonIcon("check-bold"),
        #     cont
        #     )
        #   )
        #   #highchartOutput("depistagePlot",width = "100%") %>% withSpinner()
        #   
        #   
        # )
      )
      
      
    
      
    ),
    tags$hr(),

    h3(argonIcon("pin-3"),"Les villes touchées")
    )

})

output$mapAbjPlot <- renderggiraph({
  df <-covid_communes %>% 
    mutate(tips = paste0("<b>",Commune, "</b> <br/>",
                         "<b>Confirmés :", Confirmed, "</b> <br/>",
                         "<b>Guéris :", Recovered, "</b><br/>",
                         "<b>Décès :", Deaths, "</b>")) %>%
    select(Commune, Confirmed, tips)
  
  df1 <- ab_map %>% left_join(df, by=c("Commune"))
  df1$Confirmed[df1$Commune=="BANCO"] <- 0
  
  gg <- ggplot(df1, aes( x = Long, y = Lat ) ) +
    geom_polygon_interactive(aes(fill = as.factor(Confirmed), group = group,
                                 tooltip = tips, data_id = Commune), color = "black") +
    geom_text(data = center, aes(x = Long, y = Lat, label = Commune), size = 4) +
    theme_void()+
    scale_fill_manual(values=c("forestgreen",colorRampPalette(c("#FFFFCC", "#FFA083"), space = "Lab")(9)),
                      guide = guide_legend(title = "Nombre de cas"),
                      breaks = c("2","11","14","16","19","23","35","264","284"),na.value = "powderblue")
  
  girafe(ggobj = gg, width_svg = 10, height_svg =9)
  # theme(legend.position =  "none")
  
})

output$chartUI = renderUI({
  tagList(
    argonRow(
      argonColumn(
        width = 7,
        argonRow(
          # argonColumn(
          #   width = 5,
          #   # tags$strong("Country specific plots"),
          #   dateRangeInput( inputId = "dateRange", 
          #                   label = NULL,
          #                   start = min(covid_ci$date),
          #                   end   = Sys.Date() - 1,
          #                   min = min(covid_ci$date),
          #                   max   = Sys.Date() - 1,
          #                   format = "dd-mm-yyyy",
          #                   width = "100%"
          #   )
          # ),
          argonColumn(
            width = 3,
            prettySwitch(inputId = "scaleCountry",
                         label = "Echelle Logarthimic",
                         value = F,
                         status = "primary",
                         fill = T,
                         inline = T,
                         width = "100%"
            )
          ),
          argonColumn(
            width = 1,
            offset = 1,
            dropdownButton(
              tagList(
                prettyRadioButtons(
                  inputId = "countryPlotOptions",
                  label = NULL,
                  choices = setNames(c(1:4),c("CAS CUMULES",
                                              paste0("NOUVEAUX CAS (",Sys.Date() - 1,")"),
                                              paste0("DECES (",Sys.Date() - 1,")"),
                                              paste0("GUERIS (",Sys.Date() - 1,")")
                  )
                  ),
                  selected = "1",
                  shape = c("round"),
                  outline = T,
                  fill = T,
                  width = "100%"
                )
              ),
              status = "primary",
              size = "sm",
              circle = T,
              icon = icon("wrench"),
              right = T,
              margin = "10px",
              inputId = "countryPlotOptionsDropdown"
            ),
            bsPopover("countryPlotOptionsDropdown", title = NULL, content = "Cliquez pour choisir une option de graphe", placement = "left", trigger = "hover",
                      options = NULL)
          )
        ),
        argonRow(
          argonColumn(
            width = 12,
            div(
              id = "cumulativeCountryPlotDiv",
              highchartOutput("cumulativeCountryPlot",width = "100%") %>% withSpinner()
            ),
            hidden(
              div(
                id = "newCasesCountryPlotDiv",
                highchartOutput("newCasesCountryPlot",width = "100%") %>% withSpinner()
              )
            ),
            hidden(
              div(
                id = "newCasesDeathsCountryPlotDiv",
                highchartOutput("newCasesDeathCountryPlot",width = "100%") %>% withSpinner()
              )
            ),
            hidden(
              div(
                id = "newCasesRecoveredCountryPlotDiv",
                highchartOutput("newCasesRecoveredCountryPlot",width = "100%") %>% withSpinner()
              )
            )
          )
        )
      ),
      argonColumn(
        width = 5,
        argonRow(
          argonColumn(
            width = 7,
            tags$strong("Cartographie des villes"),
            tags$br(),
            p("Cliquez sur zone de la carte pour plus d'infos")
           )
        ),
        leafletOutput("cimap"),
        #highchartOutput("worldMap",width = "100%") %>% withSpinner()
        argonRow(
          center = T,
          p("Aucun cas enregistré à l'intérieur du pays depuis le 21-04-2020")
        )
      )
    ),
    tags$hr(),
    
       h3("Proportions en fonction du total des prélèvements"),
  
    argonRow(
      center = T,
      argonColumn(
        width = 4,
        argonInfoCard(
          value = countupOutput("prelevementCount"),
          icon = icon("ambulance"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "white",
          gradient = T,
          width = 12
        )
        
      ),
      # argonColumn(
      #   width = 3,
      #   argonBadge(
      #     text = paste0("Cas Actifs = ",
      #                   prettyNum(sum(results$dataframeTotal$Unrecovered,na.rm = T),big.mark = ",")),
      #     pill = T,
      #     status = "primary"
      #   )
      # ),
      argonColumn(
        width = 4,
        argonProgress(value = round(sum(rap_jour$confirmes)/sum(rap_jour$echantillons)*100,2), status = "danger",text = "Infectes")
      ),
      argonColumn(
        
        width = 4,
        argonProgress(value = round(100-sum(rap_jour$confirmes)/sum(rap_jour$echantillons)*100,2), status = "success",text = "Sains")
      )
    ),
    tags$br(),
    argonRow(
      argonColumn(
        width = 12,
        highchartOutput("depistagePlot") %>% withSpinner()
      )
    ),
    tags$br(),
    argonRow(
      argonColumn(
        width = 12,
        highchartOutput("depistagePlot2") %>% withSpinner()
      )
    )
    
    
      
  )
  
  
})



output$cimap <- renderLeaflet({
  # req(!is.null(results$dataframeTotal))
  # canvasClickFunction <- JS("function(event) {Shiny.setInputValue('canvasClicked', [event.point.name]);}")
  # x = input$highchartOption %>% as.numeric()
  # #data = regions %>% dplyr::group_by(code) %>% 
  #  # dplyr::summarise(Confirmed=sum(value))
  # # %>% 
  # #        filter(str_detect(tolower(countryName), pattern = paste(y,collapse = "|"))) 
  # value = switch(x,"Confirmed","Recovered","Deaths","Unrecovered")
 
   # leaflet(mapdata) %>% addTiles() %>%
   #   addCircles(lng = ~lng, lat = ~lat, weight = 1,
   #              radius = ~sqrt(Confirmed) * 2000, popup = ~paste(city, ":", Confirmed),
   #              color = "#a500a5", 
   #              fillOpacity = 0.5)
  
  # corona_ci <- civ_states
  # pal_total <- colorNumeric(palette="viridis", domain = corona_ci$Confirmed, na.color="transparent")
  # 
  # 
  # map_label <- paste0("     ","<strong>",corona_ci$name,"</strong>","<br/>","Cas Confirmes: ",corona_ci$Confirmed,"<br/>",
  #                     "Gueris: ",corona_ci$Recovered,"<br/>","Deces: ",corona_ci$Deaths)%>% lapply(htmltools::HTML)
  # 
  # leaflet(civ_states, options = leafletOptions(zoomControl = FALSE)) %>%
  #   setView(-5, 8, zoom = 6) %>% 
  #   addProviderTiles(providers$Stamen.TonerLite,
  #                    options = providerTileOptions(noWrap = TRUE) 
  #   )%>% 
  #   
  #   addPolygons(group = "Cas Confirmés",
  #               weight = 2,
  #               fillColor = ~pal_total(Confirmed),
  #               fillOpacity = 0.6,
  #               label = map_label,
  #               labelOptions = labelOptions(
  #                 style = list("font-weight" = "normal", padding = "3px 8px"),
  #                 textsize = "15px",
  #                 direction = "auto"))
  
  
  bins <- c(0,1,3,5, Inf)
  pal <- colorBin("YlOrRd", domain = tmp$Confirmed,bins = bins,pretty = FALSE)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>Cas confirm&eacute;s: %g<br/>Gu&eacute;ris: %g<br/>D&eacute;c&egrave;s: %g",
    ci_map$name, ci_map$Confirmed, ci_map$Recovered, ci_map$Deaths
  ) %>% lapply(htmltools::HTML)
  
  leaflet(ci_map,options = leafletOptions(zoomControl = TRUE)) %>%
    setView(-5,8, 6) %>%
    addTiles("Esri.WorldTopoMap") %>% 
    addPolygons(fillColor = ~pal(Confirmed),
                weight = 2,
                opacity = 1,
                color = "grey",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(weight = 5,
                                             color = "#666",
                                             dashArray = "",
                                             fillOpacity = 0.7,
                                             bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "15px",
                                            direction = "auto")) %>%
    addLegend(pal = pal, values = ~density, opacity = 0.7, 
              title = "Cas confirm&eacute;s", position = "bottomright")
})

output$cumulativePlot = renderHighchart({
  confirmed_color = "#172b4d"
  active_color <- "#1f77b4"
  recovered_color <- "forestgreen"
  death_color <- "red"
  df_daily = results$dfDaily
  x = max(df_daily$totalConfirmed,df_daily$totalUnrecovered,df_daily$totalDeaths,df_daily$totalRecovered)
  y = nchar(x) - 1
  yLimit = x %>% round(-y)
  hc <- highchart() %>% 
    hc_subtitle(text = "Cas cumulés",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = df_daily$date) %>%
    hc_yAxis(title = list(text = "Nombre de personnes")) %>%
    hc_add_series(name = "Confirmés",data = df_daily$totalConfirmed) %>% 
    hc_add_series(name = "Actifs",data = df_daily$totalUnrecovered) %>% 
    hc_add_series(name = "Guéris", data = df_daily$totalRecovered) %>% 
    hc_add_series(name = "Décès", data = df_daily$totalDeaths)
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_exporting(
      enabled = TRUE
    ) %>%
    hc_colors(c(confirmed_color,active_color, recovered_color ,death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$totalCasesPlot = renderHighchart({
  confirmed_color = "#172b4d"
  active_color <- "#1f77b4"
  recovered_color <- "forestgreen"
  death_color <- "red"
  df_daily <- covid_ci %>% 
    dplyr::group_by(date) %>%
    dplyr::summarise(totalConfirmed = sum(Confirmed, na.rm = TRUE),
                     totalRecovered = sum(Recovered,na.rm = TRUE),
                     totalDeaths = sum(Deaths,na.rm = T)
    ) %>%
    dplyr::arrange(date) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(totalUnrecovered = totalConfirmed - totalRecovered - totalDeaths) #%>%
    #filter(date >= input$dateRange[1] & date <= input$dateRange[2])
  scale = ifelse(input$scaleCountry,"logarithmic","linear")
  hc <- highchart() %>% 
    hc_subtitle(text = paste0("Cas Cumulatifs en Cote d'Ivoire "),
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = df_daily$date) %>%
    hc_yAxis(title = list(text = "NOMBRE DE CAS"),type = scale) %>%
    hc_add_series(name = "Confirmés",data = df_daily$totalConfirmed) %>% 
    hc_add_series(name = "Actifs",data = df_daily$totalUnrecovered) %>% 
    hc_add_series(name = "Guéris", data = df_daily$totalRecovered) %>% 
    hc_add_series(name = "Décès", data = df_daily$totalDeaths)
  
  hc %>% 
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(confirmed_color,active_color, recovered_color ,death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})


output$communeWaterfallPlot = renderHighchart({
  # req(!is.null(input$communeWaterfallInput))
  # x=results$dataframeWaterFall %>%
  #   dplyr::filter(Commune==input$communeWaterfallInput)
  # 
  # df2 = data.frame(Name = c("Actifs","Gueris","Deces","Confirmes"),
  #                  Value = c(x$Unrecovered,x$Recovered,x$Deaths,x$Confirmed))
  # 
  # val = c(x$Unrecovered,x$Recovered,x$Deaths,x$Confirmed)
  # val = val/val[1]*100
  # 
  # df2 %>% 
  #   dplyr::mutate(percent=val)
  totalConfirmed = sum(results$dataframeTotalCI$Confirmed,na.rm = T)
  totalUnrecovered = sum(results$dataframeTotalCI$Unrecovered,na.rm = T)
  totalRecovered = sum(results$dataframeTotalCI$Recovered,na.rm = T)
  totalDeath = sum(results$dataframeTotalCI$Deaths,na.rm = T)
  
  df2 = data.frame(Name = c("Actifs","Guéris","Décès","Confirmés"),
                            Value = c(totalUnrecovered,totalRecovered,totalDeath,totalConfirmed))
  
  confirmed_color = "orange"
  actif_color = "blue"
  recovered_color = "green"
  death_color = "red"
  
  highchart() %>%
    hc_title(text = "") %>%
    #hc_subtitle(text = input$communeWaterfallInput) %>%
    hc_xAxis(categories = df2$Name,
             title=list(text="Cas")) %>%
    hc_yAxis(title=list(text="Nombre de persones")) %>%
    hc_legend(enabled=FALSE) %>% 
    hc_add_series(data = df2, type = "waterfall", 
                  hcaes(x = Name, y= Value, isSum = c(FALSE,FALSE,FALSE,TRUE),
                        color=c("orange","blue","red","grey")), 
                  size=10,                                     
                  dataLabels=list(
                    enabled=TRUE,
                    #format = '{point.percentage:.1f}',
                    style=list(
                      color="#FFFFFF",
                      fontWeight="bold",
                      textShadow="0px 0px 3px black"
                    )
                  ))%>%
    # hc_exporting(
    #   enabled = FALSE
    # ) %>%
    # hc_colors(c(actif_color,recovered_color,death_color,confirmed_color)) %>%
    hc_tooltip(pointFormat="<span style=\"color:{series.color}\">{point.y:,.0f}<br/></span>",
               shared = TRUE)
})

output$depistagePlot = renderHighchart({
  echantillon_color = "#1f77b4"
  confirme_color <- "orange"
  hc <- highchart() %>% 
    hc_subtitle(text = "",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = rap_jour$date) %>%
    hc_yAxis(title = list(text = "Nombre de personnes")) %>%
    hc_add_series(name = "échantillons",data = rap_jour$echantillons) %>%
    hc_add_series(name = "Cas confirmés",data = rap_jour$confirmes)
  
  hc %>% 
    hc_chart(type = "area") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_exporting(
      enabled = TRUE
    ) %>%
    hc_colors(c(echantillon_color,confirme_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
  
  
  })

output$depistagePlot2 = renderHighchart({
  echantillon_color = "#1f77b4"
  confirme_color <- "orange"
  hc <- highchart() %>% 
    hc_subtitle(text = "",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = rap_jour$date) %>%
    hc_yAxis(title = list(text = "Nombre de personnes")) %>%
    hc_add_series(name = "échantillons",data = rap_jour$echantillons) %>%
    hc_add_series(name = "Cas confirmés",data = rap_jour$confirmes)
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_exporting(
      enabled = TRUE
    ) %>%
    hc_colors(c(echantillon_color,confirme_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
  
  
})


output$dataTableCityWise = renderDataTable({
  x = results$dataframeCities %>%
    arrange(desc(Confirmed)) %>%
    mutate(totalActivePer = Unrecovered/Confirmed) %>%
    mutate(totalRecoveredPer = Recovered/Confirmed) %>%
    mutate(totalDeathPer = Deaths/Confirmed) %>%
    select(Villes = city , Confirmes = Confirmed, Actifs = Unrecovered,Gueris = Recovered,Morts = Deaths,"Actif (%)" = totalActivePer,"Gueri (%)" = totalRecoveredPer,"Mort (%)" = totalDeathPer)
  datatable(x,
            extensions = 'Buttons',
            rownames = FALSE,
            filter = 'top',
            options = list(
              searchHighlight = TRUE,
              pageLength = 25,
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons =
                list(
                  list(
                    extend = 'collection',
                    buttons = c('csv', 'pdf'),
                    text = 'Telecharger'
                  )
                )

            )

  ) %>%
    formatPercentage('Actif (%)',2) %>%
    formatPercentage('Gueri (%)',2) %>%
    formatPercentage('Mort (%)',2) %>%
    formatStyle(
      'Actif (%)',
      background = styleColorBar(x$'Actif (%)', '#31bed4'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>%
    formatStyle(
      'Gueri (%)',
      background = styleColorBar(x$'Gueri (%)', '#8bd431'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>%
    formatStyle(
      'Mort (%)',
      background = styleColorBar(x$'Mort (%)', '#ff5757'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )
})




output$cumulativeCountryPlot = renderHighchart({
  confirmed_color = "#172b4d"
  active_color <- "#1f77b4"
  recovered_color <- "forestgreen"
  death_color <- "red"
  df_daily <- covid_ci %>% 
    dplyr::group_by(date) %>%
    dplyr::summarise(totalConfirmed = sum(Confirmed, na.rm = TRUE),
                     totalRecovered = sum(Recovered,na.rm = TRUE),
                     totalDeaths = sum(Deaths,na.rm = T)
    ) %>%
    dplyr::arrange(date) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(totalUnrecovered = totalConfirmed - totalRecovered - totalDeaths) #%>%
    #filter(date >= input$dateRange[1] & date <= input$dateRange[2])
  scale = ifelse(input$scaleCountry,"logarithmic","linear")
  hc <- highchart() %>% 
    hc_subtitle(text = paste0("CUMULES DES CAS "),
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = df_daily$date) %>%
    hc_yAxis(title = list(text = "Cas Cumulatifs"),type = scale) %>%
    hc_add_series(name = "Confirmés",data = df_daily$totalConfirmed) %>% 
    hc_add_series(name = "Actifs",data = df_daily$totalUnrecovered) %>% 
    hc_add_series(name = "Guéris", data = df_daily$totalRecovered) %>% 
    hc_add_series(name = "Décès", data = df_daily$totalDeaths)
  
  hc %>% 
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(confirmed_color,active_color, recovered_color ,death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$newCasesCountryPlot = renderHighchart({
  newCases = results$dataframeFinal %>% 
    select(date,countryName,Confirmed) %>%
    mutate(confirmedDaily = if_else(is.na(Confirmed - shift(Confirmed,1)),
                                    Confirmed,
                                    Confirmed - shift(Confirmed,1)
    )
    
    ) #%>% 
    #filter(date >= input$dateRange[1] & date <= input$dateRange[2])
  scale = ifelse(input$scaleCountry,"logarithmic","linear") 
  x = newCases
  death_color <- "orange"
  hc <- highchart() %>% 
    hc_subtitle(text = "Cas confirmés",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$date) %>%
    hc_yAxis(title = list(text = "Nouveaux Cas"),type = scale) %>%
    hc_add_series(name = "Nouveaux Cas",data = x$confirmedDaily) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$newCasesDeathCountryPlot = renderHighchart({
  newCases = results$dataframeFinal %>% 
    select(date,countryName,Deaths) %>%
    mutate(deathDaily = if_else(is.na(Deaths - shift(Deaths,1)),
                                Deaths,
                                Deaths - shift(Deaths,1)
    )
    
    ) #%>% 
    #filter(date >= input$dateRange[1] & date <= input$dateRange[2])
  scale = ifelse(input$scaleCountry,"logarithmic","linear")
  x = newCases
  death_color <- "red"
  hc <- highchart() %>% 
    hc_subtitle(text = "Décès",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$date) %>%
    hc_yAxis(title = list(text = "Nombre de personnes"), type = scale) %>%
    hc_add_series(name = "Décès",data = x$deathDaily) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$newCasesRecoveredCountryPlot = renderHighchart({
  newCases = results$dataframeFinal %>% 
    select(date,countryName,Recovered) %>%
    mutate(recoveredDaily = if_else(is.na(Recovered - shift(Recovered,1)),
                                    Recovered,
                                    Recovered - shift(Recovered,1)
    )
    
    ) #%>% 
    #filter(date >= input$dateRange[1] & date <= input$dateRange[2])
  scale = ifelse(input$scaleCountry,"logarithmic","linear")
  x = newCases
  death_color <- "green"
  hc <- highchart() %>% 
    hc_subtitle(text = "Gueris",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$date) %>%
    hc_yAxis(title = list(text = "Guéris"),type = scale) %>%
    hc_add_series(name = "Nombre de Guéris",data = x$recoveredDaily) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

#### Country Comparision tab ----

output$countryComparisionChartsUI = renderUI({
  tagList(
    argonRow(
      argonColumn(
        width = 12,
        selectInput(
          inputId = "countryCompareList",
          label = strong("Sélectionner les communes à comparer"),
          choices = results$dataframeTotalAb$Commune,
          selected = results$dataframeTotalAb$Commune[c(1:5)],
          selectize = T,
          multiple = T,
          width = "100%"
        )
      ),
      # argonColumn(
      #   width = 3,
      #   dropdownButton(
      #     prettyRadioButtons(
      #       inputId = "countryCompareTypeofPlot",
      #       label = NULL,
      #       choices = setNames(c(1:5),c("CAS CUMULES",
      #                                   "CAS CONFIRMES",
      #                                   "ACTIFS",
      #                                   "GUERIS",
      #                                   "DECES")),
      #       selected = "1",
      #       shape = c("round"),
      #       outline = T,
      #       fill = T,
      #       width = "100%"
      #     ),
      #     label = "Options de comparaisons:",
      #     status = "primary",
      #     circle = F,
      #     icon = icon("wrench"),
      #     right = T,
      #     margin = "10px",
      #     inputId = "countryCompareTypeofPlotDropdown"
      #   )
      # ),
      # argonColumn(
      #   width = 4,
      #   dateRangeInput( inputId = "dateRangeCompare", 
      #                   label = NULL,
      #                   start = min(covid_ci$date),
      #                   end   = Sys.Date() - 1,
      #                   min = min(covid_ci$date),
      #                   max   = Sys.Date() - 1,
      #                   format = "dd-mm-yyyy",
      #                   width = "100%"
      #   )
      # ),
      argonColumn(
        width = 5,
        offset = 3,
        prettySwitch(inputId = "scaleCountryCompare",
                     label = "Echelle Logorithmique",
                     value = F,
                     status = "primary",
                     fill = T,
                     inline = T,
                     width = "100%"
        )
      )
    ),
    tags$hr(),
    argonRow(
      argonColumn(
        width = 12,
        highchartOutput("countryCompareChart") %>% withSpinner()
      )
    )
  )
})

output$countryCompareChart = renderHighchart({
  req(!is.null(input$countryCompareList))
  countryList = input$countryCompareList
  scale = ifelse(input$scaleCountryCompare,"logarithmic","linear")
  # dateRange = input$dateRangeCompare
  # plotType = switch(as.numeric(input$countryCompareTypeofPlot),
  #                    "CAS CUMULES",
  #                     "CAS CONFIRMES",
  #                     "ACTIFS",
  #                     "GUERIS",
  #                     "DECES")
  # if (plotType != "CAS CUMULES") {
  #   data =  covid_commune %>%
  #     filter(Commune %in% countryList) %>%
  #     #filter(date >= dateRange[1] & date <= dateRange[2]) %>%
  #     mutate(Active = Confirmed - Recovered - Deaths) %>%
  #     select(Commune,plotType)
  #   hc <- highchart() %>% 
  #     hc_subtitle(text = paste0("Comparaison ",plotType," cas pour ",length(countryList)," communes"),
  #                 align = "left",
  #                 style = list(color = "#2b908f", fontWeight = "bold")) %>%
  #     hc_xAxis(categories = data$date) %>%
  #     hc_yAxis(title = list(text = paste0(plotType," cas")),
  #              type = scale)
  #   for (i in 1:length(countryList)) {
  #     hc = hc_add_series(hc,
  #                        name = countryList[i],
  #                        data = data %>%
  #                          filter(Commune == countryList[i]) %>%
  #                          .[,3]
  #     )
  #   }
  #   hc %>% 
  #     hc_chart(borderColor = '#EBBA95',
  #              borderRadius = 10,
  #              borderWidth = 2
  #     ) %>%
  #     hc_exporting(
  #       enabled = TRUE
  #     ) %>%
  #     hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
  #                shared = TRUE, borderWidth = 5,table = T)
  # } else {
    data = results$dataframeTotalAb %>%
      filter(Commune %in% countryList)
    confirmed_color = "#172b4d"
    active_color <- "#1f77b4"
    recovered_color <- "forestgreen"
    death_color <- "red"
    hc <- highchart() %>% 
      hc_subtitle(text = paste0("Comparaison entre ",length(countryList)," communes"),
                  align = "left",
                  style = list(color = "#2b908f", fontWeight = "bold")) %>%
      hc_xAxis(categories = data$Commune) %>%
      hc_yAxis(title = list(text = "Cas cumulés"),type = scale) %>%
      hc_add_series(name = "Confirmés",data = data$Confirmed) %>% 
      hc_add_series(name = "Actifs",data = data$Unrecovered) %>% 
      hc_add_series(name = "Guéris", data = data$Recovered) %>% 
      hc_add_series(name = "Décès", data = data$Deaths)
    hc %>% 
      hc_chart(type = "column") %>%
      hc_chart(borderColor = '#EBBA95',
               borderRadius = 10,
               borderWidth = 2
      ) %>%
      hc_exporting(
        enabled = TRUE
      ) %>%
      hc_colors(c(confirmed_color,active_color, recovered_color ,death_color)) %>%
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 5,table = T)
  #}
  
})

output$dataTableCountryCompare = renderDataTable({
  req(!is.null(input$countryCompareList))
  countryList = input$countryCompareList
  x = results$dataframeTotalAb %>%
    filter(Commune %in% countryList) %>% 
    mutate(Unrecovered = Confirmed - ifelse(is.na(Recovered), 0, Recovered) - ifelse(is.na(Deaths), 0, Deaths)) %>% 
    arrange(desc(Confirmed)) %>%
    mutate(totalActivePer = Unrecovered/Confirmed) %>%
    mutate(totalRecoveredPer = Recovered/Confirmed) %>%
    mutate(totalDeathPer = Deaths/Confirmed) %>%
    select(COMMUNE = Commune, CONFIRMES = Confirmed, ACTIFS = Unrecovered,GUERIS = Recovered,DECES = Deaths,"ACTIFS (%)" = totalActivePer,"GUERIS (%)" = totalRecoveredPer,"DECES (%)" = totalDeathPer)

  datatable(x,
            extensions = 'Buttons',
            rownames = FALSE,
            filter = 'top',
            options = list(
              searchHighlight = TRUE,
              pageLength = 25,
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons =
                list(
                  list(
                    extend = 'collection',
                    buttons = c('csv', 'pdf'),
                    text = 'TELECHARGER'
                  )
                )
              
            )
            
  ) %>%
    formatPercentage('ACTIFS (%)',2) %>%
    formatPercentage('GUERIS (%)',2) %>%
    formatPercentage('DECES (%)',2) %>%
    formatStyle(
      'ACTIFS (%)',
      background = styleColorBar(x$'ACTIFS (%)', '#31bed4'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>%
    formatStyle(
      'GUERIS (%)',
      background = styleColorBar(x$'GUERIS (%)', '#8bd431'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>%
    formatStyle(
      'DECES (%)',
      background = styleColorBar(x$'DECES (%)', '#ff5757'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )
})

####  World cases ----
output$world = renderUI({
  tagList(
    h3("Situation dans le monde"),
    tags$br(),
    argonRow(
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("confirmedCountWorld"),
          icon = icon("users"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "warning",
          gradient = T,
          width = 12
        ),
        h6(paste0("Hier: ",
                  prettyNum(results$dataframeTotalOldCases$totalConfirmed,big.mark = ",")
        ), 
        style = 'text-align:center;
                           font-size:15px;')
      ),
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("activeCountWorld"),
          icon = icon("hospital"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "info",
          gradient = T,
          width = 12
        ),
        h6(paste0("Hier: ",
                  prettyNum(results$dataframeTotalOldCases$totalUnrecovered,big.mark = ",")
        ), 
        style = 'text-align:center;
                           font-size:15px;')
      ),
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("recoveredCountWorld"),
          icon = icon("smile"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "success",
          gradient = T,
          width = 12
        ),
        h6(paste0("Hier: ",
                  prettyNum(results$dataframeTotalOldCases$totalRecovered,big.mark = ",")
        ), 
        style = 'text-align:center;
                           font-size:15px;')
      ),
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("deathCountWorld"),
          icon = icon("heartbeat"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "danger",
          gradient = T,
          width = 12
        ),
        h6(paste0("Hier: ",
                  prettyNum(results$dataframeTotalOldCases$totalDeath,big.mark = ",")
        ), 
        style = 'text-align:center;
                           font-size:15px;')
      )
    ),
    tags$br(),
    argonRow(
      argonColumn(
        width = 8,
        tags$strong("Cliquez sur un pays pour voir les détails")
      ),
      argonColumn(
        width = 1,
        dropdownButton(
          tagList(
            prettyRadioButtons(
              inputId = "highchartOption",
              label = NULL,
              choices = setNames(c(1:4),c("CAS TOTAL","CAS GUERIS","DECES","CAS ACTIFS")),
              selected = "1",
              shape = c("round"),
              outline = T,
              fill = T,
              width = "100%"
            )
          ),
          status = "primary",
          size = "sm",
          circle = T,
          icon = icon("wrench"),
          right = T,
          margin = "10px",
          inputId = "worldMapOption"
        ),
        bsPopover("worldMapOption", title = NULL, content = "Cliquez pour voir les options", placement = "left", trigger = "hover",
                  options = NULL)
      )
    ),
    argonRow(
      argonColumn(
        width = 12,
        highchartOutput("worldMap") %>% withSpinner()
      )
    )
    # tags$br(),
    # argonRow(
    #   argonColumn(
    #     width = 5,
    #     pickerInput(
    #       inputId = "habitationInputList",
    #       label = strong("Selectionnez la(les) commune(s) d'habitation(s):"),
    #       choices = as.vector(unique(trackingDF$hab)),
    #       selected = "abobo",
    #       width = "100%",
    #       options = list(`actions-box` = TRUE,
    #                      `live-search` = TRUE
    #       ),
    #       multiple = T
    #     )
    #   ),
    #   argonColumn(
    #     width = 4,
    #     pickerInput(
    #       inputId = "travailInputList",
    #       label = strong("Selectionnez la(les) commune(s) de travail:"),
    #       choices = as.vector(unique(trackingDF$trav)),
    #       selected = "cocody",
    #       width = "100%",
    #       multiple = T
    #     )
    #   ),
    #   argonColumn(
    #     width = 3,
    #     tags$br(),
    #     actionBttn(
    #       inputId = "runTracking",
    #       label = "Demarrer le Tracking",
    #       color = "warning",
    #       block = T
    #     )
    #   )
    # ),
    # tags$hr(),
    # argonRow(
    #   argonColumn(
    #     width = 7,
    #     plotlyOutput("mapPlot") %>% withSpinner()
    #   ),
    #   argonColumn(
    #     width = 4,
    #     dataTableOutput("tableTracking") %>% withSpinner()
    #   )
    #   
    # )
  )
})

#------------- World Map

output$worldMap = renderHighchart({
  req(!is.null(results$dataframeTotal))
  canvasClickFunction <- JS("function(event) {Shiny.setInputValue('canvasClicked', [event.point.name]);}")
  x = input$highchartOption %>% as.numeric()
  data = results$dataframeTotal 
  # %>% 
  #        filter(str_detect(tolower(countryName), pattern = paste(y,collapse = "|"))) 
  value = switch(x,"Confirmed","Recovered","Deaths","Unrecovered")
  colnames(data)[5] = "name"
  highchart(type = "map",width = "100%",height = "100%") %>%
    hc_add_series_map(map = worldgeojson, df = data, value = value, joinBy = "name") %>%
    hc_colorAxis(stops = color_stops(5)) %>%
    hc_tooltip(useHTML = TRUE,
               headerFormat = '',
               pointFormat = paste0('{point.name}: {point.',value,'} ')) %>%
    hc_exporting(enabled = TRUE,filename = value) %>% 
    hc_add_theme(hc_theme_ffx()) %>%
    hc_chart(zoomType = "xy") %>%
    hc_mapNavigation(enabled = TRUE) %>%
    hc_plotOptions(series = list( 
      events = list(click = canvasClickFunction),allowPointSelect = T))
})

output$confirmedCountWorld <- renderCountup({
  results$dataframeFinal = coronavirus
  dataframeTotal <- coronavirus %>% 
    dplyr::group_by(countryName) %>%
    slice(n()) %>%
    ungroup() %>%
    dplyr::mutate(Unrecovered = Confirmed - ifelse(is.na(Recovered), 0, Recovered) - ifelse(is.na(Deaths), 0, Deaths)) %>%
    dplyr::arrange(-Confirmed) %>%
    dplyr::ungroup() %>%
    select(-c(date,region,lat,lon))
  # browser()
  results$dataframeTotal = dataframeTotal
  totalConfirmed = sum(results$dataframeTotal$Confirmed,na.rm = T)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Confirm&eacute;s ")
  countup(
    totalConfirmed,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$activeCountWorld <- renderCountup({
  totalUnrecovered = sum(results$dataframeTotal$Unrecovered,na.rm = T)
  totalConfirmed = sum(results$dataframeTotal$Confirmed,na.rm = T)
  activeCasesPer = round(((totalUnrecovered/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Actifs ",
               suffix = paste0(" (",activeCasesPer,"%)")
  )
  countup(
    totalUnrecovered,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$recoveredCountWorld <- renderCountup({
  totalRecovered = sum(results$dataframeTotal$Recovered,na.rm = T)
  totalConfirmed = sum(results$dataframeTotal$Confirmed,na.rm = T)
  totalRecoveredPer = round(((totalRecovered/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Gu&eacute;ris ",
               suffix = paste0(" (",totalRecoveredPer,"%)")
  )
  countup(
    totalRecovered,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$deathCountWorld <- renderCountup({
  totalDeath = sum(results$dataframeTotal$Deaths,na.rm = T)
  totalConfirmed = sum(results$dataframeTotal$Confirmed,na.rm = T)
  totalDeathPer = round(((totalDeath/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "D&eacute;c&egrave;s ",
               suffix = paste0(" (",totalDeathPer,"%)"))
  countup(
    totalDeath,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})

#### Forecast UI ----

output$forecastUI = renderUI({
  totalDeath = sum(results$dataframeTotalCI$Deaths,na.rm = T)
  totalConfirmed = sum(results$dataframeTotalCI$Confirmed,na.rm = T)
  totalDeathPer = round(((totalDeath/totalConfirmed)*100),1)
  tagList(
    h3("Prévision de cas d'infection en CI"),
    tags$hr(),
    argonRow(
      center = T,
      # argonColumn(
      #   width = 3,
      #   pickerInput(
      #     inputId = "communeForecastInput",
      #     label = strong("Selectionner une commune:"),
      #     choices = as.vector(results$dataframeTotalAb$Commune),
      #     selected = "Cocody",
      #     width = "100%",
      #     options = list(`live-search` = TRUE),
      #     inline = F
      #   )
      # ),
      # argonColumn(
      #   width = 3,
      #   numericInput(
      #     inputId = "populationInput",
      #     label = strong("Population (N)"),
      #     value = 5000,
      #     min = 0,
      #     width = "100%"
      #   )
      # ),
      argonColumn(
        width = 3,
        numericInput(
          inputId = "fatalityInput",
          label = strong("Taux de moralité (en %)"),
          value = totalDeathPer,
          min = 0,
          width = "100%"
        )
      ),
      argonColumn(
        width = 3,
        numericInput(
          inputId = "severeInput",
          label = strong("Cas grave (en %)"),
          value = 0,
          min = 0,
          width = "100%"
        )
      ),
      argonColumn(
        width = 2,
        dateInput( inputId = "dateForecast", 
                   label = strong("Date de prévision:"),
                   value = Sys.Date() + 10,
                   min = Sys.Date(),
                   format = "dd-mm-yyyy",
                   width = "100%"
        )
      )
    ),
    tags$hr(),
    uiOutput("forecastBadge") %>% withSpinner(),
    tags$br(),
    # argonRow(
    #   argonColumn(
    #     width = 5,
    #     highchartOutput("currentScenario") %>% withSpinner()
    #   ),
    #   argonColumn(
    #     width = 7,
    #     highchartOutput("forecastedScenario") %>% withSpinner()
    #   )
    # 
    # ),
    # tags$hr(),
    # argonRow(
    #   h3("Prevision Generale de Cas d'Infection")
    # ),
    argonRow(
      argonColumn(
        width = 5,
        highchartOutput("currentScenarioGen") %>% withSpinner()
      ),
      argonColumn(
        width = 7,
        highchartOutput("forecastedScenarioGen",width = "100%") %>% withSpinner()
      )
    ),
    tags$br(),
    argonRow(
      tags$strong("Quelques détails")
    ),
    argonRow(
      p("La saisie de certains paramètres est nécessaire pour faire la prévision:"),
      tags$ul(
        tags$li(tags$strong("Le taux de mortalité:"),"représente le taux de mortalité due au COVID-19 en 
                Cote d'Ivoire. Il permettra de prédire le nombre de décès à la date signifiée"),
        tags$li(tags$strong("Cas grave:"),"la proportion des personnes en état grave, ce qui permettra de calculer
                le nombre de personnes qui pourraient être en état grave. Ceci pourrait permettre aux autorités sanitaires de prévoir 
                le nécessaire pour ces personnes."),
        tags$li(tags$strong("La date de prévision:"),"c'est la date à laquelle l'on souhaiterait faire la prévision.")
      )
    ),
    argonRow(
      p("Les paramètres par défaut reflètent de la situation actualle du COVID-19 dans notre pays. Toutefois ils peuvent être modifiés
        par l'utilisateur selon l'évolution de la situation.")
    ),
    argonRow(
      tags$strong("Statistiques plus résumées"),
      tags$ul(
        tags$li(tags$strong("le nombre de reproduction:"),"Le nombre de reproduction donne le nombre moyen de personnes sensibles infectées par chaque personne infectieuse. En d'autres termes, le nombre de reproduction fait référence au nombre de personnes 
                en bonne santé qui sont infectées par nombre de personnes malades"),
        tags$li(tags$strong("le pic de la pandémie"),"le nombre total de personnes infectieuses à la date donnée. "),
        tags$li(tags$strong("le nombre de cas graves")),
        tags$li(tags$strong("le nombre de morts"))
        
      )
    )
      
    
  )
})

observeEvent(input$communeForecastInput,{
  x = results$dataframeTotalAb %>%
    arrange(desc(Confirmed)) %>%
    mutate(totalActivePer = Unrecovered/Confirmed) %>%
    mutate(totalRecoveredPer = Recovered/Confirmed) %>%
    mutate(totalDeathPer = Deaths/Confirmed) %>%
    select(Commune = Commune, Confirmes = Confirmed, Actifs = Unrecovered,Gueris = Recovered,Deces = Deaths,"Actifs (%)" = totalActivePer,"Gueris (%)" = totalRecoveredPer,"Deces (%)" = totalDeathPer)
  results$resultTable = x
  value = pop_coummune %>%
    filter(Commune == input$communeForecastInput)%>%
    .[,2] #%>%
    #gsub(",", "",.) %>%
    #as.numeric()
  valueDeath = results$resultTable %>%
    filter(Commune == input$communeForecastInput) %>%
    .["Deces (%)"] %>%
    as.numeric() * 100 %>%
    round(.,2)
  updateNumericInput(session,
                     inputId = "populationInput",
                     value = value)
  updateNumericInput(session,
                     inputId = "fatalityInput",
                     value = round(valueDeath,2))
})








output$currentScenario = renderHighchart({
  req(!is.null(covid_commune))
  data = covid_commune %>% 
    filter(Commune == input$communeForecastInput) %>%
    filter(Confirmed > 0) %>%
    select(date,Confirmed,Recovered)
  day = 1:(nrow(data))
  lmModel <- augment(lm(log10(data$Confirmed) ~ day, data = data))
  hc <- highchart() %>% 
    hc_subtitle(text = paste0("Cas d'Infection Cumulatif a ",input$communeForecastInput),
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = data$date) %>%
    hc_yAxis(title = list(text = "Infected Cases (Log-scale)"),type = "logarithmic") %>%
    hc_add_series(name = "Actuel",data = data$Confirmed,type = "scatter") %>% 
    hc_add_series(name = "Prévision",data = 10^lmModel$.fitted,type = "line")
  
  hc %>% 
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
  
})

output$forecastedScenario = renderHighchart({
  req(!is.null(covid_commune))
  req(!is.na(input$populationInput))
  active_color <- "#1f77b4"
  recovered_color <- "forestgreen"
  death_color <- "red"
  orange_color <- "#172b4d"
  data = covid_commune %>% 
    filter(Commune == input$communeForecastInput) %>%
    filter(Confirmed > 0) %>%
    select(date,Confirmed,Recovered)
  I = data$Confirmed[1]
  R = data$Recovered[1]
  N = input$populationInput
  Day = 1:length(data$Confirmed)
  SIR <- function(time, state, parameters) {
    par <- as.list(c(state, parameters))
    with(par, {
      dS <- -beta/N * I * S
      dI <- beta/N * I * S - gamma * I
      dR <- gamma * I
      list(c(dS, dI, dR))
    })
  }
  init <- c(S = N - I, I = I, R = R)
  RSS <- function(parameters) {
    names(parameters) <- c("beta", "gamma")
    out <- ode(y = init, times = Day, func = SIR, parms = parameters)
    fit <- out[ , 3]
    sum((data$Confirmed - fit)^2)
  }
  model = optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1),hessian = F)
  modelPar <- setNames(model$par, c("beta", "gamma"))
  t = 1:(input$dateForecast - min(as.Date(data$date)))
  fitValue <- data.frame(ode(y = init, times = t, func = SIR, parms = modelPar))
  fitValue = fitValue %>%
    mutate(date = min(as.Date(data$date)) + time)
  results$modelFit = list(params = modelPar,fitValue = fitValue)
  hc <- highchart() %>% 
    hc_subtitle(text = paste0("Prevision des cas d'infection a ",input$communeForecastInput," by ",input$dateForecast),
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = fitValue$date) %>%
    hc_yAxis(title = list(text = "Predicted Cases (Log-scale)"),type = "logarithmic") %>%
    hc_add_series(name = "Susceptible",data = round(fitValue$S,0)) %>% 
    hc_add_series(name = "Infectes",data = round(fitValue$I,0)) %>%
    hc_add_series(name = "Gueris",data = round(fitValue$R,0)) %>%
    hc_add_series(name = "Actual Infectes",data = data$Confirmed)
  
  hc %>% 
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_exporting(
      enabled = TRUE
    ) %>%
    hc_colors(c(active_color,death_color,recovered_color,orange_color)) %>%
    hc_tooltip(crosshairs = T, backgroundColor = "#FCFFC5",
               shared = T, borderWidth = 5,table = T)
  
})

output$forecastBadge = renderUI({
  req(!is.null(results$modelFit))
  req(!is.null(input$fatalityInput))
  req(!is.null(input$severeInput))
  r0 = results$modelFit$params[1] / results$modelFit$params[2]
  pandemicHeight = max(results$modelFit$fitValue$I)
  deathForecast = pandemicHeight * (input$fatalityInput/100)
  severeForecast = pandemicHeight * (input$severeInput/100)
  argonRow(
    argonColumn(
      width = 3,
      argonBadge(
        text = paste0("Nombre de reproduction = ",round(r0,2)),
        pill = T,
        status = ifelse(as.numeric(r0) < 1,"success",
                        ifelse(as.numeric(r0) < 1.5,"warning",
                               "danger")
        ),
        src = "https://nivedi.res.in/Nadres_v2/ro_material.php"
      )
    ),
    argonColumn(
      width = 1
    ),
    argonColumn(
      width = 3,
      argonBadge(
        text = paste0("Pic de la pandémie = ",prettyNum(round(pandemicHeight,0),big.mark = ",")),
        pill = T,
        status = "primary"
      )
    ),
    argonColumn(
      width = 2,
      argonBadge(
        text = paste0("Pic cas grave = ",prettyNum(round(severeForecast,0),big.mark = ",")),
        pill = T,
        status = "warning"
      )
    ),
    argonColumn(
      width = 2,
      argonBadge(
        text = paste0("pic Deces= ",prettyNum(round(deathForecast,0),big.mark = ",")),
        pill = T,
        status = "danger"
      )
    )
  )
})

######## Prevision Generale --------------------
output$currentScenarioGen = renderHighchart({
  req(!is.null(coronavirus))
  data = coronavirus %>% 
    filter(countryName == "Ivory Coast") %>%
    filter(Confirmed > 0) %>%
    select(date,Confirmed,Recovered)
  day = 1:(nrow(data))
  lmModel <- augment(lm(log10(data$Confirmed) ~ day, data = data))
  hc <- highchart() %>% 
    hc_subtitle(text = paste0("CAS D'INFECTION EN CI "),
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = data$date) %>%
    hc_yAxis(title = list(text = "INFECTES (Log-scale)"),type = "logarithmic") %>%
    hc_add_series(name = "CAS ACTUAL",data = data$Confirmed,type = "scatter") %>% 
    hc_add_series(name = "PREVISION",data = 10^lmModel$.fitted,type = "line")
  
  hc %>% 
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
  
})

output$forecastedScenarioGen = renderHighchart({
  active_color <- "#1f77b4"
  recovered_color <- "forestgreen"
  death_color <- "red"
  orange_color <- "#172b4d"
  data = coronavirus %>% 
    filter(countryName == "Ivory Coast") %>%
    filter(Confirmed > 0) %>%
    select(date,Confirmed,Recovered)
  # Initialisation de valeurs de N, S, I, R
  I = data$Confirmed[1]  # le 1er cas confirmé
  R = data$Recovered[1]  # le 1er gueri
  N = 4395000   # La population d'Abidjan
  Day = 1:length(data$Confirmed)
  SIR <- function(time, state, parameters) {
    par <- as.list(c(state, parameters))
    with(par, {
      dS <- -beta/N * I * S
      dI <- beta/N * I * S - gamma * I
      dR <- gamma * I
      list(c(dS, dI, dR))
    })
  }
  init <- c(S = N - I, I = I, R = R)
  # Ensuite, nous devons définir une fonction pour calculer le RSS , étant donné un ensemble 
  #de valeurs pour β et γ
  RSS <- function(parameters) {
    names(parameters) <- c("beta", "gamma")
    out <- ode(y = init, times = Day, func = SIR, parms = parameters)
    fit <- out[ , 3]
    sum((data$Confirmed - fit)^2)
  }
  model = optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1),hessian = F)
  modelPar <- setNames(model$par, c("beta", "gamma"))
  t = 1:(input$dateForecast - min(as.Date(data$date)))
  fitValue <- data.frame(ode(y = init, times = t, func = SIR, parms = modelPar))
  fitValue = fitValue %>%
    mutate(date = min(as.Date(data$date)) + time)
  results$modelFit = list(params = modelPar,fitValue = fitValue)
  hc <- highchart() %>% 
    hc_subtitle(text = paste0("PREVISION DE CAS D'INFECTION D'ICI ",input$dateForecast),
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = fitValue$date) %>%
    hc_yAxis(title = list(text = "PREVISION DE CAS (Log-scale)"),type = "logarithmic") %>%
    hc_add_series(name = "SUSCEPTIBLES",data = round(fitValue$S,0)) %>% 
    hc_add_series(name = "INFECTES",data = round(fitValue$I,0)) %>%
    hc_add_series(name = "GUERIS",data = round(fitValue$R,0)) %>%
    hc_add_series(name = "ACTUELS INFECTES",data = data$Confirmed)
  
  hc %>% 
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_exporting(
      enabled = TRUE
    ) %>%
    hc_colors(c(active_color,death_color,recovered_color,orange_color)) %>%
    hc_tooltip(crosshairs = T, backgroundColor = "#FCFFC5",
               shared = T, borderWidth = 5,table = T)
  
})


#### Country Specific tab

countrySpecificModal <- function(){
  countryName = input$canvasClicked
  x = results$dataframeTotal %>% 
    filter(countryName == input$canvasClicked)
  totalConfirmed = x$Confirmed
  totalConfirmedYesterday = results$dataframeOldCases %>%
    filter(countryName == input$canvasClicked) %>%
    .["Confirmed"]
  totalUnrecovered = x$Unrecovered
  activeCasesPer = round(((totalUnrecovered/totalConfirmed)*100),1)
  yesterdayActiveCountry = results$dataframeOldCases %>%
    filter(countryName == input$canvasClicked) %>%
    .["Unrecovered"]
  totalRecovered = x$Recovered
  totalRecoveredPer = round(((totalRecovered/totalConfirmed)*100),1)
  yesterdayRecoveredCountry = results$dataframeOldCases %>%
    filter(countryName == input$canvasClicked) %>%
    .["Recovered"]
  totalDeath = x$Deaths
  totalDeathPer = round(((totalDeath/totalConfirmed)*100),1)
  yesterdayDeathsCountry = results$dataframeOldCases %>%
    filter(countryName == input$canvasClicked) %>%
    .["Deaths"]
  modalDialog(
    tagList(
      argonRow(
        argonColumn(
          center = T,
          width = 12,
          h1(countryName)
        )
      ),
      argonRow(
        argonColumn(
          width = 3,
          argonBadge(text = paste0("Confirmes: ",prettyNum(totalConfirmed,big.mark = ",")), 
                     src = NULL, 
                     pill = T, 
                     status = "warning"),
          h6(paste0("Hier: ",
                    prettyNum(totalConfirmedYesterday,
                              big.mark = ",")
          ),
          style = 'text-align:center;
          font-size:15px;')
        ),
        argonColumn(
          width = 3,
          argonBadge(text = paste0("ACTIFS: ",prettyNum(totalUnrecovered,big.mark = ",")
                                   # " (",activeCasesPer,"%)"
          ), 
          src = NULL, 
          pill = T, 
          status = "info"),
          h6(paste0("Hier: ",
                    prettyNum(yesterdayActiveCountry,
                              big.mark = ",")
          ),
          style = 'text-align:center;
        font-size:15px;')
        ),
        argonColumn(
          width = 3,
          argonBadge(text = paste0("GUERIS: ",prettyNum(totalRecovered,big.mark = ",")
                                   # " (",totalRecoveredPer,"%)"
          ), 
          src = NULL, 
          pill = T, 
          status = "success"),
          h6(paste0("Hier: ",
                    prettyNum(yesterdayRecoveredCountry,
                              big.mark = ",")
          ),
          style = 'text-align:center;
        font-size:15px;')
        ),
        argonColumn(
          width = 3,
          argonBadge(text = paste0("DECES: ",prettyNum(totalDeath,big.mark = ",")
                                   # " (",totalRecoveredPer,"%)"
          ), 
          src = NULL, 
          pill = T, 
          status = "danger"),
          h6(paste0("Hier: ",
                    prettyNum(yesterdayDeathsCountry,
                              big.mark = ",")
          ),
          style = 'text-align:center;
        font-size:15px;')
        )
      ),
      tags$hr(),
      argonRow(
        argonColumn(
          width = 12,
          argonRow(
            argonColumn(
              width = 5,
              # tags$strong("Country specific plots"),
              dateRangeInput( inputId = "dateRange", 
                              label = NULL,
                              start = min(coronavirus$date),
                              end   = Sys.Date() - 1,
                              min = min(coronavirus$date),
                              max   = Sys.Date() - 1,
                              format = "dd-mm-yyyy",
                              width = "100%"
              )
            ),
            argonColumn(
              width = 4,
              prettySwitch(inputId = "scaleCountry",
                           label = "Logarithmic Scale",
                           value = F,
                           status = "primary",
                           fill = T,
                           inline = T,
                           width = "100%"
              )
            ),
            argonColumn(
              width = 1,
              offset = 1,
              dropdownButton(
                tagList(
                  prettyRadioButtons(
                    inputId = "countryPlotOptions1",
                    label = NULL,
                    choices = setNames(c(1:4),c("CAS CUMULES",
                                                paste0("NOUVEAUX CAS (",Sys.Date() - 1,")"),
                                                paste0("DECES (",Sys.Date() - 1,")"),
                                                paste0("GUERIS (",Sys.Date() - 1,")")
                    )
                    ),
                    selected = "1",
                    shape = c("round"),
                    outline = T,
                    fill = T,
                    width = "100%"
                  )
                ),
                status = "primary",
                size = "sm",
                circle = T,
                icon = icon("wrench"),
                right = T,
                margin = "10px",
                inputId = "countryPlotOptionsDropdown1"
              ),
              bsPopover("countryPlotOptionsDropdown", title = NULL, content = "Cliquez pour plus de graphique", placement = "left", trigger = "hover",
                        options = NULL)
            )
          ),
          argonRow(
            argonColumn(
              width = 12,
              div(
                id = "cumulativeCountryPlotDiv1",
                highchartOutput("cumulativeCountryPlot1",width = "100%") %>% withSpinner()
              ),
              hidden(
                div(
                  id = "newCasesCountryPlotDiv1",
                  highchartOutput("newCasesCountryPlot1",width = "100%") %>% withSpinner()
                )
              ),
              hidden(
                div(
                  id = "newCasesDeathsCountryPlotDiv1",
                  highchartOutput("newCasesDeathCountryPlot1",width = "100%") %>% withSpinner()
                )
              ),
              hidden(
                div(
                  id = "newCasesRecoveredCountryPlotDiv1",
                  highchartOutput("newCasesRecoveredCountryPlot1",width = "100%") %>% withSpinner()
                )
              )
            )
          )
        )
      )
    ),
    title = NULL,
    size = "l", 
    align = "center",
    easyClose = TRUE,
    fade = T, 
    footer = NULL
  )
}

output$cumulativeCountryPlot1 = renderHighchart({
  confirmed_color = "#172b4d"
  active_color <- "#1f77b4"
  recovered_color <- "forestgreen"
  death_color <- "red"
  df_daily <- coronavirus %>% 
    filter(countryName == input$canvasClicked) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(totalConfirmed = sum(Confirmed, na.rm = TRUE),
                     totalRecovered = sum(Recovered,na.rm = TRUE),
                     totalDeaths = sum(Deaths,na.rm = T)
    ) %>%
    dplyr::arrange(date) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(totalUnrecovered = totalConfirmed - totalRecovered - totalDeaths) %>%
    filter(date >= input$dateRange[1] & date <= input$dateRange[2])
  scale = ifelse(input$scaleCountry,"logarithmic","linear")
  hc <- highchart() %>% 
    hc_subtitle(text = paste0("Cumules des cas ",input$countrySelect),
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = df_daily$date) %>%
    hc_yAxis(title = list(text = "Nombre de personnes"),type = scale) %>%
    hc_add_series(name = "CONFIRMES",data = df_daily$totalConfirmed) %>% 
    hc_add_series(name = "ACTIFS",data = df_daily$totalUnrecovered) %>% 
    hc_add_series(name = "GUERIS", data = df_daily$totalRecovered) %>% 
    hc_add_series(name = "DECES", data = df_daily$totalDeaths)
  
  hc %>% 
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(confirmed_color,active_color, recovered_color ,death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$newCasesCountryPlot1 = renderHighchart({
  newCases = results$dataframeFinalWorld %>% 
    select(date,countryName,Confirmed) %>%
    dplyr::filter(countryName == input$canvasClicked) %>%
    mutate(confirmedDaily = if_else(is.na(Confirmed - shift(Confirmed,1)),
                                    Confirmed,
                                    Confirmed - shift(Confirmed,1)
    )
    
    ) %>% 
    filter(date >= input$dateRange[1] & date <= input$dateRange[2])
  scale = ifelse(input$scaleCountry,"logarithmic","linear") 
  x = newCases
  death_color <- "orange"
  hc <- highchart() %>% 
    hc_subtitle(text = "CAS CONFIRMES",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$date) %>%
    hc_yAxis(title = list(text = "Nombre personnes"),type = scale) %>%
    hc_add_series(name = "CAS CONFIRMES",data = x$confirmedDaily) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$newCasesDeathCountryPlot1 = renderHighchart({
  newCases = results$dataframeFinalWorld %>% 
    select(date,countryName,Deaths) %>%
    dplyr::filter(countryName == input$canvasClicked) %>%
    mutate(deathDaily = if_else(is.na(Deaths - shift(Deaths,1)),
                                Deaths,
                                Deaths - shift(Deaths,1)
    )
    
    ) %>% 
    filter(date >= input$dateRange[1] & date <= input$dateRange[2])
  scale = ifelse(input$scaleCountry,"logarithmic","linear")
  x = newCases
  death_color <- "red"
  hc <- highchart() %>% 
    hc_subtitle(text = "DECES",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$date) %>%
    hc_yAxis(title = list(text = "Nombre personne"), type = scale) %>%
    hc_add_series(name = "DECES",data = x$deathDaily) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$newCasesRecoveredCountryPlot1 = renderHighchart({
  newCases = results$dataframeFinalWorld %>% 
    select(date,countryName,Recovered) %>%
    dplyr::filter(countryName == input$canvasClicked) %>%
    mutate(recoveredDaily = if_else(is.na(Recovered - shift(Recovered,1)),
                                    Recovered,
                                    Recovered - shift(Recovered,1)
    )
    
    ) %>% 
    filter(date >= input$dateRange[1] & date <= input$dateRange[2])
  scale = ifelse(input$scaleCountry,"logarithmic","linear")
  x = newCases
  death_color <- "green"
  hc <- highchart() %>% 
    hc_subtitle(text = "GUERIS",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$date) %>%
    hc_yAxis(title = list(text = "Nombre personne"),type = scale) %>%
    hc_add_series(name = "CAS GUERIS",data = x$recoveredDaily) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$dataTableCountryWise = renderDataTable({
  req(!is.null(results$dataframeTotal))
  x = results$dataframeTotal %>%
    select(-countryCode) %>%
    arrange(desc(Confirmed)) %>%
    mutate(totalActivePer = Unrecovered/Confirmed) %>%
    mutate(totalRecoveredPer = Recovered/Confirmed) %>%
    mutate(totalDeathPer = Deaths/Confirmed) %>%
    select(PAYS = countryName, CONFIRMES = Confirmed, ACTIFS = Unrecovered,GUERIS = Recovered,DECES = Deaths,"ACTIFS (%)" = totalActivePer,"GUERIS (%)" = totalRecoveredPer,"DECES (%)" = totalDeathPer)
  results$resultTable = x
  datatable(x,
            extensions = 'Buttons',
            rownames = FALSE,
            filter = 'top',
            options = list(
              searchHighlight = TRUE,
              pageLength = 25,
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons =
                list(
                  list(
                    extend = 'collection',
                    buttons = c('csv', 'pdf'),
                    text = 'TELECHARGER'
                  )
                )
              
            )
            
  ) %>%
    formatPercentage('ACTIFS (%)',2) %>%
    formatPercentage('GUERIS (%)',2) %>%
    formatPercentage('DECES (%)',2) %>%
    formatStyle(
      'ACTIFS (%)',
      background = styleColorBar(x$'ACTIFS (%)', '#31bed4'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>%
    formatStyle(
      'GUERIS (%)',
      background = styleColorBar(x$'GUERIS (%)', '#8bd431'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>%
    formatStyle(
      'DECES (%)',
      background = styleColorBar(x$'DECES (%)', '#ff5757'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )
})



#### to check which countries are new
# 
# mapdata <- get_data_from_map(download_map_data("custom/world"))
# # # 
# results$dataframeTotal$countryName[!(results$dataframeTotal$countryName %in% mapdata$name)]
# 
# [1] "Cruise Ship"
# [2] "Holy See

observeEvent(input$aggregatePlotOptions,{
  x = as.numeric(input$aggregatePlotOptions)
  divlist = c("totalCasesPlotDiv","cumulativePlotDiv","newCasesPlotDiv","newCasesDeathsPlotDiv","newCasesRecoveredPlotDiv")
  hideAllBut(divlist,x)
})
observeEvent(input$countryPlotOptions,{
  x = as.numeric(input$countryPlotOptions)
  divlist = c("cumulativeCountryPlotDiv","newCasesCountryPlotDiv",
              "newCasesDeathsCountryPlotDiv","newCasesRecoveredCountryPlotDiv",
              "cumulativeCountryPlotDiv1","newCasesCountryPlotDiv1",
              "newCasesDeathsCountryPlotDiv1","newCasesRecoveredCountryPlotDiv1")
  hideAllBut(divlist,x)
})

observeEvent(input$countryPlotOptions1,{
  x = as.numeric(input$countryPlotOptions1)
  divlist = c("cumulativeCountryPlotDiv1","newCasesCountryPlotDiv1",
              "newCasesDeathsCountryPlotDiv1","newCasesRecoveredCountryPlotDiv1")
  hideAllBut(divlist,x)
})


observeEvent(input$canvasClicked,{
  showModal(countrySpecificModal())
})






##### debug server logic #####
output$runRCodeOutput = renderPrint({
  req(rcode())
  isolate({
    eval(parse(text = rcode()$text))
  })
})
rcode = reactiveVal()
observeEvent(input$runRCodeButton, {
  rcode(list("text" = input$runRCode, "type" = "runRCode", "rand" = runif(1)))
}, ignoreNULL = TRUE, ignoreInit = TRUE)