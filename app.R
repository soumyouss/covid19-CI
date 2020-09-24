
##### Global: options #####
Production = T 
options(scipen = 1000, expressions = 10000)
appVersion = "v2.0"
appName = "COVID-19 Data Visualization Platform"
appLongName = "EVOLUTION DU COVID-19 EN COTE D'IVOIRE "
lastUpdate = "24-09-2020"


loader <- tagList(
  waiter::spin_loaders(42),
  br(),
  h3("Chargement des données")
)

jsToggleFS <- 'shinyjs.toggleFullScreen = function() {
var element = document.documentElement,
enterFS = element.requestFullscreen || element.msRequestFullscreen || element.mozRequestFullScreen || element.webkitRequestFullscreen,
exitFS = document.exitFullscreen || document.msExitFullscreen || document.mozCancelFullScreen || document.webkitExitFullscreen;
if (!document.fullscreenElement && !document.msFullscreenElement && !document.mozFullScreenElement && !document.webkitFullscreenElement) {
enterFS.call(element);
} else {
exitFS.call(document);
}
}'

source("appFiles/packageLoad.R")
source("appFiles/dataLoad.R")
source("appFiles/CSS.R", local = TRUE)
source("appFiles/dashboardPage.R", local = TRUE)
##### User interface #####
ui <- tagList( # dependencies
  use_waiter(),
  useSweetAlert(),
  useShinyjs(),
  extendShinyjs(text = jsToggleFS),
  waiter::waiter_show_on_load(loader, color = "#000"),
# shows before anything else
  ##### CSS and style functions #####
  CSS, #CSS.R
  # Loading message
  argonDash::argonDashPage(
    title = appLongName,
  ######################## Header part ################################
    header = argonDash::argonDashHeader(
      #gradient = T,
      color = "red",
      top_padding = 2,
      bottom_padding = 0,
      #background_img = "covid.jpg",
      height = 100,
      argonRow(
        argonColumn(
          width = 2,
          argonRow(
            argonImage(src = "inhp.jpg",width = "70px")
          ),
          argonRow(
            p(Sys.Date(),style = 'color:white;')   
          )
         
        ),
        argonColumn(
          width = 8,
          argonRow(
            center = T,
            argonColumn(width = 12,
                        argonRow(
                          center = T,
                          h4(appLongName, style = 'color:white;
                                         text-align:left;
                                         vertical-align: middle;
                                         font-size:35px;')
                        )
                        
              )
          )
        ),
        argonColumn(
          width = 2,
          argonImage(src = "inp.jpg",width = "70px")
        )
      )
      # argonRow(
      #   argonColumn(width = 9,
      #               h4(appLongName, style = 'color:white;
      #                  text-align:left;
      #                  vertical-align: middle;
      #                  font-size:35px;')
      #               ),
      #   argonColumn(
      #     width = 3,
      #     argonRow(
      #       center = T,
      #       argonColumn(
      #         width = 12,
      #         center = T,
      #         h5(HTML("Data Science INP-HB: <a href='https://www.datascience.com'>datascience</a>"), style = 'color:white;text-align:right;font-size:15px;')
      #       ),
      #       argonColumn(
      #         width = 12,
      #         center = T,
      #         HTML("<img src='inp.jpg' width=70>"),
      #         HTML("<img src='inhp.jpg' width=70>")
      #       )
      #     )
      #   )
      #   
      #   )
      
      
      ),
  ################ sidebar part #####################################################
    sidebar = NULL,
  ################ Body #############################################################
    body = argonDashBody(
      tags$head( tags$meta(name = "viewport", content = "width=1600"),uiOutput("body")),
      tags$br(),
           dashboardUI,
      tags$hr()
  
    )
  )
  )

##### server #####
server <- function(input, output, session) {
  printLogJs = function(x, ...) {
    logjs(x)
    T
  }
  # addHandler(printLogJs)
  if (!Production) options(shiny.error = recover)
  options(shiny.sanitize.errors = TRUE, width = 160)
  
  session$onSessionEnded(function() {
    stopApp()
    # q("no")
  })
  source("appFiles/dashboardServer.R", local = TRUE)
  # Hide the loading message when the rest of the server function has executed
  waiter_hide() # will hide *on_load waiter
}

# Run the application
shinyApp(ui = ui, server = server)