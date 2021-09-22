library(shiny)
library(shinythemes)
library(ggtern)
library(compositions)
library(DT)
library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)
library(shinyjs)
library(shinyBS)
library(cowplot)
library(waffle)
#library(shiny.i18n)
library(MuMIn)
library(data.table)
#library(magick)

# Data pre-processing ----
#setwd("~/Data")

#n_row = 22050
# DATA = array(dim = c(n_row,  7,  2, 2, 3, 2),
#              dimnames = list(c(1:n_row),
#                              c("Explo", "ls",'low', 'med', 'high', "variable", "value"),
#                              c("1", "0"),
#                              c("quantile_30", "quantile_20"),
#                              c("7", "10", "13"),
#                              c('mean', "max")))
# maxrows = 0
# for (env in c("1", "0")){
#   for (quantile in  c("quantile_30", "quantile_20")){
#     for (no_plots in c("7", "10", "13")){
#       for (method_in_ls in c('mean', "max")){
#         path = paste("Raw/Data_", env, "_",quantile, "_", no_plots, "_" , method_in_ls, ".csv", sep = "")
#         if(file.exists(path)){
#         mat = fread(path)
#         mat[, combi := paste(low, med, high)]
#         mat[, n_ls := 1:.N, by = c('Explo','variable', 'combi')]
#         mat = mat[n_ls < 8,]
#         #maxrows = max(maxrows, nrow(mat))
#         DATA[ 1:nrow(mat), , env, quantile, no_plots, method_in_ls] <- as.matrix(mat[, c("Explo", "ls",'low', 'med', 'high', "variable", "value")])
#       }
#     }    }
# }}
 
 #View(DATA[, , , , , ])


# saveRDS(DATA, file = "~ShinyApp/data/All_data.rds")
# saveRDS(DATA, file = "~ShinyApp/data/All_data_half.csv")

# All_data_half takes half the number of landscape simulations to make the app a bit faster
data_raw = readRDS("data/All_data_half.csv")

col2 <- colorRampPalette(c('#a50026',
                           '#d73027',
                           '#f46d43',
                           '#fdae61',
                           '#fee090',
                           '#ffffbf',
                           '#e0f3f8',
                           '#abd9e9',
                           '#74add1',
                           '#4575b4',
                           '#313695'))

my_scale_average_threshold = function(x, threshold = NA, method){
  if (method == 'Average'){
    max = quantile(x, 0.975, na.rm = TRUE)
    if (is.nan(max)){
      max = max(x, na.rm = T)}
    min = quantile(x, 0.025,  na.rm = TRUE)
    if (is.nan(min)){min = min(x, na.rm = T)}
    y = (x-min)/(max-min)
    y[y>1] = 1
    y[y<0] = 0
    y[is.nan(y)] = 0
    }
  if (method == 'Threshold' | method == "Compromise"){
    real_tresh = quantile(x, threshold, na.rm = TRUE)
    print(paste(threshold, real_tresh))
    y = ifelse(x >= real_tresh, 1, 0)
  }
  return(y)}

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),


  tags$head(
    # this changes the size of the popovers
    tags$style(".tipify{width:400px;height:40px;}",
               ".selectize-input { font-size: 12px; line-height: 12px;} .selectize-dropdown { font-size: 12px; line-height: 12px; }")),
 
  # App title ----
  titlePanel(
    fluidRow(
      column(9, "Optimal landscape composition",
             br()#,
          #  div(style = "float: left;",
          #       actionButton(inputId='change', label="Switch to German", style='padding:4px; font-size:30%',
          #                    icon = icon("globe"), 
          #                    onclick ="document.location.href = 'https://neyret.shinyapps.io/DE_landscape_composition_for_multifunctionality/'")
          #   )
      ), 
      column(3,  
             img(height = 60, width = 170, src = "BE_logo.jpg")
      )
    ), windowTitle = "Optimal landscape composition app"
  ),
  
  sidebarLayout(fluid = TRUE,
   sidebarPanel( 
           strong("Choose a preset...",
           fluidRow(
             tipify(
               actionButton("locals_Button", "", icon = icon("house-user")),
              "Locals", 
               placement="right", trigger = "hover", options=list(container="body")
             ),
             tipify(
               actionButton("farmer_Button", "", icon = icon("apple-alt")),
               "Farmers", 
               placement="right", trigger = "hover", options=list(container="body")
             ),
             tipify(
               actionButton("NGO_Button", "", icon = icon("pagelines")),
               "Conservationists", 
               placement="right", trigger = "hover", options=list(container="body")
             ),
             tipify(
               actionButton("tourist_Button", "", icon = icon("map-signs")),
               "Tourism", 
               placement="right", trigger = "hover", options=list(container="body")
             )),
           br(),
           tipify(strong("... or directly weight the services: ")
                  , "0 = unimportant, 1 = very important", placement="right", trigger = "hover", options=list(container="body")),
           br(),
           br(),
           tipify(fluidRow(
             column(4, img(src = "Flower.png", style ="width: 100% ; max-width: 80px")),
             column(7,  sliderInput("Richness_weight", fluidRow("Conservation"), min = 0, max = 1, value = 1, step = 0.5, ticks = FALSE))),
             "Conservation value is estimated based on the number of plant species found in the landscape", 
             placement="right", trigger = "hover", options=list(container="body")
           ),
           tipify(fluidRow(
             column(4, img(src = "Cow.png", style ="width: 100% ; max-width: 80px")),
             column(7,  sliderInput("Productivity_weight", fluidRow("Production"), min = 0, max = 1, value = 1, step = 0.5, ticks = FALSE))),
             "Production value corresponds fodder production, i.e. to the biomass produced by a field multiplied by plants shoot protein content.", 
             placement="right", trigger = "hover", options=list(container="body")
           ),
           tipify(fluidRow(
             column(4, img(src = "Camera.png", style ="width: 100% ; max-width: 80px")),
             column(7,  sliderInput("Aesthetic_weight", fluidRow("Aesthetic value"), min = 0, max = 1, value = 1, step = 0.5, ticks = FALSE))),
             "Aesthetic value integrates the number of bird families in the landscape, the cover by flowers, and the abundance of butterflies.", 
             placement="right", trigger = "hover", options=list(container="body")
           ),
           tipify(fluidRow(
             column(4, img(src = "Cstock.png",  style ="width: 80% ; max-width: 70px")),
             column(7,  sliderInput("C_stock_weight", fluidRow("Carbon stocks"), min = 0, max = 1, value = 1, step = 0.5, ticks = FALSE))),
             "Organic carbon stock of the topsoil (0-10 cm) is a proxy of climate change mitigation potential.", 
             placement="right", trigger = "hover", options=list(container="body")
           ),
           tipify(fluidRow(
             column(4, img(src = "RegID.png",  style ="width: 80% ; max-width: 70px")),
             column(7,  sliderInput("RegID_weight", fluidRow("Regional identity"), min = 0, max = 1, value = 1, step = 0.5, ticks = FALSE))),
             "Regional identity is estimated based on the cover of culturally important plants, the richness of culturally important birds, and the presence of historical habitats (e.g. Juniperus grasslands).", 
             placement="right", trigger = "hover", options=list(container="body")
           ),
           tipify(fluidRow(
             column(4, img(src = "harvest.png",  style ="width: 80% ; max-width: 70px")),
             column(7,  sliderInput("Harvest_weight", fluidRow("Foraging"), min = 0, max = 1, value = 1, step = 0.5, ticks = FALSE))),
             "Foraging opportunities is estimated based on the cover of edible plants found in the landscape.", 
             placement="right", trigger = "hover", options=list(container="body")
           ),
           
           checkboxInput("options", "More options"),
           conditionalPanel(
             condition = "input.options == true",
             
             uiOutput("method"),
             selectInput("method", label = fluidRow('Method for calculation of multifunctionality:'), 
                         choices = c('Threshold scenario' = 'Threshold', 'Compromise scenario'= 'Compromise' , 'Average' = 'Average')
             ),
             bsTooltip("method", 
                       "In the threshold method, multifunctionality is calculated as the number of services reaching a given treshold in a given landscape for all landscapes. In the compromise method, multifunctionality is calculated as 1 if the services are above the threshold, 0 otherwise. In the average method it is calculated as the average of all services in a given landscape.",
                       placement="right", trigger = "hover", options=list(container="body")),
             
             
             conditionalPanel(
               condition = "input.method == 'Threshold' || input.method == 'Compromise'",
               sliderInput("Threshold", "Select a threshold: ", min = 0, max = 1, value = 0.5, step = 0.05)
             ),
             
             selectInput("corr_uncorr", label = fluidRow('Use data corrected for environmental drivers:'),
                         c('No' = '0', 'Yes' = '1'), selected = '1'),
             selectInput("quantile_2030", label = fluidRow("Restrict data to fewer sites, without 'intermediate' intensity"),
                         c('No' = 'quantile_30', 'Yes' = 'quantile_20'), selected = 'quantile_30'),
             selectInput("no_plots", label = fluidRow('Choose number of sites'),
                         c('7', '10', '13'), selected = '10'),
             selectInput("method_ls", label = fluidRow('Landscape-scale service value'),
                         c('Sum' = 'mean', 'Max' = 'max'), selected = 'mean')
           ),

           bsTooltip("quantile_2030", 
                     "Use all sites, classified based on lowest, medium or highest land use intensity thirds; or restrict to lowest, medium and highest fifths",
                     placement="right", trigger = "hover", options=list(container="body")),
           
           bsTooltip("no_plots", 
                     "Number of sites included in each simulated landscape.",
                     placement="right", trigger = "hover", options=list(container="body")),
           
           bsTooltip("method_ls", 
                     "Within each landscape, calculate landscape-scale service as either the sum (or gamma diversity for diversities) or the maximum of the values for the service in individual sites.",
                     placement="right", trigger = "hover", options=list(container="body")),
           
           bsTooltip("corr_uncorr", 
                     "Default uses scaled raw data data corrected for the environment by removing the effect (linear regression and keep residuals) of multiple environmental covariates.",
                     placement="right", trigger = "hover", options=list(container="body"))
    )
   ),
    
    
    
    # main panel
   mainPanel(
    # class="col-xs-8 col-sm-8",
           tabsetPanel(id="main",
                       tabPanel("About",
                                column(12,
                                       h4("Introduction"),
                                       div(style = "line-height:2; text-align: justify", 
                                           "This app is designed as a tool to explore the effect of landscape composition on multifunctionality, i.e. the simultaneous delivery of multiple ecosystem services. It shows that the optimal strategies differ depending on 1. the goal chosen by land managers - for instance, providing a few services at high level ('threshold scenario') or all services at intermediate level ('compromise scenario', see below and original paper for details) - and 2. the relative demand for multiple ecosystem services. You can find the preprint of this paper on", 
                                                a("BioRXiv", href="https://www.biorxiv.org/content/10.1101/2020.07.17.208199v1"),"." ),
                                       br(),
                                       h4("Context"),
                                       img(src = "map.png", style ="width: 100% ; max-width: 200px; padding: 0px 30px 0px 50px;", align = "right"),
                                       div(style = "line-height:2; text-align: justify", "The",  a("Biodiversity Exploratories ", href="https://www.biodiversity-exploratories.de/en/"), "are a large-scale research platform covering three regions in the North, Central and South-West Germany. 50 grassland plots are monitored in each regions, covering the full land use intensity gradient found in the regions. In each plot, multiple ecosystem services and functions have been measured since 2008. Each plot is classified in three intensity classes depending on the yearly amount of fertiliser it receives, the number of mowing events and the grazing intensity of the grassland. The figure below shows the average value of these three parameters in the low, medium and high intensity grasslands of the three regions."),
                                       br(),
                                       br(),
                                       img(src = "LUI_fig.png", style ="width: 100% ; max-width: 600px; padding: 0px 30px 0px 50px;", align = "center")
                                )),
                       tabPanel("Instructions",
                                column(12,h4("How to use this app?"),
                                       div(style = "line-height:2; text-align: justify", 
                                           br(),
                                           "The relative demand for each service is entered through the sidebar. You can use presets by clicking on the top icons of the sidebar, which sets the relative weighting to the average demand of the corresponding stakeholder group, or manually change the weightings. In the 'Results' panel, the top figure (triangles) presents the value of multifunctionality for all possible landscape compositions, in terms of proportion of land used at low, medium and high intensity. By clicking on the plot, you get more details on a specific landscape composition: in the lower plot, you can  then see the scaled value of each service for the selected landscape composition. You may also explore different sensitivity analyses by clicking on the 'More options' menu:",
                                           br(),
                                           br(),
                                           strong("Multifunctionality measure:"),
                                            "by default, multifuntionality is calculated as the proportion of services above the median for all considered landscapes ('threshold' scenario). It corresponds to a case where land managers need to provide high levels of at least some services. Other options include calculating multifunctionality measured as 1 if all ervices are above a given threshold, 0 otherwise ('compromise' scenario, i.e. managers must provide at least some level of  all services). In both cases, the threshold can be changed. Multifunctionality can also be calculated as the average of all services.",
                                            br(),
                                           br(),
                                           strong("Number of plots:"),
                                            "each simulated landscape can be composed of 10 (default), 7 or 13 sites.",
                                         br(),
                                         br(),
                                         strong("Correction for the environment: "),
                                            "by default, the analysis uses data corrected for the environment (i.e., residuals from linear models including soil, climate and topographical environmental as explanatory covariates). Analysis using raw,uncorrected data is also possible.",
                                         br(),
                                         br(),
                                         strong("Classification into land use intensity classes: "),
                                            "by default, the sites are classified into low, medium and high intensity plots based on whether they belong to the lowest, medium or highest third of the land use intensity index (check methods in the article for details.)To avoid intermediate sites (e.g. which could be classified as both low and medium, or both medium and high) the analysis can also be realised ononly the lowest, medium and highest fifth of the land use intensity index. In that case, the number of sites per landscape is limited to 7."
                                       )
                                )                          
                       ),
                       tabPanel("Results",
                                # Output: Formatted text for caption ----
                                h3(textOutput("caption")),
                                
                                # Output: Plot of the requested variable against mpg ----
                                h4('Landscape Composition and Multifunctionality'),
                                div(style = "line-height:2; text-align: justify","Hover your cursor over the triangle to see the landscape compositions, i.e the proportions of high, medium and low intensity grassland."),
                                fluidRow(
                                  div(style = "position:relative", 
                                      plotOutput("mpgPlot", click = "plot_click", width = "100%", height = 250,
                                                 hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"))), 
                                  uiOutput("hover_info")
                                ),
                                
                                h4('Ecosystem service levels'),
                                div(style = "line-height:2; text-align: justify","Click on the triangle to see the ecosystem service levels and multifunctionality of each landscape composition below."),
                                fluidRow(plotOutput("radarplot2", width = "97%",   height = 250)),
                                bsTooltip("radarplot2", 
                                          "The coloured areas indicate the values of each service in the different regions. The height of the coloured lines indicate the value multifunctionality based on average or threshold, depending on the chosen method.",
                                          placement="right", trigger = "hover", options=list(container="body"))
                                
                                
                                
                       ),
                       
                       #tabPanel(i18n$t("Debug"),
                       #         # Output: Plot of the requested variable against mpg ----
                       #         p('data'),
                       #         tableOutput('table1'),
                       #         p('table_data1'),
                       #         tableOutput('table_data1'),
                       #         p('data_average'),
                       #         tableOutput('table_data_average'),
                       #         
                       #         
                       #),
           tabPanel("Contact"
                    ,
                    HTML(

'  <form action="https://formspree.io/xyynjgra" method="POST" > 
<p> Please submit here any feedback or question you might have about this app!
</p>

  <label> Your email:  </label>
 <br>
    <input type="text" name="_replyto">
 <br>

 <label> Your message:  </label>
 <br>
    <textarea style="width: 100%; minwidth: 500px" name="message" ></textarea>
 <br>

 <button id="contact-form-submit"  >Send</button>
 </form> 

 <script>
  // HACK: clone the form submit button to remove shiny
  // event listener and allow normal processing of the form
  $(document).on("shiny:sessioninitialized", () => {
    const button = document.getElementById("contact-form-submit");
    const new_button = button.cloneNode(true);
    button.parentNode.replaceChild(new_button, button);
  });
</script>
')
                      
)
))))


# Define server 
server <- function(input, output, session) {
  observeEvent(input$change,  { js$closeWindow(); 
    #stopApp() 
    })
  
  
   observe({
    x <- input$quantile_2030
    if (x != "quantile_30") {
      choices <- c(7)
      selected = c(7)
    } else {
      choices <- c(7, 10, 13)
      selected = c(10)
    }
    updateSelectInput(session, "no_plots", choices = choices, selected = selected)
  })
  
  
  data_to_use = reactive({
      if (input$quantile_2030 == 'quantile_30'){
        use_no_plots = input$no_plots}
      else {use_no_plots =  "7"}

    data  = data.frame(data_raw[, , input$corr_uncorr, input$quantile_2030, use_no_plots, input$method_ls])
    data = data[complete.cases(data),]
    data$Explo = dplyr::recode(factor(data$Explo), 'A' = 'South-West', 'H' = 'Central', 'S' = 'North')
    levels(data$Explo) = c('South-West', 'Central', 'North')
    data[ c('low', 'med', 'high', 'value')] = sapply(data[ c('low', 'med', 'high', 'value')], function(x){as.numeric(as.character(x))})
    print(nrow(unique(data)))
    setDT(data)
    return(data)
  })

  output$table1 <- renderTable(data_to_use()[1:20,])

  # Either average or threshold
  data_1 = reactive({
    data_test_merge =  copy(data_to_use())
    if (input$method == 'Average'){
      print('A')
      data_test_merge[, value := my_scale_average_threshold(value, NA, method = 'Average'), by = list(Explo, variable)]
      data = dcast.data.table(data_test_merge, Explo + ls ~ variable, value.var = 'value')
      data[, c("Conservation", "Production", "Aesthetic", "C_stock", 'Harvesting', 'RegID') := 
             list(Ric_tot, 
                  Protein,
                  (flowers + Birds_families + butterflies)/3  , 
                  C_stock,
                  edible,
                  (Birds_charism  + plants_charism)/2)
      ]}
    if (input$method == "Threshold"){
      print('T')
      data_test_merge[, value := my_scale_average_threshold(value, input$Threshold, method = "Threshold"), by = list(Explo, variable)]
      data = dcast.data.table(data_test_merge, Explo + ls ~ variable, value.var = 'value')
      data[, c("Conservation", "Production", "Aesthetic", "C_stock", 'Harvesting', 'RegID') := 
                     list(Ric_tot, 
                          Protein,
                          (flowers + Birds_families + butterflies)/3  , 
                          C_stock,
                          edible,
                          (Birds_charism  + plants_charism)/2)
      ]
     # data = data_test_merge %>% 
      #  group_by(Explo, variable) %>% 
      #  mutate(value = my_scale_average_threshold(value, threshold = input$Threshold, method = input$method)) %>%
      #  spread(variable, value)# %>%
       # rowwise() %>% 
      #  mutate(Conservation  =  Ric_tot,
      #        Production     =  Protein,
      #        Aesthetic      =  (flowers + Birds_families + butterflies)/3  ,
      #        C_stock        =  C_stock,
      #        RegID          =  (juniperus +plants_charism + Birds_charism)/3,
      #        Harvesting     =  edible)
    }
    
    
    
    if (input$method == "Compromise"){
      print('Compro')
      data_test_merge2 = data_test_merge
      data_average0 = data_test_merge2[ , list(value = my_scale_average_threshold(value, NA, method = "Average"), ls = ls), list(Explo, variable)]
      data_average0 = dcast.data.table(data_average0, Explo + ls ~ variable, value.var = "value")
      data = data_average0[, c("Conservation", "Production", "Aesthetic", "C_stock", 'Harvesting', 'RegID') := list(Ric_tot, 
                                                                                                   Protein,
                                                                                                   (flowers + Birds_families + butterflies)/3  , 
                                                                                                   C_stock,
                                                                                                   edible,
                                                                                                   (Birds_charism  + plants_charism)/2),
                                    by = Explo]
      data[, c("Conservation", "Production", "Aesthetic", "C_stock", 'Harvesting', 'RegID') := lapply(.SD, function(x){
               my_scale_average_threshold(x, input$Threshold, method = "Threshold")}), by = Explo, .SDcols = c("Conservation", "Production", "Aesthetic", "C_stock", 'Harvesting', 'RegID')]
      # data = data_test_merge %>% 
      #   group_by(Explo, variable) %>% 
      #   mutate(value = my_scale_average_threshold(value, threshold = NA, method = "Average")) %>%
      #   spread(variable, value) %>%
      #   rowwise() %>% 
      #   mutate( Conservation =   Ric_tot,
      #           Production   =   Protein,
      #           Aesthetic    =   (flowers + Birds_families + butterflies)/3  ,
      #           C_stock      =   C_stock,
      #           RegID      =    (juniperus +plants_charism + Birds_charism)/3,
      #           Harvesting = edible)  %>% 
      #   as.data.frame() %>%
      #   group_by(Explo) %>% 
      #   mutate( Conservation =   my_scale_average_threshold(Conservation, input$Threshold, method = "Threshold"),
      #           Production   =   my_scale_average_threshold(Production, input$Threshold, method = "Threshold"),
      #           Aesthetic    =   my_scale_average_threshold(Aesthetic, input$Threshold, method = "Threshold"),
      #           C_stock      =   my_scale_average_threshold(C_stock, input$Threshold, method = "Threshold"),
      #           RegID        =   my_scale_average_threshold(RegID, input$Threshold, method = "Threshold"),
      #           Harvesting   =   my_scale_average_threshold(Harvesting, input$Threshold, method = "Threshold"))

    }
     return(data)
  })
  

  output$table_data1 <- renderTable(data_1()[1:20,])
  
  weights <- reactive({list(Richness_weight = input$Richness_weight,
                            Productivity_weight = input$Productivity_weight,
                            Aesthetic_weight = input$Aesthetic_weight,
                            C_stock_weight = input$C_stock_weight,
                            RegID_weight = input$RegID_weight,
                            Harvest_weight = input$Harvest_weight) }) %>%  debounce(800)
  
  # Weight services
  data_average = reactive({
    if (input$method != "Compromise"){
      data = copy(data_1()) 
      data[, Multifunctionality := (Conservation*weights()$Richness_weight +
                                 Production*weights()$Productivity_weight + 
                                 Aesthetic*weights()$Aesthetic_weight + 
                                 C_stock*weights()$C_stock_weight +
                                 Harvesting*weights()$Harvest_weight +
                                 RegID*weights()$RegID_weight) 
          /    (weights()$Richness_weight  + weights()$Productivity_weight + 
                   weights()$Aesthetic_weight + weights()$C_stock_weight +
                   weights()$Harvest_weight   + weights()$RegID_weight)]
        }
    if (input$method == "Compromise"){
      weights01 = c(weights()$Richness_weight, weights()$Productivity_weight,
                    weights()$Aesthetic_weight, weights()$C_stock_weight,
                    weights()$Harvest_weight, weights()$RegID_weight)>0
      names(weights01) = c("Conservation", "Production", "Aesthetic", "C_stock", 'Harvesting', 'RegID')
      
      data = copy(data_1()) 
      data[, c("Conservation", "Production", "Aesthetic", "C_stock", 'Harvesting', 'RegID') := 
             list(Conservation*weights()$Richness_weight, 
                  Production*weights()$Productivity_weight, 
                  Aesthetic*weights()$Aesthetic_weight, 
                  C_stock*weights()$C_stock_weight, 
                  Harvesting*weights()$Harvest_weight,
                  RegID*weights()$RegID_weight)]
      data[, Multifunctionality := Reduce(`*`, .SD), .SDcols = names(weights01[weights01 == TRUE])]

        }
    data = cbind(data[,  c("Explo", "Multifunctionality", 'Conservation', 'Production','Aesthetic', 'C_stock', 'RegID', 'Harvesting')], 
                 dcast(data_to_use(), Explo + ls    +   low   +    med + high~variable)[, c('low', 'med', 'high')])
    data =  melt(data, id.vars  = c('low', 'med', 'high', 'Explo'), measure.vars = c("Multifunctionality", 'Conservation', 'Production', 'C_stock', 'Aesthetic', 'RegID', 'Harvesting'))
    data$Explo = factor(data$Explo, levels =c('South-West', 'Central', 'North'))
    return(data)
  })
  
  
  #### Interpolate results over a grid
  mygrid = data.frame(rbind(
    expand.grid(
      x = seq(0, 1, length.out = 100),
      y = seq(0, 1, length.out = 100),
      Region = c('South-West', 'Central', 'North'),
      pretty_variables = c('Conservation', 'Production','Aesthetic', 'C_stock', 'RegID', 'Harvesting', 'Multifunctionality'),
      value = NA#,
     # r2 = NA
    )))
  mygrid$high = mygrid$x - mygrid$y*tan(pi/6)
  mygrid$medium = mygrid$y/(tan(pi/3)*0.5)
  mygrid$low = 1 - mygrid$high  - mygrid$medium
  mygrid = mygrid[mygrid$high >= 0,]
  mygrid = mygrid[mygrid$low >= 0,]
  
  
  #### Extract new value ####
  values <- reactiveValues(default = 0)
  if (isolate({values$default}) == 0){
    clicks = reactiveValues()
    clicks$x = 0.495
    clicks$y = 0.29
  }
  observeEvent(input$plot_click, {
    clicks$x = input$plot_click$x
    clicks$y = input$plot_click$y
  })
  high.n = reactive({
    h = round(clicks$x - clicks$y*tan(pi/6), 2)
    if(h < 0){h = 0}
    if(h > 1){h = 1}
    return(h)})
  med.n =  reactive({
    m = round(clicks$y/(tan(pi/3)*0.5), 2)
    if (m < 0){m = 0}
    if (m > 1){m = 1}
    if (m + high.n() > 1){m = 1-high.n()}
    return(m)})
  low.n =  reactive({
    l = round(1 - high.n() - med.n(), 2)
    if(l < 0){l = 0}
    if(l > 1){l = 1}
    return(l)})
  
  # Calculate the whole grid to plot
  mygrid_new = reactive({
    mygrid_new = mygrid
    for (Exp in c('South-West', 'Central', 'North')){
      for (var in c('Conservation', 'Production','Aesthetic', 'C_stock', 'RegID', 'Harvesting', 'Multifunctionality')){
        modelbetalin = glm(value ~ poly(low, high, degree = 2), data = data_average()[data_average()$variable == var &  data_average()$Explo == Exp &  !is.na(data_average()$value),], family='binomial')
        mygrid_new[mygrid_new$Region == Exp & mygrid_new$pretty_variables == var,]$value = predict(modelbetalin,  mygrid_new[mygrid_new$Region == Exp & mygrid_new$pretty_variables ==var,] , type="response")
        #mygrid_new[mygrid_new$Region == Exp & mygrid_new$pretty_variables == var,]$r2 = round(r.squaredLR(modelbetalin)[1], 2)
        mygrid_new = mygrid_new[mygrid_new$high >= 0,]
        mygrid_new = mygrid_new[mygrid_new$low >= 0,]
      }}
    return(mygrid_new)
  }) 
  
  new_val = reactive({
    new_val = matrix(ncol = 3, nrow = 7)
    colnames(new_val) = c('South-West', 'Central', 'North')
    rownames(new_val) = c('Conservation', 'Production','Aesthetic', 'C_stock', 'RegID', 'Harvesting', 'Multifunctionality')
    keep_row = c(weights()$Richness_weight, weights()$Productivity_weight, weights()$Aesthetic_weight, weights()$C_stock_weight,weights()$RegID_weight, weights()$Harvest_weight, 1) > 0
    for (i in c('South-West', 'Central', 'North')){
      for (val in rownames(new_val)){
        new_val[val, i] =  mean(mygrid_new()[mygrid_new()$Region == i & mygrid_new()$pretty_variables == val &
                                         mygrid_new()$high > high.n() - 0.1 & mygrid_new()$high < high.n() + 0.1 &
                                         mygrid_new()$medium > med.n() - 0.1 & mygrid_new()$medium < med.n() + 0.1 , 
                                        ]$value)}}
    new_val = melt(new_val)
    if(sum(keep_row)< 7){
      new_val[!rep(keep_row, 3), ]$value = NA}
    return(new_val)
  })
  
  #Generate a plot of the requested variable
  output$mpgPlot <- renderPlot({
    gg =  ggplot(mygrid_new()[mygrid_new()$pretty_variables == "Multifunctionality",], 
                 aes(z = value, x= x, y = y, fill = value, color = value)) +
      geom_raster() +  theme_bw() +
      facet_grid(~ Region) +
      scale_fill_gradientn(colors = col2(20), name = "Multifunctionality", limits=c(0.0,1.0)) +
      scale_color_gradientn(colors = col2(20), name = "Multifunctionality", limits=c(0.0,1.0))+
      theme(panel.background = element_blank(),
            panel.border = element_blank(),
            axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            aspect.ratio=1)+
      geom_point(data = data.frame(Y = med.n()*tan(pi/3)*0.5,
                                  X = high.n() +  med.n()*tan(pi/3)*0.5*tan(pi/6)),
                 aes(x = X, y = Y), color = 'black', size = 0.4, inherit.aes = FALSE) +
      geom_path(data = data.frame(x = c(0, 0.5, 1, 0, 0.5), y = c(0, 0.86, 0, 0, 0.86)), aes(x = x, y= y),color = "grey50", inherit.aes = F, lwd = 0.3) +
      geom_path(data = data.frame(x = c(-0.01, 0.5, 1.1, -0.01, 0.5), y = c(-0.01, 0.87, -0.01, -0.01, 0.87)), aes(x = x, y= y),color = "white", inherit.aes = F, lwd = 0.73) +
      geom_text(data = data.frame(lab = c('%low', '%medium', '%high'), x = c(0, 0.62, 0.95), y = c(-0.05, 0.85, -0.05)),
                aes(x = x, y =y, label = lab), inherit.aes = F, size = 4, fontface = "italic")#+
     #geom_text(data = datar2, aes(x = 0.2, y = 0.75, label = r), inherit.aes = F, size = 4) 
    return(gg)
  })
  
  
  output$hover_info <- renderUI({
    
    plot_hover <- input$plot_hover
    if (length(input$plot_hover) == 0) return(NULL)
    high.hover = reactive({
      h = input$plot_hover$x - input$plot_hover$y*tan(pi/6)
      if(h < 0){h = 0}
      if(h > 1){h = 1}
      return(round(h, 2))})
    med.hover =  reactive({
      m = input$plot_hover$y/(tan(pi/3)*0.5)
      if (m < 0){m = 0}
      if (m > 1){m = 1}
      if (m + high.hover() > 1){m = 1-high.hover()}
      return(round(m, 2))
    })
    low.hover =  reactive({
      l = 1 - high.hover() - med.hover()
      if(l < 0){l = 0}
      if(l > 1){l = 1}
      return(round(l,2))})
  

  
    style <- paste0(
      "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.9); font-size: 12px; padding: 2px 2px 2px 2px; ",
      "left:", plot_hover$coords_css$x + 10 , "px; top:", plot_hover$coords_css$y + 10, "px;")
    # actual tooltip created as wellPanel
    
    wellPanel(
      style = style,
      HTML( paste0('<p style="color: #7DC072 "> <b> Low intensity: </b>', low.hover()*100, "%", "</p>",
                    '<p style="color: #D3C60D "> <b>  Medium intensity: </b>', med.hover()*100,  "%", "</p>",
                    '<p style="color: #F48C60 "> <b> High intensity: </b>', high.hover()*100, "%",  "</p>"
                    )),
      renderPlot(
         waffle(c('Low' = low.hover()*100, 'Medium' = med.hover()*100, 'High' = high.hover()*100),
                colors = c('#9CCF94', '#F5EC66', '#F48C60'), size = 0.5) + 
          guides(fill = FALSE) +
          theme(plot.margin = unit(c(0,0,0,0), "points"))
         , height = 100,  width = 100
         ))
  })
  
  output$radarplot2 <- renderPlot({
    p = ggplot(data= new_val()[new_val()$Var1 != 'Multifunctionality',],aes(x=Var2 ,y= value, fill= Var1))+
      geom_bar(stat="identity", width = 0.8, alpha = 0.7, position = position_dodge(width = 0.8))+
      theme_bw(base_size = 13.5) +
      scale_fill_manual(values = c('#C3D759','#F25F29', "#239489","#312C4B","#258ebb","#a05689"), labels = c('Conservation', 'Production', 'Aesthetic value', 'Carbon stocks', 'Regional identity', 'Foraging')) + 
      scale_color_manual(values = c('#C3D759','#F25F29', "#239489","#312C4B","#258ebb", "#a05689")) + 
      xlab("") + ylab("Value for selected landscape\n(scaled to maximum)") +
      geom_segment(data = cbind(new_val()[new_val()$Var1 == 'Multifunctionality',],
                                x = c(0.6, 1.6, 2.6 ),
                                xend = c(1.4, 2.4, 3.4)),
                   aes(y = value, yend = value,
                       x = x , xend = xend), color = "black", lwd = 1.5, inherit.aes = FALSE) +
      scale_x_discrete(labels = c('South-West', 'Central', 'North'), position = 'top') +
      guides(fill = guide_legend(title = '', title.position = 'top', position = "none"), 
             color = guide_legend(title = '', title.position = 'top', position = "none")) +
      theme(
        # Remove panel border
        panel.border = element_blank(),
        # Remove panel grid lines
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'bottom',  legend.title.align = 0.5) +
      scale_y_continuous(minor_breaks = 1, breaks = c(0, 0.25, 0.5, 0.75, 1), position = "right", limits = c(0,1)) 
    
    plot(p)
    
  })
  
  
  
  
  
  #### updates inputs
  observeEvent(input$locals_Button, {
    updateSliderInput(session, "Richness_weight",     value = 1)
    updateSliderInput(session, "Productivity_weight", value = 0.626)
    updateSliderInput(session, "Aesthetic_weight",    value = 0.696)
    updateSliderInput(session, "C_stock_weight",      value = 0.515)
    updateSliderInput(session, "Harvest_weight",    value = 0.268)
    updateSliderInput(session, "RegID_weight",      value = 0.390)
  }) 
  
  observeEvent(input$farmer_Button, {
    updateSliderInput(session, "Richness_weight",     value = 0.547)
    updateSliderInput(session, "Productivity_weight", value = 1)
    updateSliderInput(session, "Aesthetic_weight",    value = 0.282)
    updateSliderInput(session, "C_stock_weight",      value = 0.223)
    updateSliderInput(session, "Harvest_weight",    value = 0.108)
    updateSliderInput(session, "RegID_weight",      value = 0.322)
  }) 
  
  observeEvent(input$NGO_Button, {
    updateSliderInput(session, "Richness_weight",     value = 1)
    updateSliderInput(session, "Productivity_weight", value = 0.295) 
    updateSliderInput(session, "Aesthetic_weight",    value = 0.366) 
    updateSliderInput(session, "C_stock_weight",      value = 0.45)
    updateSliderInput(session, "Harvest_weight",    value = 0.169)
    updateSliderInput(session, "RegID_weight",      value = 0.282)
  }) 
  
  observeEvent(input$tourist_Button, {
    updateSliderInput(session, "Richness_weight",     value = 1)
    updateSliderInput(session, "Productivity_weight", value = 0.396)
    updateSliderInput(session, "Aesthetic_weight",    value = 0.874)
    updateSliderInput(session, "C_stock_weight",      value = 0.518)
    updateSliderInput(session, "Harvest_weight",    value = 0.305)
    updateSliderInput(session, "RegID_weight",      value = 0.796)
  }) 
  
}

shinyApp(ui, server)


