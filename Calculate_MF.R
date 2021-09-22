# This series of scripts reproduces the results and sensitivity analyses of the paper: 
# Neyret et al. (2021). Assessing the impact of grassland management on landscape multifunctionality. Ecosystem Services.

# This script takes as input the pre-calculated landscape values for all simulations
# (ecosystem service and environmental variables)
# based on the specified parameters:
#      - environmental correction (yes 1, no 0)
#      - lui classification method (20% or 30% quantiles)
#      - number of plots per landscape (7, 10 or 13)
#      - method for landscape-level service calculation (max or mean)   
# Then it calculates multifunctionality based on the demand.

print("Calculating multifunctionality")
data_merge = fread(paste('Data/Data_', environmental_correction,'_',lui_class_method,'_', no_plots,'_',method_within_landscape, '.csv', sep = ''))

all_service_combinations = expand_grid("Ric" = c(0,1),
                                       "Prod" = c(0,1),
                                       "Aest" = c(0,1),
                                       "C_stock"= c(0,1),
                                       'Forag'= c(0,1),
                                       'RegId'= c(0,1))
all_service_combinations = all_service_combinations[rowSums(all_service_combinations)>0,]

if (method %in% c('Threshold_perc', 'Average', 'Threshold')){
 data_test_merge[, value := scale_average_threshold(value, threshold_to_use, method = method), by = list(Explo, variable)]
 data_average = dcast.data.table(data_test_merge, Explo + ls ~ variable, value.var = 'value')
 data_average[, c("Ric", "Prod", "Aest", "C_stock", 'Forag', 'RegId') := 
                    list(Ric_tot, 
                         Protein,
                         (flowers + Birds_families + butterflies)/3  , 
                         C_stock,
                         edible,
                         (Birds_charism  + plants_charism)/2)
                         ]
 
 for (k in 1:nrow(all_service_combinations)){
   Row = all_service_combinations[k,]
   colName = paste('$', paste(colnames(Row)[Row>0], collapse = '_'),sep = '')
   data_average[, (colName) := rowMeans(.SD), .SDcols = colnames(Row)[Row>0]]
 } 
 
 for (st in Demand_use$Stakeholder){
   weights = Demand_use[Stakeholder == st,]
   colName = paste('$', st,sep = '')
   data_average[, (colName) := 
                  (Prod * weights$Total_protein +
                  Ric * weights$Richness +
                  C_stock * weights$C_stock +
                  Forag * weights$Harvesting +
                  RegId * weights$Reg_id +
                  Aest * weights$Aesthetic)/(weights$Total_protein + weights$Richness +  weights$C_stock +weights$Harvesting+ weights$Reg_id +weights$Aesthetic) ]
 }
 }
if (method == "minimise_trade_offs"){
  data_test_merge2 = data_test_merge
  setDT(data_test_merge2)
  data_average0 = data_test_merge2[ , list(value = scale_average_threshold(value, NA, method = "Average"), ls = ls), list(Explo, variable)]
  data_average0 = dcast.data.table(data_average0, Explo + ls ~ variable, value.var = "value")
  data_average1 = data_average0[, c("Ric", "Prod", "Aest", "C_stock", 'Forag', 'RegId') := list(Ric_tot, 
                                       Protein,
                                       (flowers + Birds_families + butterflies)/3  , 
                                       C_stock,
                                       edible,
                                       (Birds_charism  + plants_charism)/2),
                                       by = Explo]
  data_average1[, c("Ric", "Prod", "Aest", "C_stock", 'Forag', 'RegId') := lapply(.SD, function(x){
            scale_average_threshold(x, threshold_to_use, method = "Threshold")}), 
            by = Explo, .SDcols = c("Ric", "Prod", "Aest", "C_stock", 'Forag', 'RegId')]
  data_average =  copy(data_average1)

  for (k in 1:nrow(all_service_combinations)){
    Row = all_service_combinations[k,]
    colName = paste('$', paste(colnames(Row)[Row>0], collapse = '_'),sep = '')
    data_average[, (colName) := Reduce(`*`, .SD), .SDcols = colnames(Row)[Row>0]]
  }
  
  for (st in Demand_use$Stakeholder){
    weights = Demand_use[Stakeholder == st,]
    colName = paste('$', st,sep = '')
    data_average[, (colName) := 
                   Prod * weights$Total_protein +
                   Ric * weights$Richness +
                   C_stock * weights$C_stock +
                   Forag * weights$Harvesting +
                   RegId * weights$Reg_id +
                   Aest * weights$Aesthetic]
  }
}


data_average = merge(data_average, unique(data_test_merge[, c('Explo',  'ls','low', 'med', 'high','lui_mean', 'lui_sd')]))

data_average_melt = melt.data.table(data_average, id.vars  = c('low', 'med', 'high', 'Explo', 'lui_mean', 'lui_sd', 'ls'),
                                    measure.vars = colnames(data_average)[grepl('\\$',colnames(data_average))])
data_average_melt[, variable := gsub('\\$', '', variable)]

data_average_melt$Region = factor(data_average_melt$Explo)
levels(data_average_melt$Region) =  c('South-West', 'Central', 'North')

data_average_melt[, 'nservices_num' := sum(sapply(c('Ric','Prod','Aest','C_stock','Forag','RegId'), 
                                              function(x){grepl(x, variable)})),
                  by = 1:nrow(data_average_melt)
]

data_average_melt[, 'nservices' := nservices_num]

data_average_melt[, pretty_variables := variable]
