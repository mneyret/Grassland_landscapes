# This series of scripts reproduces the results and sensitivity analyses of the paper: 
# Neyret et al. (2021). Assessing the effects of landscape management on grassland multifunctionality in Germany. Ecosystem Services.

# This script takes as input the raw simulation values and for each simulation
# calculates multifunctionality

print("Calculating multifunctionality")
 
 raw_sim_data2 = data.table(rbind(cbind(raw_sim_data[1, ,], env_sim_data[1, ,]),
                                  cbind(raw_sim_data[ 2, ,], env_sim_data[2, ,]),
                                  cbind(raw_sim_data[3, ,], env_sim_data[3, ,])))
 
 colnames(raw_sim_data2) = c('Ric_tot', 'Birds_families', 'Birds_charism', 
                            'Protein', 'C_stock', 'flowers',  'butterflies', 
                             'juniperus', 'plants_charism', 'edible',
                             'low', 'med', 'high', 'lui_mean', 'lui_sd',
                             "Tmean.Annual", "Precip.Annual", "Soil.depth", "pH", "Clay", 'Sand',
                             "Grassland.1000.CW", "TWI", 'Axis1', 'Axis2', 'Axis3', 'Heterogeneity'#, 'fert_mean', 'fert_sd', 'mow_mean', 'mow_sd', 'graz_mean', 'graz_sd'
                             )
 
 raw_sim_data2$Explo = rep(c('A', 'H', 'S'), each = (n_combi*n_ls))
 raw_sim_data2$ls = rep(1:(n_combi*n_ls), 3)
 
 raw_sim_data2 = raw_sim_data2[!is.na(Ric_tot),]
 if (environmental_correction == TRUE){
 raw_sim_data2[, Ric_tot := residuals(lm(Ric_tot ~ Explo + Precip.Annual +Tmean.Annual + Soil.depth + pH + Clay + Sand + Grassland.1000.CW+TWI+Heterogeneity), trace = F)]
 raw_sim_data2[, Birds_families := residuals(lm(Birds_families ~  Explo + Precip.Annual + Tmean.Annual + Soil.depth+pH+Clay +Sand+Grassland.1000.CW+TWI+ Heterogeneity), trace = F)]
 raw_sim_data2[, Birds_charism := residuals(lm(Birds_charism ~ Explo +Precip.Annual + Tmean.Annual + Soil.depth+pH+Clay +Sand+Grassland.1000.CW+TWI+ Heterogeneity), trace = F)]
 }
 
 data_test_merge = melt.data.table(raw_sim_data2, id.var = c('Explo', 'ls', 'low', 'med', 'high', 'lui_mean', 'lui_sd'))
 data_test_merge_tosave = melt.data.table(raw_sim_data2, id.var = c('Explo', 'ls', 'low', 'med', 'high','lui_mean', 'lui_sd'))

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
