
# This series of scripts reproduces the results and sensitivity analyses of the paper: 
# Neyret et al. (2021). Assessing the impact of grassland management on landscape multifunctionality. Ecosystem Services.

## This script creates artificial landscapes and calculates 
# landscape-level ES and environmental factors values.

# This script takes as input the es_data dataset from the Plot_scale.R script.
# This script is provided for INFORMATION PURPOSE ONLY: it won't run as it is!

print("Starting simulations")


# Calculate number of possible landscape composition depending on the number of plots per landscape
grid = setDT(expand_grid(low  = 0:no_plots,
                         med  = 0:no_plots,
                         high = 0:no_plots))
n_ls = nrow(grid[low + med + high == no_plots,])

# Set number of combinations per landscape composition
n_combi =  round(1000/n_ls)

# Create result matrix
raw_sim_data  = array(dim = c( 3,  n_combi*n_ls, 15))
env_sim_data  = array(dim = c( 3,  n_combi*n_ls, 12))


for (j in 1:length(unique(es_data$Exploratory))) {

  exploratory_data <-  es_data[which(es_data$Exploratory == unique(es_data$Exploratory)[j]), ]

  env_exploratory_data <-  env_data[which(env_data$Exploratory == unique(env_data$Exploratory)[j]), ]
  
  abundance_data <- All_abundances_cast[which(All_abundances_cast$Exploratory == unique(es_data$Exploratory)[j] ), ]

    # Selects LUI  
  lui_data = data.frame(env_data[which(es_data$Exploratory == unique(es_data$Exploratory)[j] ), c('luiclass', 'Plot')])
  rownames(lui_data) = 1:50
  # Generates landscape combinations
  lui_no_na = lui_data[!is.na(lui_data$luiclass), 'luiclass'] # If we take LUI in 20-20-20%, removes NA values
  names(lui_no_na) = rownames(lui_data[!is.na(lui_data$luiclass),])
  plots_selected_matrix = create_new_combinations(no_plots, n_combi, lui_no_na)
  
  # PCA of environmental variables
  pca_input = data.frame(env_exploratory_data[, c("Tmean.Annual", "Precip.Annual", "Soil.depth", "pH", "Silt","Clay", 'Sand',
                                       "Grassland.1000.CW", "TWI")])
  rownames(pca_input) = env_exploratory_data$Plot
  pca_env = dudi.pca(pca_input, 
                   scale=TRUE, scannf = FALSE, nf = 3)
  pca_data = pca_env$li
  
  # Starts generation of each landscape
  
  for (i in 1:(nrow(plots_selected_matrix))) {
    # Plot selection
    plots_selected_all <- exploratory_data$Plot[plots_selected_matrix[i,]]
    
    # LUI
    lui_mean = mean(env_exploratory_data[Plot %in% plots_selected_all,]$LUI, na.rm = T)
    lui_sd   = sd(env_exploratory_data[Plot %in% plots_selected_all,]$LUI, na.rm = T)

    lui_selected_plots = lui_data[lui_data$Plot %in% plots_selected_all,]$luiclass
    lui_each_class = sapply(1:3, function(x) {length(lui_selected_plots[lui_selected_plots == x]) }) / no_plots

    ### Landscape-level services, MEAN
    if (method_within_landscape == 'mean'){
    ric_tot = specnumber(abundance_data[Plot %in% plots_selected_all, colSums(.SD), .SDcols = all_plants])
    birds_families = specnumber(abundance_data[Plot %in% plots_selected_all, colSums(.SD), .SDcols = all_bird_families])
    birds.charism = specnumber(abundance_data[Plot %in% plots_selected_all, colSums(.SD), .SDcols = most_charismatic_birds])
    
    services = exploratory_data[Plot %in% plots_selected_all, lapply(.SD, mean, na.rm = T), 
                     .SDcols = c(
                                 'Tot_protein',
                                 'Soil.C.stock_2011',
                                 'sqrtTotal_flower_cover',
                                 'sqrtbutterfly_abundance',
                                 'Uniqueness_juniperus',  'Charismatic_plants',
                                 'Cover_edible'  )]
    
    
    }
    
    if (method_within_landscape == 'max'){
    ### Landscape-level services, MAX
      ric_tot =  abundance_data[Plot %in% plots_selected_all, max(specnumber(.SD)), .SDcols = all_plants]
      birds_families =  abundance_data[Plot %in% plots_selected_all, max(specnumber(.SD)), .SDcols = all_bird_families]
      birds.charism =  abundance_data[Plot %in% plots_selected_all, max(specnumber(.SD)), .SDcols = most_charismatic_birds]
      
      services = exploratory_data[Plot %in% plots_selected_all, lapply(.SD, max, na.rm = T), 
                                  .SDcols = c(
                                              'Tot_protein',
                                              'Soil.C.stock_2011',
                                              'sqrtTotal_flower_cover',
                                              'sqrtbutterfly_abundance',
                                              'Uniqueness_juniperus',  'Charismatic_plants',
                                              'Cover_edible'  )]
      
    }
    

    
    # Fill the big dataset
    raw_sim_data[ j, i, ] = c(ric_tot, birds_families,birds.charism,
                              as.numeric(services),
                              lui_each_class,
                            lui_mean, lui_sd
                            )
    
    
    # Fill the environment dataset
    # Average values
    env_sim_data[j, i, 1:8] = colMeans(env_exploratory_data[Plot %in% plots_selected_all,c("Tmean.Annual", 'Precip.Annual', "Soil.depth", "pH", "Clay", 'Sand',
                                                                                  "Grassland.1000.CW", "TWI")], na.rm = T) 
  #  # Environmental variability
    env_sim_data[j, i, 12] = convhulln(pca_data[plots_selected_all,], options = c('FS'))$vol
    
  }

}

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

fwrite(data_test_merge_tosave, file = paste('Data/Data_', environmental_correction,'_',lui_class_method,'_', no_plots,'_',method_within_landscape, '.csv', sep = ''))

