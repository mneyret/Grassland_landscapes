# This series of scripts reproduces the results and sensitivity analyses of the paper: 
# Neyret et al. (submitted). Landscape management for grassland multifunctionality). Ecosystem Services.

# This script creates the ES and abundance dataset, corrected (or not) for the environment

setwd('/Users/Margot/Desktop/Research/Senckenberg/Project_Landscape_MF/Landscape_composition/Data/Final_data')

print("Preparing data")



if (rerun_prep == FALSE){
  ES_grasslands = fread(file = 'ES_grasslands.csv')
  services_all = colnames(ES_grasslands)[colnames(ES_grasslands) != 'Plot']
ES_grasslands[, Exploratory := substr(Plot, 1, 1)]

env_data = fread( 'env_data.csv')
if (lui_class_method == 'quantile_30'){
  env_data[, luiclass := luiclass_30]
}
if (lui_class_method == 'quantile_20'){
  env_data[, luiclass := luiclass_20]
}
env_variables = c('pH', 'Tmean.Annual', 'Precip.Annual', 'Soil.depth', 'TWI', 'Clay', 'Silt', 'Grassland.1000.CW')

All_abundances_aggr = fread('All_abundances_aggr.csv')
all_plants <- as.character(unique(All_abundances_aggr[Group_broad == "plant", ]$Species))
all_birds <- as.character(unique(All_abundances_aggr[Group_broad == "bird", ]$Species))
all_bird_families <- as.character(unique(All_abundances_aggr[Group_broad == "bird", ]$Family))

All_abundances_species <- data.table(dcast(All_abundances_aggr, Plot ~ Species, value.var = "value", fill = 0))
All_abundances_families <- data.table(dcast(All_abundances_aggr[!is.na(Family),], Plot ~ Family, value.var = "value", fill = 0, fun.aggregate = sum))

All_abundances_cast = merge(All_abundances_species, All_abundances_families, by = 'Plot')
All_abundances_cast[, Exploratory := substr(Plot, 1, 1)]

most_charismatic_birds = c("Sturnus_vulgaris"    ,    "Ciconia_ciconia"       , "Cuculus_canorus"       ,  "Turdus_merula"           ,"Luscinia_megarhynchos"  ,
"Erithacus_rubecula"  ,    "Buteo_buteo"           , "Corvus_corax"          ,  "Garrulus_glandarius"     ,"Milvus_milvus"          ,
 "Alauda_arvensis"    ,     "Cyanistes_caeruleus"  ,  "Falco_tinnunculus"    ,   "Troglodytes_troglodytes", "Hirundo_rustica"        ,
 "Parus_major"        ,     "Pica_pica"            ,  "Fringilla_coelebs"    ,   "Grus_grus")


Demand_use = fread( 'Demand_use.csv')

}


if (rerun_prep == TRUE){
#### Prepare the environment dataset ####
### Environmental data
Plot_id_matching <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Environmental_data_for_correction/20826_Basic\ Information\ of\ all\ Experimental\ Plots_EPs_1.7.5/20826.txt")
All_soil_data <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Environmental_data_for_correction/GP\ Soils\ and\ Carbon\ stocks\ master\ sheet\ with\ terrain.xlsx", sheet = 1))
All_soil_data[rw == 3515825 & hw == 5360120 & id == "A2277", PlotID := "A2277"]
All_soil_data <- merge(All_soil_data, Plot_id_matching[ActivePlot == "yes", c("EP_PlotID", "PlotID")], by = "PlotID")
All_soil_data$Core_depth <- All_soil_data[, "Core depth (cm)"]
All_soil_data_f <- All_soil_data[, list(Exploratory,
                                        Plot = ifelse(nchar(EP_PlotID) == 5, EP_PlotID, paste(substr(EP_PlotID, 1, 3), 0, substr(EP_PlotID, 4, 4), sep = "")),
                                        #Core_depth = as.numeric(Core_depth),
                                        elevation = as.numeric(elevation),
                                        slope = as.numeric(slope),
                                        TWI = as.numeric(TWI)
)][order(Plot), ]
Texture <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Environmental_data_for_correction/14686_MinSoil_2011_Soil_Texture_1.9.13/14686.txt")
Texture[, Plot := ifelse(nchar(EP_Plotid) == 5, EP_Plotid, paste(substr(EP_Plotid, 1, 3), 0, substr(EP_Plotid, 4, 4), sep = ""))][order(Plot), ]
Texture[, Clay := as.numeric(Clay)]
Texture <- Texture[, c('Clay', 'Silt','Sand') := list( mean((Clay) / (Clay + Fine_Silt + Medium_Silt + Coarse_Silt + Fine_Sand + Medium_Sand + Coarse_Sand), na.rm = T),
                           mean((Fine_Silt + Medium_Silt + Coarse_Silt) / (Clay + Fine_Silt + Medium_Silt + Coarse_Silt + Fine_Sand + Medium_Sand + Coarse_Sand), na.rm = T),
                          mean((Fine_Sand + Medium_Sand + Coarse_Sand) / (Clay + Fine_Silt + Medium_Silt + Coarse_Silt + Fine_Sand + Medium_Sand + Coarse_Sand), na.rm = T)), by = Plot]
All_soil_data_f <- merge(All_soil_data_f, Texture)

Grassland_env <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Environmental_data_for_correction/may2019_grassland_functions.csv")
Grassland_use_env <- Grassland_env[, c("Plot", "pH", "Cstock_2011") := list(Plotn, PH, Soil.C.stock)][, .SD, .SDcols = c("Plot", "pH", "Cstock_2011")]

# Climate_data <- fread("/Users/Margot/Desktop/Research/Senckenberg/Data/Environment/climate_data/plots.csv")
# Climate_data[, Plot := plotID]
# Climate <- Climate_data[datetime >= 2008 & datetime < 2013, list("Mean_Temp" = mean(Ta_200, na.rm = T), "Mean_precip" = mean(precipitation_radolan, na.rm = T)), by = "Plot"]
# For some reason the climate data and soil depth is incomplete so I'm using Fons' data
Climate_data <- fread('/Users/Margot/Desktop/Research/Senckenberg/Data/Raw_data_landscape/Data_vdP2019/Biodiversity\ Exploratories\ Ecosystem\ Function\ data.txt')
env_final <- merge.data.table(Grassland_use_env, Climate_data[, list(Tmean.Annual, Precip.Annual,Plot, Soil.depth = as.numeric(Soil.depth))], all.x = T)
env_final = merge.data.table(env_final, All_soil_data_f, all.x = T)

### Grassland % data 
old_env_data = data.table(read_excel('/Users/Margot/Desktop/Research/Senckenberg/Data/Environment/env_data.xlsx'))
env_final = merge(env_final, old_env_data[, list(Plot, Grassland.1000.CW)])

### LUI data
LUI_global = fread('/Users/Margot/Desktop/Research/Senckenberg/Data/Environment/LUI_input_data/LUI_standardized_global.txt') 
LUI_regional = fread('/Users/Margot/Desktop/Research/Senckenberg/Data/Environment/LUI_input_data/LUI_standardized_regional.txt')
LUI_regional[, Year := gsub('separately\\(', '', YEAR)][, Year := as.numeric(gsub('\\)', '', Year))]
LUI_all = merge(LUI_global[, list('LUI_global' = LUI, Year = Year, 'Plot' = ifelse(nchar(PLOTID) == 5, PLOTID, paste(substr(PLOTID, 1, 3), 0, substr(PLOTID, 4, 4), sep = '')))], 
                LUI_regional[, list('LUI_regional' = LUI,Year = Year, 'Plot' = ifelse(nchar(PLOTID) == 5, PLOTID, paste(substr(PLOTID, 1, 3), 0, substr(PLOTID, 4, 4), sep = '')))], by = c('Plot', 'Year'))
LUI_mean = LUI_all[Year >= 2007 & Year < 2013, list(LUI_global = mean(LUI_global), LUI_regional = mean(LUI_regional)), by = Plot]
LUI_mean[, lui_class_global := list(classify_LUI(LUI_global, 'quantile_30'))]
LUI_mean[, lui_class_regional_30 := list(classify_LUI(LUI_regional, 'quantile_30')), by = list('Exploratory' = substr(Plot, 1, 1))]
LUI_mean[, lui_class_regional_20 := list(classify_LUI(LUI_regional, 'quantile_20')), by = list('Exploratory' = substr(Plot, 1, 1))]
LUI_mean[,lui_class_regional_20 := dplyr::recode(lui_class_regional_20, '3' = '2', '5'='3')]
         #LUI_mean[, lui_class_regional := list(classify_LUI(LUI_regional, lui_class_method)), by = list('Exploratory' = substr(Plot, 1, 1))]

# Test if the classification is different
LUI_mean[, fisher.test(lui_class_global, lui_class_regional_30)]
LUI_mean[, fisher.test(lui_class_global, lui_class_regional_30), by = substr(Plot, 1, 1)]

# Merge
env_data = merge(env_final, LUI_mean[,list(luiclass_30 = factor(lui_class_regional_30),
                                           luiclass_20 = factor(lui_class_regional_20),'Plot' = Plot, 'LUI' = LUI_regional)], by = 'Plot')

### fill in missing (SCALED) data environmental factors
setDT(env_data)
env_variables = c('pH', 'Tmean.Annual', 'Precip.Annual', 'Soil.depth', 'TWI', 'Clay', 'Silt', 'Grassland.1000.CW')
env_data_complete  = mice(env_data[,..env_variables],  group = env_data$Exploratory, printFlag = F)
env_data[, env_variables] = mice::complete(env_data_complete, printFlag = F)

fwrite(env_data, 'env_data.csv')


#### Prepare the ES dataset ####
# Redlist
Redlist_species = fread('/Users/Margot/Desktop/Research/Senckenberg/Project_Landscape_MF/Landscape_composition/Data/Final_data/regional_redlist.csv')
Redlist_species[Species == 'Polygala_comosa_agg', Species := 'Polygala_comosa_aggr.']
Redlist_species[, region := dplyr::recode(Region, 'Thuringen' = 'H', 'BW' = 'A', 'Brandenburg'= 'S')]

# Edibles and charismatics
Edible_charismatic <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Grassland_data/Edible_culturally_important_plant_species.csv", sep = "\t", quote = "")[-1, ]
colnames(Edible_charismatic) <- gsub(" ", "_", colnames(Edible_charismatic))
all_edibles_plants <- gsub(" ", "_", Edible_charismatic[!is.na(Edible) & Edible != "", Species])
common_edibles_plants <- gsub(" ", "_", Edible_charismatic[!is.na(Commonly_harvested) & Commonly_harvested != "", Species])
charismatic_plants <- gsub(" ", "_", Edible_charismatic[!is.na(Cultural_overall) & Cultural_overall != "", Species])

# Removing large trees, except oak
charismatic_plants <- charismatic_plants[!(charismatic_plants %in% c("Abies_alba", "Picea_abies"))]

Charismatic_birds <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Bird_cultural_value/bird_species_edited.xlsx"))
Charismatic_birds_melt <- melt.data.table(Charismatic_birds[Species_German_Latin != "Bird species missing?"], id.vars = c("Species", "Species_German_Latin", "Bird_species_English"))
Charismatic_birds_melt[, value_use := dplyr::recode(value, "1" = "NA", "2" = "0", "3" = "1", "4" = "5")]
bird_scores <- Charismatic_birds_melt[, list(
  species_score = mean(as.numeric(value_use), na.rm = T),
  Species = gsub(" ", "_", Species)
), by = Species][order(Species), c(2, 3)]
bird_scores <- rbind(bird_scores, list(species_score = mean(bird_scores[grepl("Parus", Species), species_score]), Species = "Parus_montanus"))

# Abundances and richness for plants and birds
Abundance_grasslands <- fread("/Users/Margot/Desktop/Research/Senckenberg/Data/Abundances/Dataset_clean.txt")
bird_families = data.table(read_excel('/Users/Margot/Desktop/Research/Senckenberg/Data/Abundances/Bird_families.xlsx'))
Abundance_grasslands = merge(Abundance_grasslands, bird_families, all = T)
Abundance_grasslands <- Abundance_grasslands[Group_broad %in% c("Plant", "Birds"), ]
Abundance_grasslands[, Group_broad := tolower(gsub("s", "", Group_broad))]
Abundance_grasslands <- Abundance_grasslands[Year < 2013, list(value = sum(value, na.rm = T)), by = c("Species", "Plot", "Group_broad", 'Family')]
Abundance_grasslands[Species == "Delichon_urbica", Species := "Delichon_urbicum"]
Abundance_grasslands[, Exploratory := substr(Plot, 1, 1)]
Abundance_grasslands = Abundance_grasslands[value > 0,]

Abundance_grasslands[, Species := dplyr::recode(Species,
                                          "Achillea_millefolium" = "Achillea_millefolium_aggr.",
                                          "Alchemilla_sp" = "Alchemilla_vulgaris_aggr.",
                                          "Bromus_horderaceus" = "Bromus_hordeaceus_aggr.incl_B_commutatus",
                                          "Capsella_bursa_pastoris" = "Capsella_bursa-pastoris",
                                          "Carex_muricata_aggr" = "Carex_muricata_aggr.",
                                          "Euphrasia_sp_cf" = "Euphrasia_rostkoviana_aggr.",
                                          "Festuca_rubra" = "Festuca_rubra_aggr.",
                                          "Galium_mollugo_agg" = "Galium_mollugo_aggr.",
                                          "Hordeum_vulgare" = "Hordeum_vulgare_aggr.",
                                          "Leucanthemum_vulgare_agg" = "Leucanthemum_vulgare_aggr.",
                                          "Oenothera_biennis" = "Oenothera_biennis_agg",
                                          "Persicaria_lapathifolium" = "Persicaria_lapathifolia",
                                          "Phleum_pratense" = "Phleum_pratense_aggr.",
                                          "Platanthera_bifolia" = "Platanthera_bifolia_agg",
                                          "Poa_pratensis_agg" = "Poa_pratensis_aggr.",
                                          "Primula_veris" = "Primula_elatior_veris_agg",
                                          "Ranunculus_auricomus" = "Ranunculus_auricomus_agg",
                                          "Rosa_canina_agg" = "Rosa_canina_aggr.",
                                          "Trifolium_cf_montanum" = "Trifolium_montanum",
                                          "Vicia_sativa" = "Vicia_sativa_aggr.",
                                          "Vicia_tetrasperma" = "Vicia_tetrasperma_aggr.",
                                          "Vicia_cracca" = "Vicia_cracca_aggr.",
                                          "Bacidina_arnoldiana" = "Bacidina_arnoldiana_agg",
                                          "Cladonia_cf__ramulosa" = "Cladonia_ramulosa",
                                          "Cladonia_pyxidata_ssp__chlorophaea" = "Cladonia_pyxidata_agg",
                                          "Lecanora_cf__chlarotera" = "Lecanora_chlarotera",
                                          "Lecanora_cf__conizaeoides" = "Lecanora_conizaeoides",
                                          "Lecanora_cf__symmicta" = "Lecanora_symmicta",
                                          "Lepraria_cf__incana" = "Lepraria_incana",
                                          "Micarea_cf__prasina" = "Micarea_prasina",
                                          "Opegrapha_varia_var__varia" = "Opegrapha_varia",
                                          "Physcia_cf__aipolia" = "Physcia_aipolia",
                                          "Placynthiella_cf__dasaea" = "Placynthiella_dasaea",
                                          "Usnea_cf__filipendula" = "Usnea_filipendula",
                                          "Amblystegium_cf_subtile" = "Amblystegium_subtile",
                                          "Brachythecium_cf_starkei" = "Brachythecium_starkei",
                                          "Brachythecium_cf_velutinum" = "Brachythecium_velutinum",
                                          "Bryum_capillare" = "Bryum_capillare_agg",
                                          "Orthotrichum_cf_affine" = "Orthotrichum_affine",
                                          "Plagiomnium_affine" = "Plagiomnium_affine_agg"
)]
Abundance_grasslands <- Abundance_grasslands[!(Species %in% c(
  "Unknown_species"
)), ]

ab_birds <- dcast(Abundance_grasslands[Species %in% most_charismatic_birds, ], Plot ~ Species, value.var = "value", fill = 0)
bird_scores = bird_scores[(Species %in% Abundance_grasslands$Species),]
most_charismatic_birds <- bird_scores[order(species_score, decreasing = T), Species][1:round(nrow(bird_scores) / 4)]


Edible_charismatic_richness <- Abundance_grasslands[value > 0,
                                                            list(
                                                              Cover_edible = sum(value[Species %in% all_edibles_plants]), 
                                                              Redlist_richness = specnumber(value[Species %in% Redlist_species[region == Exploratory,Species]]),
                                                              Redlist_cover = sum(value[Species %in% Redlist_species[region == Exploratory,Species]]),
                                                              Charismatic_plants = sum(value[Species %in% charismatic_plants]),
                                                              Charismatic_birds_richness = length(value[Species %in% most_charismatic_birds]),
                                                              Plant_richness = length(unique(Species[Group_broad == "plant"])),
                                                              Bird_richness = length(unique(Species[Group_broad == "bird"])),
                                                              Bird_family_richness = length(unique(Family[Group_broad == "bird"])),
                                                              Uniqueness_juniperus = ("Juniperus_communis" %in% Species)
                                                            ),
                                                            by = c("Plot", "Exploratory")
]
 
####  Abundances ####
All_abundances_aggr <- Abundance_grasslands[, list(value = sum(as.numeric(value), na.rm = T)), by = c("Species", "Plot", "Group_broad", 'Family')]
fwrite(All_abundances_aggr, 'All_abundances_aggr.csv')

all_plants <- as.character(unique(All_abundances_aggr[Group_broad == "plant", ]$Species))
all_birds <- as.character(unique(All_abundances_aggr[Group_broad == "bird", ]$Species))
all_bird_families <- as.character(unique(All_abundances_aggr[Group_broad == "bird", ]$Family))

All_abundances_species <- data.table(dcast(All_abundances_aggr, Plot ~ Species, value.var = "value", fill = 0))
All_abundances_families <- data.table(dcast(All_abundances_aggr[!is.na(Family),], Plot ~ Family, value.var = "value", fill = 0, fun.aggregate = sum))

All_abundances_cast = merge(All_abundances_species, All_abundances_families, by = 'Plot')
All_abundances_cast[, Exploratory := substr(Plot, 1, 1)]



# Fill other services
ES_grasslands <- read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Grassland_data/ES_data.xlsx")
ES_grasslands <- data.table(ES_grasslands[, colnames(ES_grasslands) != "Org_C_stock_maybe2006?"])
ES_grasslands[, Biomass_production := Productivity]
# Cover of edible and charismatic species
ES_grasslands <- merge(ES_grasslands[, c("Plot",  "Total_flower_cover", "Biomass_production", "butterfly_abundance", "Soil.C.stock_2011", 'Nshoot_2009_2001')],
                       Edible_charismatic_richness,
                       all.x = T, by = 'Plot'
)

# Impute missing values
ES_grasslands$Uniqueness_juniperus <- as.numeric(ES_grasslands$Uniqueness_juniperus)

drop
### Data manipulation 
### Fill in empty service values using MICE
services = c( 'Total_flower_cover', 'Biomass_production','Charismatic_birds_richness', 'Nshoot_2009_2001','butterfly_abundance', 'Soil.C.stock_2011',  'Cover_edible', 'Redlist_richness', 'Redlist_cover', 'Charismatic_plants', 'Plant_richness', 'Bird_richness', 'Bird_family_richness', 'Uniqueness_juniperus')
ES_grasslands[, Soil.C.stock_2011 :=as.numeric(Soil.C.stock_2011)]
ES_grasslands_complete  = mice(ES_grasslands[,..services],  group = ES_grasslands$Exploratory, printFlag = F)
ES_grasslands[, services] = mice::complete(ES_grasslands_complete, printFlag = F)

ES_grasslands[, c("sqrtTotal_flower_cover", "sqrtbutterfly_abundance", "sqrtCover_edible", "sqrtCharismatic_plants") :=
                lapply(.SD, sqrt), .SDcols = c("Total_flower_cover", "butterfly_abundance", "Cover_edible", "Charismatic_plants")]

ES_grasslands[, Tot_protein := Biomass_production*Nshoot_2009_2001]

services_all = unique(c(services,c( 'Redlist_richness', 'Redlist_cover', 'sqrtTotal_flower_cover','Tot_protein', 'Charismatic_birds_richness', 'Bird_family_richness', 'Biomass_production', 'Nshoot_2009_2001','sqrtbutterfly_abundance', 'Soil.C.stock_2011',  'Cover_edible',  'Charismatic_plants', 'Plant_richness', 'Bird_richness', 'Bird_family_richness', 'Uniqueness_juniperus')
))

fwrite(file = 'ES_grasslands.csv', ES_grasslands[, c('Plot','Redlist_cover','Biomass_production', 'Nshoot_2009_2001', 'sqrtTotal_flower_cover', 'sqrtbutterfly_abundance',  'Bird_family_richness','Plant_richness',
                                                     'Soil.C.stock_2011', 'Tot_protein','Cover_edible', 'Uniqueness_juniperus', 'Charismatic_plants', 'Charismatic_birds_richness')])

### Demand data
Demand <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Stakeholders\ weightings/Data_Socudes270720.xlsx", sheet = 3))[!is.na(ID) & cluster != 'NA',]

Demand[, Stakeholder := dplyr::recode(Group, 
                                      'Agriculture' = "Agric",
                                      "Economy" = "Econ",
                                      'Forestry' = "Forestry",
                                      "Hunting"= "Hunting" ,
                                      'Landowner'= "Landowner" ,
                                      "Local heritage association"  = "Loc_her_asso",
                                      'Locals' = "Locals",
                                      "Nature conservation associations" = "Nat_cons_asso" ,
                                      "Policy & administration" = "Policy_admin",
                                      "Press" = "Press" ,
                                      "Quarrying" = "Quarrying",
                                      "Regional development programmes"= "Reg_dev_prog",
                                      'Research' = "Research" ,
                                      'Tourism' = "Tourism"
)]

Demand[, 4:15] = Demand[, lapply(.SD, as.numeric), .SDcols = 4:15]


# Selection of takeholder groups: how much do each values grasslands
Demand[, Grassland_prop := c(Livestock_production + Biodiversity_protection + Climate_regulation + Harvesting+Regional_identity+Landscape_aesthetic)/(Livestock_production + Food_production + Biodiversity_protection +  
       Climate_regulation + Timber + Energy_from_raw_materials + Energy_from_technology + Harvesting +  Landscape_aesthetic   +Regional_identity + Leisure + Hunting)]          

Demand_to_plot = melt.data.table(Demand, id.vars = c('ID', 'cluster',     'Group', 'Stakeholder', 'Grassland_prop'), variable.name = 'Service')
Demand_to_plot = Demand_to_plot[, list('MEAN' = mean(value), 'SD' = sd(value)), by = c('Stakeholder', 'Service')]
Demand_to_plot[, Service_pretty := dplyr::recode(Service, 'Biodiversity_protection'  = 'Biodiversity protection', 
                                                     'Climate_regulation'  = 'Climate regulation',
                                                     'Livestock_production'  = 'Livestock production' ,
                                                     'Landscape_aesthetic' = 'Landscape aesthetic',
                                                     'Regional_identity' = 'Regional identity',  
                                                     'Harvesting' = 'Harvesting',  
                                                       'Food_production' = 'Food production' , 
                                                     'Timber' = 'Timber production' , 
                                                     'Energy_from_raw_materials' = 'Energy from raw materials' ,
                                                     'Energy_from_technology'  = 'Energy from technology'  ,              
                                                      'Leisure'   = 'Leisure',
                                                     'Hunting' = 'Hunting' )]
Demand_to_plot[, Service_pretty := factor(Service_pretty, levels = c('Biodiversity protection' ,  'Climate regulation' ,'Livestock production'  ,'Landscape aesthetic','Regional identity',   'Harvesting',  
                                                       'Food production' ,  'Timber production' , 'Energy from raw materials' ,'Energy from technology'                 
                                                       ,'Leisure'  ,'Hunting'  )[12:1])]
Demand_to_plot_mean = Demand_to_plot[, list('MEAN' = mean(MEAN)), by = Service_pretty]

Demand_to_plot[, Stakeholder_pretty := dplyr::recode(Stakeholder,
                                                     "Locals"   = 'Locals',    
                                                     "Tourism"     =  'Tourism sector',
                                                     "Research"   =  'Research',
                                                     "Landowner"    =  'Landowners',
                                                     "Agric"       =  'Farmers',
                                                     "Policy_admin" =  'Policy and administration',
                                                     "Forestry"     =  'Forestry',
                                                     "Loc_her_asso" =  'Local heritage associations',
                                                     "Hunting"      =  'Hunting',
                                                     "Econ"        =  'Economy',
                                                     "Nat_cons_asso" =  'Nature conservation associations',
                                                     "Reg_dev_prog" =  'Regional development programs',
                                                     "Press"       =  'Press',
                                                     "Quarrying" =  'Quarrying sector')]
Demand_to_plot[, Stakeholder_pretty := factor(Stakeholder_pretty, levels = 
                                                    c(  'Farmers',
                                                      'Locals',    
                                                      'Tourism sector',
                                                      'Nature conservation associations',
                                                      'Research',
                                                      'Landowners',
                                                      'Policy and administration',
                                                      'Forestry',
                                                      'Local heritage associations',
                                                      'Hunting',
                                                      'Economy',
                                                      'Regional development programs',
                                                      'Press',
                                                      'Quarrying sector'))]


#Demand_to_plot[, Order := as.numeric(rank(MEAN, ties.method = 'first')), by = 'Service']
#Demand_to_plot[, Order := (Order-mean(Order))/18, by = 'Service']
#Demand_to_plot[, Service_num := as.numeric(Service), by = 'Service']

#ggplot(Demand_to_plot, aes(x = Service_num+Order, y = MEAN, ymin = MEAN-SD, ymax = MEAN+SD, color = Stakeholder))+ geom_point() +
#  geom_errorbar(width = 0)
Stakeholder_SI_a =ggplot(Demand_to_plot, aes(x = Service_pretty, y = MEAN, color = Stakeholder_pretty,  shape = Stakeholder_pretty)) +
         geom_point(size = 2, stroke = 1.5) + theme_bw(base_size = 14) +
  geom_point(data = Demand_to_plot_mean, aes(x = Service_pretty, y = MEAN), inherit.aes = F, color = 'black', shape = 4, size = 3, stroke = 2) + 
  scale_shape_manual(values = c(19, 19, 19, 19, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), name = 'Stakeholder group') +
  ylab('Average demand score') + xlab('Service') +
#  geom_rect(aes(xmin = 6.5, xmax = 12.5, ymin = -10, ymax = +40), fill = NA, color = 'red') +
  annotation_custom(rectGrob(gp=gpar( col="red",  fill=NA, lwd = 2)), xmin = 6.5, xmax = 12.5, ymin = -7, ymax = 21.8) +
  coord_flip(, clip = "off") + scale_color_manual( name = 'Stakeholder group',values = c(  '#1f78b4', '#e31a1c', '#6a3d9a', '#33a02c', '#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f', '#ff7f00', '#cab2d6', '#ffff99', '#fccde5', '#d9d9d9', '#b15928'))

ggsave(Stakeholder_SI_a, file = '/Users/Margot/Desktop/Research/Senckenberg/Documents/Papers/Landscape_compo/R1/Fig_stakeholder_demand.jpeg', width = 12, height = 5)

Demand[, list(Grassland_priority = mean(Grassland_prop)), by = Stakeholder ]  [order(Grassland_priority, decreasing = T),] 

groups = c('Locals', 'Agric','Nat_cons_asso','Tourism')

Demand_small = Demand[Stakeholder %in%groups,list('Total_protein' =Livestock_production,
                                                  'Richness' = Biodiversity_protection ,
                                                  'C_stock' = Climate_regulation ,
                                                  Harvesting,
                                                  'Reg_id' = Regional_identity,
                                                  'Aesthetic' = Landscape_aesthetic,
                                                  Stakeholder = Stakeholder) ]
# Normalisation
Demand_small[, tot := Total_protein + Richness+ C_stock + Harvesting+ Reg_id+ Aesthetic]
Demand_small[,c('Total_protein', 'Richness',  'C_stock', 'Harvesting' ,'Reg_id' ,'Aesthetic') :=
               lapply(.SD, function(x){x/tot}), .SDcols = c('Total_protein', 'Richness',  'C_stock', 'Harvesting' ,'Reg_id' ,'Aesthetic')]

Demand_use = Demand_small[, 
             lapply(.SD, mean, na.rm = T), 
             by = Stakeholder,
             .SDcols = c('Total_protein', 'Richness',  'C_stock', 'Harvesting' ,'Reg_id' ,'Aesthetic')]

Stakeholder_SI_b =
 ggplot(melt.data.table(Demand_use)[, variable := factor(variable, levels = c('Total_protein', 'Richness', 'C_stock', 'Aesthetic', 'Harvesting', 'Reg_id'))], 
                         aes(x = Stakeholder, y = value, fill = variable)) +geom_col(position = position_dodge()) +
   theme_bw(base_size = 14) +
   ylab('Relative demand (%)') + xlab('Stakeholder') +
   scale_x_discrete(labels=c("Farmers","Locals","Nature cons.\nassociations","Tourism\nsector"))+
   scale_fill_manual(breaks = c('Total_protein', 'Richness',  'C_stock','Aesthetic', 'Harvesting' ,'Reg_id' ),
                     labels = c('Livetock production', 'Biodiversity conservation', 'Climate change mitigation', 'Landscape aesthetic value',
                                'Harvesting', 'Regional ID'), name = 'Ecosystem service',
                     values = c("#f45f37","#c5d86d","#2e294d","#1b998b","#a05689","#258ebb"))
ggsave(Stakeholder_SI_b, file = '/Users/Margot/Desktop/Research/Senckenberg/Documents/Papers/Landscape_compo/R1/Fig_stakeholder_demand_b.jpeg', width = 10, height = 6)


fwrite(Demand_use, 'Demand_use.csv')
}

