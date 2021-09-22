# This series of scripts reproduces the results and sensitivity analyses of the paper: 
# Neyret et al. (2021). Assessing the impact of grassland management on landscape multifunctionality. Ecosystem Services.

# This script creates the ES and abundance dataset, corrected (or not) for the environment.
# As not all the raw data is not publicly available, this is provided for INFORMATION PURPOSE ONLY: this script won't run as it is!
# Some of the raw data is publicly available on the Biodiversity Exploratories information system (Bexis). 
# Questions about non-publicly available raw data may be addressed to the corresponding author, but access should ultimately 
# be discussed with data owners.

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


Demand_use = data.table(Stakeholder = c("Locals"     ,   "Tourism"   ,    "Agric"     ,    "Nat_cons_asso"),
                        Total_protein =c(0.1790968, 0.1018663, 0.4027865, 0.1151149),
                        Richness = c(0.2861763, 0.2570771, 0.2203899, 0.3902995),
                        C_stock =c(0.14733826, 0.13315769, 0.08981409, 0.17574749),
                        Harvesting =c(0.07667805, 0.07846794, 0.04364067, 0.06601922),
                        Reg_id =c(0.1116049, 0.2047033, 0.1298042, 0.1099849),
                        Aesthetic = c(0.1991057, 0.2247277 ,0.1135646, 0.1428340))

}


if (rerun_prep == TRUE){
#### Prepare the environment dataset ####
### Environmental data
Plot_id_matching <- fread("Raw_data/20826_Basic\ Information\ of\ all\ Experimental\ Plots_EPs_1.7.5/20826.txt") # Data publicly available from https://www.bexis.uni-jena.de/ddm/data/Showdata/20826
All_soil_data <- data.table(read_excel("Raw_data/GP\ Soils\ and\ Carbon\ stocks\ master\ sheet\ with\ terrain.xlsx", sheet = 1)) # Data not currently publicly available
All_soil_data[rw == 3515825 & hw == 5360120 & id == "A2277", PlotID := "A2277"]
All_soil_data <- merge(All_soil_data, Plot_id_matching[ActivePlot == "yes", c("EP_PlotID", "PlotID")], by = "PlotID")
All_soil_data$Core_depth <- All_soil_data[, "Core depth (cm)"]
All_soil_data_f <- All_soil_data[, list(Exploratory,
                                        Plot = ifelse(nchar(EP_PlotID) == 5, EP_PlotID, paste(substr(EP_PlotID, 1, 3), 0, substr(EP_PlotID, 4, 4), sep = "")),
                                        elevation = as.numeric(elevation),
                                        slope = as.numeric(slope),
                                        TWI = as.numeric(TWI)
)][order(Plot), ]

Texture <- fread("Raw_data/14686_MinSoil_2011_Soil_Texture_1.9.13/14686.txt") # Data available from https://www.bexis.uni-jena.de/ddm/data/Showdata/14686
Texture[, Plot := ifelse(nchar(EP_Plotid) == 5, EP_Plotid, paste(substr(EP_Plotid, 1, 3), 0, substr(EP_Plotid, 4, 4), sep = ""))][order(Plot), ]
Texture[, Clay := as.numeric(Clay)]
Texture <- Texture[, c('Clay', 'Silt','Sand') := list( mean((Clay) / (Clay + Fine_Silt + Medium_Silt + Coarse_Silt + Fine_Sand + Medium_Sand + Coarse_Sand), na.rm = T),
                           mean((Fine_Silt + Medium_Silt + Coarse_Silt) / (Clay + Fine_Silt + Medium_Silt + Coarse_Silt + Fine_Sand + Medium_Sand + Coarse_Sand), na.rm = T),
                          mean((Fine_Sand + Medium_Sand + Coarse_Sand) / (Clay + Fine_Silt + Medium_Silt + Coarse_Silt + Fine_Sand + Medium_Sand + Coarse_Sand), na.rm = T)), by = Plot]
All_soil_data_f <- merge(All_soil_data_f, Texture)

Grassland_env <- fread("Raw_data/may2019_grassland_functions.csv") # Data available from synthesis dataset, currently not publicly available https://www.bexis.uni-jena.de/ddm/data/Showdata/27087
Grassland_use_env <- Grassland_env[, c("Plot", "pH", "Cstock_2011") := list(Plotn, PH, Soil.C.stock)][, .SD, .SDcols = c("Plot", "pH", "Cstock_2011")]

Climate_data <- fread('Raw_data/Biodiversity\ Exploratories\ Ecosystem\ Function\ data.txt') # Data available from van der Plas et al. 2019 https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2664.13260;  https://doi.org/10.6084/m9.figshare.6870788
env_final <- merge.data.table(Grassland_use_env, Climate_data[, list(Tmean.Annual, Precip.Annual,Plot, Soil.depth = as.numeric(Soil.depth))], all.x = T)
env_final = merge.data.table(env_final, All_soil_data_f, all.x = T)

### Grassland % data 
BEF_data = fread('Raw_data/31018.csv') # Data from https://doi.org/10.1038/s41467-021-23931-1; https://www.bexis.uni-jena.de/ddm/data/Showdata/31018
env_final = merge(env_final, BEF_data[, list(Plot, Grassland.1000.CW)])

### LUI data
# Data can be obtained through the LUI tool https://www.bexis.uni-jena.de/lui/main/index
LUI_global = fread('Raw_data/LUI_standardized_global.txt') 
LUI_regional = fread('Raw_data/LUI_standardized_regional.txt')
LUI_regional[, Year := gsub('separately\\(', '', YEAR)][, Year := as.numeric(gsub('\\)', '', Year))]
LUI_all = merge(LUI_global[, list('LUI_global' = LUI, Year = Year, 'Plot' = ifelse(nchar(PLOTID) == 5, PLOTID, paste(substr(PLOTID, 1, 3), 0, substr(PLOTID, 4, 4), sep = '')))], 
                LUI_regional[, list('LUI_regional' = LUI,Year = Year, 'Plot' = ifelse(nchar(PLOTID) == 5, PLOTID, paste(substr(PLOTID, 1, 3), 0, substr(PLOTID, 4, 4), sep = '')))], by = c('Plot', 'Year'))
LUI_mean = LUI_all[Year >= 2007 & Year < 2013, list(LUI_global = mean(LUI_global), LUI_regional = mean(LUI_regional)), by = Plot]
LUI_mean[, lui_class_global := list(classify_LUI(LUI_global, 'quantile_30'))]
LUI_mean[, lui_class_regional_30 := list(classify_LUI(LUI_regional, 'quantile_30')), by = list('Exploratory' = substr(Plot, 1, 1))]
LUI_mean[, lui_class_regional_20 := list(classify_LUI(LUI_regional, 'quantile_20')), by = list('Exploratory' = substr(Plot, 1, 1))]
LUI_mean[,lui_class_regional_20 := dplyr::recode(lui_class_regional_20, '3' = '2', '5'='3')]

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
# Edibles and charismatics
# Lists provided by Steffen Boch and Valentin Klaus
all_edibles_plants <- c(
 "Achillea_millefolium"             ,  "Achillea_millefolium_aggr."    ,    "Aegopodium_podagraria" ,           
"Alliaria_petiolata"                , "Allium_cf_oleraceum"            ,   "Allium_schoenoprasum"    ,         
"Allium_ursinum"                    , "Allium_vineale"                 ,   "Angelica_sylvestris"     ,         
 "Anthriscus_sylvestris"            ,"Aphanes_arvensis"               ,   "Arctium_lappa"          ,          
 "Arctium_minus"                    ,"Arctium_nemorosum"              ,   "Arctium_sp"             ,          
 "Arctium_tomentosum"               ,"Aruncus_dioicus"                ,   "Astragalus_glycyphyllos",          
 "Bellis_perennis"                  ,"Bistorta_officinalis"           ,   "Botrychium_lunaria"     ,          
 "Brassica_napus"                   ,"Brassica_rapa"                  ,   "Bunium_bulbocastanum"   ,          
 "Cardamine_hirsuta"                ,"Cardamine_pratensis"            ,   "Carum_carvi"            ,          
 "Centaurea_cyanus"                 ,"Chenopodium_album"              ,   "Cichorium_intybus"      ,          
 "Cirsium_oleraceum"                ,"Corylus_avellana"               ,   "Crataegus_sp"           ,          
 "Daucus_carota"                    ,"Elymus_repens"                  ,   "Erigeron_acris"         ,          
 "Fagus_sylvatica"                  ,"Fallopia_convolvulus"           ,   "Fallopia_dumetorum"     ,          
 "Filipendula_ulmaria"              ,"Fragaria_vesca"                 ,   "Fragaria_viridis"       ,          
 "Galium_odoratum"                  ,"Glechoma_hederacea"             ,   "Heracleum_sphondylium"  ,          
 "Hordeum_vulgare"                  ,"Hordeum_vulgare_aggr."          ,   "Hypericum_hirsutum"     ,          
 "Hypericum_maculatum"              ,"Hypericum_montanum"             ,   "Hypericum_perforatum"   ,          
 "Juniperus_communis"               ,"Lamium_album"                   ,   "Lamium_maculatum"       ,          
 "Lapsana_communis"                 ,"Larix_decidua"                  ,   "Leontodon_hispidus"     ,          
 "Lepidium_campestre"               ,"Malus_sp"                       ,   "Mentha_aquatica"        ,          
 "Mentha_arvensis"                  ,"Oenothera_biennis"              ,   "Oenothera_biennis_agg"  ,          
 "Origanum_vulgare"                 ,"Oxalis_acetosella"              ,   "Papaver_rhoeas"         ,          
 "Pastinaca_sativa"                 ,"Phyteuma_orbiculare"            ,   "Phyteuma_spicatum"      ,          
 "Picea_abies"                      ,"Polygonatum_verticillatum"      ,   "Polygonum_hydropiper"   ,          
 "Polygonum_minus"                  ,"Polypodium_vulgare"             ,   "Primula_veris"          ,          
 "Prunus_avium"                     ,"Prunus_padus"                   ,   "Prunus_serotina"        ,          
 "Prunus_sp"                        ,"Prunus_spinosa"                 ,   "Pyrus_communis"         ,          
 "Ranunculus_ficaria"               ,"Raphanus_raphanistrum"          ,   "Ribes_alpinum"          ,          
 "Ribes_uva-crispa"                 ,"Robinia_pseudoacacia"           ,   "Rosa_canina_agg"        ,          
 "Rosa_canina_aggr."                ,"Rosa_sp"                        ,   "Rubus_caesius"          ,          
 "Rubus_fruticosus_corylifolius_agg","Rubus_idaeus"                   ,   "Rubus_sp"               ,          
 "Rumex_acetosa"                    ,"Rumex_acetosella"               ,   "Rumex_sanguineus"       ,          
 "Rumex_thyrsiflorus"               ,"Salvia_pratensis"               ,   "Sambucus_nigra"         ,          
 "Sanguisorba_minor"                ,"Secale_cereale"                 ,   "Secale_sp"             ,           
 "Silene_dioica"                    ,"Silene_vulgaris"                ,   "Sinapis_alba"          ,           
 "Sinapis_arvensis"                 ,"Sisymbrium_officinale"          ,   "Sonchus_oleraceus"     ,           
 "Sorbus_aria"                      ,"Sorbus_aucuparia"               ,   "Sorbus_torminalis"     ,           
 "Spergula_arvensis"                ,"Stellaria_media"                ,   "Symphytum_officinale"  ,           
 "Taraxacum_officinale"             ,"Taraxacum_sp"                   ,   "Thlaspi_arvense"       ,           
 "Thymus_pulegioides"               ,"Tragopogon_pratensis"           ,   "Trifolium_hybridum"    ,           
 "Triticum_aestivum"                ,"Urtica_dioica"                  ,   "Vaccinium_myrtillus"   ,           
 "Valeriana_officinalis"            ,"Valerianella_dentata"           ,   "Valerianella_locusta" )

charismatic_plants <- c(
"Acer_campestre"                  ,    "Acer_platanoides"                   ,"Acer_pseudoplatanus"           ,     
"Acer_sp"                         ,    "Anemone_nemorosa"                   ,"Anemone_ranunculoides"         ,      
"Arum_maculatum"                  ,    "Atropa_belladonna"                  ,"Bellis_perennis"               ,     
 "Betula_pendula"                 ,     "Briza_media"                       , "Calluna_vulgaris"             ,      
 "Campanula_glomerata"            ,     "Campanula_patula"                  , "Campanula_persicifolia"       ,      
 "Campanula_rapunculoides"        ,     "Campanula_rotundifolia"            , "Campanula_sp"                 ,      
 "Campanula_trachelium"           ,     "Carlina_acaulis"                   , "Carpinus_betulus"             ,      
 "Centaurea_cyanus"               ,     "Centaurium_erythraea"              , "Cichorium_intybus"            ,      
 "Colchicum_autumnale"            ,     "Conium_maculatum"                  , "Convallaria_majalis"          ,      
 "Corylus_avellana"               ,     "Daphne_mezereum"                   , "Digitalis_purpurea"           ,      
 "Equisetum_arvense"              ,     "Fraxinus_excelsior"                , "Galium_odoratum"              ,      
 "Gentiana_verna"                 ,     "Gentianella_germanica"             , "Geum_urbanum"                 ,      
 "Hedera_helix"                   ,     "Hepatica_nobilis"                  , "Hordeum_vulgare"              ,      
 "Hordeum_vulgare_aggr."          ,     "Juniperus_communis"                , "Larix_decidua"                ,      
 "Leucanthemum_vulgare_agg"       ,     "Leucanthemum_vulgare_aggr."        , "Leucojum_vernum"              ,      
 "Malus_sp"                       ,     "Myosotis_arvensis"                 , "Myosotis_discolor"            ,      
 "Myosotis_ramosissima"           ,     "Myosotis_sylvatica"                , "Origanum_vulgare"             ,      
 "Oxalis_acetosella"              ,     "Papaver_rhoeas"                    , "Paris_quadrifolia"            ,      
 "Phragmites_australis"           ,     "Populus_tremula"                   , "Primula_elatior_veris_agg"    ,      
 "Primula_veris"                  ,     "Prunus_avium"                      , "Pseudolysimachion_spicatum"   ,      
 "Pulsatilla_vulgaris"            ,     "Quercus_petraea"                   , "Quercus_robur"                ,      
 "Quercus_sp"                     ,     "Rosa_canina_agg"                   , "Rosa_canina_aggr."            ,      
 "Rosa_sp"                        ,     "Salix_caprea"                      , "Salix_cinerea"                ,      
 "Salix_sp"                       ,     "Sambucus_nigra"                    , "Secale_cereale"               ,      
 "Secale_sp"                      ,     "Sorbus_aucuparia"                  , "Taraxacum_officinale"         ,      
 "Taraxacum_sp"                   ,     "Taxus_baccata"                     , "Thymus_pulegioides"           ,      
 "Tilia_cordata"                  ,     "Tilia_platyphyllos"                , "Tilia_sp"                     ,      
 "Trifolium_alpestre"             ,     "Trifolium_arvense"                 , "Trifolium_campestre"          ,      
 "Trifolium_cf_montanum"          ,     "Trifolium_dubium"                  , "Trifolium_hybridum"           ,      
 "Trifolium_medium"               ,     "Trifolium_montanum"                , "Trifolium_pratense"           ,      
 "Trifolium_repens"               ,     "Trifolium_sp"                      , "Triticum_aestivum"            ,      
 "Ulmus_glabra"                   ,     "Urtica_dioica"                     , "Veronica_arvensis"            ,      
 "Veronica_chamaedrys"            ,     "Veronica_filiformis"               , "Veronica_hederifolia"         ,      
 "Veronica_montana"               ,     "Veronica_officinalis"              , "Veronica_peregrina"           ,      
 "Veronica_persica"               ,     "Veronica_polita"                   , "Veronica_serpyllifolia"       ,      
 "Veronica_teucrium"              ,     "Viola_arvensis"                    , "Viola_collina"                ,      
 "Viola_hirta"                    ,     "Viola_reichenbachiana_riviniana_agg", "Viola_sp"                    ,       
 "Viscum_album")   


# Data provided by a survey run by Sophie Peter
most_charismatic_birds = c("Sturnus_vulgaris"    ,    "Ciconia_ciconia"       , "Cuculus_canorus"       ,  "Turdus_merula"           ,"Luscinia_megarhynchos"  ,
  "Erithacus_rubecula"  ,    "Buteo_buteo"           , "Corvus_corax"          ,  "Garrulus_glandarius"     ,"Milvus_milvus"          ,
  "Alauda_arvensis"    ,     "Cyanistes_caeruleus"  ,  "Falco_tinnunculus"    ,   "Troglodytes_troglodytes", "Hirundo_rustica"        ,
  "Parus_major"        ,     "Pica_pica"            ,  "Fringilla_coelebs"    ,   "Grus_grus")

# Abundances and richness for plants and birds
Abundance_grasslands <- fread("Raw_data/27707.txt") # https://www.bexis.uni-jena.de/ddm/data/Showdata/27707
bird_families = data.table(Species =  c("Acrocephalus_palustris"     ,   "Acrocephalus_schoenobaenus" ,   "Acrocephalus_scirpaceus"   ,    "Aegithalos_caudatus"       ,   
                                "Alauda_arvensis"           ,    "Anser_anser"                 ,  "Anthus_pratensis"         ,     "Anthus_trivialis"             ,
                                "Ardea_cinerea"             ,    "Buteo_buteo"                 ,  "Carduelis_cannabina"      ,     "Carduelis_carduelis"          ,
                                 "Carduelis_chloris"        ,     "Carduelis_spinus"           ,   "Ciconia_ciconia"         ,      "Coccothraustes_coccothraustes",
                                 "Columba_oenas"            ,     "Columba_palumbus"           ,   "Corvus_corax"            ,      "Corvus_corone_cornix"         ,
                                 "Corvus_corone_corone"     ,     "Coturnix_coturnix"          ,   "Cuculus_canorus"         ,      "Cyanistes_caeruleus"          ,
                                 "Delichon_urbicum"         ,     "Dendrocopos_major"          ,   "Dryocopus_martius"       ,      "Emberiza_calandra"            ,
                                 "Emberiza_citrinella"      ,     "Emberiza_schoeniclus"       ,   "Erithacus_rubecula"      ,      "Falco_tinnunculus"            ,
                                 "Fringilla_coelebs"        ,     "Gallinago_gallinago"        ,   "Garrulus_glandarius"     ,      "Grus_grus"                    ,
                                 "Hippolais_icterina"       ,     "Hirundo_rustica"            ,   "Jynx_torquilla"          ,      "Lanius_collurio"              ,
                                 "Locustella_naevia"        ,     "Lullula_arborea"            ,   "Luscinia_megarhynchos"   ,      "Milvus_migrans"               ,
                                 "Milvus_milvus"            ,     "Motacilla_alba"             ,   "Motacilla_flava"         ,      "Muscicapa_striata"            ,
                                 "Oenanthe_oenanthe"        ,     "Parus_cristatus"            ,   "Parus_major"             ,      "Parus_montanus"               ,
                                 "Parus_palustris"          ,     "Passer_domesticus"          ,   "Passer_montanus"         ,      "Periparus_ater"               ,
                                 "Pernis_apivorus"          ,     "Phasianus_colchicus"        ,   "Phoenicurus_ochruros"    ,      "Phoenicurus_phoenicurus"      ,
                                 "Phylloscopus_collybita"   ,     "Phylloscopus_trochilus"     ,   "Pica_pica"               ,      "Picus_viridis"                ,
                                 "Prunella_modularis"       ,     "Pyrrhula_pyrrhula"          ,   "Regulus_ignicapillus"    ,      "Regulus_regulus"              ,
                                 "Saxicola_rubetra"         ,     "Saxicola_rubicola"          ,   "Sitta_europaea"          ,      "Sturnus_vulgaris"             ,
                                 "Sylvia_atricapilla"       ,     "Sylvia_borin"               ,   "Sylvia_communis"         ,      "Sylvia_curucca"               ,
                                 "Troglodytes_troglodytes"  ,     "Turdus_iliacus"             ,   "Turdus_merula"           ,      "Turdus_philomelos"            ,
                                 "Turdus_pilaris"           ,     "Turdus_viscivorus"          ,   "Vanellus_vanellus" ),
                                Family = c(  "Acrocephalidae", "Acrocephalidae" ,"Acrocephalidae " ,"Aegithalidae"    ,"Alaudidae"       ,"Anatidae"        ,"Motacillidae"   ,
                                             "Motacillidae"  ,  "Ardeidae"      ,  "Accipitridae"  ,  "Fringillidae"  ,  "Fringillidae"  ,  "Fringillidae"  ,  "Fringillidae"   ,
                                             "Ciconiidae"    ,  "Fringillidae"  ,  "Columbidae"    ,  "Columbidae"    ,  "Corvidae"      ,  "Corvidae"      ,  "Corvidae"       ,
                                             "Phasianidae"   ,  "Cuculidae"     ,  "Paridae"       ,  "Hirundinidae"  ,  "Picidae"       ,  "Picidae"       ,  "Emberizidae"    ,
                                             "Emberizidae"   ,  "Emberizidae"   ,  "Muscicapidae"  ,  "Falconidae"    ,  "Fringillidae"  ,  "Scolopacidae"  ,  "Corvidae"       ,
                                             "Gruidae"       ,  "Acrocephalidae",  "Hirundinidae"  ,  "Picidae"       ,  "Laniidae"      ,  "Locustellidae" ,  "Alaudidae"      ,
                                             "Muscicapidae"  ,  "Accipitridae"  ,  "Accipitridae"  ,  "Motacillidae"  ,  "Motacillidae"  ,  "Muscicapidae"  ,  "Muscicapidae"   ,
                                             "Paridae"       ,  "Paridae"       ,  "Paridae"       ,  "Paridae"       ,  "Passeridae"    ,  "Passeridae"    ,  "Paridae"        ,
                                             "Accipitridae"  ,  "Phasianidae"   ,  "Muscicapidae"  ,  "Muscicapidae"  ,  "Phylloscopidae",  "Phylloscopidae",  "Corvidae"       ,
                                             "Picidae"       ,  "Prunellidae"   ,  "Fringillidae"  ,  "Regulidae"     ,  "Regulidae"     ,  "Muscicapidae"  ,  "Muscicapidae"   ,
                                             "Sittidae"      ,  "Sturnidae"     ,  "Sylviidae"     ,  "Sylviidae"     ,  "Sylviidae"     ,  "Sylviidae"     ,  "Troglodytidae"  ,
                                             "Turdidae"      ,  "Turdidae"      ,  "Turdidae"      ,  "Turdidae"      ,  "Turdidae"      ,  "Charadriidae"   ))


Abundance_grasslands = merge(Abundance_grasslands, bird_families, all = T)
Abundance_grasslands_birds_plants <- Abundance_grasslands[Group_broad %in% c("Plant", "Birds"), ]
Abundance_grasslands_birds_plants[, Group_broad := tolower(gsub("s", "", Group_broad))]
Abundance_grasslands_birds_plants <- Abundance_grasslands_birds_plants[Year < 2013, list(value = sum(value, na.rm = T)), by = c("Species", "Plot", "Group_broad", 'Family')]
Abundance_grasslands_birds_plants[Species == "Delichon_urbica", Species := "Delichon_urbicum"]
Abundance_grasslands_birds_plants[, Exploratory := substr(Plot, 1, 1)]
Abundance_grasslands_birds_plants = Abundance_grasslands_birds_plants[value > 0,]

# Correct some species names
Abundance_grasslands_birds_plants[, Species := dplyr::recode(Species,
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
Abundance_grasslands_birds_plants <- Abundance_grasslands_birds_plants[!(Species %in% c(
  "Unknown_species"
)), ]


Edible_charismatic_richness <- Abundance_grasslands_birds_plants[value > 0,
                                                            list(
                                                              Cover_edible = sum(value[Species %in% all_edibles_plants]), 
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
All_abundances_aggr <- Abundance_grasslands_birds_plants[, list(value = sum(as.numeric(value), na.rm = T)), by = c("Species", "Plot", "Group_broad", 'Family')]
fwrite(All_abundances_aggr, 'All_abundances_aggr.csv')

all_plants <- as.character(unique(All_abundances_aggr[Group_broad == "plant", ]$Species))
all_birds <- as.character(unique(All_abundances_aggr[Group_broad == "bird", ]$Species))
all_bird_families <- as.character(unique(All_abundances_aggr[Group_broad == "bird", ]$Family))

All_abundances_species <- data.table(dcast(All_abundances_aggr, Plot ~ Species, value.var = "value", fill = 0))
All_abundances_families <- data.table(dcast(All_abundances_aggr[!is.na(Family),], Plot ~ Family, value.var = "value", fill = 0, fun.aggregate = sum))

All_abundances_cast = merge(All_abundances_species, All_abundances_families, by = 'Plot')
All_abundances_cast[, Exploratory := substr(Plot, 1, 1)]



### Fill other services
# List of services

ES_functions = fread("Raw_data/27087.csv") # Data available from synthesis dataset, currently not publicly available https://www.bexis.uni-jena.de/ddm/data/Showdata/27087

ES_grasslands = ES_functions[, list(Soil.C.stock_2011 = Soil.C.stock,
                                 Nshoot_2009_2011 = c(Nshoot.2009 + Nshoot.2010 + Nshoot.2011 +Nshoot.2012)/4)]


# Productivity = fread('Raw_data/Productivity.csv')    
# Calculated based on Biomass data 2008-2012 (https://www.bexis.uni-jena.de/ddm/data/Showdata/27087) and code provided in Simons and Weisser (2018)

Flower_cover = fread("Raw_data/15026_Flower numbers_2.1.3/15026.csv") # https://www.bexis.uni-jena.de/ddm/data/Showdata/21688?version=2
Butterflies = Abundance_grasslands[Group_broad == 'Lepidoptera', butterfly_abundance := sum(value, na.rm = T), by = Plot]
Juniperus = Abundance_grasslands[, Uniqueness_juniperus := sum(value[Species == 'Juniperus_communis'], na.rm = T)>0, by = Plot]

ES_grasslands = merge.data.table(ES_grasslands, Flower_cover[, .SD, .SDcols = c('Plot', 'Total_flower_cover')], all = T)
ES_grasslands = merge.data.table(ES_grasslands, Butterflies[, .SD, .SDcols = c('Plot', 'butterfly_abundance')], all = T)
ES_grasslands = merge.data.table(ES_grasslands, Juniperus[, .SD, .SDcols = c('Plot', 'Uniqueness_juniperus')], all = T)
ES_grasslands = merge.data.table(ES_grasslands, Productivity[, .SD, .SDcols = c('Plot', 'Biomass_production')], all = T)

ES_grasslands <- merge.data.table(ES_grasslands[, c("Plot",  "Total_flower_cover", "Biomass_production", "butterfly_abundance", "Soil.C.stock_2011", 'Nshoot_2009_2011')],
                       Edible_charismatic_richness,
                       all.x = T, by = 'Plot'
)

ES_grasslands$Uniqueness_juniperus <- as.numeric(ES_grasslands$Uniqueness_juniperus)


### Data manipulation 
### Fill in empty service values using MICE
services = c( 'Total_flower_cover', 'Biomass_production','Charismatic_birds_richness', 'Nshoot_2009_2011','butterfly_abundance', 'Soil.C.stock_2011',  'Cover_edible', 'Redlist_richness', 'Redlist_cover', 'Charismatic_plants', 'Plant_richness', 'Bird_richness', 'Bird_family_richness', 'Uniqueness_juniperus')
ES_grasslands[, Soil.C.stock_2011 :=as.numeric(Soil.C.stock_2011)]
ES_grasslands_complete  = mice(ES_grasslands[,..services],  group = ES_grasslands$Exploratory, printFlag = F)
ES_grasslands[, services] = mice::complete(ES_grasslands_complete, printFlag = F)

ES_grasslands[, c("sqrtTotal_flower_cover", "sqrtbutterfly_abundance", "sqrtCover_edible", "sqrtCharismatic_plants") :=
                lapply(.SD, sqrt), .SDcols = c("Total_flower_cover", "butterfly_abundance", "Cover_edible", "Charismatic_plants")]

ES_grasslands[, Tot_protein := Biomass_production*Nshoot_2009_2011]

services_all = unique(c(services,c( 'Redlist_richness', 'Redlist_cover', 'sqrtTotal_flower_cover','Tot_protein', 'Charismatic_birds_richness', 'Bird_family_richness', 'Biomass_production', 'Nshoot_2009_2011','sqrtbutterfly_abundance', 'Soil.C.stock_2011',  'Cover_edible',  'Charismatic_plants', 'Plant_richness', 'Bird_richness', 'Bird_family_richness', 'Uniqueness_juniperus')
))

fwrite(file = 'ES_grasslands.csv', ES_grasslands[, c('Plot','Redlist_cover','Biomass_production', 'Nshoot_2009_2011', 'sqrtTotal_flower_cover', 'sqrtbutterfly_abundance',  'Bird_family_richness','Plant_richness',
                                                     'Soil.C.stock_2011', 'Tot_protein','Cover_edible', 'Uniqueness_juniperus', 'Charismatic_plants', 'Charismatic_birds_richness')])

}

