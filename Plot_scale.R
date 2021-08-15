# Extract correlation matrix
print("plot")

#corrtest_mat <- corr.test(ES_grasslands_corr[, c(2,4:15)])
#a <- matrix(paste(
#  round(corrtest_mat$r, 2),
#  ifelse(corrtest_mat$p > 0.05, "",
#    ifelse(corrtest_mat$p > 0.01, "*",
#      ifelse(corrtest_mat$p > 0.001, "**", "***")
#    )
#  )
#), nrow = 13)
#colnames(a) <- rownames(a) <- colnames(corrtest_mat$r)


ES_grasslands_corr <- cbind(
  ES_grasslands[, .SD, .SDcols = c("Plot", 'Uniqueness_juniperus', 'Exploratory')],
  ES_grasslands[,
      lapply(.SD, function(x) {
        x <- as.numeric(x)
        r <- .BY[[1]]
        mod <- step(lm(x ~ ., data = env_data[, c("Exploratory", "Tmean.Annual",'Precip.Annual', "Soil.depth", "pH", "Clay", 'Sand',
                                                                  "Grassland.1000.CW",
                                                                  "TWI")]), trace = F)
        return(residuals(mod))
      }),
     # by = Exploratory,
      .SDcols = services_all[services_all != 'Uniqueness_juniperus']
    ]
  )

# Store data for further use
if (environmental_correction == TRUE) {
  es_data = copy(ES_grasslands_corr)
} else {
  es_data = copy(ES_grasslands)
}



# SPlot-level analysis
if (environmental_correction == TRUE) {
  data_plot = copy(ES_grasslands_corr)
  ES_grasslands_corr = merge(ES_grasslands_corr, env_data[, c('Plot', 'LUI')], by = 'Plot')
} else {
  data_plot = copy(ES_grasslands)
  ES_grasslands = merge(ES_grasslands, env_data[, c('Plot', 'LUI')], by = 'Plot')
  
}
data_plot[, (services_all) := lapply(.SD, as.numeric), .SDcols = services_all]
data_plot[, (services_all[services_all !='Uniqueness_juniperus']) := lapply(.SD, scale01), by = Exploratory, .SDcols = services_all[services_all !='Uniqueness_juniperus']]
data_plot <- data_plot[, `:=`(
  Aesthetic = (scale01(sqrtTotal_flower_cover) + scale01(sqrtbutterfly_abundance) + scale01(Bird_family_richness)) / 3,
  Richness = (Plant_richness),
  C_stock = Soil.C.stock_2011,
  Productivity = Tot_protein,
  Harvesting = (Cover_edible),# + Fungi_richness) / 2,
  Reg_id = (scale01(Uniqueness_juniperus) + scale01(Charismatic_plants) + scale01(Charismatic_birds_richness)) / 3
)]

data_plot[, c("luiclass", "LUI")] <- env_data[, c("luiclass", "LUI")]
data_plot[, luiclass := as.factor(luiclass)]
data_plot$Exploratory <- factor(data_plot$Exploratory)

data_plot_melt <- melt.data.table(data_plot, id.var = c("Plot", "Exploratory", "luiclass", "LUI"))

data_plot_melt[, Region := Exploratory]
levels(data_plot_melt$Region) <- c("SW", "C", "N")

data_plot_melt[, pretty_variables := dplyr::recode(variable,
  "sqrtTotal_flower_cover" = "Flower cover",
  "Tot_protein" = "Productivity",
  "Productivity" = "Biomass production",
  "sqrtbutterfly_abundance" = "Butterfly abundance",
  "C_stock" = "C stock",
  "Nshoot_2009_2001" = "Plant protein content",
 "Redlist_cover" = "Cover by redlist species",
#  "Redlist_richness" = "Richness of redlist species",
  "Charismatic_plants" = "Cover by charismatic plants",
  "Charismatic_birds_richness" = "Richness of charismatic birds",
  "Plant_richness" = "Plant richness",
  "Bird_richness" = "Bird richness",
  "Bird_family_richness" = "Bird family richness",
  "Uniqueness_juniperus" = "Juniperus grassland",
#  "Fungi_richness" = "Edible fungi richness",
  "Aesthetic" = "Aesthetic value",
  "Richness" = "Conservation value",
  "Harvesting" = "Harvesting",
  "Reg_id" = "Regional identity"
)]

data_plot_melt$pretty_variables <- factor(data_plot_melt$pretty_variables,
  levels = c(
    "Conservation value",
  "Plant richness",
 # "Bird richness",
#  "Cover by redlist species",
  #"Richness of redlist species",
 'Bird richness',
  
    "Aesthetic value",
  "Flower cover",
  "Butterfly abundance",
  "Bird family richness",

    "Productivity",
  "Biomass production",
  "Plant protein content",

    "C stock",
    "Harvesting",
# "Cover by edible plants",
# "Edible fungi richness",
    
   "Regional identity" ,  
 "Cover by charismatic plants",
 "Richness of charismatic birds",
 "Juniperus grassland"
 )
)


#data_plot_letters <- data_plot_melt[!is.na(pretty_variables), model_letters(value, luiclass, Region), by = list(pretty_variables)]
#to_print <- data_plot_melt[, list(mean = mean(value), sd = sd(value)), by = list(pretty_variables, Region, luiclass)]

#to_print <- merge(data_plot_letters, to_print)
#to_print[, to_print := paste(round(mean, 1), " +/- ", round(sd, 1), " (", gsub(" ", "", Letter), ")", sep = "")]

#View(dcast(to_print, pretty_variables + luiclass ~ Region, value = to_print))
library(ggpubr)
data_plot_melt = data_plot_melt[!is.na(pretty_variables) & !is.na(luiclass),]


stat.test_main <- data_plot_melt[(pretty_variables %in% c(  "Conservation value",   "Aesthetic value",  "Productivity",  "C stock",   "Harvesting",  "Regional identity")),] %>%
  group_by(pretty_variables, Region) %>%
  t_test( value ~ luiclass, p.adjust.method = 'fdr', conf.level = 0.95) %>%
  add_xy_position(x = "Region", dodge = 0.8)
stat.test_main = data.table(stat.test_main)
stat.test_main[p.adj.signif == 'ns' & p.adj < 0.1, p.adj.signif := '°']


bxp_main <- ggboxplot(
  data_plot_melt[(pretty_variables %in% c(  "Conservation value",   "Aesthetic value",  "Productivity",  "C stock",   "Harvesting",  "Regional identity")),], x = "Region", y = "value", 
  fill  = "luiclass") + 
  stat_pvalue_manual(data = stat.test_main[p.adj.signif != 'ns',], label = "p.adj.signif", tip.length = 0) +
  facet_wrap(~pretty_variables, ncol = 3) +
  theme_bw() +
  scale_fill_brewer(palette = "Accent", name = "Land use intensity", labels = c("Low", "Medium", "High")) +
  xlab("Region") +
  ylab("Standardised value") 

stat.test_full <- data_plot_melt[!(variable == 'Uniqueness_juniperus' & Region %in% c('C','N')),] %>%
  group_by(pretty_variables, Region) %>%
  t_test( value ~ luiclass, p.adjust.method = 'fdr') %>%
  add_xy_position(x = "Region", dodge = 0.8)
stat.test_full = data.table(stat.test_full)
stat.test_full[p.adj.signif == 'ns' & p.adj < 0.1, p.adj.signif := '°']



extract_model = function(x, y){
  mod = lm(x ~ y)
  mod.emm <- emmeans(mod, "y", type = "response")
  mcld = multcomp::cld(mod.emm, alpha = 0.05, Letters = letters)
  mcld = mcld[order(mcld$y),]
  return(list(luiclass= mcld$y, 'text' =paste(round(mcld$emmean, 2), ' +/- ', round(mcld$SE, 2), ' ', mcld$.group, sep = '')))
}

SI_table = data_plot_melt[, extract_model(value, luiclass), by = c('Exploratory', 'variable')]
View(dcast(SI_table, variable + luiclass~ Exploratory))

bxp_full <- ggboxplot(
  data_plot_melt, x = "Region", y = "value", 
  fill  = "luiclass") + 
  stat_pvalue_manual(data = stat.test_full, label = "p.adj.signif", tip.length = 0) +
  facet_wrap(~pretty_variables, ncol = 4) +
  theme_bw() +
  scale_fill_brewer(palette = "Accent", name = "Land use intensity", labels = c("Low", "Medium", "High")) +
  xlab("Region") +
  ylab("Standardised value") 
bxp_full

if (saveplots) {
  ggsave(plot = bxp_main, paste(c("/Users/Margot/Desktop/Research/Senckenberg/Project_Landscape_MF/Landscape_composition/Results/Sensitivity analyses/boxplot", environmental_correction, "_lui", lui_class_method, ".pdf"), collapse = ""), width = 10)
}
