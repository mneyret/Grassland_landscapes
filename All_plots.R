# # This series of scripts reproduces the results and sensitivity analyses of the paper: 
# Neyret et al. (2021). Assessing the impact of grassland management on landscape multifunctionality. Ecosystem Services.

# This final script takes as input the outputs of Calculate_MF.R and 
# creates all figures presented in the paper and SI

### Correlations ####
print('all plots')
if (method != 'Threshold_perc'){
  
library(svglite)
 get_corr = function(data){
  colnames(data) = c('Production','Aesthetic value','Regional ID', 'Foraging','C stocks', 'Conservation value' )
  M<-corr.test(data, use = "pairwise")
  M$r[lower.tri(M$r, diag = TRUE)] = NA
  tmp = melt(M$r, value.name = 'correlation')
  M$p[lower.tri(M$p, diag = TRUE)] = NA
  tmp_p = melt(M$p, value.name = 'p.value')
  return(list(correlation = tmp$correlation, p.value = tmp_p$p.value, Var1 =tmp_p$Var1, Var2 = tmp$Var2 ))
}

data_average[,  corr.test(.SD),   .SDcols = c( '$Prod','$Aest','$RegId','$Forag', '$C_stock' , '$Ric')]

all_corr = data_average[,  get_corr(.SD),   by= Explo, .SDcols = c( '$Prod','$Aest','$RegId','$Forag', '$C_stock' , '$Ric')]
all_corr = all_corr[!is.na(all_corr$correlation),]

all_corr[, Region := factor(Explo)]
levels(all_corr$Region) = c('South-West', 'Central', 'North')
all_corr_plot = ggplot(all_corr, aes(x = Var1, y = Var2, fill = correlation, size = abs(correlation))) +
  geom_point(shape = 21, color = 'black')  +
  facet_wrap(~ Region, nrow = 1) + ylab('') + xlab('') +
  geom_point(data = all_corr[all_corr$p.value > 0.05,], shape = 4, size = 5) +
  scale_fill_gradientn(limits = c(-0.8, 0.8), colors = col2(50), breaks = seq(-0.6, 0.6, 0.3), name = "Correlation") +
  scale_size(range = c(2, 10), name = "Correlation", breaks = seq(0, 0.5, 0.1), guide = F) + theme_bw() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal', axis.text.x = element_text(angle = 45, hjust = 1))

if(saveplots){
  ggsave(plot = all_corr_plot, paste(c('Sensitivity_analyses/corrplot_envcorr', environmental_correction,'_lui', lui_class_method, '_plots', no_plots, '_', method_within_landscape, '_method', method, threshold_to_use, '.pdf'), collapse = ""), width = 7, height = 3.5)
}

}

##### Ternary plots manual #####

add_padding = function(x, padding = 0.1){
  y1 = scale(x, center = TRUE, scale = TRUE)
  y2 = (1+ padding)*y1
  y = y2*sd(x) + mean(x)
  return(as.numeric(y))
}

interpolate_tern = function(data_to_plot, tern = TRUE){
 # Grid creation
    if(tern){
  mygrid = data.frame(rbind(
    expand.grid(
      x = seq(0, 1, length.out = 100),
      y = seq(0, 1, length.out = 100),
      Region = unique(data_to_plot$Region),
      pretty_variables = unique(data_to_plot$pretty_variables),
      value = NA
    )))
  mygrid$high = mygrid$x - mygrid$y*tan(pi/6)
  mygrid$medium = mygrid$y/(tan(pi/3)*0.5)
  mygrid$low = 1 - mygrid$high  - mygrid$medium
  }

  if(!tern){
    mygrid = data.frame(rbind(
      expand.grid(
        lui_mean = seq(1, 3, length.out = 200),
        lui_cv = seq(0, 0.7, length.out = 200),
        Explo = c('A', 'H', 'S'),
        Region = unique(data_to_plot$Region),
        pretty_variables = unique(data_to_plot$pretty_variables),
        value = NA,
        in_range = NA
      )))
    data_to_plot[, lui_mean_padded := add_padding(lui_mean, 0.1),
                 by = Region][, lui_cv_padded := add_padding(lui_cv, 0.1), by = Region]

        ch <- data_to_plot[, .SD[c(chull(.SD), chull(.SD)[1])], .SDcols = c('lui_mean_padded', 'lui_cv_padded'), by = Region]
    polyA <- SpatialPolygons(list(Polygons(list(Polygon(ch[ch$Region == 'South-West', 2:3])),1)))
    polyH <- SpatialPolygons(list(Polygons(list(Polygon(ch[ch$Region == 'Central', 2:3])),1)))
    polyS <- SpatialPolygons(list(Polygons(list(Polygon(ch[ch$Region == 'North', 2:3])),1)))

    mygrid_poly= mygrid
    coordinates(mygrid_poly) <- ~ lui_mean+lui_cv

    mygrid[mygrid$Region == 'South-West',]$in_range <- over(mygrid_poly[mygrid$Region == 'South-West',], polyA)
    mygrid[mygrid$Region == 'Central',]$in_range <- over(mygrid_poly[mygrid$Region == 'Central',], polyH)
    mygrid[mygrid$Region == 'North',]$in_range <- over(mygrid_poly[mygrid$Region == 'North',], polyS)
  }

  mygrid = merge(mygrid, unique(data_to_plot[, c('pretty_variables', 'nservices')]), by = 'pretty_variables')
  rlist = expand.grid(Region = unique(data_to_plot$Region), pretty_variables = unique(data_to_plot$pretty_variables), r2 = NA, r = NA, range = NA)
  rlist = merge(rlist, unique(data_to_plot[, c('pretty_variables', 'nservices')]), by = 'pretty_variables')

  # Fill grid with model interpolation

  for (Exp in unique(data_to_plot$Region)){
    for (var in unique(data_to_plot$pretty_variables)){
     if (tern){
      modelbetalin = glm(value ~ poly(low, high, degree = 2), data = data_to_plot[pretty_variables == var &  Region == Exp & !is.na(value)], family = "binomial")
      mygrid[mygrid$Region == Exp & mygrid$pretty_variables ==var,]$value = predict(modelbetalin,  mygrid[mygrid$Region == Exp & mygrid$pretty_variables ==var,], type="response")
      mygrid = mygrid[mygrid$high >= 0,]
      mygrid = mygrid[mygrid$low >= 0,]
      }
      if (!tern){
        modelbetalin = glm(value ~ poly(lui_mean, lui_cv, degree = 2), data = data_to_plot[pretty_variables == var &  Region == Exp & !is.na(value)], family = "binomial")
        mygrid[mygrid$Region == Exp & mygrid$pretty_variables ==var,]$value = predict(modelbetalin,  mygrid[mygrid$Region == Exp & mygrid$pretty_variables ==var,], type="response")
        mygrid <- mygrid[!is.na(mygrid$in_range),]
        }

      rlist[rlist$Region == Exp& rlist$pretty_variable ==var,'r2'] = as.character(round(r.squaredLR(modelbetalin)[1], 2))
      rlist[rlist$Region == Exp& rlist$pretty_variable ==var,'range'] = diff(quantile(range(mygrid[mygrid$Region == Exp & mygrid$pretty_variables == var,]$value, na.rm = T), c(0.025, 0.975)))    #range(mygrid[mygrid$Region == Exp & mygrid$pretty_variables == var,]$value, na.rm = T))
    }}

  rlist$r = paste("R^2==",rlist$r2 , sep = '')
  return(list(grid = mygrid, R2 = rlist[, c('pretty_variables','Region', 'nservices','r','r2')], 
              range = rlist[, c('pretty_variables','Region','range','r','r2', 'nservices')] ))
}


 outplot_tern = function(mygrid, rlist, tern = TRUE){
   if(!tern){
     mygrid$x = mygrid$lui_mean
     mygrid$y = mygrid$lui_cv
   }
print("a")
     gg =   ggplot(mygrid, aes(z = value, x= x, y = y, fill = value, color = value)) +
       geom_raster() +  theme_bw() +
       facet_grid(facets =  Region ~  pretty_variables, labeller = label_wrap_gen(width=30)) +
      # facet_nested(cols = vars(pretty_variables), rows = vars(nline, Region), switch = 'y') +
       scale_fill_gradientn(colors = col2(20), name = "Multifunctionality", limits=c(0.0,1.0)) +
       scale_color_gradientn(colors = col2(20), name = "Multifunctionality", limits=c(0.0,1.0))
     print("b")
     if(tern){
       print("c")
       gg = gg + geom_text(data = data.frame(lab = c('%l', '%m', '%h'), x = c(0, 0.62, 0.95), y = c(-0.05, 0.85, -0.05)),
                 aes(x = x, y =y, label = lab), inherit.aes = F, size = 3, fontface = "italic")  +
         theme(panel.background = element_blank(),
               panel.border = element_blank(),
               axis.title=element_blank(),
               axis.text=element_blank(),
               axis.ticks=element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               aspect.ratio=1) +
        geom_text(data = rlist, aes(x = 0.2, y = 0.93, label = r), inherit.aes = F, parse = TRUE, size = 4) +
        geom_path(data = data.frame(x = c(0, 0.5, 1, 0, 0.5), y = c(0, 0.86, 0, 0, 0.86)), aes(x = x, y= y), inherit.aes = F, lwd = 0.7) 
       }
     if(!tern){
       print("d")
       gg = gg + scale_y_continuous(position = "right") +
         theme( aspect.ratio=1) + ylab('LUI coefficient of variation') + xlab('LUI mean') + xlim(c(0.8, 3))+
         geom_text(data = rlist, aes(x = 2, y = 0.73, label = r), inherit.aes = F, parse = TRUE, size = 4)
     }
     print("e")
     return(gg)
      }

 data_average_melt[, nline := ifelse(nservices == 1, 'Single service',
                                     ifelse(nservices %in% 2:3, '2-3 services','Multiple services')), by = nservices]
 data_average_melt[, pretty_variables := gsub('C_stock', 'C stock', variable), by = variable]
 data_average_melt[, pretty_variables := gsub('_', ' + ', pretty_variables), by = pretty_variables]
 data_average_melt[, pretty_variables := gsub('RegId', 'Reg ID', pretty_variables), by = pretty_variables]
 
 fwrite(data_average_melt, '/Users/Margot/Desktop/savedata.csv')
 
 ### Fig 4
 plots_fig4 <- lapply(unique(data_average_melt$nservices>1), function(o) {
   plots_fig4_data =  interpolate_tern(data_average_melt[(data_average_melt$nservices>1)==o & Region == 'Central' & data_average_melt$pretty_variables %in% c('Ric', 'Prod', 'Aest', 'C stock', 'Reg ID', 'Forag',
                                                                                                               'Ric + Prod', 'Prod + C stock', 'Ric + Aest', 'Ric + Forag + Reg ID','Ric + Prod + Aest + C stock', 'Ric + Prod + Aest + C stock + Forag + Reg ID'),])
   plots_fig4_data$grid[, 'pretty_variables'] = factor(plots_fig4_data$grid[, 'pretty_variables'] , levels = c('Ric', 'Prod', 'Aest', 'C stock', 'Reg ID', 'Forag',
                                                                                                               'Ric + Prod', 'Prod + C stock', 'Ric + Aest', 'Ric + Forag + Reg ID','Ric + Prod + Aest + C stock', 'Ric + Prod + Aest + C stock + Forag + Reg ID'))
   outplot_tern(plots_fig4_data$grid, plots_fig4_data$R2)
 })
 
 grobs_fig4 = lapply(plots_fig4, ggplotGrob)
 gg_fig4 = grid.arrange(grobs = grobs_fig4, ncol = 1,  align = "v", axis = "l")
 ggsave(plot = gg_fig4, filename =  paste(c('Sensitivity_analyses/Fig4_', environmental_correction,'_lui', lui_class_method, '_plots', no_plots, '_', method_within_landscape, '_method', method, threshold_to_use, '2.pdf'), 
                                          collapse = ""), width = 10, height = 5)
 
 
 plots_fig5_data = interpolate_tern(data_average_melt[Region == 'Central' & data_average_melt$pretty_variables %in% c('Ric + Prod','Ric + Aest', 'Ric + Aest + Reg ID','Ric + Aest + C stock', 'Ric + Prod + Aest + C stock', 'Ric + Prod + Aest + C stock + Forag + Reg ID'),])
 plots_fig5_data$grid[, 'pretty_variables'] = factor( plots_fig5_data$grid[, 'pretty_variables'] , levels = c('Ric + Prod','Ric + Aest', 'Ric + Aest + Reg ID','Ric + Aest + C stock', 'Ric + Prod + Aest + C stock', 'Ric + Prod + Aest + C stock + Forag + Reg ID'))
 plots_fig5  = outplot_tern(plots_fig5_data$grid, plots_fig5_data$R2)
 ggsave(plot = plots_fig5, filename =  paste(c('Sensitivity_analyses/Fig5_', environmental_correction,'_lui', lui_class_method, '_plots', no_plots, '_', method_within_landscape, '_method', method, threshold_to_use, '2.pdf'), collapse = ""), width = 11, height = 3)
 
 ### Stakeholders
 
 plots_figstakeholders_data = interpolate_tern(data_average_melt[ data_average_melt$pretty_variables %in% c("Locals"  ,    "Tourism" ,     "Agric",    "Nat + cons + asso"),])
 #levels(plots_figstakeholders_data$grid[, 'pretty_variables']) = c("Locals"  ,    "Tourism" ,     "Agric",    "Nat cons asso")
 plots_figstakeholders  = outplot_tern(plots_figstakeholders_data$grid, plots_figstakeholders_data$R2) +
   scale_fill_gradientn(colors = col2(20), name = "Multifunctionality", limits=c(0.2,0.8)) +
   scale_color_gradientn(colors = col2(20), name = "Multifunctionality", limits=c(0.2,0.8))
   
 ggsave(plot = plots_figstakeholders, filename =  paste(c('Sensitivity_analyses/Figstakeholders_', environmental_correction,'_lui', lui_class_method, '_plots', no_plots, '_', method_within_landscape, '_method', method, threshold_to_use, '2.pdf'), collapse = ""), width = 11, height = 5)
 

 
 
 ### Supplementary
 plots_SI <- lapply(unique(data_average_melt$nline), function(o) {
   plots_SI_data = interpolate_tern(data_average_melt[data_average_melt$nline == o & data_average_melt$pretty_variables %in% c('Ric', 'Prod', 'Aest', 'C stock', 'Reg ID', 'Forag',
                                                                                        'Ric + Prod', 'Prod + C stock', 'Prod + Forag','Ric + Prod + Reg ID', 'Ric + Aest + Reg ID','Ric + Prod + Aest',
                                                                                        'Ric + Prod + Aest + C stock','Ric + Prod + C stock + Reg ID', 'Ric + Prod + Aest + Forag','Ric + Prod + Aest + C stock + Reg ID', 'Ric + Prod + Aest + C stock + Forag', 'Ric + Prod + Aest + C stock + Forag + Reg ID'),])
   plots_SI_data$grid[, 'pretty_variables'] = factor(plots_SI_data$grid[, 'pretty_variables'] , levels = c('Ric', 'Prod', 'Aest', 'C stock', 'Reg ID', 'Forag',
                                                                                                            'Ric + Prod', 'Prod + C stock', 'Prod + Forag','Ric + Prod + Reg ID', 'Ric + Aest + Reg ID','Ric + Prod + Aest',
                                                                                                            'Ric + Prod + Aest + C stock','Ric + Prod + C stock + Reg ID', 'Ric + Prod + C stock + Forag', 'Ric + Prod + Aest + Forag','Ric + Prod + Aest + C stock + Reg ID', 'Ric + Prod + Aest + C stock + Forag', 'Ric + Prod + Aest + C stock + Forag + Reg ID'))
   outplot_tern(plots_SI_data$grid, plots_SI_data$R2)
 })
 
 grobs_SI = lapply(plots_SI, ggplotGrob)
 gg_SI = grid.arrange(grobs = grobs_SI, ncol = 1,  align = "v", axis = "l")
 ggsave(plot = gg_SI, filename =  paste(c('Sensitivity_analyses/TERN_SI_', environmental_correction,'_lui', lui_class_method, '_plots', no_plots, '_', method_within_landscape, '_method', method, threshold_to_use, '2.pdf'), collapse = ""), width = 10, height = 15)

 
 if (method == 'Threshold' & no_plots == 10 & lui_class_method == 'luiquantile_30' ){
 data_average_melt[, lui_cv := lui_sd/lui_mean]
   plots_SI_lui <- lapply(unique(data_average_melt$nline), function(o) {
     plots_SI_lui_data = interpolate_tern(tern = F, data_average_melt[data_average_melt$nline == o & data_average_melt$pretty_variables %in% c('Ric', 'Prod', 'Aest', 'C stock', 'Reg ID', 'Forag',
                                                                                                                 'Ric + Prod', 'Prod + C stock', 'Prod + Forag','Ric + Prod + Reg ID', 'Ric + Aest + Reg ID','Ric + Prod + Aest',
                                                                                                                 'Ric + Prod + Aest + C stock','Ric + Prod + C stock + Reg ID', 'Ric + Prod + Aest + Forag','Ric + Prod + Aest + C stock + Reg ID', 'Ric + Prod + Aest + C stock + Forag', 'Ric + Prod + Aest + C stock + Forag + Reg ID'),])
     plots_SI_lui_data$grid[, 'pretty_variables'] = factor(plots_SI_lui_data$grid[, 'pretty_variables'] , levels = c('Ric', 'Prod', 'Aest', 'C stock', 'Reg ID', 'Forag',
                                                                                                             'Ric + Prod', 'Prod + C stock', 'Prod + Forag','Ric + Prod + Reg ID', 'Ric + Aest + Reg ID','Ric + Prod + Aest',
                                                                                                             'Ric + Prod + Aest + C stock','Ric + Prod + C stock + Reg ID', 'Ric + Prod + C stock + Forag', 'Ric + Prod + Aest + Forag','Ric + Prod + Aest + C stock + Reg ID', 'Ric + Prod + Aest + C stock + Forag', 'Ric + Prod + Aest + C stock + Forag + Reg ID'))
    outplot_tern(plots_SI_lui_data$grid, plots_SI_lui_data$R2,tern = F)
   })
 grobs = lapply(plots_SI_lui, ggplotGrob)
 gg = grid.arrange(grobs = grobs, ncol = 1,  align = "v", axis = "l")
 ggsave(plot = gg, filename =  paste(c('Sensitivity_analyses/MEAN_SD', environmental_correction,'_lui', lui_class_method, '_plots', no_plots, '_', method_within_landscape, '_method', method, threshold_to_use, '2.pdf'), collapse = ""), width = 10, height = 12)
}


#### Multifunctionality range depending on ... ####
Ranges =  data.table(interpolate_tern(data_average_melt, tern = TRUE)$range)
Ranges = Ranges[nservices != 0,]
Ranges$range = as.numeric(Ranges$range)
data_plots2 = merge.data.table(data_plot, env_data[,.SD, .SDcols = colnames(env_data)[!(colnames(env_data) %in% c('LUI', 'Exploratory'))]], by = 'Plot')
data_plots2[ , Region := factor(data_plots2$Exploratory)]
levels(data_plots2$Region) = c('South-West', 'Central', 'North')
Ranges = Ranges[order(Ranges$nservices),]

Ranges$Richness    = ifelse(grepl('Ric', Ranges$pretty_variables), 1, 0)
Ranges$Tot_protein = ifelse(grepl('Prod', Ranges$pretty_variables), 1, 0)
Ranges$Aesthetic   = ifelse(grepl('Aest', Ranges$pretty_variables), 1, 0) 
Ranges$C_stock = ifelse(grepl('stock', Ranges$pretty_variables), 1, 0)   
Ranges$Foraging   = ifelse(grepl('Forag', Ranges$pretty_variables), 1, 0) 
Ranges$Reg_id = ifelse(grepl('Reg', Ranges$pretty_variables), 1, 0) 

Ranges$mean_abs  = NA
Ranges$SRV  = NA
Ranges$SRM = NA
Ranges$coeff_ratio = NA
Ranges$corr_LUI = NA
Ranges$corr_ratio = NA
Ranges$cor_max= NA
data_plots2$Foraging = data_plots2$Harvesting
for (i in 1:nrow(Ranges)){
  explo = Ranges[i,]$Region
  a = Ranges[i, c("Tot_protein", 'Richness', 'Aesthetic', 'C_stock', 'Foraging', 'Reg_id')]
  b = unlist(a); b = names(b[b>0])
 
  # Correlation among the 4 main ES
  M<-cor(data.frame(data_plots2[Region == explo, c("Tot_protein", 'Richness', 'Aesthetic', 'C_stock', 'Foraging', 'Reg_id')]))
  M[ upper.tri(M, diag = TRUE)] = NA
  Ranges$meancor[i] = mean(M[b,b], na.rm = T)
  
  M_lui<-cor(data.frame(data_plots2[Region == explo, c("Aesthetic", "Richness", "Tot_protein","C_stock", 'Foraging', 'Reg_id', 'LUI')]))[7,]

  Ranges$SRV[i] = var(as.numeric(M_lui[b]))
  Ranges$SRM[i] = mean(abs(as.numeric(M_lui[b])))
  
  # Correlation between ES and LUI / correlation between ES and main env factor
  if (Ranges$nservices[i] ==1){
    v = as.character(Ranges$pretty_variables[i])
    if( v == "Ric"){ v = "Richness"}
    if( v == "Aest"){ v = "Aesthetic"}
    if( v == "Forag"){ v = "Foraging"}
    if( v == "Prod"){ v = "Tot_protein"}
    if( v == "Reg ID"){ v = "Reg_id"}
    if( v == "C stock"){ v = "C_stock"}
    
    
    M2 = cor(data.frame(data_plots2[Region == explo, c(v,"LUI", env_variables), with=FALSE]))[,1]
    Ranges$corr_LUI[i] = M2[2]
    Ranges$cor_max[i] = names(M2[M2 == max(M2[3:10])])
    Ranges$corr_ratio[i] = abs(M2[2])/sum(abs(M2[3:10]))
    
    model_coeff = numeric(9)
    names(model_coeff) = c("LUI", env_variables)
    for (k in names(model_coeff)){
      mod = lm(scale(unlist(data_plots2[Region == explo, ..v]))~ scale(unlist(data_plots2[data_plots2$Region == explo, ..k])))
      model_coeff[k] = coefficients(mod)[2]
    }
    Ranges$coeff_ratio[i] = abs(model_coeff[1])/max(abs(model_coeff[2:9]))
    
    }}

Ranges = data.table(Ranges)

# Model: multifunctionality range variation with the ratio of LUI effect: environmental effects
print(summary(lm(range~coeff_ratio, Ranges[nservices==1,])))
pcor = paste(summary(lm(range~coeff_ratio, Ranges[nservices==1,]))$coefficients[2,c(1, 2, 4) ], collapse = "--")
gg_ranges_corr_ratio = ggplot(Ranges[nservices==1 ,], aes(x = coeff_ratio, y = range, color = Region)) + 
  geom_smooth( aes(x = coeff_ratio, y = range), method = 'lm', inherit.aes = FALSE, color = 'black',
               alpha = 0.2) +
  geom_point(size = 3) + 
  ylab('Ecosystem service range') + xlab('Relative strength of LUI effect') +
  scale_color_brewer(palette = "Accent", name = "Region")+
  theme_bw() +
  theme(legend.position = 'none')

if(saveplots){
  ggsave(plot = gg_ranges_corr_ratio, paste(c('Sensitivity_analyses/ggrangescorrratio', environmental_correction,'_lui', lui_class_method, '_plots', no_plots, '_', method_within_landscape, '_method', method, threshold_to_use, '.pdf'), collapse = ""), width = 5, height = 3.5)
}

# Model: multifunctionality range variation with the service response variance
Ranges[, SRM_high := SRM>median(SRM)]
summary(lm(range~SRV*SRM_high, Ranges[nservices>0 & SRM_high == TRUE,])) 
print(summary(lm(range~SRV*SRM_high, Ranges[nservices>0,])))

mod = lm(range~SRV*SRM_high, Ranges[nservices>0,])
psrv = paste(summary(emtrends(mod, ~SRM_high, var = 'SRV'))[2, 2], 
             summary(emtrends(mod, ~SRM_high, var = 'SRV'))[2, 3],
             summary(mod)$coefficients[4,c( 4) ], sep = "--")
gg_ranges_srv = ggplot( Ranges[nservices>0,], aes(x = SRV, y = range, color = Region)) + 
  geom_smooth( aes(x = SRV, y = range, linetype = SRM_high), method = 'lm', inherit.aes = FALSE, color = 'black',
               alpha = 0.2) +
  geom_point(size = 3, aes(shape = SRM_high)) + 
  scale_shape_manual(values = c(1, 16)) + 
  scale_linetype_manual(values = c(2, 1)) + 
  
  ylab('Multifunctionaliy range') + xlab('Service response variance') +
  scale_color_brewer(palette = "Accent", name = "Region")+
  theme_bw() +
  theme(legend.position = 'none') 

if(saveplots){
  ggsave(plot = gg_ranges_srv, paste(c('Results/Sensitivity_analyses/ggrangessrv', environmental_correction,'_lui', lui_class_method, '_plots', no_plots, '_', method_within_landscape, '_method', method, threshold_to_use, '.pdf'), collapse = ""), width = 5, height = 3.5)
}

print(summary(lm(range~nservices, Ranges )))
pnvar = paste(summary(lm(range~nservices, Ranges))$coefficients[2,c(1, 2, 4) ], collapse = "--")

# Model: multifunctionality range variation with the number of services included
gg_ranges_nES = ggplot(Ranges, aes(x = nservices, range, color = Region, fill = Region)) + 
  geom_smooth(data = Ranges, aes(x = nservices, range), color = "black", inherit.aes = F
              , method = 'lm', alpha = 0.2) +
  geom_point(size = 3) +
  ylab('Multifunctionality range') + xlab('Number of ecosystem services included') + 
  scale_fill_brewer(palette = "Accent", name = "Region") +
  scale_color_brewer(palette = "Accent", name = "Region")+
  theme_bw() + 
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.position = 'none' ) 

if(saveplots){
  ggsave(plot = gg_ranges_nES, paste(c('Sensitivity_analyses/ggrangesnES', environmental_correction,'_lui', lui_class_method, '_plots', no_plots, '_', method_within_landscape, '_method', method, threshold_to_use, '.pdf'), collapse = ""), width = 5, height = 3.5, dpi = 800,   bg = "transparent")
}


# This fills the table in which model results (equivalent to Table C2)
a =paste(c(environmental_correction,'_lui', lui_class_method, '_plots', no_plots, '_', method_within_landscape, '_method', method, threshold_to_use, '_', pcor, '_', psrv, '_', pnvar), collapse = "")
write.table(a, file = "Result_data.csv", append = TRUE)

