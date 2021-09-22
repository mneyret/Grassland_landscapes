# This series of scripts reproduces the results and sensitivity analyses of the paper: 
# Neyret et al. (2021). Landscape management for grassland multifunctionality. Ecosystem Services.

new_newperm = function(n_low, n_med, n_high, lui_no_na, n_ls){
  cache = list()
  new_perms = matrix(nrow = n_ls, ncol = 1)
  k = 0
  loop = 0
  while (k < n_ls){
    p <- c(sample(as.numeric(names(lui_no_na[lui_no_na == 1])), n_low), 
           sample(as.numeric(names(lui_no_na[lui_no_na == 2])), n_med), 
           sample(as.numeric(names(lui_no_na[lui_no_na == 3])), n_high) )
    p <- sort(p)

    hash.p <- paste(p, collapse="-")
    if (is.null(cache[[hash.p]])) {
      k = k+1
      new_perms[k,] =  hash.p
      cache[[hash.p]] = 1
    }
  #  else{print('exists')}
    loop = loop+1
    if (loop == 100) {
      print('too many iterations')
      break
    }
  }
  return(new_perms[!is.na(new_perms)])
}

create_new_combinations = function(n_plots, n_ls_per_comb, lui_no_na){
  all_comb = expand.grid(n_low = 0:n_plots, n_med = 0:n_plots, n_high = 0:n_plots)
  all_comb = all_comb[rowSums(all_comb) ==n_plots,]
  setDT(all_comb)
  
  combs = all_comb[, new_newperm(n_low, n_med, n_high, lui_no_na, n_ls_per_comb), by = 1:nrow(all_comb)]
  combs_mat = matrix(as.numeric(unlist(sapply(combs$V1, function(x){strsplit(x, split = "-")}))), ncol = n_plots, byrow = T)
  
  return(combs_mat)
}

classify_LUI = function(X, method = "quantile_30"){
  if (method == "quantile_30"){
      lui_class = as.numeric(cut(X, breaks = quantile(X, c(0, 0.33,0.66, 1)), include.lowest = TRUE))
      lui_class[is.na(lui_class)] = 1
    }
  if (method == "quantile_20"){
      lui_class = as.numeric(cut(X, breaks = quantile(X, c(0, 0.2,0.4, 0.6, 0.8, 1))))
      lui_class[is.na(lui_class)] = 1
      lui_class[lui_class %in% c(2,4)] = NA
      lui_class = factor( lui_class )
    }
  return(lui_class)
}

scale01 <- function(x) {
  x <- as.numeric(x)
  max <- quantile(x, 0.99, na.rm = T)
  min <- min(x, na.rm = T)
  y <- (x - min) / (max - min)
  y[y < 0] <- 0
  y[y > 1] <- 1
  return(y)
}
stat_test = function(y, x){
  res = data.frame(pairs(emmeans( lm(y ~ x), 'x')))
  return(list(group1 = substr(res$contrast, 1, 1),
              group2 = substr(res$contrast, 5, 5),
              pval = res$p.value))}

model_letters = function(value, luiclass, Exploratory, type = 'group'){
  if (type == 'group'){
    mod = lm(value ~ luiclass*Exploratory)
    l = CLD(emmeans(mod, ~luiclass, by = 'Exploratory'), Letters = LETTERS)
    return(list(luiclass = l$luiclass, Letter = tolower(l$.group), Region = l$Exploratory))}
  else {
    mod = lm(value ~ luiclass*Exploratory)
    l = summary(emtrends(mod, var = 'luiclass', by = 'Exploratory'), null=0, infer=c(TRUE,TRUE))
    l$stars =  ifelse(l$p.value> 0.05, 'n.s.', ifelse(l$p.value> 0.01, '*', ifelse(l$p.value> 0.001, "**", '***')))
    return(list(Exploratory = l$Exploratory, stars = tolower(l$stars)))
  }
}

scale_average_threshold = function(x, threshold = NA, method){
  if (method == 'Average'){
    max = quantile(x, 0.975, na.rm = TRUE)
    if (is.nan(max)){
      max = max(x, na.rm = T)
      plot('lala')}
    min = quantile(x, 0.025,  na.rm = TRUE)
    if (is.nan(min)){min = min(x, na.rm = T)}
    y = (x-min)/(max-min)
    y[y>1] = 1
    y[y<0] = 0
    y[is.nan(y)] = 0}
  if (method == 'Threshold' | method == "minimise_trade_offs"){
    print('threshold')
    real_tresh = quantile(x, threshold, na.rm = TRUE)
    print(real_tresh)
    y = ifelse(x >= real_tresh, 1, 0)
  }
  if (method == 'Threshold_perc'){
    max = quantile(x, 0.975, na.rm = TRUE)
    real_tresh = max*threshold
    y = ifelse(x >= real_tresh, 1, 0)
  }
  return(y)}
env_pca_function = function(x){
  env_pca = dudi.pca(x, , scannf = FALSE, nf = 3)
  return(env_pca$li)
}


find_range = function(value, low, med, ternary = TRUE){
  library(compositions)
  
  mygrid = data.frame(rbind(
    expand.grid(
      low = seq(0, 1, length.out = 100),
      med = seq(0, 1, length.out = 100)
    )))
  mygrid$high = 1-mygrid$low-mygrid$med
  mygrid = cbind(mygrid, ilr(mygrid))
  
  
  if (ternary == TRUE){
    ilr_dat = ilr(data.frame(low, med, 1-low-med))}
  else {
    ilr_dat = data.frame('V1' = low, 'V2' = med)
  }
  
  model = lm(value ~ poly(V1, V2, degree = 2), ilr_dat)
  mygrid$value = predict(model, mygrid)
  mygrid$value[mygrid$value > 1 | mygrid$value < 0 ] = NA
  
  return(diff(quantile(mygrid$value, probs = c(0.05, 0.95), na.rm = T)))
}
