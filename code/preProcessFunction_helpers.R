# the input server object contains preprocessing parameters followed by the
# index specifying the order in which the functions are to be applied. this
# function matches the preprocessing step to the parameters + index
preProcess_params <- function(preProcStep,ind){
  
  if(preProcStep == "Downsample"){
    
    return(c(paste0("downsamp",ind)))
    
  }
  if(preProcStep == "Crop"){
    
    return(c(paste0("cropRegion",ind),paste0("cropOffset",ind)))
    
  }
  if(preProcStep == "Level"){
    
    return(c(paste0("level",ind)))
    
  }
  if(preProcStep == "Erode"){
    
    return(c(paste0("erodeRegion",ind),paste0("erodeRadius",ind)))
    
  }
  if(preProcStep == "Filter"){
    
    return(c(paste0("filterType",ind),paste0("filterParams",ind)))
    
  }
  
}

# this is a helper for the preprocess procedure on the server side. this will
# return a preprocessing function that with necessary parameters filled-in
preProcess_partial <- function(preProcStep,paramValues){
  
  if(preProcStep == "Downsample"){
    
    if((paramValues[[which(str_detect(string = names(paramValues),pattern = "params1"))]] <= 0) | ((paramValues[[which(str_detect(string = names(paramValues),pattern = "params1"))]] %% 1) != 0)){
      
      showNotification("Enter a positive whole number for the Stride parameter of the Downsample step",type = "error")
      validate(need(paramValues[[which(str_detect(string = names(paramValues),pattern = "params1"))]] > 0 & ((paramValues[[which(str_detect(string = names(paramValues),pattern = "params1"))]] %% 1) == 0),message = FALSE))
      
    }
    
    return(purrr::partial(x3ptools::x3p_sample,m = !!paramValues[[which(str_detect(string = names(paramValues),pattern = "params1"))]]))
    
  }
  if(preProcStep == "Crop"){
    
    paramValues <- paramValues[order(names(paramValues))]
    
    cropOffset <- paramValues[[which(str_detect(string = names(paramValues),pattern = "params2"))]]
    
    if(((cropOffset %% 1) != 0)){
      
      showNotification("Enter a positive whole number for the Stride parameter of the Downsample step",type = "error")
      validate(need(((cropOffset %% 1) == 0),message = FALSE))
      
    }
    
    return(purrr::partial(cmcR::preProcess_crop,region = tolower(!!paramValues[[which(str_detect(string = names(paramValues),pattern = "params1"))]]),offset = !!paramValues[[which(str_detect(string = names(paramValues),pattern = "params2"))]]))
    
  }
  if(preProcStep == "Level"){
    
    if(paramValues[[which(str_detect(string = names(paramValues),pattern = "params1"))]] == "Mean"){
      
      return(purrr::partial(cmcR::preProcess_removeTrend,statistic = tolower(!!paramValues[[which(str_detect(string = names(paramValues),pattern = "params1"))]])))
      
    }
    else{
      return(purrr::partial(cmcR::preProcess_removeTrend,statistic = "quantile",method = "fn",tau = .5))
    }
  }
  if(preProcStep == "Erode"){
    
    paramValues <- paramValues[order(names(paramValues))]
    
    erodeRadius <- paramValues[[which(str_detect(string = names(paramValues),pattern = "params2"))]]
    
    if((erodeRadius <= 0) | ((erodeRadius %% 1) != 0)){
      
      showNotification("Enter a positive whole number for the Radius parameter of the Erode step",type = "error")
      validate(need(erodeRadius > 0 & ((erodeRadius %% 1) == 0),message = FALSE))
      
    }
    
    return(purrr::partial(cmcR::preProcess_erode,region = tolower(!!paramValues[[which(str_detect(string = names(paramValues),pattern = "params1"))]]),morphRadius = !!paramValues[[which(str_detect(string = names(paramValues),pattern = "params2"))]]))
    
  }
  if(preProcStep == "Filter"){
    
    # sometimes(?) the filter type comes after the wavelength cutoffs, so we'll
    # look for the element starting with "params2"
    filtParams <- paramValues[[which(str_detect(string = names(paramValues),pattern = "params2"))]] %>%
      stringr::str_split(",") %>%
      .[[1]] %>%
      purrr::map_dbl(as.numeric)
    
    if(paramValues[[which(str_detect(string = names(paramValues),pattern = "params1"))]] == "Lowpass"){
      
      if(length(filtParams) != 1 | any(filtParams) <= 0){
        
        showNotification("Enter one positive whole number for the Wavelength parameter of the Filter step",type = "error")
        validate(need(length(filtParams) == 1 & all(filtParams) > 0,message = FALSE))
        
      }
      
      return(purrr::partial(cmcR::preProcess_gaussFilter,wavelength = !!filtParams,filtertype = "lp"))
      
      
    }
    else{
      
      if(length(filtParams) != 2 | any(filtParams) <= 0){
        
        showNotification("Enter two positive, comma-separated whole numbers for the Wavelengths parameter of the Filter step",type = "error")
        validate(need(length(filtParams) == 2 & all(filtParams) > 0,message = FALSE))
        
      }
      
      return(purrr::partial(cmcR::preProcess_gaussFilter,wavelength = !!filtParams,filtertype = "bp"))
      
    }
    
    
  }
  if(preProcStep == "Delete"){
    
    # browser()
    
    validate(need(str_detect(paramValues[[which(str_detect(string = names(paramValues),pattern = "params1"))]],"^\\#[0-9]{1,}") | paramValues[[which(str_detect(string = names(paramValues),pattern = "params1"))]] == "NULL",message = FALSE))
    
    return(purrr::partial(x3pDeleteMask,color = paramValues[[which(str_detect(string = names(paramValues),pattern = "params1"))]]))
    
  }
  if(preProcStep == "Trim"){
    return(purrr::partial(impressions::x3p_cropWS,croppingThresh = 1))
  }
  
}

x3pDeleteMask <- function(x3p,color = "#ff0000"){
  
  if(color == "NULL"){
    color <- names(table(dat$mask))[which.max(table(dat$mask))]
  }
  
  if(all(dim(x3p$surface.matrix) == dim(x3p$mask))){
    x3p$surface.matrix[x3p$mask == color] <- NA
  }
  else{
    x3p$surface.matrix[t(as.matrix(x3p$mask)) == color] <- NA
  }
  
  return(x3p)
  
}