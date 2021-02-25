
make.WL = function(filename = NULL,
                   data = NULL,
                   randomise = NULL,
                   equilibrate = NULL,
                   sample.path = NULL,
                   sample.prefix = NULL,
                   method.path = NULL,
                   eq.method = NULL,
                   eq.MRM = NULL,
                   analysis.method = NULL,
                   wash.method = NULL,
                   final.wash.method.1 = NULL,
                   final.wash.method.2 = NULL,
                   nSTD = NULL,
                   splitWash = NULL,
                   full.Set = F,
                   brackets = NULL,
                   development = NULL){
  if(is.null(filename)&is.null(data)){stop("no filename or data specified!")}
  if(is.null(filename)){
    sample.info = data%>%
      dplyr::select(`Sample Name`, `Sample Position`)
  }else{
    sample.info = read_csv(file = filename) %>%
      dplyr::select(`Sample Name`, `Sample Position`)
  }
  eq.MRM = ifelse(is.null(eq.MRM), analysis.method, eq.MRM)
  eq.method = ifelse(is.null(eq.method), eq.MRM, eq.method)
  
  brackets = min(brackets,nrow(sample.info))

  WL.samples = sample.info %>%
    mutate(Method = file.path(method.path,analysis.method,fsep = "\\"),
           `Data File` = file.path(sample.path,paste(sample.prefix,`Sample Name`,sep = "_"),fsep = "\\"),
           `Sample Type` = "Sample",
           `Level Name` = "")
  
  if(randomise == T){
    WL.samples = WL.samples[sample(1:nrow(WL.samples),size = nrow(WL.samples),replace = F),]
  }
  
  if(is.null(nSTD)|is.na(nSTD)|nSTD==0){
    WL = WL.samples
    
    
    WL = WL %>%
      dplyr::group_by(`Sample Name`) %>%
      dplyr::mutate(#`Sample Name` = paste(`Sample Name`,sprintf("r%02d",1:length(`Sample Name`)),sep = "-"),
        `Data File` = file.path(sample.path,paste(sample.prefix,
                                                  paste(`Sample Name`,sprintf("r%02d",1:length(`Sample Name`)),sep = "-"),
                                                  sep = "_"),fsep = "\\")) %>%
      dplyr::ungroup()
    
  }else{
    
    if(equilibrate == T){
      WL.eq = data_frame(`Sample Name` = c("eq",
                                           "blank-00",
                                           sprintf("STD%02d-00",1:nSTD)),
                         `Sample Position` = c("No Injection",
                                               sprintf("Vial %d",1:(nSTD+1))),
                         Method = c(file.path(method.path,eq.method,fsep = "\\"),
                                    rep(file.path(method.path,eq.MRM,fsep = "\\"),nSTD+1)),
                         `Data File` = as.character(sapply(paste(sample.prefix,`Sample Name`,sep = "_"), function(x)(file.path(sample.path,x,fsep = "\\")))),
                         `Sample Type` = c(rep("Blank",2),rep("Calibration",nSTD)),
                         `Level Name` = c("",1:(nSTD+1)))
      
    }else{
      
      WL.eq = NULL
    }
    

    
    WL.Standards = data_frame(`Sample Name` = c("blank",sprintf("STD%02d",1:nSTD)),
                              `Sample Position` = sprintf("Vial %d",1:(nSTD+1)),
                              Method = file.path(method.path,analysis.method,fsep = "\\"),
                              `Data File` = file.path(sample.path,paste(sample.prefix,`Sample Name`,sep = "_"),fsep = "\\"),
                              `Sample Type` = c("Blank",rep("Calibration",nSTD)),
                              `Level Name` = as.character(0:nSTD))
    
    
    if(nrow(WL.samples)/brackets == floor(nrow(WL.samples)/brackets)){
      n.std = nrow(WL.samples)/brackets
    }else{
      n.std = ceiling(nrow(WL.samples)/brackets)
    }
    
    if(full.Set == T){
      WL = bind_rows(WL.eq,
                     WL.Standards %>%
                       dplyr::filter(`Level Name` %in% c(0:nSTD)) %>%
                       arrange(desc(`Level Name`)),
                     lapply(1:n.std,function(y){
                    
                       
                       temp  = WL.Standards %>%
                         arrange(desc(`Level Name`))
                       
                       slice.min = brackets*(y-1)+1
                       slice.max = min(brackets*y,nrow(WL.samples))
                       
                       temp = WL.samples[slice.min:slice.max,] %>%
                         bind_rows(temp)
                       
                       return(temp)
                     })
      )
    }else{
    WL = bind_rows(WL.eq,
                   WL.Standards %>%
                     dplyr::filter(`Level Name` %in% c(0,seq(1,nSTD,2))) %>%
                    arrange(desc(`Level Name`)),
                   lapply(1:n.std,function(y){
                     if(y %% 2 == 0){
                       my.filter = c(0,seq(1,nSTD,2))      
                     }else{
                       my.filter = c(0,seq(2,nSTD,2))
                     }
                     
                     temp  = WL.Standards %>%
                       dplyr::filter(`Level Name` %in% my.filter) %>%
                     arrange(desc(`Level Name`))
                     
                     slice.min = brackets*(y-1)+1
                     slice.max = min(brackets*y,nrow(WL.samples))
                     
                     temp = WL.samples[slice.min:slice.max,] %>%
                       bind_rows(temp)
                     
                     return(temp)
                   })
                   )
    }
    
    WL = WL %>%
      dplyr::group_by(`Sample Name`) %>%
      dplyr::mutate(#`Sample Name` = paste(`Sample Name`,sprintf("r%02d",1:length(`Sample Name`)),sep = "-"),
                    `Data File` = file.path(sample.path,paste(sample.prefix,
                                                              paste(`Sample Name`,sprintf("r%02d",1:length(`Sample Name`)),sep = "-"),
                                                              sep = "_"),fsep = "\\")) %>%
      dplyr::ungroup()
    
  }
  
  if(!is.null(splitWash)){
    WL.wash = data_frame(`Sample Name` = sprintf("wash-%03d",1:nrow(WL)),
               `Sample Position` = "No Injection",
               Method = file.path(method.path,wash.method,fsep = "\\"),
               `Data File` = as.character(sapply(paste(sample.prefix,`Sample Name`,sep = "_"), function(x)(file.path(sample.path,x,fsep = "\\")))),
               `Sample Type` = "Blank",
               `Level Name` = "")
    
    WL = WL %>%
     mutate(index = paste0(sprintf("%03d",1:nrow(WL)),"1")) %>%
    bind_rows(WL.wash %>%
               mutate(index = paste0(sprintf("%03d",1:nrow(WL)),"2"))) %>%
    arrange(index) %>%
    dplyr::select(-index)
    
    WL = WL %>%
      dplyr::mutate(ID = 1:nrow(.)) %>%
      dplyr::mutate(`Data File` = str_replace(`Data File`,"runningID",sprintf("%03d",ID))) %>%
      dplyr::select(-ID)
    
    return(WL)
  }else{
    
    WL = WL %>%
      dplyr::mutate(ID = 1:nrow(.)) %>%
      dplyr::mutate(`Data File` = str_replace(`Data File`,"runningID",sprintf("%03d",ID))) %>%
      dplyr::select(-ID)
    
    return(WL)
  }
  
############## to implement at a later time... too much effort to calculate the used solvent  
#  WL.finalWash = data_frame(`Sample Name` = sprintf("finalwash_%03d",1:4),
 #                           `Sample Position` = "No Injection",
  #                          Method = c(rep(file.path(method.path,final.wash.method.1,fsep = "\\"),2),
   #                                    rep(file.path(method.path,final.wash.method.2,fsep = "\\"),2)),
    #                        `Data File` = as.character(sapply(`Sample Name`, function(x)(file.path(sample.path,"equilibration",x,fsep = "\\")))),
     #                       `Sample Type` = "Blank",
      #                      `Level Name` = "")
#  WL = WL %>%
 #   bind_rows(WL.finalWash)
  
}




