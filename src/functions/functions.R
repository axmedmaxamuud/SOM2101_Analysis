
driving <- function(res,colnames_indi,max=T) {
  # res[is.na(res)] <- 1
  n = nrow(res)
  if (max) {
    res$max = apply(res, 1, max ,na.rm = T)
    max_if <- res %>% mutate_all(.funs = ~ .x >= 4) %>% mutate_all(as.numeric) %>% select(-max)
    max_if <- apply(max_if, 2, sum, na.rm = T)
    names(max_if) <- colnames_indi 
    dr <- tibble::rownames_to_column(max_if %>% melt() %>% as.data.frame() , "Indicator") %>% mutate(
      value = paste0(round((value / n)*100,2),"%")
    ) 
    colnames(dr)[2] <- "max"
    return(dr)
  } else{
    res$max = apply(res, 1, min ,na.rm = T)
    max_if <- res %>% mutate_all(.funs = ~ .x < 3) %>% mutate_all(as.numeric) %>% select(-max)
    max_if <- apply(max_if, 2, sum, na.rm = T)
    names(max_if) <- colnames_indi 
    dr <- tibble::rownames_to_column(max_if %>% melt() %>% as.data.frame() , "Indicator") %>% mutate(
      value = paste0(round((value / n)*100,2),"%")
    ) 
    colnames(dr)[2] <- "min"
    return(dr)
  }}




generate_from_binaries <- function(data,select_multiple_questions) {
  
  do.call("cbind",map(select_multiple_questions,function(question_name,data) {
    df <- data %>% select(grep(paste0("^",question_name,"\\."), names(data))) 
    colnames(df) <- gsub(paste0(question_name,"\\."),"",colnames(df))
    map2_df(df, names(df), ~  replace(.x, .x==1, .y) %>% replace(. == 0, NA)) %>% 
      unite(!!sym(question_name),names(df),remove=TRUE,na.rm=TRUE,sep=" ") %>% 
      as.data.frame() %>% 
      mutate_all(list(~na_if(.,""))) 
  },data)) 
  
}



get_question_type <- function(question_name) {
  type = questions %>% filter(grepl(paste0("^",question_name,"$"),name)) %>% pull(type)
  if (length(type)==0) {
    if(!question_name %in% colnames(data)) {
      return("NotFound")
    } else {
      return(
        case_when(
          class(data[[question_name]]) == "character" ~ "categorical" ,
          class(data[[question_name]]) == "numeric" ~ "numerical" ,
        )
      )
    }
    
  } else 
  { return (case_when(
    str_detect(type,"^select_(multiple|one)") ~ "categorical",
    str_detect(type,"^(integer|calculate)$") ~ "numerical"
  ))}}

  
correct_other_binaries <- function(data,other_questions_binary,other_questions) {
  do.call("cbind",map2(other_questions_binary,other_questions,
                       function(binary_other_colname , other_colname , data) {
                         df <- data %>% select(grep(sprintf("(%s|%s)",binary_other_colname,other_colname), names(data))) 
                         df <- df %>% mutate(
                           "{binary_other_colname}" := ifelse(is.na(!!sym(other_colname)),
                                                              ifelse(is.na(!!sym(binary_other_colname)),as.numeric(NA),0)
                                                              ,(!!sym(binary_other_colname)))
                         )
                         
                         df[,1]
                         
                       }
                       ,data)) 
  
}

generate_template <- function(cluster_name, questions, indicators) {
  
  
  columns_to_extract <- c(
    "uuid",
    "region",
    "district",
    "idp_settlement",
    map(questions, ~ data %>%
          select(matches(sprintf("((^%s|^.*\\.%s)\\.|(^.*\\.%s|^%s)$)", .x, .x, .x, .x))) %>%
          # select(matches(sprintf("(^%s\\.|^%s$)", .x, .x))) %>%
          colnames()) %>% unlist()
  )
  
  

  template <- data %>% select(all_of(columns_to_extract))
  
  for (indicator in c(indicators, "Final HH Score")) {
    template[[indicator]] <- as.integer(NA)
  }
  
  template$"_uuid" <- template$uuid
  template$Key <- data$strata
  template$Area <- data$district
  template[["Population group"]] <- data$population_group
  
  template
}


percent_response <- function(x, df, ..., x_name = NULL, group = NULL) {
  if (is.null(x_name)) {
    
    x_name <- deparse(substitute(x))
    
  }
  
  group_var <- group_vars(df)
  
  args <- list(...)
  args <- unlist(args)
  args <- paste0("^", args, "$")
  
  
  if (!is.null(group) & nrow(df) == length(x)) {
    x <- x[df[[group_var]] == group]
  }
  
  x <- x[!is.na(x)]
  x <- as.character(x)
  
  
  if (length(x) == 0) {
    NA
  } else {
    pct <- sum(str_detect(x, args))
    pct
  }
}

get_group <- function(df) {
  group <- group_vars(df)
  quo(unique(!!sym(group)))
}


fn_select_one_mode_shelter_IDP <- function(x) {
  
  if(all(is.na(x))){return(NA)}
  
  if (all("none"==na.omit(x))) {
    return("5")
  } else if (all("not_sure"==na.omit(x))) {
    return(NA)
  }  else {
    return(case_when(
      any( c("brick","stone") %in% x ) ~ "1",
      any( c("timer_", "cgi", "mud", "collective","stick") %in% x ) ~ "2",
      any( c("tent", "unfinished") %in% x ) ~ "3",
      any( c("buul") %in% x ) ~ "4"
      
      
    ))
  }
  
}



fn_select_one_mode_shelter_HC <- function(x) {
  
  if(all(is.na(x))){return(NA)}
  
  if (all("none"==na.omit(x))) {
    return("5")
  } else if (all("not_sure"==na.omit(x))) {
    return(NA)
  }  else {
    return(case_when(
      any( c("brick","stone") %in% x ) ~ "1",
      any( c("buul","timer_", "cgi", "mud", "collective","stick") %in% x ) ~ "2",
      any( c("tent", "unfinished") %in% x ) ~ "3"
      
      
    ))
  }
  
}



fn_select_one_mode <- function(x) {
  if(all(is.na(x))){return(NA)}
  
  if (all("none"==na.omit(x))) {
    return("none")
  } else if (all("not_sure"==na.omit(x))) {
    return("not_sure")
  }
  x <- x[x!="not_sure"]
  x <- x[x!="none"]
  uniqx <- unique(na.omit(x))
  
  
  if (length(which(tabulate(match(x, uniqx)) == max(tabulate(match(x, uniqx))))) > 1) {
    return("NC")
  }
  
  uniqx[which.max(tabulate(match(x, uniqx)))]
  
}


map_to_master_table <- function(results_object, filename, questionnaire = NULL){
  summary_table_single <- function(x, questions = questionnaire){
    if(!is.null(questions)){
      x <- map_to_labeled(result = x, questionnaire = questions)
    }
    y <- NULL
    no_pvalue <- is.null(x$hypothesis.test$result$p.value)
    no_hypothesis.test <- is.null(x$hypothesis.test$name)
    if(no_pvalue|no_hypothesis.test){
      x$hypothesis.test$result$p.value <- NA
      x$hypothesis.test$name <- NA
    }
    if(!is.null(x$summary.statistic)){
      
      y <- data.frame(x$summary.statistic,
                      p.value = x$hypothesis.test$result$p.value
                      #test.name = x$hypothesis.test$name
      )
    }
    return(y)
  }
  results_object <- lapply(results_object,function(x){x$summary.statistic<-as.data.frame(x$summary.statistic,stringsAsFactors=F);x})
  df <- results_object %>% lapply(summary_table_single) %>% do.call(rbind, .)
  map_to_file(df, filename)
}


