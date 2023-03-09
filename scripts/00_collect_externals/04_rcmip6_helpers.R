cmip_download <- function(results,
                          max_year = 2050,
                          root = cmip_root_get(),
                          user = Sys.info()[["user"]],
                          comment = NULL, ...) {
  root <- path.expand(root)
  
  # Evaluate these now so that if they involve expressions that can fail,
  # they fail early.
  force(user)
  force(comment)
  
  if (inherits(results, "cmip_simple")) {
    results <- cmip_unsimplify(results)
  }
  
  files <- lapply(seq_len(nrow(results)), function(i) {
    cmip_download_one(results[i, ], max_year = max_year, root = root, user = user, comment = comment, ...)
  })
  
  downloaded <- vapply(files, function(x) all(!is.na(x)), logical(1))
  
  # Some instances can fail in one replica but not others,
  # so we have to list all instances and remove the
  all_instances <- results[["instance_id"]]
  downloaded_instances <- all_instances[downloaded]
  failed_instances <- setdiff(all_instances, downloaded_instances)
  
  if (length(failed_instances) != 0) {
    warning("Failed to download some instances, query them with,\n", instance_query(failed_instances))
  }
  return(invisible(files))
  
}


instance_query <- function(x) {
  start <- "cmip_search(c(\""
  space <- paste0(", \n", paste0(rep(" ", nchar(start)), collapse = ""))
  x <- paste(x, collapse = space)
  
  paste0(start, x, "\"))")
}

cmip_download_one <- function(result,
                              max_year = 2050,
                              root = here::here("data", "raw", "climate_model_output"),
                              # root = cmip_root_get(),
                              user = Sys.info()[["user"]],
                              comment = NULL, ...) {
  dir <- result_dir(result, root = root)
  
  use_https <- list(...)[["use_https"]]
  # Some results have multiple folders? (CMIP5, seems like)
  sink <- lapply(dir, dir.create,  showWarnings = FALSE, recursive = TRUE)
  
  url <- paste0("https://", result$index_node, "/search_files/", result$id, "/", result$index_node, "/?limit=999")
  
  info <- httr::RETRY("GET", url = url)
  httr::warn_for_status(info)
  
  if (httr::http_error(info)) {
    return(NA_character_)
  }
  
  info <- httr::content(info)$response$docs
  
  files <-  vapply(info, function(i) {
    url <- strsplit(i$url[[1]], "\\|")[[1]][1]
    date <- as.numeric(substr(strsplit(strsplit(url, "gn\\_")[[1]][2], "-")[[1]][1], 1, 4))
    
    if (date > max_year) {
      return(NA_character_)
    }
    
    i$version <- result$version  # CMIP5 seems to have a different version in the file thing?
    file <- file.path(result_dir(i), i$title)
    message(glue::glue(tr_("Downloading {i$title}...")))
    checksum_file <- paste0(file, ".chksum")
    checksum_type  <- tolower(i$checksum_type[[1]])
    
    if (file.exists(file)) {
      if (file.exists(checksum_file)) {
        local_checksum <- readLines(checksum_file)
      } else {
        local_checksum <- digest::digest(file = file, algo = checksum_type)
        writeLines(text = local_checksum, con = checksum_file)
      }
      
      checksum <- i$checksum[[1]]
      
      if (local_checksum == checksum) {
        message(tr_("Skipping (matching checksum)."))
        return(file)
      }
    }
    
    # Workaround para evitar el proxy de mierda de la UBA
    if (isTRUE(use_https)) {
      url <- gsub("http:", "https:", url)
    }
    
    message(glue::glue(tr_("Downloading from {url}")))
    response <- try(httr::RETRY("GET", url = url,
                                times = 3,
                                httr::write_disk(file, overwrite = TRUE),
                                httr::progress()))
    
    # RETRY will raise a stop() if the last try is a curl error
    # so we need to capture it.
    if (inherits(response, "try-error"))  {
      warning(response)
      unlink(file)
      return(NA_character_)
    }
    
    # Trap http errors
    if (httr::http_error(response)) {
      httr::warn_for_status(response, task = NULL)
      unlink(file)
      return(NA_character_)
    }
    
    # Compute and save checksum. Perhaps we need to also check if it's correct, but
    # what should we do if it's not?
    local_checksum <- digest::digest(file = file, algo = checksum_type)
    writeLines(text = local_checksum, con = checksum_file)
    
    log <- paste(as.character(as.POSIXlt(Sys.time(), tz = "UTC")), "-", user)
    writeLines(c(log, comment), file.path(result_dir(i), paste0(tools::file_path_sans_ext(i$title), ".log")))
    file
  }, character(1))
  
  
  if (any(is.na(files))) {
    result[["complete_download"]] <- FALSE
  } else {
    result[["complete_download"]] <- TRUE
  }
  
  if (any(!is.na(files))) {
    writeLines(jsonlite::serializeJSON(result, pretty = TRUE),
               file.path(dir, "model.info"),)
  }
  
  files
}


result_dir <- function(result,
                       # root = cmip_root_get(),
                       root = here::here("data", "raw", "climate_model_output")) {
  
  template <- result[["directory_format_template_"]][[1]]
  if (is.null(template)) {
    # This is CMIP5
    template <- cmip5_folder_template
  }
  
  dir <- glue::glue_data(result,
                         template,
                         .open = "%(",
                         .close = ")s"
  )
  
  dir
}


cmip_size <- function(results) {
  res <- sum(results$size)/1024/1024
  class(res) <- c("cmip_size", class(res))
  res
}


#' @export
print.cmip_size <- function(x, ...) {
  cat(signif(x, 3), "Mb")
}




tr_ <- function(...) {
  gettextf(..., domain = "R-rcmip6")
}



cmip6_folder_template <- "%(root)s/%(mip_era)s/%(activity_drs)s/%(institution_id)s/%(source_id)s/%(experiment_id)s/%(member_id)s/%(table_id)s/%(variable_id)s/%(grid_label)s/%(version)s"

cmip6_file_template <- c("%(variable_id)s_%(table_id)s_%(source_id)s_%(experiment_id)s_%(member_id)s_%(grid_label)s_%(time_start)s-%(time_end)s.nc",
                         "%(variable_id)s_%(table_id)s_%(source_id)s_%(experiment_id)s_%(member_id)s_%(grid_label)s.nc")

# from https://pcmdi.llnl.gov/mips/cmip5/docs/cmip5_data_reference_syntax_v1-02_marked.pdf
cmip5_folder_template <- "%(root)s/%(project)s/%(product)s/%(institute)s/%(model)s/%(experiment)s/%(time_frequency)s/%(realm)s/%(cmor_table)s/%(ensemble)s/%(version)s/%(variable)s/"
