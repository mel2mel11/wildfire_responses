# 01_trend_helper_functions.R

library(gtrendsR)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)


# 1. settings
KEYWORD <- "air quality"
START_YEAR <- 2006
TODAY <- Sys.Date()
SPAN_YEARS <- 5
OVERLAP_YEARS <- 1
PAUSE_BETWEEN <- 15
MAX_ATTEMPTS <- 3
PAUSE_429 <- c(60, 180, 300)
checkpoint_dir <- "trends_checkpoints_air_quality"
failure_dir <- "trends_failures_air_quality"

# 2. GOOGLE TRENDS DMAs

get_dma_table <- function() {
  
  dma_vec <- c(
    
    "US-AL-630",
    "US-GA-522",
    "US-AL-606",
    "US-AL-691",
    "US-MS-711",
    "US-FL-686",
    "US-AL-698",
    
    "US-AK-743",
    "US-AK-745",
    "US-AK-747",
    
    "US-AZ-753",
    "US-AZ-789",
    
    "US-CA-771",
    "US-AR-670",
    "US-AR-734",
    "US-AR-693",
    "US-TN-640",
    "US-AR-628",
    "US-LA-612",
    "US-MO-619",
    
    "US-CA-800",
    "US-CA-868",
    "US-CA-802",
    "US-CA-866",
    "US-CA-803",
    "US-OR-813",
    "US-CA-828",
    "US-CA-804",
    "US-NV-811",
    "US-CA-862",
    "US-CA-825",
    "US-CA-807",
    "US-CA-855",
    
    "US-CO-752",
    "US-CO-751",
    "US-CO-773",
    
    "US-CT-533",
    
    "US-PA-504",
    "US-MD-576",
    
    "US-FL-571",
    "US-FL-592",
    "US-FL-561",
    "US-FL-528",
    "US-FL-534",
    "US-FL-656",
    "US-GA-530",
    "US-FL-539",
    "US-FL-548",
    "US-GA-525",
    "US-GA-524",
    "US-GA-520",
    "US-TN-575",
    "US-GA-503",
    "US-GA-507",
    
    "US-HI-744",
    
    "US-ID-757",
    "US-ID-758",
    "US-WA-881",
    "US-ID-760",
    
    "US-IL-648",
    "US-IL-602",
    "US-IL-682",
    "US-IN-649",
    "US-IL-632",
    "US-IL-675",
    "US-IA-717",
    "US-IL-610",
    "US-MO-609",
    "US-IN-581",
    "US-OH-515",
    "US-IN-509",
    "US-IN-527",
    "US-IN-582",
    "US-KY-529",
    "US-IN-588",
    "US-IA-637",
    "US-IA-679",
    "US-NE-652",
    "US-MO-631",
    "US-MN-611",
    "US-IA-624",
    "US-KS-603",
    "US-MO-616",
    "US-MO-638",
    "US-KS-605",
    "US-KS-678",
    "US-KY-736",
    "US-WV-564",
    "US-TN-557",
    "US-KY-541",
    "US-TN-659",
    "US-VA-531",
    
    "US-LA-644",
    "US-LA-716",
    "US-LA-642",
    "US-LA-643",
    "US-LA-622",
    
    "US-ME-537",
    "US-ME-500",
    "US-ME-552",
    
    "US-MD-512",
    "US-MD-511",
    
    "US-NH-506",
    
    "US-MA-521",
    "US-MA-543",
    
    "US-MI-583",
    "US-MI-505",
    "US-MI-513",
    "US-MI-563",
    "US-MI-551",
    "US-MI-553",
    "US-OH-547",
    "US-MI-540",
    
    "US-WI-676",
    
    "US-ND-724",
    
    "US-WI-702",
    
    "US-MN-737",
    "US-MN-613",
    
    "US-MS-746",
    "US-MS-673",
    "US-MS-647",
    "US-MS-710",
    "US-MS-718",
    
    "US-MO-604",
    
    "US-MT-756",
    "US-MT-754",
    "US-MT-798",
    "US-MT-755",
    "US-MT-766",
    "US-ND-687",
    "US-MT-762",
    
    "US-NE-759",
    "US-NE-722",
    "US-NE-740",
    
    "US-SD-725",
    
    "US-NV-839",
    
    "US-UT-770",
    
    "US-NY-523",
    "US-NY-501",
    
    "US-NM-790",
    
    "US-TX-634",
    "US-TX-765",
    
    "US-NY-532",
    "US-NY-502",
    "US-NY-514",
    "US-NY-565",
    "US-NY-538",
    "US-NY-555",
    "US-NY-526",
    "US-NY-549",
    
    "US-NC-517",
    "US-SC-570",
    "US-NC-518",
    "US-NC-545",
    "US-SC-567",
    "US-VA-544",
    "US-NC-560",
    "US-NC-550",
    
    "US-OH-510",
    "US-OH-535",
    "US-OH-542",
    "US-OH-558",
    "US-WV-597",
    "US-OH-554",
    "US-OH-536",
    "US-OH-596",
    
    "US-OK-650",
    "US-OK-657",
    "US-OK-671",
    "US-OK-627",
    
    "US-OR-821",
    "US-OR-801",
    "US-OR-820",
    
    "US-WA-810",
    
    "US-PA-516",
    "US-PA-566",
    "US-PA-574",
    "US-PA-508",
    "US-PA-577",
    
    "US-SC-519",
    "US-SC-546",
    
    "US-SD-764",
    
    "US-TN-639",
    
    "US-TX-662",
    "US-TX-635",
    "US-TX-692",
    "US-TX-600",
    "US-TX-623",
    "US-TX-636",
    "US-TX-618",
    "US-TX-749",
    "US-TX-651",
    "US-TX-633",
    "US-TX-661",
    "US-TX-641",
    "US-TX-709",
    "US-TX-626",
    "US-TX-625",
    
    "US-WV-559",
    
    "US-VA-584",
    "US-VA-569",
    "US-VA-556",
    "US-VA-573",
    
    "US-WA-819",
    
    "US-WV-598",
    
    "US-WI-658",
    "US-WI-669",
    "US-WI-617",
    "US-WI-705",
    
    "US-WY-767"
  )
  
  dma_vec <- sort(unique(dma_vec))
  
  if (length(dma_vec) != 210) {
    stop(
      "DMA universe error: expected 210 DMAs, found ",
      length(dma_vec)
    )
  }
  
  tibble(
    dma_code = dma_vec
  )
}


get_dma_vec <- function() {
  get_dma_table()$dma_code
}


# 3. chunks
build_chunks <- function(
    start_year = START_YEAR,
    end_date = TODAY,
    span_years = SPAN_YEARS,
    overlap_years = OVERLAP_YEARS
) {
  
  starts <- seq(
    from = start_year,
    to = as.integer(format(end_date, "%Y")),
    by = span_years - overlap_years
  )
  
  chunks <- map2_dfr(
    starts,
    seq_along(starts),
    function(y, i) {
      
      start_date <- as.Date(
        paste0(y, "-01-01")
      )
      
      if (i == length(starts)) {
        
        end_date_chunk <- end_date
        
      } else {
        
        end_year <- y + span_years - 1
        
        end_date_chunk <- as.Date(
          paste0(end_year, "-12-31")
        )
      }
      
      tibble(
        chunk_id = i,
        start_year = y,
        end_year = as.integer(
          format(end_date_chunk, "%Y")
        ),
        start_date = start_date,
        end_date = end_date_chunk
      )
    }
  )
  
  chunks
}


#4. load existing chunks
# never overwrite chunks if they already exist
load_chunks <- function() {
  
  chunk_file <- file.path(
    checkpoint_dir,
    "chunks_used.rds"
  )
  
  if (file.exists(chunk_file)) {
    
    message(
      "Using existing chunk definitions: ",
      chunk_file
    )
    
    chunks <- readRDS(chunk_file)
    
  } else {
    
    message(
      "chunks_used.rds not found. Creating new chunk definitions."
    )
    
    chunks <- build_chunks()
    
    dir.create(
      checkpoint_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    saveRDS(
      chunks,
      chunk_file
    )
  }
  
  chunks
}


# 5. checkpoint path
checkpoint_path <- function(
    keyword,
    dma,
    chunk_id
) {
  
  safe_keyword <- gsub(
    "[^A-Za-z0-9]",
    "_",
    keyword
  )
  
  safe_dma <- gsub(
    "[^A-Za-z0-9]",
    "_",
    dma
  )
  
  file.path(
    checkpoint_dir,
    paste0(
      safe_keyword,
      "_",
      safe_dma,
      "_chunk",
      chunk_id,
      ".rds"
    )
  )
}


# 6. load existing checkpoints
load_checkpoints <- function() {
  
  files <- list.files(
    checkpoint_dir,
    pattern = "^air_quality_US_[A-Z]{2}_[0-9]{3}_chunk[0-9]+\\.rds$",
    full.names = TRUE
  )
  
  message(
    "Checkpoint files found: ",
    length(files)
  )
  
  if (length(files) == 0) {
    stop(
      "No checkpoint files found in: ",
      checkpoint_dir
    )
  }
  
  checkpoint_list <- map(
    files,
    function(f) {
      
      dat <- tryCatch(
        readRDS(f),
        error = function(e) {
          
          message(
            "Could not read: ",
            basename(f),
            " | ",
            conditionMessage(e)
          )
          
          NULL
        }
      )
      
      if (is.null(dat)) {
        return(NULL)
      }
      
      required_cols <- c(
        "date",
        "hits",
        "chunk_id",
        "geo_req"
      )
      
      missing_cols <- setdiff(
        required_cols,
        names(dat)
      )
      
      if (length(missing_cols) > 0) {
        
        message(
          "Skipping invalid checkpoint: ",
          basename(f),
          " | missing: ",
          paste(
            missing_cols,
            collapse = ", "
          )
        )
        
        return(NULL)
      }
      
      dat %>%
        mutate(
          date = as.Date(date),
          hits = suppressWarnings(
            as.numeric(as.character(hits))
          ),
          chunk_id = as.integer(chunk_id),
          geo_req = as.character(geo_req)
        )
    }
  )
  
  checkpoint_list <- compact(
    checkpoint_list
  )
  
  if (length(checkpoint_list) == 0) {
    stop(
      "No valid checkpoint files could be loaded."
    )
  }
  
  message(
    "Successfully loaded checkpoint files: ",
    length(checkpoint_list)
  )
  
  bind_rows(checkpoint_list)
}


# 7. build coverage
build_coverage <- function(
    dma_vec,
    chunks,
    raw_existing
) {
  
  coverage <- expand_grid(
    geo_req = dma_vec,
    chunk_id = chunks$chunk_id
  )
  
  existing <- raw_existing %>%
    filter(
      chunk_id %in% chunks$chunk_id,
      geo_req %in% dma_vec
    ) %>%
    distinct(
      geo_req,
      chunk_id
    ) %>%
    mutate(
      saved = TRUE
    )
  
  coverage <- coverage %>%
    left_join(
      existing,
      by = c(
        "geo_req",
        "chunk_id"
      )
    ) %>%
    mutate(
      saved = coalesce(
        saved,
        FALSE
      )
    ) %>%
    arrange(
      chunk_id,
      geo_req
    )
  
  dma_completeness <- coverage %>%
    group_by(geo_req) %>%
    summarise(
      chunks_expected = n(),
      chunks_saved = sum(saved),
      chunks_missing = sum(!saved),
      complete = all(saved),
      missing_chunks = paste(
        chunk_id[!saved],
        collapse = ", "
      ),
      .groups = "drop"
    )
  
  list(
    coverage = coverage,
    missing = coverage %>%
      filter(!saved),
    dma_completeness = dma_completeness
  )
}


# 8. google trends REPAIR function
# Downloads ONE missing DMA × chunk combination.

get_trends_repair <- function(
    keyword,
    dma,
    start_date,
    end_date,
    max_attempts = MAX_ATTEMPTS,
    pause_429 = PAUSE_429
) {
  
  last_error <- NA_character_
  
  for (attempt in seq_len(max_attempts)) {
    
    message(
      "Attempt ",
      attempt,
      "/",
      max_attempts,
      " | ",
      dma,
      " | ",
      start_date,
      " to ",
      end_date
    )
    
    result <- tryCatch(
      
      gtrends(
        keyword = keyword,
        geo = dma,
        time = paste(
          start_date,
          end_date
        ),
        onlyInterest = TRUE
      ),
      
      error = function(e) e
    )
    
    if (!inherits(result, "error")) {
      
      if (
        !is.null(result$interest_over_time) &&
        nrow(result$interest_over_time) > 0
      ) {
        
        dat <- result$interest_over_time
        
        return(
          list(
            status = "success",
            data = dat,
            error = NA_character_
          )
        )
      }
      
      last_error <- "No interest_over_time data returned."
      
    } else {
      
      last_error <- conditionMessage(
        result
      )
    }
    
    message(
      "Request failed: ",
      last_error
    )
    
    if (attempt < max_attempts) {
      
      wait <- pause_429[
        min(
          attempt,
          length(pause_429)
        )
      ]
      
      message(
        "Waiting ",
        wait,
        " seconds before retry..."
      )
      
      Sys.sleep(wait)
    }
  }
  
  list(
    status = "failed",
    data = NULL,
    error = last_error
  )
}


# 9. harmonize one chunk

# The scale for the next chunk is calculated against the
# ALREADY HARMONIZED previous chunk.
#
# Only weeks where BOTH values are > 0 are used.

harmonize_dma <- function(
    dma_data,
    chunk_ids
) {
  
  # Require ALL requested chunks
  
  available_chunks <- sort(
    unique(dma_data$chunk_id)
  )
  
  if (!all(chunk_ids %in% available_chunks)) {
    
    missing_chunks <- setdiff(
      chunk_ids,
      available_chunks
    )
    
    return(
      list(
        success = FALSE,
        reason = paste0(
          "Missing required chunk(s): ",
          paste(
            missing_chunks,
            collapse = ", "
          )
        ),
        data = NULL,
        diagnostics = tibble()
      )
    )
  }
  
  
  # Split requested chunks
  
  chunk_data <- dma_data %>%
    filter(
      chunk_id %in% chunk_ids
    ) %>%
    arrange(
      chunk_id,
      date
    ) %>%
    split(
      .$chunk_id
    )
  
  
  # First chunk is reference scale
  
  first_chunk <- chunk_data[[as.character(chunk_ids[1])]]
  
  harmonized <- first_chunk %>%
    transmute(
      date = date,
      hits = hits,
      original_chunk = chunk_ids[1],
      scale_factor = 1,
      cumulative_scale = 1
    )
  
  diagnostics <- list()
  
  
  # Sequential harmonization
  
  if (length(chunk_ids) > 1) {
    
    for (i in 2:length(chunk_ids)) {
      
      previous_chunk <- chunk_ids[i - 1]
      
      current_chunk <- chunk_ids[i]
      
      current <- chunk_data[[as.character(current_chunk)]] %>%
        select(
          date,
          hits
        )
      
      
      # Use ALREADY HARMONIZED previous chunk
      
      previous_harmonized <- harmonized %>%
        filter(
          original_chunk == previous_chunk
        ) %>%
        select(
          date,
          previous_hits = hits
        )
      
      
      # Find overlapping weeks
      
      overlap <- previous_harmonized %>%
        inner_join(
          current %>%
            rename(
              current_hits = hits
            ),
          by = "date"
        )
      
      
      n_overlap_total <- nrow(
        overlap
      )
      
      
      # Only positive-positive observations
      positive_overlap <- overlap %>%
        filter(
          !is.na(previous_hits),
          !is.na(current_hits),
          is.finite(previous_hits),
          is.finite(current_hits),
          previous_hits > 0,
          current_hits > 0
        )
      
      
      n_positive_overlap <- nrow(
        positive_overlap
      )
      
      
      # Cannot harmonize without positive overlap
      
      if (n_positive_overlap == 0) {
        
        diagnostics[[length(diagnostics) + 1]] <- tibble(
          previous_chunk = previous_chunk,
          current_chunk = current_chunk,
          n_overlap_total = n_overlap_total,
          n_positive_overlap = 0,
          scale_factor = NA_real_,
          success = FALSE,
          reason = "No positive overlap"
        )
        
        return(
          list(
            success = FALSE,
            reason = paste0(
              "No positive overlap between chunks ",
              previous_chunk,
              " and ",
              current_chunk
            ),
            data = NULL,
            diagnostics = bind_rows(
              diagnostics
            )
          )
        )
      }
      
      
      # Multiplicative scale factor
      
      scale_factor <- median(
        positive_overlap$previous_hits /
          positive_overlap$current_hits,
        na.rm = TRUE
      )
      
      
      if (
        !is.finite(scale_factor) ||
        scale_factor <= 0
      ) {
        
        diagnostics[[length(diagnostics) + 1]] <- tibble(
          previous_chunk = previous_chunk,
          current_chunk = current_chunk,
          n_overlap_total = n_overlap_total,
          n_positive_overlap = n_positive_overlap,
          scale_factor = NA_real_,
          success = FALSE,
          reason = "Invalid scale factor"
        )
        
        return(
          list(
            success = FALSE,
            reason = paste0(
              "Invalid scale factor between chunks ",
              previous_chunk,
              " and ",
              current_chunk
            ),
            data = NULL,
            diagnostics = bind_rows(
              diagnostics
            )
          )
        )
      }
      
      
      # Cumulative scaling
      #
      # Determine cumulative scale already applied to the
      # previous chunk.
      
      previous_cumulative_scale <- harmonized %>%
        filter(
          original_chunk == previous_chunk
        ) %>%
        summarise(
          cumulative_scale = first(
            cumulative_scale
          )
        ) %>%
        pull(
          cumulative_scale
        )
      
      cumulative_scale <-
        previous_cumulative_scale *
        scale_factor
      
      
      # Apply cumulative scale to current RAW chunk
      
      current_harmonized <- current %>%
        mutate(
          hits = hits * cumulative_scale,
          original_chunk = current_chunk,
          scale_factor = scale_factor,
          cumulative_scale = cumulative_scale
        )
      
      
      # Append
      
      harmonized <- bind_rows(
        harmonized,
        current_harmonized
      )
      
      
      # Diagnostics
      
      diagnostics[[length(diagnostics) + 1]] <- tibble(
        previous_chunk = previous_chunk,
        current_chunk = current_chunk,
        n_overlap_total = n_overlap_total,
        n_positive_overlap = n_positive_overlap,
        scale_factor = scale_factor,
        cumulative_scale = cumulative_scale,
        success = TRUE,
        reason = "Harmonized"
      )
    }
  }
  
  
  # Successful result
  
  list(
    success = TRUE,
    reason = "Successfully harmonized",
    data = harmonized,
    diagnostics = bind_rows(
      diagnostics
    )
  )
}


# 10. RUN ONE HARMONIZATION SCENARIO

run_harmonization_scenario <- function(
    raw_data,
    dma_vec,
    chunk_ids,
    scenario_name
) {
  
  message(
    "\n============================================"
  )
  
  message(
    "SCENARIO: ",
    scenario_name
  )
  
  message(
    "Chunks: ",
    paste(
      chunk_ids,
      collapse = " -> "
    )
  )
  
  message(
    "============================================"
  )
  
  
  results <- map(
    dma_vec,
    function(dma) {
      
      dma_data <- raw_data %>%
        filter(
          geo_req == dma
        )
      
      result <- harmonize_dma(
        dma_data = dma_data,
        chunk_ids = chunk_ids
      )
      
      list(
        geo_req = dma,
        result = result
      )
    }
  )
  
  
  # DMA-level summary
  
  dma_summary <- map_dfr(
    results,
    function(x) {
      
      diagnostics <- x$result$diagnostics
      
      n_successful_transitions <- if (
        nrow(diagnostics) == 0
      ) {
        0
      } else {
        sum(
          diagnostics$success,
          na.rm = TRUE
        )
      }
      
      tibble(
        geo_req = x$geo_req,
        scenario = scenario_name,
        chunks_used = paste(
          chunk_ids,
          collapse = "-"
        ),
        harmonized = isTRUE(
          x$result$success
        ),
        failure_reason = if (
          isTRUE(x$result$success)
        ) {
          NA_character_
        } else {
          x$result$reason
        },
        n_transitions = length(chunk_ids) - 1,
        n_successful_transitions =
          n_successful_transitions
      )
    }
  )
  
  
  # Successful harmonized data
  
  successful_data <- map_dfr(
    results,
    function(x) {
      
      if (!isTRUE(x$result$success)) {
        return(tibble())
      }
      
      x$result$data %>%
        mutate(
          geo_req = x$geo_req,
          scenario = scenario_name,
          .before = 1
        )
    }
  )
  
  
  # Transition diagnostics
  
  diagnostics <- map_dfr(
    results,
    function(x) {
      
      if (
        is.null(x$result$diagnostics) ||
        nrow(x$result$diagnostics) == 0
      ) {
        return(tibble())
      }
      
      x$result$diagnostics %>%
        mutate(
          geo_req = x$geo_req,
          scenario = scenario_name,
          .before = 1
        )
    }
  )
  
  
  # Scenario-level summary
  
  scenario_summary <- dma_summary %>%
    summarise(
      scenario = scenario_name,
      chunks_used = paste(
        chunk_ids,
        collapse = "-"
      ),
      total_dmas = n(),
      harmonized_dmas = sum(
        harmonized
      ),
      failed_dmas = sum(
        !harmonized
      ),
      percent_harmonized =
        100 * mean(harmonized)
    )
  
  
  list(
    summary = scenario_summary,
    dma_summary = dma_summary,
    data = successful_data,
    diagnostics = diagnostics
  )
}


# Function to diagnose possible wider overlaps 
diagnose_failed_overlaps <- function(
    raw_data,
    failed_transitions
) {
  
  if (nrow(failed_transitions) == 0) {
    return(tibble())
  }
  
  # Keep only the chunks involved in failed transitions
  relevant_chunks <- unique(
    c(
      failed_transitions$previous_chunk,
      failed_transitions$current_chunk
    )
  )
  
  # Summarize positive search weeks by DMA × chunk × year
  positive_by_year <- raw_data %>%
    filter(
      chunk_id %in% relevant_chunks,
      !is.na(hits),
      is.finite(hits),
      hits > 0
    ) %>%
    mutate(
      year = as.integer(format(date, "%Y"))
    ) %>%
    group_by(
      geo_req,
      chunk_id,
      year
    ) %>%
    summarise(
      positive_weeks = n(),
      .groups = "drop"
    )
  
  
  # Previous chunk
  previous <- positive_by_year %>%
    inner_join(
      failed_transitions %>%
        select(
          geo_req,
          previous_chunk,
          current_chunk
        ),
      by = "geo_req"
    ) %>%
    filter(
      chunk_id == previous_chunk
    ) %>%
    group_by(
      geo_req,
      previous_chunk,
      current_chunk
    ) %>%
    summarise(
      previous_positive_years = paste(
        paste0(
          year,
          " (",
          positive_weeks,
          " weeks)"
        ),
        collapse = ", "
      ),
      .groups = "drop"
    )
  
  
  # Current chunk
  current <- positive_by_year %>%
    inner_join(
      failed_transitions %>%
        select(
          geo_req,
          previous_chunk,
          current_chunk
        ),
      by = "geo_req"
    ) %>%
    filter(
      chunk_id == current_chunk
    ) %>%
    group_by(
      geo_req,
      previous_chunk,
      current_chunk
    ) %>%
    summarise(
      current_positive_years = paste(
        paste0(
          year,
          " (",
          positive_weeks,
          " weeks)"
        ),
        collapse = ", "
      ),
      .groups = "drop"
    )
  
  
  # Combine with existing failure diagnostics
  failed_transitions %>%
    select(
      geo_req,
      previous_chunk,
      current_chunk,
      n_overlap_total,
      n_positive_overlap
    ) %>%
    left_join(
      previous,
      by = c(
        "geo_req",
        "previous_chunk",
        "current_chunk"
      )
    ) %>%
    left_join(
      current,
      by = c(
        "geo_req",
        "previous_chunk",
        "current_chunk"
      )
    ) %>%
    mutate(
      previous_positive_years = coalesce(
        previous_positive_years,
        "None"
      ),
      current_positive_years = coalesce(
        current_positive_years,
        "None"
      )
    ) %>%
    arrange(
      previous_chunk,
      current_chunk,
      geo_req
    )
}