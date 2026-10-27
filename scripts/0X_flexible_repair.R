# =============================================================================
# FLEXIBLE, DMA-BY-DMA GOOGLE TRENDS OVERLAP REPAIRS
# =============================================================================
# Based on Marissa's backward-walking pseudocode. For each unresolved DMA:
#   * Begin with a SAVED late window (usually original_5, 2022–2026).
#   * Look for positive weeks in its EARLIEST 1, 2, or 3 calendar years.
#   * Propose an earlier four-year request with that many years of overlap.
#   * Verify positive-positive MATCHING WEEKLY DATES after each download.
#   * If it fails, try a wider overlap; never assume positivity in one window
#     guarantees a valid link in a newly normalized Google Trends request.
#   * Repeat backwards toward 2014 (or earlier, if explicitly configured).
#   * Save every new request as a separate checkpoint read by Script 02.
#   * Skip windows already on disk and keep a per-DMA log.
#
# IMPORTANT: This is a complementary adaptive mode of Script 03. Do not run the
# older fixed-window Script 03 with DOWNLOAD=TRUE simultaneously.
# No requests are made unless DOWNLOAD=TRUE. Run Script 02 and then the
# existing 03_repair_failed_overlaps_2014_onward.R in diagnostic-only mode
# after each batch to measure how many DMAs became connected.
# =============================================================================

PROJECT_DIR <- "C:/Users/Melodie Nekoorad/Documents/Research/Air_Quality"
DOWNLOAD <- TRUE
MAX_DMAS <- 5L                # At most five DMAs per run
MAX_NEW_REQUESTS <- 10L       # Absolute safety cap across all DMAs in one run
GOAL_START_YEAR <- 2014L     # Change to 2010/2006 to extend before 2014
MAX_OVERLAP_YEARS <- 3L      # Marissa's initial 1 -> 2 -> 3-year search
WINDOW_YEARS <- 4L          # New requests cover four calendar years
MIN_POSITIVE_WEEKS <- 1L    # Exploratory matching rule from Script 02
PAUSE_SECONDS <- 20
MAX_ATTEMPTS <- 3L
# If TRUE, can use existing original chunks as bridges without downloading.
REUSE_SAVED_WINDOWS <- TRUE

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr)
  library(tibble); library(stringr)
})

root <- normalizePath(PROJECT_DIR, mustWork = TRUE)
processed_dir <- file.path(root, "data", "processed")
diag_dir <- file.path(root, "results", "diagnostics")
# SAME repair folder used by Script 02. Do not change without updating Script 02.
repair_dir <- file.path(root, "data", "checkpoints", "repair")
dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(repair_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(GOAL_START_YEAR >= 2004L, MAX_OVERLAP_YEARS >= 1L,
          MAX_OVERLAP_YEARS < WINDOW_YEARS,
          MAX_DMAS >= 1L, MAX_NEW_REQUESTS >= 1L)

original_path <- file.path(processed_dir, "air_quality_original_raw.rds")
status_path <- file.path(diag_dir, "repair_status_2014_onward.csv")
if (!file.exists(original_path) || !file.exists(status_path)) {
  stop("Run Scripts 01, 02 and the diagnostic version of Script 03 first.")
}
original <- as_tibble(readRDS(original_path)) |>
  mutate(window_id = paste0("original_", chunk_id), date = as.Date(date)) |>
  select(dma_code, window_id, date, hits)
status <- read_csv(status_path, show_col_types = FALSE)
if (!all(c("dma_code", "connected") %in% names(status))) stop("Bad status file")

# Read previously downloaded repairs, including those from earlier batches.
load_repairs <- function() {
  paths <- list.files(repair_dir, pattern = "\\.rds$", full.names = TRUE)
  map_dfr(paths, function(path) {
    x <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.null(x) || !is.data.frame(x) ||
        !all(c("dma_code", "window_id", "date", "hits") %in% names(x))) {
      warning("Skipping non-repair RDS: ", basename(path))
      return(tibble())
    }
    as_tibble(x) |>
      mutate(date = as.Date(date)) |>
      select(dma_code, window_id, date, hits)
  })
}

all_saved <- bind_rows(original, load_repairs()) |>
  filter(!is.na(date)) |>
  distinct(dma_code, window_id, date, .keep_all = TRUE)

# The original late window is preferred as the anchor, to avoid a new request
# for 2021–2025 when 2022–2026 already exists. To force an exact 2021–2025
# anchor instead, that would be an additional, explicitly planned download.
window_info <- function(x) {
  x |> group_by(window_id) |>
    summarise(first_date = min(date), last_date = max(date),
              n_positive = sum(is.finite(hits) & hits > 0), .groups = "drop")
}

# A usable connection needs at least one shared date with positive hits in
# BOTH requests. A positive value in the current window alone is not enough.
positive_matches <- function(a, b) {
  left <- a |> select(date, hits_a = hits)
  right <- b |> select(date, hits_b = hits)
  joined <- inner_join(left, right, by = "date")
  sum(is.finite(joined$hits_a) & joined$hits_a > 0 &
        is.finite(joined$hits_b) & joined$hits_b > 0)
}

# For a backward extension, inspect the EARLIEST years of the current series.
# (The pseudocode inspects the latest years, but that would choose an overlap
# at the wrong end when walking BACKWARDS.) Return the shortest positive span.
positive_in_earliest_years <- function(x, max_years = MAX_OVERLAP_YEARS) {
  first_year <- as.integer(format(min(x$date) + 6, "%Y"))
  for (k in seq_len(max_years)) {
    cutoff <- as.Date(sprintf("%04d-01-01", first_year + k))
    if (any(x$date < cutoff & is.finite(x$hits) & x$hits > 0)) return(k)
  }
  NA_integer_
}

# Candidate four-year windows: if current starts in 2022 and overlap=1,
# request 2019–2022; overlap=2 => 2020–2023; overlap=3 => 2021–2024.
# Because years are inclusive, the candidate has the chosen count of
# overlapping calendar years with the current window.
make_candidate <- function(dma, current_start_year, overlap_years) {
  end_year <- current_start_year + overlap_years - 1L
  start_year <- end_year - WINDOW_YEARS + 1L
  start_date <- as.Date(sprintf("%04d-01-01", start_year))
  end_date <- as.Date(sprintf("%04d-12-31", end_year))
  window_id <- paste0("repair_", format(start_date, "%Y%m%d"), "_",
                      format(end_date, "%Y%m%d"))
  checkpoint <- file.path(repair_dir, paste0(gsub("-", "_", dma), "_",
                                             window_id, ".rds"))
  tibble(dma_code = dma, window_id, start_date, end_date,
         overlap_years, checkpoint, already_saved = file.exists(checkpoint))
}

# A network request is the ONLY place where new data are downloaded.
fetch_candidate <- function(candidate) {
  if (!requireNamespace("gtrendsR", quietly = TRUE)) stop("Install gtrendsR")
  for (attempt in seq_len(MAX_ATTEMPTS)) {
    response <- tryCatch(gtrendsR::gtrends(
      keyword = "air quality", geo = candidate$dma_code,
      time = paste(candidate$start_date, candidate$end_date),
      onlyInterest = TRUE), error = function(e) e)
    if (!inherits(response, "error") &&
        !is.null(response$interest_over_time) &&
        nrow(response$interest_over_time) > 0) {
      x <- response$interest_over_time
      h <- trimws(as.character(x$hits))
      cleaned <- tibble(dma_code = candidate$dma_code,
                        window_id = candidate$window_id,
                        date = as.Date(x$date),
                        hits = suppressWarnings(as.numeric(h)),
                        hits_raw = h, censored = h %in% c("<1", "< 1"))
      if (!anyNA(cleaned$date) && !anyDuplicated(cleaned$date)) {
        saveRDS(cleaned, candidate$checkpoint)
        return(list(ok = TRUE, data = cleaned, error = NA_character_))
      }
      error_message <- "invalid or duplicate response dates"
    } else {
      error_message <- if (inherits(response, "error"))
        conditionMessage(response) else "empty response"
    }
    if (attempt < MAX_ATTEMPTS) Sys.sleep(60 * attempt)
  }
  list(ok = FALSE, data = NULL, error = error_message)
}

# The flexible process is STATEFUL: one DMA can require several sequential
# windows, but a run is capped by MAX_NEW_REQUESTS. Saved repairs are reused.
failed_dmas <- status |> filter(!connected) |> pull(dma_code) |> unique()
failed_dmas <- head(failed_dmas, MAX_DMAS)
requests_used <- 0L
log_rows <- list()
planned_rows <- list()

for (dma in failed_dmas) {
  message("DMA: ", dma)
  dma_data <- all_saved |> filter(dma_code == dma)
  late <- dma_data |> filter(window_id == "original_5")
  if (!nrow(late)) {
    log_rows[[length(log_rows) + 1L]] <- tibble(
      dma_code = dma, step = 0L, action = "skip", detail = "original_5 absent")
    next
  }
  current <- late
  current_id <- "original_5"
  step <- 0L
  visited <- character()
  
  repeat {
    step <- step + 1L
    # Weekly Google dates can begin up to six days before Jan 1.
    # Shift by six days to identify the intended first calendar year.
    current_start <- as.integer(format(min(current$date) + 6, "%Y"))
    if (current_start <= GOAL_START_YEAR) {
      log_rows[[length(log_rows) + 1L]] <- tibble(
        dma_code = dma, step, action = "reached_goal",
        detail = paste("Earliest connected window starts", current_start))
      break
    }
    if (step > 30L) stop("Safety stop: unexpected number of steps for ", dma)
    
    # Reuse a previously saved older window if it ALREADY connects to the
    # current window. Prefer the oldest such saved window to maximize reach.
    existing <- dma_data |>
      filter(window_id != current_id) |>
      group_by(window_id) |>
      group_split()
    possible <- map_dfr(existing, function(z) {
      start <- min(z$date)
      if (start >= min(current$date)) return(tibble())
      n_matches <- positive_matches(z, current)
      if (n_matches < MIN_POSITIVE_WEEKS) return(tibble())
      tibble(window_id = z$window_id[1], first_date = start,
             n_positive_matches = n_matches)
    })
   
    # If no existing saved windows can connect to the current window,
    # skip the reuse step and proceed to generating new repair windows.
    
    if (nrow(possible) > 0) {
      possible <- possible |>
        filter(!window_id %in% visited) |>
        arrange(first_date)
    }
    
    if (REUSE_SAVED_WINDOWS && nrow(possible)) {
      chosen <- possible[1, ]
      visited <- c(visited, current_id)
      current_id <- chosen$window_id
      current <- dma_data |> filter(window_id == current_id)
      log_rows[[length(log_rows) + 1L]] <- tibble(
        dma_code = dma, step, action = "reuse_saved",
        detail = paste(current_id, "positive shared weeks:",
                       chosen$n_positive_matches))
      next
    }
    
    suggested_overlap <- positive_in_earliest_years(current)
    if (is.na(suggested_overlap)) {
      log_rows[[length(log_rows) + 1L]] <- tibble(
        dma_code = dma, step, action = "stop_no_positive_anchor",
        detail = paste("No positive observations in first",
                       MAX_OVERLAP_YEARS, "calendar years of", current_id))
      break
    }
    
    # Try the smallest positive overlap first, then progressively wider ones.
    # All candidates still have a start year earlier than the current window.
    candidates <- map_dfr(suggested_overlap:MAX_OVERLAP_YEARS,
                          ~ make_candidate(dma, current_start, .x))
    linked <- FALSE
    for (j in seq_len(nrow(candidates))) {
      candidate <- candidates[j, ]
      planned_rows[[length(planned_rows) + 1L]] <- candidate |>
        mutate(step = step, anchor_window = current_id)
      if (candidate$window_id %in% visited) next
      
      if (candidate$already_saved) {
        trial <- tryCatch(readRDS(candidate$checkpoint), error = function(e) NULL)
        if (is.null(trial)) next
        trial <- as_tibble(trial) |> mutate(date = as.Date(date))
        action <- "read_saved"
      } else if (!DOWNLOAD) {
        # Dry run: plan ONE candidate and stop this DMA. We cannot know if
        # the next window connects until its actual Google response is saved.
        log_rows[[length(log_rows) + 1L]] <- tibble(
          dma_code = dma, step, action = "planned_only",
          detail = paste(candidate$window_id, "overlap years:",
                         candidate$overlap_years))
        break
      } else {
        if (requests_used >= MAX_NEW_REQUESTS) break
        result <- fetch_candidate(candidate)
        requests_used <- requests_used + 1L
        if (!result$ok) {
          log_rows[[length(log_rows) + 1L]] <- tibble(
            dma_code = dma, step, action = "download_failed",
            detail = paste(candidate$window_id, result$error))
          next
        }
        trial <- result$data
        action <- "downloaded"
        Sys.sleep(PAUSE_SECONDS)
      }
      
      n_matches <- positive_matches(trial, current)
      log_rows[[length(log_rows) + 1L]] <- tibble(
        dma_code = dma, step,
        action = if (n_matches >= MIN_POSITIVE_WEEKS) "linked" else "no_link",
        detail = paste(candidate$window_id, "source:", action,
                       "positive shared weeks:", n_matches))
      if (n_matches >= MIN_POSITIVE_WEEKS) {
        dma_data <- bind_rows(dma_data, trial |>
                                select(dma_code, window_id, date, hits)) |>
          distinct(window_id, date, .keep_all = TRUE)
        visited <- c(visited, current_id)
        current_id <- candidate$window_id
        current <- trial
        linked <- TRUE
        break
      }
    }
    if (!linked) break
    if (requests_used >= MAX_NEW_REQUESTS) break
  }
}

# Use timestamped diagnostics to preserve previous exploratory batches.
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_tbl <- bind_rows(log_rows)
plan_tbl <- bind_rows(planned_rows)
write_csv(log_tbl, file.path(diag_dir, paste0("flexible_repair_log_", stamp, ".csv")))
write_csv(plan_tbl, file.path(diag_dir, paste0("flexible_repair_plan_", stamp, ".csv")))
message("Finished. New requests this run: ", requests_used,
        ". DMAs selected: ", length(failed_dmas), ".")
message("Next: rerun Script 02, then diagnostic Script 03 to check 2014+ connectivity.")
