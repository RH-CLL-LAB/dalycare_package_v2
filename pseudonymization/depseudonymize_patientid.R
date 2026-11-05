# depseudonymize_patientid.R
#
# Translate patientid -> CPR for a data file, using the DALY-CARE key file.
# Runs LOCALLY only (P:/ or L: drive). CPR must never leave the local drives:
# the output is written to the same closed folder as the input unless told
# otherwise, and the console only prints counts, never row content.
#
# Usage (from R):
#   source("depseudonymize_patientid.R")
#   depseudonymize_patientid(
#     input  = "<closed folder>/<file>.xlsx",
#     output = "<closed folder>/<file>_cpr.xlsx"   # optional
#   )
#
# Usage (from a shell):
#   Rscript depseudonymize_patientid.R <input> [output] [key_file]
#
# Rules followed (see the pseudonymization workflow):
#   * CPR and ids are always read as text.
#   * CPR is cleaned only with pad_cpr(): trimws -> remove "-" -> left-pad to 10
#     with "0". Letters are never removed (replacement CPRs contain letters).
#   * The same cleaning is applied to the key file.
#   * Stops if the id column does not look like PATIENTID, if a PATIENTID maps
#     to several CPRs, or if the output would be written without a CPR column.

DEFAULT_KEY_FILE <- "P:/RH/KOS/Lukkede Mapper/DALYCAKEYS/Key/MapingRHID.csv"

pad_cpr <- function(x) {
  x <- gsub("-", "", trimws(as.character(x)), fixed = TRUE)
  x[x == ""] <- NA_character_
  ifelse(is.na(x), NA_character_, stringr::str_pad(x, width = 10, side = "left", pad = "0"))
}

# Excel may read an id as "101.0"; trim whitespace and a trailing ".0".
clean_id <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("\\.0+$", "", x)
  x[x == ""] <- NA_character_
  x
}

read_text_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    as.data.frame(readxl::read_excel(path, col_types = "text"), stringsAsFactors = FALSE)
  } else if (ext == "csv") {
    utils::read.csv(path, colClasses = "character", check.names = FALSE,
                    na.strings = character(0), stringsAsFactors = FALSE)
  } else if (ext %in% c("tsv", "txt")) {
    utils::read.delim(path, colClasses = "character", check.names = FALSE,
                      na.strings = character(0), stringsAsFactors = FALSE)
  } else {
    stop("Unsupported input type: .", ext, " (use xlsx, csv or tsv)")
  }
}

find_col <- function(df, name, what) {
  hit <- which(tolower(names(df)) == tolower(name))
  if (length(hit) != 1) {
    stop("Expected exactly one ", what, " column named '", name, "' (case-insensitive). ",
         "Columns found: ", paste(names(df), collapse = ", "))
  }
  names(df)[hit]
}

read_key <- function(key_file) {
  if (!file.exists(key_file)) {
    dir <- dirname(key_file)
    stop("Key file not found: ", key_file, "\nFiles in ", dir, ": ",
         if (dir.exists(dir)) paste(list.files(dir), collapse = ", ") else "(folder missing)")
  }
  key <- utils::read.csv(key_file, colClasses = "character", check.names = FALSE,
                         na.strings = character(0), stringsAsFactors = FALSE)
  cpr_col <- find_col(key, "CPR", "key CPR")
  id_col  <- find_col(key, "PATIENTID", "key PATIENTID")
  key <- data.frame(PATIENTID = clean_id(key[[id_col]]),
                    CPR = pad_cpr(key[[cpr_col]]),
                    stringsAsFactors = FALSE)
  key <- key[!is.na(key$CPR) & !is.na(key$PATIENTID), ]
  unique(key)
}

depseudonymize_patientid <- function(input, output = NULL, key_file = DEFAULT_KEY_FILE,
                                     id_col = "patientid", unmatched_file = NULL,
                                     min_id_share = 0.5) {
  df <- read_text_table(input)
  id_name <- find_col(df, id_col, "id")
  n_in <- nrow(df)

  if (any(grepl("cpr", names(df), ignore.case = TRUE))) {
    stop("Input already has a column with 'cpr' in its name - refusing to continue.")
  }

  df$.id_clean <- clean_id(df[[id_name]])
  key <- read_key(key_file)

  # The id column must look like PATIENTID, otherwise it is probably CPR already.
  share <- mean(df$.id_clean[!is.na(df$.id_clean)] %in% key$PATIENTID)
  if (is.nan(share) || share < min_id_share) {
    stop("Only ", round(100 * ifelse(is.nan(share), 0, share)),
         "% of '", id_name, "' values match PATIENTID in the key (need > ",
         100 * min_id_share, "%). The column may already be CPR, or the wrong key/file is used.")
  }

  # One PATIENTID must not map to several CPRs.
  dup <- unique(key$PATIENTID[duplicated(key$PATIENTID)])
  dup <- dup[dup %in% df$.id_clean]
  if (length(dup) > 0) {
    stop(length(dup), " PATIENTID in the data have more than one CPR in the key. Stopping.")
  }

  df$.row <- seq_len(n_in)
  merged <- merge(df, key, by.x = ".id_clean", by.y = "PATIENTID", all.x = TRUE, sort = FALSE)
  merged <- merged[order(merged$.row), , drop = FALSE]
  stopifnot(nrow(merged) == n_in)

  n_unmatched <- sum(is.na(merged$CPR) & !is.na(merged$.id_clean))
  if (n_unmatched > 0) {
    if (is.null(unmatched_file)) {
      unmatched_file <- file.path(dirname(input), "unmatched_patientid.csv")
    }
    utils::write.csv(data.frame(patientid = unique(merged$.id_clean[is.na(merged$CPR) & !is.na(merged$.id_clean)])),
                     unmatched_file, row.names = FALSE)
  }

  out <- merged[, setdiff(names(merged), c(".id_clean", ".row", id_name, "CPR")), drop = FALSE]
  out <- cbind(CPR = merged$CPR, out, stringsAsFactors = FALSE)
  # CPR is kept as text in the output; the id column is replaced by CPR.

  if (is.null(output)) {
    output <- file.path(dirname(input),
                        paste0(tools::file_path_sans_ext(basename(input)), "_cpr.xlsx"))
  }
  if (tolower(tools::file_ext(output)) != "xlsx") stop("Output must be .xlsx")
  openxlsx::write.xlsx(out, output)

  message("Rows in: ", n_in, ", rows out: ", nrow(out))
  message("Unique ids in: ", length(unique(stats::na.omit(df$.id_clean))),
          ", unique CPR out: ", length(unique(stats::na.omit(out$CPR))))
  message("Ids without CPR match: ", n_unmatched,
          if (n_unmatched > 0) paste0(" (listed in ", unmatched_file, ")") else "")
  invisible(output)
}

if (sys.nframe() == 0 && !interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) stop("Usage: Rscript depseudonymize_patientid.R <input> [output] [key_file]")
  depseudonymize_patientid(
    input    = args[1],
    output   = if (length(args) >= 2) args[2] else NULL,
    key_file = if (length(args) >= 3) args[3] else DEFAULT_KEY_FILE
  )
}
