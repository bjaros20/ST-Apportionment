# inventory.R

# set seed
set.seed(26)

#Set Directory
setwd("~/Documents/GitHub/ST-Apportionment")


# inventory.R
dir.create("repo_audit", showWarnings = FALSE)
files <- list.files(".", recursive = TRUE, all.files = TRUE, include.dirs = FALSE, no.. = TRUE)
# Drop .git internals from inventory
files <- files[!grepl("^\\.git(/|$)", files)]

get_git_last_commit <- function(path) {
  cmd <- sprintf('git log -1 --format="%%h|%%ad|%%an|%%s" --date=short -- "%s"', path)
  out <- tryCatch(system(cmd, intern = TRUE), error = function(e) NA_character_)
  if (length(out) == 0) return(NA_character_)
  parts <- strsplit(out, "\\|")[[1]]
  if (length(parts) < 4) return(NA_character_)
  data.frame(last_commit = parts[1], last_date = parts[2], last_author = parts[3], last_subject = parts[4])
}

rows <- lapply(files, function(f) {
  info <- file.info(f)
  ext  <- tools::file_ext(f)
  meta <- get_git_last_commit(f)
  data.frame(
    path = f,
    ext = ifelse(nzchar(ext), ext, NA),
    size_bytes = info$size,
    mtime = as.character(info$mtime),
    last_commit = meta$last_commit,
    last_date = meta$last_date,
    last_author = meta$last_author,
    last_subject = meta$last_subject,
    stringsAsFactors = FALSE
  )
})
inv <- do.call(rbind, rows)
write.csv(inv, "repo_audit/inventory.csv", row.names = FALSE)
message("Wrote repo_audit/inventory.csv")

