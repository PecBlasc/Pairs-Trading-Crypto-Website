# -----------------------------------------------
# 6. Script to generate automated Reports
# Project: Pairs Trading Thesis
# -----------------------------------------------

library(rmarkdown)

# Create output folder
dir.create("Reports", showWarnings = FALSE)

# Copy CSS to the working directory (where the .Rmd file is located)
file.copy("Reports/custom_report_style.css", "custom_report_style.css", overwrite = TRUE)

for (i in 1:nrow(optimized_results)) {
  row <- optimized_results[i, ]
  
  file_name <- paste0(
    gsub("[-/]", "", row$Crypto1),
    "_",
    gsub("[-/]", "", row$Crypto2),
    "_report.html"
  )
  
  render(
    input = "pair_report_template.Rmd",
    output_file = file_name,
    output_dir = "Reports",
    output_format = html_document(css = "custom_report_style.css"),
    params = list(
      asset1 = row$Crypto1,
      asset2 = row$Crypto2,
      entry = row$Entry,
      exit = row$Exit,
      crypto_data = crypto_data
    ),
    envir = new.env()
  )
}
