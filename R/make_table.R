# Install and load required packages

## Package names
packages <- c("knitr", "kableExtra", "flextable")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, function(x) {
  suppressPackageStartupMessages(library(x, character.only = TRUE))}))

# Output type helper function
output_type = function() {
  out <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if(length(out) == 0) "html" else out
}

# The function
make_table <- function(tmp, caption = "", digits = 0, font_size = 10, escape = TRUE) {

  if(output_type() == "html") {
    
    # if(any(apply(tmp, 2, function(k) grepl("\n", k)))) {
    #   special_characters <- TRUE
    #   
    #   tmp[1:ncol(tmp)] <- lapply(tmp, function(k) gsub("\n", "<br>", k))
    # } else {
    #   special_characters <- FALSE
    # }
    
    kableExtra::kbl(
      tmp,
      caption = caption,
      format.args = list(big.mark = ""),
      digits = digits,
      row.names = FALSE,
      escape = escape
      ) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "condensed"))

  } else if (output_type() == "docx") {

    FitFlextableToPage <- function(ft, pgwidth = 6){
      ft_out <- ft %>% flextable::set_table_properties(layout = "autofit") %>% flextable::autofit()
      ft_out <- flextable::width(ft_out, width = dim(ft_out)$widths*pgwidth/(flextable::flextable_dim(ft_out)$widths))
      return(ft_out)
    }

    flextable::flextable(tmp) %>%
      flextable::fontsize(size = font_size, part="all") %>%
      flextable::set_caption(caption = caption) %>%
      # flextable::set_table_properties(layout = "autofit") %>%
      flextable::align_nottext_col(align = "center") %>%
      flextable::align_text_col(align = "left") %>%
      flextable::colformat_int(big.mark = "", na_str = "") %>%
      flextable::colformat_double(big.mark = "", digits = digits, na_str = "") %>% 
      flextable::theme_booktabs() 
    
    # FitFlextableToPage(ft)

  } else {
    tmp %>%
      knitr::kable(
        format = "latex",
        booktabs = TRUE,
        longtable = TRUE,
        row.names = FALSE,
        escape = escape,
        format.args = list(big.mark = ""),
        digits = digits) %>%
      kableExtra::kable_styling(latex_options = c("striped"))
  }
}
