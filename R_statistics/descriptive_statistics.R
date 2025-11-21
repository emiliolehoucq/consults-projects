# Script for descriptive statistics

# Source data -------------------------------------------------------------

source("1_data_cleaning.r")

# Loading libraries -------------------------------------------------------

library(gt)

# Define functions --------------------------------------------------------

make_distribution_table <- function(data, var, var_name_title) {
  
  var_sym <- rlang::ensym(var)
  var_name <- rlang::as_label(var_sym)
  
  tab <- data %>%
    tabyl(!!var_sym) %>%
    rename(" " = !!var_sym, "Count" = n, "Percentage" = percent) %>%
    adorn_pct_formatting(digits = 2) %>%
    gt() %>%
    tab_header(title = md(paste0("**Distribution of ", var_name_title, "**"))) %>%
    tab_style(
      style = cell_text(style = "italic"),
      locations = cells_column_labels(columns = everything())
    )
  
  filename <- paste0("table_", var_name, ".html")
  gtsave(tab, filename = filename)
  
  filename_word <- paste0("table_", var_name, ".docx")
  gtsave(tab, filename = filename_word)
  
  return(tab)
  
}

make_cross_tab <- function(data, second_var, var_name_title) {
  
  var_sym  <- rlang::ensym(second_var)
  var_name <- rlang::as_label(var_sym)
  
  tab <- data %>%
    tabyl(<VAR>, !!var_sym) %>%
    rename(" " = <VAR>) %>%
    adorn_percentages(denominator = "col") %>%
    adorn_pct_formatting(digits = 0) %>%
    adorn_ns() %>%
    gt() %>%
    tab_header(title = md(paste0("**<VAR> against ", var_name_title, "**")))
  
  filename <- paste0("table_<VAR>_vs_", var_name, ".html")
  gtsave(tab, filename = filename)
  
  filename_word <- paste0("table_<VAR>_vs_", var_name, ".docx")
  gtsave(tab, filename = filename_word)
  
  return(tab)
  
}

make_ma_rate_table <- function(data, var1, var2, title_end) {
  
  var1_sym  <- rlang::ensym(var1)
  var2_sym  <- rlang::ensym(var2)
  var1_name <- rlang::as_label(var1_sym)
  var2_name <- rlang::as_label(var2_sym)
  
  tab <- data %>%
    group_by(!!var1_sym, !!var2_sym) %>%
    summarise(n = n(), n_value = sum(<VAR> == "VALUE", na.rm = TRUE)) %>%
    mutate(report = paste0(scales::percent(n_value / n, accuracy = 1), " (", n_value, "/", n, ")")) %>%
    select(-n, -n_value) %>%
    pivot_wider(names_from = !!var1_sym, values_from = report) %>%
    rename(" " = !!var2_sym) %>%
    gt() %>%
    tab_header(title = md(paste0("**Rate of <VALUE> vs <VALUE> by ", title_end, "**")))
  
  filename_html <- paste0("table_<VALUE_RATE>_by_", var2_name, "_and_", var1_name, ".html")
  gtsave(tab, filename = filename_html)
  
  filename_docx <- paste0("table_<VALUE_RATE>_by_", var2_name, "_and_", var1_name, ".docx")
  gtsave(tab, filename = filename_docx)
  
  return(tab)
  
}

# Univariate tables -------------------------------------------------------

make_distribution_table(data_tables, <VAR>, "TITLE")
make_distribution_table(data_tables, <VAR>, "TITLE")
make_distribution_table(data_tables, <VAR>, "TITLE")
make_distribution_table(data_tables, <VAR>, "TITLE")
make_distribution_table(data_tables, <VAR>, "TITLE")

# Cross tabs --------------------------------------------------------------

make_cross_tab(data_tables, <VAR>, "TITLE")
make_cross_tab(data_tables, <VAR>, "TITLE")
make_cross_tab(data_tables, <VAR>, "TITLE")
make_cross_tab(data_tables, <VAR>, "TITLE")

# <VALUE> rates ----------------------------------------------------------------

make_ma_rate_table(data_tables, <VAR>, <VAR>, "TITLE")
make_ma_rate_table(data_tables, <VAR>, <VAR>, "TITLE")
make_ma_rate_table(data_tables, <VAR>, <VAR>, "TITLE")
make_ma_rate_table(data_tables, <VAR>, <VAR>, "TITLE")
make_ma_rate_table(data_tables, <VAR>, <VAR>, "TITLE")
 