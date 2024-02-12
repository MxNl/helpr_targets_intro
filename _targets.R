library(targets)
# library(crew)

options(clustermq.scheduler = "multiprocess")
tar_option_set(
  # deployment = "worker",
  # controller = crew_controller_local(workers = 3),
  packages = c(
    "dplyr",
    "readr",
    "tidyr",
    "lubridate",
    "forcats",
    "scales",
    "patchwork",
    "ggplot2"
  )
)


preprocess_gwl_data <- function(x) {
  x |> 
    as_tibble() |> 
    drop_na(gwl) |> 
    mutate(date = as_date(date)) |> 
    rename(well_id = fk_well) |> 
    mutate(well_id = as_factor(well_id))
}


plot_gwl_data <- function(x) {
  
  Sys.sleep(10)
  
  x |> 
    ggplot(aes(date, gwl)) +
    geom_line(colour = "steelblue") +
    theme_minimal() +
    labs(x = NULL, y = "Groundwater Level [m a.s.l.]") +
    scale_x_date(
      breaks = "5 years", 
      date_minor_breaks = "year", 
      labels = label_date("%Y")
      )
}


list(
  tar_target(
    data_gwl_file,
    "helpr_targets_gwl.csv",
    format = "file"
  ),
  tar_target(
    data_gwl,
    read.csv(data_gwl_file)
  ),
  tar_target(
    data_gwl_preprocessed,
    preprocess_gwl_data(data_gwl) |> 
    group_by(well_id) |> 
    group_split(),
    iteration = "list"
  ),
  tar_target(
    data_gwl_plot,
    plot_gwl_data(data_gwl_preprocessed),
    pattern = map(data_gwl_preprocessed),
    iteration = "list",
    deployment = "worker"
  ),
  tar_target(
    single_plot,
    data_gwl_plot |> 
      patchwork::wrap_plots(ncol = 1)
  )
)


