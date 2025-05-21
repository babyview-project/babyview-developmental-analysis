load_eng_aoas <- function() {
  read_csv(here("intermediates", "eng_aoas.csv")) |>
    select(-intercept, -slope) |>
    pivot_wider(names_from = "measure",
                values_from = "aoa") |>
    mutate(item_definition = item_definition |>
             str_replace_all(c(
               " \\(object\\)" = "",
               " \\(animal\\)" = "",
               " \\(food\\)" = "",
               " \\(beverage\\)" = "",
               " \\(not beverage\\)" = "",
               "soda/pop" = "soda",
               "tissue/kleenex" = "tissue",
               " \\(on phone\\)" = "",
               "/.*" = "",
               "\\*" = "",
               "woof woof" = "woof",
               "quack quack" = "quack",
               "yum yum" = "yum"
             ))) |>
    group_by(item_definition) |>
    summarise(produces = min(produces, na.rm = T),
              understands = min(understands, na.rm = T),
              .groups = "drop") |>
    mutate(understands = ifelse(is.infinite(understands), NA, understands))
}
