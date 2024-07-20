library(tidycensus)

metro <- full_join(
  tibble(
    get_acs(
      geography = "tract",
      variables = c(
        population = "B01003_001",
        medincome = "B19013_001"
      ),
      county = c(
        "mecklenburg",
        "union",
        "cabarrus",
        "gaston",
        "iredell",
        "rowan",
        "lincoln",
        "cleveland"
      ),
      state = "nc",
      year = 2022,
      geometry = TRUE,
      output = "wide"
    )
  ),
  tibble(
    get_acs(
      geography = "tract",
      variables = c(
        population = "B01003_001",
        medincome = "B19013_001"
      ),
      county = c(
        "york",
        "lancaster"
      ),
      state = "sc",
      year = 2022,
      geometry = TRUE,
      output = "wide"
    )
  )
) %>%
  st_as_sf() %>%
  separate(NAME,
    sep = "; ",
    into = c("tract", "county", "state")
  ) %>%
  mutate(
    tract = str_trim(str_sub(tract, start = 13)),
    county = str_trim(str_sub(county, end = -7))
  ) %>%
  rename(geoid = GEOID)

metro_transit <- metro %>%
  filter(county == "Mecklenburg" |
    county == "Gaston" |
    county == "Cabarrus" |
    county == "York" |
    county == "Union")
