# load_data
# script for reading and processing 2019 GE result data 
# Author: Roger Beecham
###############################################################################

# Read in data from 2017 and 2015 from Electoral Commission.
data <- read_csv("./data/data_2015_2017.csv")

# From 2017, recode parties.
# Recode speaker (Bercow) to Conservative
# Recode Democratic Unionist Party to DUP
# Recode Labour and Co-operative to Labour
# Recode Scottish National Party to SNP
# Recode UK Independence Party to UKIP
data <- data %>% mutate(
  party = case_when(
    party == "Speaker" ~ "Conservative", # speaker (Bercow) to Conservative
    party == "Democratic Unionist Party" ~ "DUP",
    party == "Labour and Co-operative" ~ "Labour",
    party == "Scottish National Party" ~ "SNP",
    party == "UK Independence Party" ~ "UKIP",
    party == "Liberal Democrats" ~ "Liberal Democrat",
    party == "Social Democratic and Labour Party" ~ "SDLP",
    party == "Ulster Unionist Party" ~ "UUP",
    party == "Green Party" ~ "Green",
    TRUE ~ as.character(party)),
  party=gsub("\\s", "_", party)
) %>%
  # Remove columns that are not needed.
  select(-c(pano, surname, first_name))

# Load in unverified 2019 result data.
data_2019 <- read_csv("./data/data_2019.csv") %>%
  # Rename parties for consistency with 2017 data.
  rename("Conservative"="con", "Labour"="lab", "Liberal_Democrat" = "lib", "Plaid_Cymru" = "plc", "Brexit_Party"="brx", "DUP"="dup", "Other"="oth", "SDLP"="sdl", "SNP"="snp", "Independent"="ind", "Speaker"="spk", "UKIP"="ukp", "UUP"="uup", "Sinn_Fein"="snf", "Green"="grn", "Alliance"="all") %>%
  # Reshape dataset so it can be joined with Electoral Commission data.
  select(-c(electorate, turnout, winning_party, previous_party, gain, second_party, majority)) %>%
  pivot_longer(-c(code, constituency), names_to="party", values_to="valid_votes") %>%
  # Add a year column and rename code to ons_code.
  add_column(year=2019) %>% rename("ons_code"="code") %>%
  # Recode speaker to Labour.
  filter(valid_votes!=0) %>%
  mutate(party=if_else(party=="Speaker", "Labour", party))

# Load in outlines of constituencies -- simplified using mapshapr.
constituency_boundaries <- st_read("./data/constituency_boundaries.geojson", crs=27700)

# Load in Region lookup.
region_lookup <- read_csv("./data/constituency_region_lookup.csv")

# Recode the electoral commission data with Other category for consistency.
# Also remove 2015 data and non-GB constituencies.
elected_parties_names <- c("Conservative","Labour","Liberal_Democrat","Plaid_Cymru","Brexit_Party", "DUP", "Other", "SDLP", "SNP", "Independent", "Speaker", "UKIP", "UUP", "Sinn_Fein", "Green", "Alliance")
data <- data %>% inner_join(region_lookup %>% select(ons_code=PCON17CD,region_name=EER17NM)) %>% filter(year==2017) %>%
  mutate(party=if_else(party %in% elected_parties_names, party, "Other")) %>% select(-region_name) %>%
  group_by(ons_code, constituency, year, party) %>%
  summarise(valid_votes=sum(valid_votes))
rm(elected_parties_names)

# Merge 2019 and electoral commision data
data <- bind_rows(data_2019, data)

# Load in estimated deprivation data at contituency-level
uk_deprivation_con <- read_csv("./data/uk_deprivation_con.csv")

