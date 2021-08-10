# 2015 Population Pyramids for LMDC and Jefferson County
# by Eric Book

# Proportional values are calculated from total counts per Race per Population (LMDC or Jefferson County)

# Source: Louisville Metro Government Open Data. [Daily LMDC Population Snapshots.](https://data.louisvilleky.gov/dataset/daily-lmdc-population-snapshots). 2019


# Notes:
# 1. Tried to adapt the ACS data to the LMDC data as much as possible
# 2. Don't know whether or how to incorporate the HISP variable. Leaving out or leave it in? Need it for a hispanic count. Not straight forward at least to me on how I should use the HISP categories to compare to the LMDC data. For now, I'm including it and using the Hispanic, Nonhispanic categories.
# 3. Unclear how to use the other race descriptions in the ACS data. Should I have aggregated all the categories? Went with the "<race> only" categories.



pacman::p_load(extrafont, grid, gridExtra, lubridate, tidycensus, tidyverse)




# ==== Jeff Data ====


jeff_char <- get_estimates(
      geography = "county",
      product = "characteristics",
      breakdown = c("SEX", "AGEGROUP", "RACE", "HISP"),
      breakdown_labels = TRUE,
      state = "KY",
      county = "Jefferson",
      year = 2015
)




jeff_recode <- jeff_char %>% 
   select(Race = RACE, Age = AGEGROUP, Gender = SEX, jeff = value, HISP) %>% 
   filter(HISP == "Non-Hispanic",
          !Age %in% c("All ages", "Age 0 to 4 years",
                      "Age 5 to 9 years", "Age 10 to 14 years",
                      "Age 15 to 19 years"),
          !Race %in% c("All races",
                       "Two or more races",
                       "White alone or in combination",
                       "Black alone or in combination",
                       "American Indian and Alaska Native alone or in combination",
                       "Asian alone or in combination",
                       "Native Hawaiian and Other Pacific Islander alone or in combination"),
          Gender != "Both sexes") %>% 
   mutate(Race = recode(Race,
                        "American Indian and Alaska Native alone" = "Amer Indian/Alaska Native",
                        "Black alone" = "Black-origins of Africa",
                        "White alone" = "White/Eurp/ N.Afr/Mid Eas")) %>% 
   select(-HISP)


# LMDC has these race descriptions combined
comb_race <- jeff_recode %>% 
   select(Age, Gender, Race, jeff) %>%
   filter(Race == "Asian alone" | Race == "Native Hawaiian and Other Pacific Islander alone") %>% 
   group_by(Age, Gender) %>% 
   nest() %>% 
   mutate(data = map(data, function(df){
      df %>% 
         summarize(Race = "Asian or Pacific Islander",
                   jeff = sum(jeff))
   })) %>% 
   unnest(cols = c(data))


# Creating a Hispanic variable to match LMDC's
# Using the <race> only categories. Don't have any guidance on this yet
jeff_hisp <- jeff_char %>% 
   select(Race = RACE, Age = AGEGROUP, Gender = SEX, jeff = value, HISP) %>% 
   filter(HISP == "Hispanic",
          !Age %in% c("All ages", "Age 0 to 4 years",
                      "Age 5 to 9 years", "Age 10 to 14 years",
                      "Age 15 to 19 years"),
          !Race %in% c("All races",
                       "Two or more races",
                       "White alone or in combination",
                       "Black alone or in combination",
                       "American Indian and Alaska Native alone or in combination",
                       "Asian alone or in combination",
                       "Native Hawaiian and Other Pacific Islander alone or in combination"),
          Gender != "Both sexes") %>% 
   select(-Race) %>% 
   group_by(Age, Gender, HISP) %>% 
   summarize(sum = sum(jeff)) %>% 
   rename(Race = HISP, jeff = sum)


# Come together
jeff_group <- jeff_recode %>% 
   filter(Race != "Asian alone",
          Race != "Native Hawaiian and Other Pacific Islander alone") %>% 
   bind_rows(comb_race, jeff_hisp) %>%
   mutate(Age = gsub("Age | years", "", Age),
          Race = as.factor(Race))


 
# ==== LMDC Data ====


types <- c("iicTicccccccccT")


lmdc <- read_csv(here::here("data", "LMDC_Population_Snapshots.csv"), col_types = types) %>%
   filter(CurrentAge != 238)


# create age_group variable, filter for 2015, removed other and unknown since they aren't included in the ACS
lmdc_15_mis <- lmdc %>% 
   mutate(year = year(SnapshotDateTime),
          Age = Hmisc::cut2(lmdc$CurrentAge, cuts = seq(0, 100, 5)),
          Gender = as.factor(Gender),
          RaceDescription = as.factor(RaceDescription)) %>% 
   filter(is.na(ChargeDisposition),
          year == "2015",
          !RaceDescription %in% c("Other", "UNKNOWN")) %>%
   mutate(RaceDescription = fct_drop(RaceDescription)) %>% 
   distinct(BookingNumber, BookingDate, .keep_all = TRUE) %>% 
   select(Race = RaceDescription, Gender, Age)


# spread-fill-gather dance in order to fill in zeros where categories have no values
lmdc_group_fill <- lmdc_15_mis %>% 
   count(Race, Gender, Age) %>%
   spread(Age, n, fill = 0) %>% 
   gather(key = Age, value = n, -Race, -Gender) %>% 
   spread(Gender, n, fill = 0) %>% 
   gather(key = Gender, value = n, -Age, -Race)


# Match my Age labels to ACS' (Geez Harrell really jacked up his cut2 labels)
# Removing 15-19 year olds since there aren't any 15-17 year olds aren't in LMDC.
lmdc_age <- lmdc_group_fill %>% 
   mutate(Age = gsub("\\[|\\)|\\]", "", Age),
          Age = gsub("^ ", "", Age),
          Age = gsub("(, )", ",", Age)) %>%
   separate(col = Age, into = c("a", "b"), sep = ",") %>% 
   mutate(b = as.integer(b) - 1) %>% 
   unite(a, b, col = "Age", sep = " to ") %>% 
   filter(Age != "15 to 19")


# Combine two age categories into one
comb_age <- lmdc_age %>% 
   select(Race, Gender, Age, n) %>%
   filter(Age == "85 to 89" | Age == "95 to 99") %>% 
   group_by(Race, Gender) %>% 
   nest() %>% 
   mutate(data = map(data, function(df){
      df %>% 
         summarize(Age = "85 and older",
                   n = sum(n))
   })) %>% 
   unnest(cols = c(data))


# Come together
lmdc_group <- lmdc_age %>% 
   filter(Age != "85 to 89",
          Age != "95 to 99") %>% 
   bind_rows(comb_age) %>% 
   rename(lmdc = n) %>% 
   mutate(Gender = recode(Gender, "M" = "Male", "F" = "Female"))



pop_data <- jeff_group %>% 
   inner_join(lmdc_group, by = c("Age", "Gender", "Race")) %>% 
   gather(key = "Group", value = "Value", -Age, -Gender, -Race) %>% 
   group_by(Race, Group) %>% 
   mutate(Total = sum(Value),
          Proportion = round(100*(Value/Total), 1)) %>%
   ungroup() %>% 
   mutate_if(is.character, as.factor) %>%
   mutate(Value = ifelse(Gender == "Male", -Value, Value),
          Proportion = ifelse(Gender == "Male", -Proportion, Proportion))



# ==== Pyramid Plot =====



pyr_plot <- ggplot() +
   geom_bar(data = pop_data %>% filter(Group == "jeff"),
            aes(x = Age, y = Proportion, fill = Group),
            width = .77,
            stat = "identity",
            position = "identity"
   ) +
   geom_bar(data = pop_data %>% filter(Group == "lmdc"),
            aes(x = Age, y = Proportion, fill = Group),
            width = 0.4,
            stat = "identity",
            position = "identity"
   ) +
   coord_flip() +
   scale_y_continuous(labels = abs) +
   geom_hline(yintercept = 0) +
   facet_wrap(.~Race, scales = "free") +
   theme_minimal(base_family = "Roboto") +
   theme(legend.position = c(0.89, 0.25), axis.text = element_text(face = "bold")) +
   scale_fill_manual(values = c("gray", "darkred"), labels = c("Jefferson County", "LMDC")) +
   ylab("Male | Female (%)") +
   labs(title = "Demographic comparision between Jefferson County and Louisville Metro Correctional populations",
        subtitle = "2015",
        fill = "",
        caption = "Source: Louisville Metro Government Open Data. Daily LMDC Population Snapshots. 2019\nSource: US Census Bureau population estimates & tidycensus R package"
        )
pyr_plot


grid.newpage()



# ==== Pyramid Plot w/table ====


pop_tab <- pop_data %>%
   mutate(Value = abs(Value)) %>% 
   group_by(Race, Group) %>% 
   summarize(Total = sum(Value)) %>% 
   group_by(Group) %>% 
   mutate(Group_Total = sum(Total),
          Proportion = round(100*(Total/Group_Total), 1)) %>%
   ungroup() %>% 
   mutate(Group = recode(Group, "jeff" = "Jefferson County", "lmdc" = "LMDC")) %>% 
   select(Race, Group, Proportion)


# font size may look bad in zoom but makes everything fit correctly in the png
theme_table1 <- ttheme_minimal(
   core = 
      list(fg_params =
              list(fontsize = 10, fontface = "plain"),
           bg_params = list(fill = "white")),
   base_family = "Roboto")


theme_table2 <- ttheme_minimal(
   core = 
      list(fg_params =
              list(fontsize = 10, fontface = "plain"),
           bg_params = list(fill = c(rep(c("gray", "darkred"), 4)),
                            alpha = 0.75)),
   base_family = "Roboto")


theme_table3 <- ttheme_minimal(
   core = 
      list(fg_params =
              list(fontsize = 10, fontface = "plain"),
           bg_params = list(fill = "white")),
   base_family = "Roboto")


tab_grob1 <- pop_tab["Race"] %>% 
   tableGrob(., rows = NULL, cols = NULL, theme = theme_table1)
tab_grob2 <- pop_tab["Group"] %>% 
   tableGrob(., rows = NULL, cols = NULL, theme = theme_table2)
tab_grob3 <- pop_tab["Proportion"] %>% 
   tableGrob(., rows = NULL, cols = NULL, theme = theme_table3)

pop_tab_grob <- gtable_combine(tab_grob1, tab_grob2, tab_grob3)


pyr_grob <- ggplotGrob(
   pyr_plot +
      theme(legend.position = "top") +
      guides(fill = "none") +
      labs(
         caption = "Table values are race proportions for each group (Jefferson County, LMDC)\nSource: Louisville Metro Government Open Data. Daily LMDC Population Snapshots. 2019\nSource: US Census Bureau population estimates & tidycensus R package")
)


grid.draw(pyr_grob)

table_vp <- viewport(x = 0.86, y = .29,
                     just = c("right", "bottom"),
                     height = 0.02, width = 0.05)

pushViewport(table_vp)

grid.draw(pop_tab_grob)

popViewport()

# pyr_tab <- grid.grab()


