# gt table for fiscal years 2014/2015 and 2015/2016


# sections:
# 1. set-up
# 2. 2014 - 2015
# 3. 2015 - 2016
# 4. form voltron



# ==== set-up ====


library(tidyverse)
library(lubridate)
library(gt)


types <- c("iicTicccccccccT")


lmdc <- read_csv(here::here("data", "LMDC_Population_Snapshots.csv"), col_types = types) %>%
   filter(CurrentAge != 238)

# booking numbers that have at least one charge disposition
bn_with_disp <- lmdc %>% 
   select(BookingNumber, BookingDate, ChargeDisposition) %>%
   filter(!is.na(ChargeDisposition)) %>% 
   distinct(BookingNumber) %>%
   pull(1)

# Find the snapdatetime with the first disposition entry for those bookingnumbers with dispositions and have bookingdates after the startdate of the database
first_disp_date <- lmdc %>%
   filter(BookingNumber %in% bn_with_disp) %>% 
   filter(as.Date(BookingDate) >= min(as.Date(SnapshotDateTime))) %>%
   distinct(BookingNumber, ChargeDisposition, SnapshotDateTime, .keep_all = TRUE) %>% 
   group_by(BookingNumber, BookingDate) %>%
   filter(!is.na(ChargeDisposition)) %>% 
   mutate(end_pretrial = first(as.Date(SnapshotDateTime))) %>% 
   distinct(BookingNumber, BookingDate, .keep_all = TRUE) %>%
   ungroup() %>% 
   select(BookingNumber, BookingDate, end_pretrial)



# ==== 2014 - 2015 ====


# Filter 2014-2015, pretrial, and misdemeanor observations; bin CurrentAge variable
lmdc_14_15_mis <- lmdc %>%
   filter(BookingNumber %in% bn_with_disp) %>% 
   filter(as.Date(BookingDate) >= min(as.Date(SnapshotDateTime))) %>%
   left_join(first_disp_date, by = c("BookingNumber", "BookingDate")) %>%
   mutate(SnapshotDateTime = as.Date(SnapshotDateTime)) %>% 
   filter(SnapshotDateTime < end_pretrial,
          between(SnapshotDateTime, as.Date("2014-07-11"), as.Date("2015-06-30")),
          ChargeCategory == "MISDEMEANOR") %>% 
   mutate(age_group = Hmisc::cut2(CurrentAge, cuts = seq(0, 100, 10))) %>% 
   distinct(BookingNumber, BookingDate, .keep_all = TRUE) %>%
   select(Gender, RaceDescription, MaritalStatus, age_group)


# get the counts; format some strings; shape into the form gt likes
lmdc_1415_mis_count <- lmdc_14_15_mis %>% 
   select(Gender, Race = RaceDescription, `Marital Status` = MaritalStatus, Age = age_group) %>% 
   group_by(Gender, Race, `Marital Status`, Age) %>% 
   gather(key = "groupname", value = "rowname") %>% 
   mutate(groupname = ifelse(groupname == "`Marital Status`", "Marital Status", groupname),
          rowname = str_to_title(rowname),
          rowname = ifelse(is.na(rowname), "Missing", rowname)
   ) %>% 
   count(groupname, rowname) %>% 
   group_by(groupname) %>% 
   mutate(n_1415_mis = n/sum(n)) %>% 
   select(-n) %>% 
   ungroup()


# felonies
lmdc_14_15_fel <- lmdc %>%
   filter(BookingNumber %in% bn_with_disp) %>% 
   filter(as.Date(BookingDate) >= min(as.Date(SnapshotDateTime))) %>%
   left_join(first_disp_date, by = c("BookingNumber", "BookingDate")) %>%
   mutate(SnapshotDateTime = as.Date(SnapshotDateTime)) %>% 
   filter(SnapshotDateTime < end_pretrial,
          between(SnapshotDateTime, as.Date("2014-07-11"), as.Date("2015-06-30")),
          ChargeCategory == "FELONY") %>% 
   mutate(age_group = Hmisc::cut2(CurrentAge, cuts = seq(0, 100, 10))) %>% 
   distinct(BookingNumber, BookingDate, .keep_all = TRUE) %>%
   select(Gender, RaceDescription, MaritalStatus, age_group, SnapshotDateTime)


lmdc_1415_fel_count <- lmdc_14_15_fel %>% 
   select(Gender, Race = RaceDescription, `Marital Status` = MaritalStatus, Age = age_group) %>% 
   group_by(Gender, Race, `Marital Status`, Age) %>% 
   gather(key = "groupname", value = "rowname") %>% 
   mutate(groupname = ifelse(groupname == "`Marital Status`", "Marital Status", groupname),
          rowname = str_to_title(rowname),
          rowname = ifelse(is.na(rowname), "Missing", rowname)
   ) %>% 
   count(groupname, rowname) %>% 
   group_by(groupname) %>% 
   mutate(n_1415_fel = n/sum(n)) %>% 
   select(-n) %>% 
   ungroup()



# ===== 2015 - 2016 ====


lmdc_15_16_mis <- lmdc %>%
   filter(BookingNumber %in% bn_with_disp) %>%
   filter(as.Date(BookingDate) >= min(as.Date(SnapshotDateTime))) %>%
   left_join(first_disp_date, by = c("BookingNumber", "BookingDate")) %>%
   mutate(SnapshotDateTime = as.Date(SnapshotDateTime)) %>% 
   filter(SnapshotDateTime < end_pretrial,
          between(SnapshotDateTime, as.Date("2015-07-01"), as.Date("2016-06-29")),
          ChargeCategory == "MISDEMEANOR") %>% 
   mutate(age_group = Hmisc::cut2(CurrentAge, cuts = seq(0, 100, 10))) %>% 
   distinct(BookingNumber, BookingDate, .keep_all = TRUE) %>%
   select(Gender, RaceDescription, MaritalStatus, age_group)


lmdc_1516_mis_count <- lmdc_15_16_mis %>% 
   select(Gender, Race = RaceDescription, `Marital Status` = MaritalStatus, Age = age_group) %>% 
   group_by(Gender, Race, `Marital Status`, Age) %>% 
   gather(key = "groupname", value = "rowname") %>% 
   mutate(groupname = ifelse(groupname == "`Marital Status`", "Marital Status", groupname),
          rowname = str_to_title(rowname),
          rowname = ifelse(is.na(rowname), "Missing", rowname)
   ) %>% 
   count(groupname, rowname) %>% 
   group_by(groupname) %>% 
   mutate(n_1516_mis = n/sum(n)) %>% 
   select(-n) %>% 
   ungroup()


lmdc_15_16_fel <- lmdc %>%
   filter(BookingNumber %in% bn_with_disp) %>% 
   filter(as.Date(BookingDate) >= min(as.Date(SnapshotDateTime))) %>%
   left_join(first_disp_date, by = c("BookingNumber", "BookingDate")) %>%
   mutate(SnapshotDateTime = as.Date(SnapshotDateTime)) %>% 
   filter(SnapshotDateTime < end_pretrial,
          between(SnapshotDateTime, as.Date("2015-07-01"), as.Date("2016-06-29")),
          ChargeCategory == "FELONY") %>% 
   mutate(age_group = Hmisc::cut2(CurrentAge, cuts = seq(0, 100, 10))) %>% 
   distinct(BookingNumber, BookingDate, .keep_all = TRUE) %>%
   select(Gender, RaceDescription, MaritalStatus, age_group)


lmdc_1516_fel_count <- lmdc_15_16_fel %>% 
   select(Gender, Race = RaceDescription, `Marital Status` = MaritalStatus, Age = age_group) %>% 
   group_by(Gender, Race, `Marital Status`, Age) %>% 
   gather(key = "groupname", value = "rowname") %>% 
   mutate(groupname = ifelse(groupname == "`Marital Status`", "Marital Status", groupname),
          rowname = str_to_title(rowname),
          rowname = ifelse(is.na(rowname), "Missing", rowname)
   ) %>% 
   count(groupname, rowname) %>% 
   group_by(groupname) %>% 
   mutate(n_1516_fel = n/sum(n)) %>% 
   select(-n) %>% 
   ungroup()



# ==== form voltron ====


df_list <- list(lmdc_1516_fel_count, lmdc_1415_mis_count, lmdc_1415_fel_count, lmdc_1516_mis_count)

lmdc_ididit <- reduce(df_list, full_join) %>%
   replace(., is.na(.), 0)




fiscal_14_to_16_tab <- gt(lmdc_ididit %>% group_by(groupname)) %>% 
   fmt_missing(
      columns = everything(),
      missing_text = "0"
   ) %>%
   fmt_percent(
      columns = 3:6,
      decimals = 2
   ) %>% 
   cols_merge(
      columns = vars(n_1516_fel, n_1516_mis),
      pattern = "{1}<br>({2})"
   ) %>% 
   cols_merge(
      columns = vars(n_1415_fel, n_1415_mis),
      pattern = "{1}<br>({2})"
   ) %>%
   cols_move_to_start(
      columns = vars(n_1415_fel)
   ) %>% 
   cols_label(
      n_1516_fel = md("July 1, 2015     
                                  to    
                              June 29, 2016"),
      n_1415_fel = md("July 11, 2014    
                              to    
                            June 30, 2015")) %>% 
   cols_align(align = "center",
              columns = TRUE) %>% 
   data_color(
      columns = 3:6,
      colors = scales::col_numeric(
         palette =  pals::ocean.tempo(100),
         domain = c(0, 1.00),
         na.color = "#ffffff")) %>%
   tab_footnote(
      footnote = "Labels are of the form, [lower, upper). Left endpoints are inclusive while right endpoints are not, except for the last interval, [lower,upper]",
      locations =cells_row_groups(groups = "Age")) %>%
   tab_footnote(
      footnote = "Upper value represents percentage of inmates with felonies while the bottom parenthesized value for misdemeanors",
      locations = cells_title(groups = "title")) %>% 
   tab_footnote(
      footnote = "Calculations were made for only those inmates with booking dates at or after the start date of the database, July 11, 2014.",
      locations = cells_title(groups = "title")) %>% 
   tab_source_note(
      source_note = md(
         "Source: Louisville Metro Government Open Data. [Daily LMDC Population Snapshots.](https://data.louisvilleky.gov/dataset/daily-lmdc-population-snapshots). 2019")) %>% 
   tab_header(
      title = "Characteristics of pretrial inmates with felony and misdemeanor charges") %>% 
   tab_options(
      column_labels.font.size = "small",
      table.font.size = "small",
      data_row.padding = px(3))

fiscal_14_to_16_tab
