# For Question 1
# 2015 demographic breakdown of pre-trial inmates with felony and misdemeanor charges

# by Eric Book

# Source: Louisville Metro Government Open Data. [Daily LMDC Population Snapshots.](https://data.louisvilleky.gov/dataset/daily-lmdc-population-snapshots). 2019


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


lmdc_15_mis <- lmdc %>%
   filter(BookingNumber %in% bn_with_disp) %>% 
   filter(as.Date(BookingDate) >= min(as.Date(SnapshotDateTime))) %>%
   left_join(first_disp_date, by = c("BookingNumber", "BookingDate")) %>% 
   filter(as.Date(SnapshotDateTime) < end_pretrial) %>% 
   mutate(month = month(SnapshotDateTime, label = TRUE, abbr = FALSE),
          year = year(SnapshotDateTime),
          age_group = Hmisc::cut2(CurrentAge, cuts = seq(0, 100, 10)),
          month = str_replace(month, "$", "_mis")) %>% 
   filter(year == "2015",
          ChargeCategory == "MISDEMEANOR") %>%
   distinct(BookingNumber, BookingDate, .keep_all = TRUE) %>% 
   select(Gender, RaceDescription, MaritalStatus, age_group, month)


lmdc_15_fel <- lmdc %>%
   filter(BookingNumber %in% bn_with_disp) %>% 
   filter(as.Date(BookingDate) >= min(as.Date(SnapshotDateTime))) %>%
   left_join(first_disp_date, by = c("BookingNumber", "BookingDate")) %>% 
   filter(as.Date(SnapshotDateTime) < end_pretrial) %>% 
   mutate(month = month(SnapshotDateTime, label = TRUE, abbr = FALSE),
          year = year(SnapshotDateTime),
          age_group = Hmisc::cut2(CurrentAge, cuts = seq(0, 100, 10)),
          month = str_replace(month, "$", "_fel")) %>% 
   filter(year == "2015",
          ChargeCategory == "FELONY") %>%
   distinct(BookingNumber, BookingDate, .keep_all = TRUE) %>% 
   select(Gender, RaceDescription, MaritalStatus, age_group, month)


lmdc_ididit_mis <- lmdc_15_mis %>% 
   select(month, Gender, Race = RaceDescription, `Marital Status` = MaritalStatus, Age = age_group) %>% 
   group_by(month, Gender, Race, `Marital Status`, Age) %>% 
   gather(key = "groupname", value = "rowname", -month) %>% 
   mutate(groupname = ifelse(groupname == "`Marital Status`", "Marital Status", groupname),
          rowname = str_to_title(rowname),
          rowname = ifelse(is.na(rowname), "Missing", rowname)
          ) %>% 
   count(month, groupname, rowname) %>%
   group_by(month, groupname) %>% 
   mutate(n = n/sum(n)) %>%
   spread(month, n) %>% 
   ungroup()


lmdc_ididit_fel <- lmdc_15_fel %>% 
   select(month, Gender, Race = RaceDescription, `Marital Status` = MaritalStatus, Age = age_group) %>% 
   group_by(month, Gender, Race, `Marital Status`, Age) %>% 
   gather(key = "groupname", value = "rowname", -month) %>% 
   mutate(groupname = ifelse(groupname == "`Marital Status`", "Marital Status", groupname),
          rowname = str_to_title(rowname),
          rowname = ifelse(is.na(rowname), "Missing", rowname)
          ) %>% 
   count(month, groupname, rowname) %>% 
   group_by(month, groupname) %>% 
   mutate(n = n/sum(n)) %>%
   spread(month, n) %>% 
   ungroup()


# group_by, spread, and join automatically order the months alphabetically and it can't be stopped,so re-ordering by select statement is necessary
lmdc_ididit <- lmdc_ididit_mis %>% 
   full_join(lmdc_ididit_fel, by = c("groupname", "rowname")) %>%
   replace(., is.na(.), 0) %>% 
   select(groupname, rowname, January_fel, February_fel, March_fel, April_fel, May_fel, June_fel, July_fel, August_fel, September_fel, October_fel, November_fel, December_fel, January_mis, February_mis, March_mis, April_mis, May_mis, June_mis, July_mis, August_mis, September_mis, October_mis, November_mis, December_mis)



prior_15_tab <- gt(lmdc_ididit %>% group_by(groupname)) %>% 
   fmt_missing(
      columns = everything(),
      missing_text = "0"
   ) %>%
   fmt_percent(
      columns = 3:26,
      decimals = 1
   ) %>%
   cols_merge(
      columns = vars(January_fel, January_mis),
      pattern = "{1}<br>({2})"
   ) %>% 
   cols_merge(
      columns = vars(February_fel, February_mis),
      pattern = "{1}<br>({2})"
   ) %>% 
   cols_merge(
      columns = vars(March_fel, March_mis),
      pattern = "{1}<br>({2})"
   ) %>% 
   cols_merge(
      columns = vars(April_fel, April_mis),
      pattern = "{1}<br>({2})"
   ) %>% 
   cols_merge(
      columns = vars(May_fel, May_mis),
      pattern = "{1}<br>({2})"
   ) %>% 
   cols_merge(
      columns = vars(June_fel, June_mis),
      pattern = "{1}<br>({2})"
   ) %>% 
   cols_merge(
      columns = vars(July_fel, July_mis),
      pattern = "{1}<br>({2})"
   ) %>% 
   cols_merge(
      columns = vars(August_fel, August_mis),
      pattern = "{1}<br>({2})"
   ) %>% 
   cols_merge(
      columns = vars(September_fel, September_mis),
      pattern = "{1}<br>({2})"
   ) %>% 
   cols_merge(
      columns = vars(October_fel, October_mis),
      pattern = "{1}<br>({2})"
   ) %>% 
   cols_merge(
      columns = vars(November_fel, November_mis),
      pattern = "{1}<br>({2})"
   ) %>% 
   cols_merge(
      columns = vars(December_fel, December_mis),
      pattern = "{1}<br>({2})"
   ) %>% 
   cols_label(
      January_fel = "January",
      February_fel = "February",
      March_fel = "March",
      April_fel = "April",
      May_fel = "May",
      June_fel = "June",
      July_fel = "July",
      August_fel = "August",
      September_fel = "September",
      October_fel = "October",
      November_fel = "November",
      December_fel = "December"
   ) %>%
   data_color(
      columns = 3:26,
      colors = scales::col_numeric(
         palette =  pals::ocean.tempo
(100),
         domain = c(0, 1.00),
         na.color = "#ffffff"   
      )
   ) %>%
   tab_footnote(
      footnote = "Labels are of the form, [lower, upper). Left endpoints are inclusive while right endpoints are not, except for the last interval, [lower,upper]",
      locations = cells_row_groups(groups = "Age")
   ) %>%
   tab_footnote(
      footnote = "Upper value represents monthly percentage for inmates with felonies while the bottom parenthesized value for misdemeanors",
      locations = cells_title(groups = "title")
   ) %>% 
   tab_footnote(
      footnote = "Calculations were made for only those inmates with booking dates at or after the start date of the database, July 11, 2014.",
      locations = cells_title(groups = "title")
      ) %>% 
   tab_source_note(
      source_note = md(
         "Source: Louisville Metro Government Open Data. [Daily LMDC Population Snapshots.](https://data.louisvilleky.gov/dataset/daily-lmdc-population-snapshots). 2019"
      )
   ) %>% 
   tab_header(
      title = "Characteristics of pretrial inmates with felony and misdemeanor charges",
      subtitle = "2015"
   ) %>% 
   tab_options(
      column_labels.font.size = "small",
      table.font.size = "small",
      data_row.padding = px(3)
   )

prior_15_tab


