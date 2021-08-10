# Drug-Traffic association analysis

# by Eric Book

# Source: Louisville Metro Government Open Data. [Daily LMDC Population Snapshots.](https://data.louisvilleky.gov/dataset/daily-lmdc-population-snapshots). 2019

# Associations between Traffic and Drug charges are measured by a Cramer's V calculation. Cramer stats with p-values < 0.05 and the charge pairs linked to those stats are extracted. The top 20 Cramer stats and charge pair are broken down by race and tabulated.


# Sections:
# 1. Set-up
# 2. Prepare Data
# 3. Get the Cramer Values
# 4. Top Association Values
# 5. Race proportions for top values
# 6. Odds Ratios




#@@@@@@@@@@@@@@
# Set-up ----
#@@@@@@@@@@@@@@


library(kableExtra)
library(tidyverse)
library(furrr)
library(progressr)

plan(multisession, workers = 6)


types <- c("iccTicccccccccT")
lmdc <- read_csv(here::here("data", "LMDC_Population_Snapshots.csv"), col_types = types) %>% 
   filter(CurrentAge != 238)




#@@@@@@@@@@@@@@@@@@@@
# Prepare Data ----
#@@@@@@@@@@@@@@@@@@@@


# fixing some traffic charges labelled as DRUGS category and vice versa, plus other various miscategorizings
lmdc_corrected <- lmdc %>%
   mutate(ChargeGroup = ifelse(Charge == "TAMPERING WITH PHYSICAL EVIDENCE", "HINDERING/INTIMIDATING", ChargeGroup),
          ChargeGroup = ifelse(Charge == "COMPLICITY TAMPERING WITH PHYSICAL EVIDENCE", "HINDERING/INTIMIDATING", ChargeGroup),
          ChargeGroup = ifelse(Charge == "TRAFFICKING IN SYNTHETIC DRUGS 1ST OFFENSE- CLASS A MISD", "DRUGS", ChargeGroup),
          ChargeGroup = ifelse(Charge == "POSSESSION OF STOLEN MAIL", "THEFT", ChargeGroup),
          ChargeGroup = ifelse(Charge == "ILLEGAL POSSESSION OF LEGEND DRUG", "DRUGS", ChargeGroup),
          ChargeGroup = ifelse(Charge == "DRUG PARAPHERNALIA-ADVERTISEMENT", "DRUGS", ChargeGroup),
          ChargeGroup = ifelse(Charge == "POSSESSION OF STOLEN MAIL", "GENERAL FELONY", ChargeGroup),
          ChargeGroup = ifelse(Charge == "TRAFFICKING IN FINANCIAL INFORMATION", "GENERAL FELONY", ChargeGroup),
          ChargeGroup = ifelse(Charge == "OPERATING ON SUSPENDED OR REVOKED OPERATORS LICENSE", "TRAFFIC", ChargeGroup),
          ChargeGroup = ifelse(Charge == "FAILURE TO WEAR SEAT BELTS", "TRAFFIC", ChargeGroup),
          ChargeGroup = ifelse(Charge == "LOITERING", "LOITERING", ChargeGroup))

rm(lmdc)


# Make indicator vars from traffic and drug charges
lmdc_td <-  lmdc_corrected %>% 
   select(BookingNumber, BookingDate, ChargeGroup, Charge) %>%
   filter(ChargeGroup == "DRUGS" | ChargeGroup == "TRAFFIC") %>%
   select(-ChargeGroup) %>%
   distinct() %>% 
   group_by(BookingNumber, BookingDate) %>% 
   nest() %>% 
   mutate(data = purrr::map(data, function(df) {
      fastDummies::dummy_cols(df, select_columns = "Charge", remove_selected_columns = TRUE)
   })) %>% 
   unnest(cols = c(data)) %>% 
   ungroup() %>% 
   replace(., is.na(.), 0) %>% 
   rename_with(.fn = ~stringr::str_remove(., "Charge_"), .cols = starts_with("Charge_"))


traffic_charge_names <- lmdc_corrected %>% 
   filter(ChargeGroup == "TRAFFIC") %>% 
   select(Charge) %>% 
   distinct() %>% 
   pull(1)

traffic_charge_vars <- lmdc_td %>% 
   select(traffic_charge_names)


drug_charge_names <- lmdc_corrected %>% 
   filter(ChargeGroup == "DRUGS") %>% 
   select(Charge) %>% 
   distinct() %>% 
   pull(1)

drug_charge_vars <- lmdc_td %>% 
   select(drug_charge_names)




#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Get the Cramer values ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# Yates adds correction for 2x2 matrices to correct for upward bias. We're using table() and calculatiing the chisq with two binary vars, so its appropriate here.
cv <- function(x, y) {
   t <- table(x, y)
   results <- suppressWarnings(chisq.test(t, correct = TRUE))
   chi <- results$statistic
   pvalue <- results$p.value
   cramer_stat <- sqrt(chi / (NROW(x) * (min(dim(t)) - 1)))
   structure(list(cramer = cramer_stat, pval = pvalue))
}

# gets cramer's V and pvals from cv function
get.V <-function(y){
   col.y<-ncol(y)
   V<-matrix(ncol=col.y,nrow=col.y)
   PV<-matrix(ncol=col.y,nrow=col.y)
   for(j in 2:col.y){
      cv_output<-cv(y[,1],y[,j])
      V[1,j]<-cv_output$cramer
      PV[1,j] <- cv_output$pval
   }
   diag(V) <- 1
   return(list(CramersV = V, PValues = PV))
}


# gets correlations between drug charges and traffic charges
cramer_FUN <- function(dc_var, p){
   # see below, needs to be inside the function
   fill_mat <- function(cow){
      for(i in 1:nrow(cow)) {
         x <- cow[i,1]
         y <- cow[i,2]
         analysis_mat[x, y] <- cramer_mat[x, y]
      }
      return(analysis_mat)
   }
   df <- bind_cols(drug_charge_vars[dc_var], traffic_charge_vars) %>% 
      as.data.frame()
   # gets cramers values and pvals
   mat <- get.V(df)
   # cramer values
   cramer_mat <- mat[[1]]
   # gets matrix locations of values with signif pvals
   signif_pvals_indices <- as.data.frame(which(mat[[2]] < 0.05, arr.ind=TRUE))
   # empty matrix
   analysis_mat <- matrix(nrow = ncol(df), ncol = ncol(df))
   # fills matrix with cramer values that have signif pvals
   other_mat <- fill_mat(signif_pvals_indices)
   df_names <- colnames(df)
   # selecting drug charge that we're looping and deselecting it's identity column
   other_df <- data.frame(var = other_mat[1, -1])
   colnames(other_df) <- df_names[1]
   p()
   return(other_df)
}


drug_charge_names <- names(drug_charge_vars)

with_progress({
   p <- progressor(steps = length(drug_charge_names))
   corr_analysis <- future_map_dfc(drug_charge_names, cramer_FUN, p = p)
})

gc()

rownames(corr_analysis) <- names(traffic_charge_vars)

write_rds(corr_analysis, "results/drugs-traffic-assoc/traf-drug-corr-mat-yates.rds")




#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Top association values ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# corr_analysis <- read_rds("models/objects/traf-drug-corr-mat-yates.rds")

# get 20 largest cramer vals and their matrix coordinates
nlargest <- function(m, n, sim = FALSE) {
   mult <- 1;
   if (sim) mult <- 2;
   res <- order(m, decreasing = TRUE)[seq_len(n) * mult];
   pos <- arrayInd(res, dim(m), useNames = TRUE);
   list(values = m[res],
        position = pos)
}


top_corr_vals <- nlargest(as.matrix(corr_analysis), 20)

# added one to col indexes b/c of rownames_to_column below
# sorting to keep indexing similar to origiaal corr_analysis matrix
top_corr_cols <- sort(unique(top_corr_vals$position[,2] + 1))
top_corr_rows <- sort(unique(top_corr_vals$position[,1]))


# An extra value (line 21) got picked up that's not supposed to be there
top_assoc_df <- corr_analysis %>% 
   rownames_to_column(var = "Traffic Charge") %>% 
   select(`Traffic Charge`, all_of(top_corr_cols)) %>% 
   slice(top_corr_rows) %>% 
   gather(key = "Drug Charge", "Cramer's V", -`Traffic Charge`, na.rm = TRUE) %>% 
   mutate(`Cramer's V` = round(`Cramer's V`, 5)) %>% 
   arrange((desc(`Cramer's V`))) %>% 
   slice(-21) %>% 
   mutate(ID = row_number())

knitr::kable(top_assoc_df %>% select(-ID),
             format = "html",
             caption = "Top 20 association values for drugs-traffic charge pairs") %>%
   kableExtra::kable_styling(bootstrap_options = c("hover", "condensed"), full_width = F, position = "left", fixed_thead = T) %>% 
   kableExtra::row_spec(1:20, color = "black")


write_rds(top_assoc_df, "results/drugs-traffic-assoc/drugs-traffic-top-cramer-vals.rds")


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Race proportions for top values ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# top_assoc_df <- read_rds("models/objects/drugs-traffic-top-cramer-vals.rds")

race_comp_FUN <- function(x) {
   row_charges <- top_assoc_df %>% 
      select(`Traffic Charge`, `Drug Charge`) %>% 
      slice(x) %>% 
      unlist(., use.names=FALSE)
   
   race_comp <- lmdc_corrected %>% 
      select(BookingNumber, BookingDate, RaceDescription, Charge) %>% 
      filter(Charge %in% row_charges) %>%
      distinct(BookingNumber, BookingDate, RaceDescription) %>%
      count(RaceDescription) %>% 
      mutate(proportion = round(n/sum(n), 2),
             ID = x)
   
   return(race_comp)
}


race_comp_df <- map_dfr(1:20, race_comp_FUN) %>% 
   left_join(top_assoc_df, by = "ID") %>%
   mutate(`Traffic Charge` =  str_to_sentence(`Traffic Charge`),
          `Drug Charge` = str_to_sentence(`Drug Charge`)) %>% 
   select(`Traffic Charge`, `Drug Charge`, `Cramer's V`, Race = RaceDescription, N = n, Proportion = proportion)

write_rds(race_comp_df, "results/drugs-traffic-assoc/drugs-traffic-assoc-by-race.rds")


knitr::kable(race_comp_df,
             format = "html",
             caption = "Drugs-traffic charge associations broken down by race") %>%
   kable_styling(bootstrap_options = c("hover", "condensed"), full_width = F, position = "left", fixed_thead = T) %>% 
   row_spec(1:40, color = "black") %>% 
   collapse_rows(columns = 1:3, valign = "top")




#@@@@@@@@@@@@@@@@@@@
# Odds Ratios ----
#@@@@@@@@@@@@@@@@@@@


# Goal is to transform data into a Frequency table format for xtabs()
# ChargeGroup has quite a few NAs. Charge: possession of a controlled substance had NAs in ChargeGroup. I converted it to "DRUGS." I suspect some of the Charge: Wanton endangerments are drunk driving or other TRAFFIC charges but I left those unchanged.
ll_dat <- lmdc_corrected %>% 
   select(BookingNumber, BookingDate, RaceDescription, ChargeGroup, Charge) %>%
   mutate(ChargeGroup = if_else(is.na(ChargeGroup) & str_detect(Charge, "POSS CONT SUB"), "DRUGS", ChargeGroup),
          ChargeGroup = if_else(ChargeGroup == "DRUGS" | ChargeGroup == "TRAFFIC", ChargeGroup, "Other")) %>% 
   filter(RaceDescription == "Black-origins of Africa" | RaceDescription == "White/Eurp/ N.Afr/Mid Eas") %>% 
   distinct() %>% 
   select(-Charge) %>% 
   filter(!is.na(ChargeGroup)) %>% 
   group_by(BookingNumber, BookingDate, RaceDescription) %>% 
   count(ChargeGroup) %>% 
   mutate(n = ifelse(n > 0, 1, 0)) %>% 
   pivot_wider(names_from = "ChargeGroup", values_from = "n") %>% 
   mutate_at(.vars = c("DRUGS", "TRAFFIC", "Other"), ~ifelse(is.na(.x), "No", "Yes")) %>%
   group_by(RaceDescription,TRAFFIC, DRUGS, Other) %>%
   summarize(Freq = n()) %>% 
   rename(Race = RaceDescription, Traffic = TRAFFIC, Drugs = DRUGS)


# Traffic as the given variable
# Rev flips the columns in the partial tables so Drugs = 1 is the reference column
ll_tab <- xtabs(Freq ~ Race + Drugs + Traffic, data = ll_dat) %>% 
   DescTools::Rev(., margin = 2)
write_rds(ll_tab, "results/drugs-traffic-assoc/log-linear-xtab.rds")

# Drugs as the given variable
ll_tab2 <- xtabs(Freq ~ Race + Traffic + Drugs, data = ll_dat) %>% 
   DescTools::Rev(., margin = 2)
write_rds(ll_tab2, "results/drugs-traffic-assoc/log-linear-xtab2.rds")

# Tree map
fob <- ftable(ll_tab)
# fob
# memisc::show_html(memisc::format_html(fob))

library(swatches)
library(extrafont)

intrique <- read_palette("palettes/Intrigue.ase", use_names = FALSE)

fill_colors <- t(matrix(c(rep(intrique[4], 4), rep(intrique[1], 4)), nrow=4))


vcd::strucplot(ll_tab, gp = grid::gpar(fill = fill_colors, fontfamily = "roboto"))


# log-linear model selection

ind_mod <- MASS::loglm(~ Drugs + Traffic + Race, data = ll_tab)
ind_mod
# Independent model rejected, so there's an association present


sat_mod <- MASS::loglm(~ Drugs * Traffic * Race, data = ll_tab)
sat_mod
# model is perfect so overdispersion isn't an issue
plot(sat_mod)

# Race will be the ostensible "response" which means we can narrow our model pool to: (DT,R), (DT, DR), (DT, TR), (DT, DR, TR), and (DTR)

joint_ind <- MASS::loglm(~ Drugs + Traffic + Race + Drugs*Traffic, data = ll_tab)
# pval < 0.05, so model rejected
joint_ind

cond_dt_dr <- MASS::loglm(~ Drugs + Traffic + Race + Drugs*Traffic + Drugs*Race, data = ll_tab)
# rejected
cond_dt_dr

cond_dt_tr <- MASS::loglm(~ Drugs + Traffic + Race + Drugs*Traffic + Traffic*Race, data = ll_tab)
# rejected
cond_dt_tr

homog_mod <- MASS::loglm(~ Drugs + Traffic + Race + Drugs*Traffic + Drugs*Race + Traffic*Race, data = ll_tab)
# rejected, have to use the saturated model
homog_mod

# Says, given that the inmate has a traffic charge, a black inmate is on average 1.0239294 more likely than a white inmate to have a drug charge.
dtr_or <- vcd::oddsratio(ll_tab, log = FALSE)
dtr_or

# Shows the OR above includes 1, which means that there isn't enough evidence to be confident there's an effect after all.
dtr_or_ci <- confint(dtr_or)
dtr_or_ci


plot(dtr_or, ylim = c(0.6, 1.5), main = "Odds ratios for Black inmates vs White inmates having a drug charge")



