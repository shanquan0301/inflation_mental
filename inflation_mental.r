#package------------
library(lubridate)
library(magrittr)
library(DataDescription)
library(ggsci)
library(AFR)
library(vars)
library(lmtest)
library(tidyverse)
#dat_reg--------------------
mdat <- dat_appointment %>% group_by(year, month) %>% summarise(
  num = sum(COUNT_OF_APPOINTMENTS)/10000000
)
mdat <- full_join(mdat, dat_price_index, by = c("year", "month"))
mdat <- full_join(mdat, readxl::read_excel("price_index.xlsx", sheet = "death"),
                  by = c("year", "month"))
mdat <- full_join(mdat, readxl::read_excel("price_index.xlsx", sheet = "mental"),
                  by = c("year", "month"))

mdat <- mdat %>% mutate(
  child = child/1000,
  adults = adults/1000,
  elder = elder/1000
)
mdat <- mdat %>% arrange(year, month)


outcome <- c("child", "adults", "elder")
cova <- names(mdat)[c(5, 8:23)]
dat_reg <- mdat %>% filter(date >= as.Date("2022-04-01") | date < as.Date("2020-03-01"))
dat_reg <- dat_reg %>% mutate(
  treat = year >= 2022
)

#table 1---------------
for(outc in outcome){
  print(outc)
  for(cov in cova) {
    print(str_glue("--{cov}"))
    eval(parse(text = str_glue("
          res_order <- dat_reg %>% filter(!is.na(`{cov}`)) %$% 
                        VARselect(`{cov}`, lag.max = 4, type='const')
                               ")))
    
    res_order <- min(res_order$selection) - 1
    if(res_order == 0){
      eval(parse(text = str_glue("reg <- lm({outc} ~ `{cov}`*treat + factor(month) + factor(year) + unem, data = dat_reg)")))
    }
    
    if(res_order == 1){
      eval(parse(text = str_glue("reg <- lm({outc} ~ `{cov}`*treat + lag(`{cov}`, 1)*treat + factor(month) + factor(year) + unem, data = dat_reg)")))
    }
    
    if(res_order == 2){
      eval(parse(text = str_glue("reg <- lm({outc} ~ `{cov}`*treat + lag(`{cov}`, 1)*treat + lag(`{cov}`, 2)*treat + factor(month) + factor(year) + unem, data = dat_reg)")))
    }
    
    if(res_order == 3){
      eval(parse(text = str_glue("reg <- lm({outc} ~ `{cov}`*treat + lag(`{cov}`, 1)*treat + lag(`{cov}`, 2)*treat + lag(`{cov}`, 3)*treat + factor(month) + factor(year) + unem, data = dat_reg)")))
    }
    
    res <- reg_comb(reg = reg,
                    round_ci = 2,
                    round_p = 3,
                    p_value = "`Pr(>|t|)`",
                    comb_ci = "coef[ci_low, ci_high], p_value")
    res$out <- outc
    res$treat <- cov
    res <- res %>% filter(str_detect(variable, "treat")) %>% 
      select(out, treat, comb_ci, p_value, star)
    
    
    if(res_order %in% c(0)){
      res <- res[2, c(1, 2, 3, 5)]
    }
    
    if(res_order == 1){
      res <- cbind(res[2, c(1, 2, 3, 5)], 
                   res[3, c(3, 5)])
      names(res) <- c("out", "treat", "comb_ci", "star", "lag_1", "lag_1_star")
    }
    
    if(res_order == 2){
      res <- cbind(res[2, c(1, 2, 3, 5)], 
                   res[3, c(3, 5)], 
                   res[4, c(3, 5)])
      names(res) <- c("out", "treat", "comb_ci", "star", "lag_1", "lag_1_star", "lag_2", "lag_2_star")
    }
    
    if(res_order == 3){
      res <- cbind(res[2, c(1, 2, 3, 5)], 
                   res[3, c(3, 5)], 
                   res[4, c(3, 5)],
                   res[5, c(3, 5)])
      names(res) <- c("out", "treat", "comb_ci", "star", "lag_1", "lag_1_star", "lag_2", "lag_2_star", 
                      "lag_3", "lag_3_star")
    }
    
    if(outc == outcome[1] & cov == cova[1]){
      res_f <- res
    } else {
      res_f <- bind_rows(res_f, res)
    }
  }
}

#sup tabe 1-----------
for(i in c(outcome, cova, "unem")){
  mdat <- dat_reg %>% select(i, treat) %>% ungroup()
  mdat <- mdat %>% group_by(treat) %>% mutate(id = 1:n())
  eval(parse(text = str_glue("reg_1 <- lm(`{i}`~ id, data = mdat %>% filter(treat == FALSE))")))
  res_1 <- reg_comb(reg = reg_1,
                    round_ci = 2,
                    round_p = 3,
                    p_value = "`Pr(>|t|)`",
                    comb_ci = "coef[ci_low, ci_high], p_value")
  
  eval(parse(text = str_glue("reg_2 <- lm(`{i}`~ id, data = mdat %>% filter(treat == TRUE))")))
  res_2 <- reg_comb(reg = reg_2,
                    round_ci = 2,
                    round_p = 3,
                    p_value = "`Pr(>|t|)`",
                    comb_ci = "coef[ci_low, ci_high], p_value")
  
  res <- data.frame(var = i,
                    fasle = res_1$comb_ci[2], true = res_2$comb_ci[2])
  res
  if (i == outcome[1]){res_f <- res}
  if (i != outcome[1]){res_f <- bind_rows(res_f, res)}
}

#sub table 2, with lags-------------------------
for(outc in outcome){
  print(outc)
  for(cov in cova) {
    print(str_glue("--{cov}"))
    eval(parse(text = str_glue("
          res_order <- dat_reg %>% filter(!is.na(`{cov}`)) %$% 
      VARselect(`{cov}`, lag.max = 4, type='const')
                               ")))
    
    res_order <- min(res_order$selection) - 1
    
    if(res_order == 0){
      eval(parse(text = str_glue("reg <- lm({outc} ~ `{cov}`*treat + factor(month) + factor(year) + unem, data = dat_reg)")))
    }
    
    if(res_order == 1){
      eval(parse(text = str_glue("reg <- lm({outc} ~ `{cov}`*treat + lag(`{cov}`, 1)*treat + factor(month) + factor(year) + unem, data = dat_reg)")))
    }
    
    if(res_order == 2){
      eval(parse(text = str_glue("reg <- lm({outc} ~ `{cov}`*treat + lag(`{cov}`, 1)*treat + lag(`{cov}`, 2)*treat + factor(month) + factor(year) + unem, data = dat_reg)")))
    }
    
    if(res_order == 3){
      eval(parse(text = str_glue("reg <- lm({outc} ~ `{cov}`*treat + lag(`{cov}`, 1)*treat + lag(`{cov}`, 2)*treat + lag(`{cov}`, 3)*treat + factor(month) + factor(year) + unem, data = dat_reg)")))
    }
    
    res <- reg_comb(reg = reg,
                    round_ci = 2,
                    round_p = 3,
                    p_value = "`Pr(>|t|)`",
                    comb_ci = "coef[ci_low, ci_high], p_value")
    res$out <- outc
    res$treat <- cov
    res <- res %>% filter(str_detect(variable, str_glue("{cov}"))) %>% 
      select(out, treat, comb_ci, p_value, star)
    
    if(res_order %in% c(0)){
      res <- res[1, c(1, 2, 3, 5)]
    }
    
    if(res_order == 1){
      res <- cbind(res[1, c(1, 2, 3, 5)], 
                   res[2, c(3, 5)])
      names(res) <- c("out", "treat", "comb_ci", "star", "lag_1", "lag_1_star")
    }
    
    if(res_order == 2){
      res <- cbind(res[1, c(1, 2, 3, 5)], 
                   res[2, c(3, 5)], 
                   res[3, c(3, 5)])
      names(res) <- c("out", "treat", "comb_ci", "star", "lag_1", "lag_1_star", "lag_2", "lag_2_star")
    }
    
    if(res_order == 3){
      res <- cbind(res[1, c(1, 2, 3, 5)], 
                   res[2, c(3, 5)], 
                   res[3, c(3, 5)],
                   res[4, c(3, 5)])
      names(res) <- c("out", "treat", "comb_ci", "star", "lag_1", "lag_1_star", "lag_2", "lag_2_star", 
                      "lag_3", "lag_3_star")
    }
    
    
    if(outc == outcome[1] & cov == cova[1]){
      res_f <- res
    } else {
      res_f <- bind_rows(res_f, res)
    }
  }
}

#sup figure 1-------------
mdat <- dat_reg %>% select(date, CPIH:unem, treat, child, adults, elder) %>% 
  select(-c(CPI, OOH, rent, `CPIH excl energy, food, alcohol & tobacco`, Goods, Services))
dat_plot <- mdat %>% gather(key = "cat", value = "value", -c(year, date, treat)) %>% ungroup()
dat_plot <- dat_plot %>% mutate(
  cat = factor(cat, 
               levels = c("CPIH", "Food and non-alcoholic beverages", 
                          "ALCOHOLIC BEVERAGES,TOBACCO & NARCOTICS-est pre-97", "Clothing and footwear", 
                          "HOUSING, WATER AND FUELS", "FURN, HH EQUIP & REPAIR OF THE HOUSE -", 
                          "health", "Transport", "communication", "Recreation and culture", "education",
                          "HOTELS, CAFES AND RESTAURANTS", "MISCELLANEOUS GOODS AND SERVICES",
                          "unem", "child", "adults", "elder"),
               labels = c("CPIH", "Food and non-alcoholic beverages", 
                          "Alcoholic beverages, tobacco &\nnarcotics", "Clothing and footwear", 
                          "Housing, water and fuels", 
                          "Furniture, household equip &\nrepair of the house",
                          "Health", "Transport", "Communication", "Recreation and culture", "Education", 
                          "Hotels, cafes and restaurants", "Miscellaneous goods and services",
                          "Unemployment rate",
                          "Number of children in contact \nwith mental health services",
                          "Number of adults in contact \nwith mental health services",
                          "Number of elderly population in contact \nwith mental health services"))
)

dat_plot <- dat_plot %>% mutate(
  treat = factor(treat, levels = c(FALSE, TRUE), labels = c("pre-COVID-19", "post-COVID-19"))
)

#12.76, 6.99
ggplot(data = dat_plot, aes(x = date, y = value, colour = treat)) + 
  geom_point() + 
  geom_smooth(aes(group = treat), method = "lm") + 
  scale_color_manual(values = pal_nejm("default")(8)[c(2, 1)]) + 
  facet_wrap(cat ~ ., scales = "free") + 
  theme_classic() + 
  theme(strip.text = element_text(face = "bold"),
        axis.title = element_blank(),
        legend.position = c(0.8, 0.1),
        legend.title = element_blank())

