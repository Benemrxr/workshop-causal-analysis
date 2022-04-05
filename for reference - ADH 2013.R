library(tidyverse)
library(bartik.weight)

######### Only for reference / Adjust paths before running code

### Replication example ADH: Autor, Dorn, Hanson: The China Syndrome (2013)
# URL: https://www.aeaweb.org/articles?id=10.1257/aer.103.6.2121

load(file='bartik-weight/R-code/pkg/data/ADH_master.rda')
load(file='bartik-weight/R-code/pkg/data/ADH_local.rda')
load(file='bartik-weight/R-code/pkg/data/ADH_global.rda')

# To estimate the Rotemberg weights, it's necessary to transform the local tibble from long to wide format:
ADH_local %>%
  mutate(ind = str_glue("t{year}_sh_ind_{ind}")) %>%
  spread(ind, sh_ind_, fill = 0) %>%
  print() -> ADH_local2

# Once all the data are in proper format, the bw() function will return the weight, and the just-identified IV estimates:
# Prepare variables in the master tibble
y = "d_sh_empl_mfg"
x = "d_tradeusch_pw"
controls = c("reg_midatl", "reg_encen", "reg_wncen", "reg_satl",
             "reg_escen", "reg_wscen", "reg_mount", "reg_pacif", "l_sh_popedu_c",
             "l_sh_popfborn", "l_sh_empl_f", "l_sh_routine33", "l_task_outsource",
             "t2", "l_shind_manuf_cbp")
weight = "timepwt48"

# Prepare variables in the local tibble
Z = setdiff(names(ADH_local_wide), c("czone", "year"))

# Prepare variables in the global tibble
G = "trade_"

# Estimate the weight (alpha) and the IV estimates (beta)
bw = bw(ADH_master, y, x, controls, weight, ADH_local2, Z, ADH_global, G)
bw

bw %>%
  top_n(5, alpha) %>%
  arrange(desc(alpha)) %>%
  mutate(ind = case_when(
    ind == "3571" ~ "Electronic Computers",
    ind == "3944" ~ "Games and Toys",
    ind == "3651" ~ "Household Audio and Video",
    ind == "3661" ~ "Telephone Apparatus",
    ind == "3577" ~ "Computer Equipment"
  )) %>%
  rename(g = trade_) %>%
  knitr::kable(digits = 3, caption = "Top five Rotemberg weight industries")
