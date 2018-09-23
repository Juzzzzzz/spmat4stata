library(spmat4stata)

data_long <- read_csv("./demo/queen_long.csv")

mm <- long2mat(data_long = data_long, country_id = "Country ISO3 Code",
               counter_id = "Counterpart Country ISO3 Code", contiguity = "Contiguity",
               id_is_int = FALSE, W_style = "W")
