read = read_delim(here::here("data","bank-additional-full.csv"), delim=";")
save = saveRDS(read, file=here::here("data","dataset.rds"))
