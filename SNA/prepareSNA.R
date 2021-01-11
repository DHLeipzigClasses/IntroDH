library(tidyverse)
library(jsonlite)


# generating node and edges tables to use it in gephi / cytoscape ---------

data <- fromJSON("star-wars-network-data-1.0.1/starwars-full-interactions-allCharacters.json")

nodes <- data$nodes %>% 
  as_tibble %>% 
  mutate(id = row_number()) %>% 
  select(id, name, value) %>%
  rename(scene_occ = value,
         label = name)

edges <- data$links %>% 
  as_tibble %>% 
  mutate(source = source + 1,
         target = target + 1) %>%
  rename(weight = value)

write_csv(nodes, "nodes.csv")
write_csv(edges, "edges.csv")

