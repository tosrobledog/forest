library(tidyverse)

# Delete NA values

referencelink.complete <- referencelink %>% na.omit()

# Select the right columns

referencelink.complete.right <- 
  referencelink.complete %>%
  filter(str_detect(id_paper1, "\\[[:alpha:]]*,[[:digit:]]*"))
  
# Extract the rows that are differen

diff_rows <- referencelink.complete %>% 
  anti_join(paper, by = c("id_paper1" = "id")) %>%
  separate(id_paper1,
           into = c("id_author", 
                    "year_published", 
                    "source_abbreviation_29_character", 
                    "volume", 
                    "beginning_page", 
                    "doi"),
           sep = ",",
           remove = FALSE)

diff_rows$year_published <- numeric(diff_rows$year_published)

paper_ref <- bind_rows(paper, diff_rows)


