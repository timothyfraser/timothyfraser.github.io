# testing.R

# Script for testing the functions in functions.R
library(dplyr)
source("docs/cv/functions.R")

# Format all papers
papers <- format_publications(type_filter = "paper")
papers

# Generate complete Publications section
pub_section <- generate_publication_section(section_type = "Publications")
pub_section %>% cat()

# Format specific types
reports <- format_publications(type_filter = "report")

reports
software <- format_publications(type_filter = "software")

software



# Simple card with just content
aside_content("**10** packages and dashboards") %>% cat()

# Card with title and content
aside_content(
  content = "- Emissions/Environmental Policy [~16]\n- Community Resilience [~22]",
  title = "Peer-reviewed Papers: [46]",
  style = "card"
) %>% cat()

# Elegant card with formatted content
aside_content(
  content = "By Topic:\n\n- Topic 1 [~10]\n- Topic 2 [~15]",
  title = "Publications",
  style = "card-elegant"
) %>% cat()

# Modern style with icon
aside_content(
  content = "Various research projects and collaborations.",
  title = "Research",
  style = "modern",
  icon = "fa-flask"
) %>% cat()
