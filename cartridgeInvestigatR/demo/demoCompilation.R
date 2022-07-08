library(parallaxr)

all_md_st <- list.files("demo/Markdown/",full.names = TRUE)

md_tibble <- all_md_st %>%
  purrr::map_dfr(parse_md)

generate_scroll_doc(path = "demo/demo.html",
                    inputs = md_tibble)
