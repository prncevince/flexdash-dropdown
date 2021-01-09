library(htmltools)
library(magrittr)
library(rvest)
library(xml2)

# html content files ----
dir <- './docs/'
files <- list.files(dir, pattern = "^d.*.html")
paths <- paste0(dir, files)

# html dropdown partial ----
doc_dropdown_node <- read_html('dropdown.html')
dropdown_node <- doc_dropdown_node %>% html_node('li')
for (i in files) {
  f <- strsplit(i, '\\.')[[1]][1]
  a <- as.character(a(f, href = f))
  doc_a_node <- read_html(a)
  a_node <- doc_a_node %>% html_node('a')
  dropdown_node %>% html_node('#dropdown') %>% xml_add_child(a_node)
}

# javascript ----
s <- trimws(
  gsub(
    pattern = '\\/\\*.*?\\*\\/|([^\\:]|^)\\/\\/.*$', replacement = '',
    x = readLines('filter.js', warn = F, skipNul = T)
  )
)
s <- s[s != '']
s <- as.character(tags$script(HTML(paste0(s, collapse = '\n'))))
doc_filter_node <- read_html(s)
filter_node <- doc_filter_node %>% html_node('head script')
xml_attr(filter_node, "id") <- "filter-dropdown"

# css ----
c <- trimws(
  gsub(
    pattern = '\\/\\*.*?\\*\\/|([^\\:]|^)\\/\\/.*$', replacement = '',
    x = readLines('dropdown.css')
  )
)
c <- c[c != '']
c <- as.character(tags$style(HTML(paste0(c, collapse = ''))))
doc_style_node <- read_html(c)
style_node <- doc_style_node %>% html_node('head style')
xml_attr(style_node, "id") <- "style-dropdown"

# insert ----
for (i in paths) {
  doc_node <- read_html(x = i)
  nav_right_nodes <- doc_node %>% html_nodes(css = '.nav.navbar-nav.navbar-right')
  # add js search/filter script
  script_nodes <- doc_node %>% html_nodes(css = 'head script')
  i_filter <- script_nodes %>% xml_attr('id', default = '') %>% `==`("filter-dropdown")
  if (!T %in% i_filter) {
    script_nodes[1] %>% xml_add_sibling(filter_node, .where = 'before')
  }
  # add css
  style_nodes <- doc_node %>% html_nodes(css = 'head style')
  i_style <- style_nodes %>% xml_attr('id', default = '') %>% `==`("style-dropdown")
  if (!T %in% i_style) {
    style_nodes[1] %>% xml_add_sibling(style_node, .where = 'before')
  }
  # add html dropdown
  if (! nav_right_nodes %>% xml_node('li') %>% as.character() %>% is.na()) {
    nav_right_nodes %>% xml_node('.dropdown') %>% xml_remove()
  }
  nav_right_nodes %>% xml_add_child(dropdown_node)
  write_html(x = doc_node, file = i)
}
