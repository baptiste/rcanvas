
#' @export
#' @importFrom parsermd parse_rmd as_tibble
get_markdown_structure <- function(f='dummy_modules.md'){
  rmd = parsermd::parse_rmd(f)
  header <- rmd[[1]]
  content <- rmd[-1]
  tb <- parsermd::as_tibble(content)
  tb

}

#' @export
push_structure <- function(tb, course_id){

  for (i in 1:nrow(tb)){
    ord <- i
    nmodules <- length(unique(tb$sec_h1))
    module_order <- 1
    if (is.na(tb[i,]$sec_h2)){ # top-level module line
      module_name <- tb[i,]$sec_h1
      message("create module: ", module_name)
      rcanvas::create_module(course_id, module_name, position=module_order)
      module_order <- module_order + 1
      ms <- rcanvas::list_modules(course_id)
      module_id <- ms$id[ms$name == module_name][1] # get first if multiple
      next
    }
    if (is.na(tb[i,]$sec_h3)){ # top-level section line
      create_heading_section(tb[i,]$sec_h2, module_id, course_id, position=ord)
      next
    }
    if (tb[i,]$type == "rmd_heading"){ # page title

      if(i < nrow(tb) && tb[i+1,]$type == "rmd_markdown"){ # grab body
        pagetitle <- tb[i,]$sec_h3
        body <- commonmark::markdown_html(glue_collapse(unclass(tb[i+1,]$ast)[[1]]))
        create_dummy_page(glue("{module_name} {pagetitle}"), module_id, course_id,
                          body=body, position=ord)
      } else {
        create_dummy_page(tb[i,]$sec_h3, module_id, course_id, position=ord)
      }
    }
  }

}

#' @export
create_heading_section <- function(title, module_id, course_id, position){
  message("create section: ", title)
  create_module_item(course_id, module_id, title = title,
                     type='SubHeader',
                     indent = 1, position=position)

}


#' @export
create_dummy_page <- function(title, module_id, course_id, body=NULL, position=1){

  if(is.null(body)){
    message("create page from title only: ", title)
    create_wpage(course_id, title = title, body="")
  } else {
    message("create page from title and body: ", title)
    create_wpage(course_id, title = title, body=body)
  }

  page_url <- tolower(gsub(" ","-",title))
  create_module_item(course_id, module_id, title = title,
                     type='Page',
                     indent = 2, position = position, page_url=page_url)
}
