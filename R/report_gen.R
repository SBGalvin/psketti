#' Generate an Item Report
#' 
#' report_gen generates a containing folder for an Rmarkdown file, in the root
#'     of the project directory. After report_gen has been run, you can go to
#'     the containing folder and render the Rmarkdown file. Future versions will
#'     allow you to generate the Rmd file and render the html file in one go by
#'     selecting html_gen = TRUE.  However, you will need to have tidyverse,
#'     knitr rmarkdown and kableExtra installed. This function is still
#'     undergoing active development.
#' 
#' @param item.names A charcter list of item names
#' @param eRm.obj dichotomous Rasch Model; an eRm object of class 'RM'.
#' @param K A character object giving the name of the response column in 
#'     data.long
#' @param ID A character object giving the name of the ID column in 
#'     data.long
#' @param Item A character object giving the name of the Item column in 
#'     data.long
#' @param data.long Long formatted data set with a column for Item names
#'     matching item.names, a column for response options, matching r_o, and an
#'     ID column name matching ID.
#' @param r_o An ordered factor object to arrange columns along by response
#'     option order.
#' @param Title the title of the Rmd report.
#' @param ptrn the prefix label of the item namesL eg i01 is "i".
#' @param html_gen Generate a html file from the report? Defaults to FALSE.
#'     Currently not functional.
#'     
#' @return A folder containing materials for report generation
#' @return If html_gen = TRUE, a html report will be rendered
#' 

#' 
#' @export


report_gen <- function(item.names, eRm.obj, data.long, Title, ptrn, ID, Item, K, r_o, html_gen = FALSE){
 
  Title2 <- NULL
  # stop if not eRM RM
  if(!eRm.obj$model %in% c("RM")){
    stop(
      paste0(
        "Error: Model not of type RM.",
        "\n",
        "Please ensure eRm object is an estimated Dichotmous Rasch Model",
        "\n",
        "using eRm::RM()"
      )
    )
  }
  
  Title2 <- paste0(gsub(Title, pattern = " ", replacement = "-"), "_", Sys.Date())

  
  # if else check for it dir exists or not ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # create dir for support
  if (!dir.exists(paste0(Title2,"-tmp", "/"))){
    dir.create(paste0(Title2,"-tmp", "/"))      # create temp dir
  }


  save(data.long, file = paste0(Title2,"-tmp", "/", "data.long", ".Rdata"))    # long format data
  save(eRm.obj, file = paste0(Title2,"-tmp", "/", "eRm.obj", ".Rdata"))        # eRm object
  save(r_o, file = paste0(Title2,"-tmp", "/", "r_o", ".Rdata"))                # response options
  save(item.names, file = paste0(Title2,"-tmp", "/", "item.names", ".Rdata"))  # item names
  save(K, file = paste0(Title2,"-tmp", "/", "K", ".Rdata"))                    # K col name
  save(Item, file = paste0(Title2,"-tmp", "/", "Item", ".Rdata"))              # Item col name
  save(ID, file = paste0(Title2,"-tmp", "/", "ID", ".Rdata"))                  # ID col name
  save(ptrn, file = paste0(Title2,"-tmp", "/", "ptrn", ".Rdata"))              # pattern name


  # yaml section ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  YAML <- paste0("---", "\n",
                 "title: ","'", Title, "'", "\n",
                 "date: ", '"`r format(Sys.time(),'," '%d %B %Y')", '`"', "\n",
                 "output:",
                 "\n  html_document:",
                 "\n    toc: true",
                 "\n    toc_depth: 3",
                 "\n    toc_float: true",
                 "\n",
                 "---",
                 "\n\n")


  # Knitr stuff ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # keep objects in quotes,
  # single character objects can print
  knitr_stuff <- paste0("\n",
                        "```{r}",
                        "\n",
                        "knitr::opts_chunk$set(echo = FALSE, out.width = '100%', warning = FALSE, message = FALSE)", "\n",
                        "library(knitr)", "\n",
                        "library(kableExtra)", "\n",
                        "library(tidyverse)", "\n",
                        "library(eRm)", "\n",
                        "library(psketti)", "\n","\n",
                        "source('D:/Thesis/PhD_Sci/R_Projects_for_Writeup/Test_New_Stuff_Private/R/al_dente.R')", "\n","\n",

                        "# Load Data ~~~~~~~~~", "\n",
                        'load(paste0("data.long", ".Rdata"))',  "\n",
                        'load(paste0("eRm.obj", ".Rdata"))',    "\n",
                        'load(paste0("r_o", ".Rdata"))',        "\n",
                        'load(paste0("item.names", ".Rdata"))', "\n",
                        'load(paste0("K", ".Rdata"))',          "\n",
                        'load(paste0("Item", ".Rdata"))',       "\n",
                        'load(paste0("ID", ".Rdata"))',         "\n",
                        'load(paste0("ptrn", ".Rdata"))',       "\n",
                        "\n",

                        "# Psketti Stuff ~~~~~~", "\n",
                        "psk_ifit      <- item_fit_table(eRm.obj = eRm.obj)", "\n",
                        "psk_msq_plot  <- psketti_msq(x = psk_ifit)", "\n",
                        "residuals     <- al_dente(eRm.obj = eRm.obj, style = 'present')", "\n",
                        "psk           <- pskettify(eRm.obj =", "eRm.obj", ") ", "\n",
                        "psk_IRF       <- psketti(pskettified_data = psk)","\n",
                        "psk_dst       <- psketti_distractor(x = ", "data.long", ", eRm.obj = ", "eRm.obj",
                        ", response_options = ", "r_o", ", ncut = 10", ", K =  K, ", "Item = Item, ", "ID = ID)", "\n",
                        "psk_tab       <- tabliatelle(x = ", "data.long", ", eRm.obj = ", "eRm.obj",
                        ", response_options = ", "r_o", ", K =  K, ", "Item =  Item, ", " ID = ID)", "\n",
                        "AS_table      <- psk_tab$Proportions.table", "\n",
                        "pp.RM         <- person.parameter(eRm.obj)", "\n",
                        "P_I_df <- data.frame(", "\n",
                        "  Type = c(rep('Person', length(pp.RM$theta.table$`Person Parameter`)), rep('Item', length(coef(eRm.obj)))),", "\n",
                        "  Logit = c(pp.RM$theta.table$`Person Parameter`, -coef(eRm.obj))", "\n",
                        ")", "\n",

                        "\n", "```", "\n\n")

  # Person Item Density Plot
  PI <- paste0("\n",
               "### Person Item Density Plot", "\n",
               "```{r PIPLOT}",
               "\n",
               "P_I_2 <- aggregate(data = P_I_df, Logit~Type, mean)", "\n\n",
               "P_I_df %>% ", "\n",
                " ggplot(aes(x = Logit, fill = Type))+", "\n",
                " geom_density(alpha = .5)+", "\n",
                " geom_vline(xintercept =P_I_2[P_I_2$Type == 'Item',]$Logit)+", "\n",
                " geom_vline(xintercept =P_I_2[P_I_2$Type == 'Person',]$Logit, lty = 'dashed')+", "\n",
                " scale_fill_manual(values = c('#475559', '#80BDAB'), name = '')+", "\n",
                " theme_minimal()+", "\n",
                " theme(legend.position = 'bottom')", "\n",
               "\n", "```", "\n\n")


  # Fit Stats ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f1 <- paste0("\n",
               "### Item Fit Statistics", "\n",
               "```{r IFIT}",
               "\n",
               "psk_ifit %>% ", "\n",
               "\tas_tibble %>% ", "\n",
               "\tmutate_if(is.numeric, round, 2) %>%", "\n",
               "\n\tkable(format = 'html',", "\n",
               "\t\t caption = 'Item Fit Statistics',", "\n",
               "\t\talign = paste(c(rep('c',ncol(psk_ifit))), collapse = '')) %>% ", "\n",
               "\tkable_styling(full_width = FALSE)",
               "\n", "```", "\n\n",

               "### MSQ Plot", "\n",
               "```{r MSQPLOT}",
               "\n",
               "psk_msq_plot",
               "\n", "```", "\n\n")

  f2 <- paste0("\n",
               "### Rasch Residuals Correlation", "\n",
               "```{r RResid}",
               "\n",
               "Rmat <- residuals$Std.Resid", "\n",

               "as.data.frame(Rmat) %>%", "\n",
               "  mutate_if(is.numeric, round, 2) %>% ", "\n",
               "  mutate_all(~cell_spec(.x, ", "\n",
               "                        color = ifelse(.x >= .2 & .x < 1.00, 'white', 'white'),", "\n",
               "                        background = ifelse(.x >= .2 & .x < 1.00, 'red','black'))) %>% ", "\n",
               "  mutate(Item = row.names(Rmat)) %>% ", "\n",
               "  select(Item, everything()) %>% ", "\n",
               "  kable(format = 'html', escape = FALSE) %>% ", "\n",
               "  kable_styling(full_width = FALSE)", "\n",

               "\n", "```", "\n\n")

  # Item plot section ~~~~~~~~~~~~~~~~~~~~~~~~~
  a123_out <- NULL
  i        <- NULL

  for (i in 1:length(item.names)) {
    a1 <- paste0("\n",
      "### Item `r str_remove(item.names[", i, "], ", "pattern = ", "'", ptrn, "'", ")`", "\n",
      "***", "\n"
    )

    a2 <- paste0(
      "```{r}",
      "\n",
      "AS_table[AS_table$Item == item.names[", i, "],]", " %>%",
      "\n\tmutate_if(is.numeric, round, 2) %>%",
      "\n\tselect(-Prop.Total) %>%",
      "\n\tkable(format = 'html',",
      "\n\t\tcaption = paste0('Andrich-Styles Table: ', item.names[",  i, "])) %>%",
      "\n\tcollapse_rows(columns = 1:2, valign = 'top') %>%",
      "\n\tkable_styling(full_width = FALSE)",
      "\n", "```", "\n\n"
    )

    a3 <- paste0(
      "```{r}",
      "\n",
      "psk_IRF$Plot.List[[item.names[", i, "]]][[1]]", "\n",
      "```", "\n\n",

      "```{r}",
      "\n",
      " psk_dst$Plot.List[[item.names[", i, "]]][[1]]","\n",
      "```", "\n\n",

      "```{r}",
      "\n",
      "residuals$Plot.List[[item.names[", i, "]]][[1]]","\n",
      "```", "\n\n"
    )

    a123 <- paste0(a1, a2, a3)

    a123_out <- paste0(a123_out, a123)

  }

  cat(YAML, knitr_stuff, PI, f1, f2, a123_out, file = paste0(Title2,"-tmp", "/", "Report-", Title2, ".Rmd"))

  # Add section to render the Rmd Report
  # Alternatively create file path as  environment object
  # that can be called in render
  
  #if(html_gen == TRUE){
  #  
  #  # internal block
  #  if (! requireNamespace("tidyverse", quietly = TRUE)){
  #    stop("Please install tidyverse: install.packages('tidyverse')")
  #  } else if(! requireNamespace("knitr", quietly = TRUE)){
  #    stop("Please install knitr: install.packages('knitr')")
  #  } else if(! requireNamespace("rmarkdown", quietly = TRUE)){
  #    stop("Please install rmarkdown: install.packages('rmarkdown')")
  #  } else {
  #    # if all checks pass then do this:
  #    rmarkdown::render(input = paste0("./",Title2,"-tmp", "/", "Report-", Title2, ".Rmd"),
  #                      output_format = "html_document",
  #                      output_file = paste0(Title2,"-tmp", "/", "Report-", Title2, ".html"))
  #  }
  #}
  

}
