# Data for Lab 7: Exploring Tree-Based Methods by Predicting Tax
Compliance

Data collected in Malawi from October 2018 - January 2019. Please see
`lab7_instructions.pdf` and `lab4_instructions.pdf` for more information
on the data context.

<div>

> **`vendor_data` Codebook**
>
> <div class="panel-tabset">
>
> #### Codebook
>
> | Variables                  | Description                                                                                                                                                                                  | Value                                                                                                                                                                                                                                 |
> |:---------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
> | `market`                   | Market ID of respondent’s market                                                                                                                                                             | Numeric 1 to 128 for privacy reasons, but represents a categorical value. **Note**: You will have to turn this into a factor/categorical variable, or **scikit-learn will get confused**!                                             |
> | `district`                 | District ID of district where respondent’s market is located                                                                                                                                 | Numeric 1 to 8 for privacy reasons, but represents a categorical value. **Note**: You will have to turn this into a factor/categorical variable, or **scikit-learn will get confused**!                                               |
> | `language`                 | Language in which survey was carried out                                                                                                                                                     | Factor with levels *Chichewa, English, Tumbuka, Yawo*                                                                                                                                                                                 |
> | `female`                   | Is respondent female or not (Note: assessed by interviewer)                                                                                                                                  | Binary. 1 = Female, 0 = Not Female                                                                                                                                                                                                    |
> | `age`                      | Respondent’s age                                                                                                                                                                             | Numeric                                                                                                                                                                                                                               |
> | `tribe`                    | Respondent’s tribe                                                                                                                                                                           | Factor with levels *Chewa, Lomwe, Ngoni, Yao, Tumbuka, Sena, Tonga*                                                                                                                                                                   |
> | `married`                  | Is respondent married?                                                                                                                                                                       | Binary. 1 = Yes, 0 = No                                                                                                                                                                                                               |
> | `education`                | Maximum level of education completed by respondent                                                                                                                                           | Factor with 18 levels from ‘None’ to ‘PhD’                                                                                                                                                                                            |
> | `educ_num`                 | Numeric version of education variable                                                                                                                                                        | Because `education` is ordered, this roughly captures how educated respondent is numerically. Note that it does not quite correspond to how many years of education a respondent has completed.                                       |
> | `literacy`                 | How well respondent was able to read a cue card. Used as a measure of literacy.                                                                                                              | Factor with levels *Could not read, Could read some of the card, Could read the whole card with difficulty, Could read the whole card with ease*                                                                                      |
> | `reading_language`         | Language in which respondent wanted to read cue card                                                                                                                                         | Factor with levels *Chichewa, English, Could not read*                                                                                                                                                                                |
> | `houses`                   | How many houses are owned by respondent’s household                                                                                                                                          | Numeric                                                                                                                                                                                                                               |
> | `acres_farmland`           | How many acres of farmland are owned by respondent’s household                                                                                                                               | Numeric                                                                                                                                                                                                                               |
> | `bicycles`                 | How many bicycles are owned by respondent’s household                                                                                                                                        | Numeric                                                                                                                                                                                                                               |
> | `chickens`                 | How many chickens are owned by respondent’s household                                                                                                                                        | Numeric                                                                                                                                                                                                                               |
> | `goats`                    | How many goats are owned by respondent’s household                                                                                                                                           | Numeric                                                                                                                                                                                                                               |
> | `basic_cell_phones`        | How many basic cell phones are owned by the respondent’s household                                                                                                                           | Numeric                                                                                                                                                                                                                               |
> | `smart_phones`             | How many smart phones are owned by the respondent’s household                                                                                                                                | Numeric                                                                                                                                                                                                                               |
> | `days_pr_week`             | How many days a week respondent sells at this market                                                                                                                                         | Numeric                                                                                                                                                                                                                               |
> | `service`                  | Does vendor sell services or goods                                                                                                                                                           | Binary. 1 = Service, 0 = Good                                                                                                                                                                                                         |
> | `yrs_in_mkt_fix`           | How many years respondent has sold at this market                                                                                                                                            | Numeric                                                                                                                                                                                                                               |
> | `profit`                   | Respondent’s average daily profit (in Malawian kwacha)                                                                                                                                       | Numeric                                                                                                                                                                                                                               |
> | `profit_lst_yr_month`      | How their profit this month compares to their profit this month last year                                                                                                                    | Factor with levels *Much lower (less than 1/2, Lower (less, but not 1/2), Same, More (more but not double), Much higher (more than double)*                                                                                           |
> | `hh_income_trim_99`        | Respondent’s household income (Note: this has been 99th percentile trimmed, where extreme values higher than the 99th percentile are dropped, as a way to reduce outliers. (Malawian kwacha) | Numeric                                                                                                                                                                                                                               |
> | `customers_pr_day_trim_99` | How many customers respondent has a day, on average (Note: 99th percentile trimmed)                                                                                                          | Numeric                                                                                                                                                                                                                               |
> | `stall_type`               | A description of the respondent’s stall (spot in the market)                                                                                                                                 | Factor with levels *Tarp, blanket or baskets on the ground; Tempoarary stall or tablet that gets put up and taken down every day; Uncovered permanent stall; Covered permanent stall WITHOUT lock; Covered permanent stall with lock* |
> | `vote_intend`              | Does respondent intend to vote in 2019 presidential election?                                                                                                                                | Binary. 1 = Yes, 0 = No                                                                                                                                                                                                               |
> | `recent_receipt_7`         | Did respondent show enumerator from within past 7 days?                                                                                                                                      | Binary. 1 = Yes, 0 = No                                                                                                                                                                                                               |
>
> #### Code to Create Codebook
>
> ``` r
> library(glue)
> library(tidyverse)
> load("vendor_data.RData")
>
> get_levels_string <- function(column, collapse = ", ", ital = T){
>   lvl_str <- paste(levels(vendor_data[[column]]), collapse = collapse)
>   
>   if(isTRUE(ital)) lvl_str <- paste0("*", lvl_str, "*")
>   
>   lvl_str
> }
>
> langs <- get_levels_string("language")
> tribes <- get_levels_string("tribe")
> lit <- get_levels_string("literacy")
> read_langs <- get_levels_string("reading_language")
> profit_comp <- get_levels_string("profit_lst_yr_month")
> stalls <- get_levels_string("stall_type", collapse = "; ")
>
> codebook <- tibble(
>   Variables = paste0("`", names(vendor_data), "`"),
>   Description = c("Market ID of respondent's market",
>                   "District ID of district where respondent's market is located",
>                   "Language in which survey was carried out",
>                   "Is respondent female or not (Note: assessed by interviewer)",
>                   "Respondent's age",
>                   "Respondent's tribe",
>                   "Is respondent married?",
>                   "Maximum level of education completed by respondent",
>                   "Numeric version of education variable",
>                   "How well respondent was able to read a cue card. Used as a measure of literacy.",
>                   "Language in which respondent wanted to read cue card",
>                   "How many houses are owned by respondent's household",
>                   "How many acres of farmland are owned by respondent's household",
>                   "How many bicycles are owned by respondent's household",
>                   "How many chickens are owned by respondent's household",
>                   "How many goats are owned by respondent's household",
>                   "How many basic cell phones are owned by the respondent's household",
>                   "How many smart phones are owned by the respondent's household",
>                   "How many days a week respondent sells at this market",
>                   "Does vendor sell services or goods",
>                   "How many years respondent has sold at this market",
>                   "Respondent's average daily profit (in Malawian kwacha)",
>                   "How their profit this month compares to their profit this month last year",
>                   "Respondent's household income (Note: this has been 99th percentile trimmed, where extreme values higher than the 99th percentile are dropped, as a way to reduce outliers. (Malawian kwacha)",
>                   "How many customers respondent has a day, on average (Note: 99th percentile trimmed)",
>                   "A description of the respondent's stall (spot in the market)",
>                   "Does respondent intend to vote in 2019 presidential election?",
>                   "Did respondent show enumerator from within past 7 days?"),
>   Value = c("Numeric 1 to 128 for privacy reasons, but represents a categorical value. **Note**: You will have to turn this into a factor/categorical variable, or **scikit-learn will get confused**!",
>             "Numeric 1 to 8 for privacy reasons, but represents a categorical value. **Note**: You will have to turn this into a factor/categorical variable, or **scikit-learn will get confused**!",
>             glue("Factor with levels {langs}"),
>             "Binary. 1 = Female, 0 = Not Female",
>             "Numeric",
>             glue("Factor with levels {tribes}"),
>             "Binary. 1 = Yes, 0 = No",
>             "Factor with 18 levels from 'None' to 'PhD'",
>             "Because `education` is ordered, this roughly captures how educated respondent is numerically. Note that it does not quite correspond to how many years of education a respondent has completed.",
>             glue("Factor with levels {lit}"),
>             glue("Factor with levels {read_langs}"),
>             rep("Numeric", 7),
>             "Numeric",
>             "Binary. 1 = Service, 0 = Good",
>             "Numeric",
>             "Numeric",
>             glue("Factor with levels {profit_comp}"),
>             "Numeric",
>             "Numeric",
>             glue("Factor with levels {stalls}"),
>             "Binary. 1 = Yes, 0 = No",
>             "Binary. 1 = Yes, 0 = No")
> )
>
> knitr::kable(codebook)
> ```
>
> </div>

</div>
