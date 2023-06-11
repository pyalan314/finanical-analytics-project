# Performance Evaluation By Simulation

This is a project aiming at performance evaluation on portfolio by method of simulation.

## Usage

-   Read report.pdf for the final output.

-   Read report.rmd for how the report is generated.

## Folder Structure

ref: <https://rfortherestofus.com/2021/08/rstudio-project-structure/>

-   root

    -   *report.rmd*: Generate the final report (code is not included in the output). Html file is output because some character is not supported by LaTex and html supports more flexible formatting. It is then converted to pdf document manually.

    -   *clean-data.rmd*: Clean data in data-raw and store in data.

-   data-raw

    -   Raw data from the internet

    -   For this project, all the files are components information of portfolios. Different formats and data structures are involved. All files are processed by clean-data.rmd and stored in data.

-   data

    -   Cleaned and processed data

    -   These are tidy files for components information of portfolios.

    -   *xxx.tidy.csv*: These files are csv with 2 columns "symbol" and "weight". All symbol are prepossessed so that it can be used directly with the module "quantmod" to query data from yahoo finance.

    -   *xxx.404.csv*: These files contain those symbol that cannot queried using "quantmod".

-   R

    -   share.r: Common functions used in .rmd files
    -   plot.r: Helper functions related to ggplot
    -   stock: Helper functions related to stock data (using quantmod)

-   rds

    -   Cached data stored in .rds format

    -   *stock.rds*: Contain a tibble with column "symbol", "date" and "price". These are data cached to avoid slow querying from quantmod everytime.

    -   comp.rds: Contain the computation test result. The test takes long time to run so the result is cached for faster rendering.

    -   *known.ok.rds*: Contain symbol that is checked to be available on yahoo

-   output

    -   Assets generated from R code.

## Contributing

-   Ery Cao

-   Rachel Gong

-   Alan Wong

-   Yuhua Zhong

## License

[MIT](https://choosealicense.com/licenses/mit/)
