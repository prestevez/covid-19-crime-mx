#!/bin/bash

docker run \
        --rm \
        -it \
        --user rstudio \
        -v "$("pwd"):/home/rstudio/local/" \
        -w /home/rstudio/local/ \
        ${1:-prestevez/r-pres:0.01} \
        R --slave -e "rmarkdown::render('covid-final-models.Rmd', output_format = 'pdf_document', output_dir = 'pdf-output/', clean = FALSE)"
