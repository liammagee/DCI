
Digital Capacity Index
========================

This project contains code for analysing and visualising
results of the *Digital Capacities Index* survey, developed by
Western Sydney University in conjunction with Google Australia.

For further information, please contact the project team at the [Young and Well CRC](http://www.uws.edu.au/ics/research/projects/yawcrc).


## Notes

The *DCI* project uses [R](https://www.r-project.org/) and [Markdown](https://daringfireball.net/projects/markdown/) heavily.

To run the main script:

```
Rscript R/main.R
```


To generate the main report:

```
Rscript -e 'rmarkdown::render("doc/DCI.Rmd")' 0.5 0.7
```
