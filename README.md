# Use R for Epidemiologic Methods 1-3

This project uses R instead of Stata to implement the labs from the Epidemiologic Methods 1-3 course series offered at the Johns Hopkins Bloomberg School of Public Health. **Note: This project is not endorsed by the course faculty. The contributors do not vouch for any of the code or its output.** Rather, this is an opportunity for students who would prefer to use R to do so and compare the results against what they would obtain using Stata.

## Why use R?

R is a free and open source alternative to proprietary software like Stata and SAS. It may be harder to learn at first, but ultimately R offers far more flexibility to scientists (in my opinion) and makes reproducible research practices easier to implement. It's also free, while Stata is expensive and the cost of SAS is just ridiculous.

## Where can I obtain R?

You can download R from the [Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/). After you download R, you can download [RStudio](https://rstudio.com/products/rstudio/download/), which makes using R much easier. R and RStudio are free.

## Where can I learn more about R?

Dr. Roger Peng in the Department of Biostatistics teaches a course called Statistical Computing during first term. His slides from the course are available [here](https://rdpeng.github.io/Biostat776/index.html). Dr. Peng, Dr. Brian Caffo, and Dr. Jeff Leek teach the [Data Science Specialization](https://www.coursera.org/specializations/jhu-data-science) on Coursera. 

Additionally, you can learn R by using the `swirl` package, which is best done using RStudio. At the console in RStudio, type each of the following lines. (Note: You only need to type `install.packages` the first time you use `swirl`.)

```{r}
install.packages("swirl")
library(swirl)
swirl()
```

## How do I use this website?

R scripts will be published to GitHub. GitHub is widely used by developers, but you do not need to know much about it to access the scripts. You can navigate the folders above much as you would on your own computer. The scripts are organized by class (e.g., 753) and lab (e.g., Lab 1). 

If you do **not** want to learn more about GitHub, just click on the link to the script, click on the "Raw" button, and save the page (which should look like plain text) using the ".R" extension (e.g., 2020_Epi753_Lab1.R). If you **do** want to learn more about GitHub, see tutorials like [this one](https://guides.github.com/activities/hello-world/).

## Can I contribute to this project? If so, how?

Please do! A major advantage of R is the active developer community, and many eyes on a problem make for better code. If you know how to use GitHub, please proceed accordingly: You can fork the repository, edit the code, and suggest changes by issuing a pull request. If you do not use GitHub, or if you would just prefer to do so, you can email me using the address in the Outlook directory.
