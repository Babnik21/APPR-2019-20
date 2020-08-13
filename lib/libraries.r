library(readr)
library(tidyverse)
library(dplyr)
library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(shiny)
library(xml2)
library(proto)
library(plyr)
library(gridExtra)
library(ggpubr)
library(tidyverse)
library(ggmap)
library(maps)
library(forcats)
library(tmap)

options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")

