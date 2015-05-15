# Shiny App for minimum spanning networks in poppr

This repository contains a shiny application that will create and display minimum spanning networks via the poppr R package. This provides a simple way of exploring visualization of these networks with your own data.

It is recommended to use poppr version 2.0 for this. It is still in development. You can install it via github:

```R
devtools::install_github("thibautjombart/adegenet") # requires adegenet 2.0
devtools::install_github("grunwaldlab/poppr@2.0-rc")
```


For an example, you can use the example from the [minimum spanning network](http://grunwaldlab.github.io/Population_Genetics_in_R/Pop_Structure.html#minimum-spanning-network) section of our primer to prepare data for exploratory visualization.

To run this application you should have installed shiny and poppr. After that you can use the following line to run the instance and a window will open in your browser.

```R
shiny::runGitHub("zkamvar/poppr_msn_shiny")
```

### Screencapture

![](20150515_monpop.png)
