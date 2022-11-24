
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/ReboundTools)](https://cran.r-project.org/package=ECCTools)-->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

<!-- [![R-CMD-check](https://github.com/earamendia/ECCTools/workflows/R-CMD-check/badge.svg)](https://github.com/earamendia/ECCTools/actions)-->
<!-- [![Codecov test coverage](https://codecov.io/gh/earamendia/ECCTools/branch/master/graph/badge.svg)](https://codecov.io/gh/earamendia/ECCTools?branch=master) -->
<!-- [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5841962.svg)](https://doi.org/10.5281/zenodo.5841962) -->
<!-- badges: end -->

# EROITools

The `R` package `EROITools` provides tools to aggregate Energy Return On
Investment (EROI) values previously calculated using a Physical Supply
Use Table (PSUT) framework to represent the Energy Conversion Chain (see
Heun, Owen, and Brockway (2018)). Previous to using the `EROITools`
package, the World Energy Extended Balances (WEEB) from the
International Energy Agency (IEA) can be loaded and tidied using the
`IEATools` and `ECCTools` `R` packages. The `Recca` `R` package then
allows analysts to calculate a wide range of EROIs at the product and
industry levels, and the `EROITools` package provides tools aggregate
the calculated EROIs by product group. Important features of the
`EROITools` package include the following:

-   Aggregations can be performed either at the global or national
    level;
-   Aggregations can be performed at both the primary and final stage of
    energy use, but also at the **useful** stage of energy use providing
    that final-to-useful efficiencies are provided by the analyst for
    each energy product;
-   At the useful stage, the package also allows analysts to conduct
    these aggregations respecting sectoral or end-use breakdowns, so
    that the average useful stage EROI of a product group can be
    calculated for a given final demand sector (for instance
    steelmaking) or for a given end-use category (for instance high
    temperature heating);
-   Last, the package allows analysts to add additional energy
    requirements (supposing that these are provided by the analyst) that
    cannot be quantified using the IEA’s WEEB such as supply chain
    energy requirements — see Brockway et al. (2019) or Brand-Correa et
    al. (2017) for examples of a quantification of such indirect energy
    flows.

## Installation

You can install `EROITools` from github with:

``` r
# install devtools if not already installed
# install.packages("devtools")
devtools::install_github("earamendia/EROITools")
# To build vignettes locally, use
devtools::install_github("earamendia/EROITools", build_vignettes = TRUE)
```

## History

This package builds upon the previous `IEATools`, `Recca`, and
`ECCTools` `R` packages and will be demonstrated in a paper as soon as
possible. The calculations conducted within the package are heavily
dependent on the Physical Supply Use Table framework introduced to
represent the Energy Conversion Chain in Heun, Owen, and Brockway (2018)
and further developed in Aramendia et al. (2022).

## More Information

Find more information, including vignettes and function documentation,
at <https://earamendia.github.io/EROITools/>.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-mr_psut_paper" class="csl-entry">

Aramendia, Emmanuel, Matthew Heun, Paul Brockway, and Peter Taylor.
2022. “Developing a Multi-Regional Physical Supply Use Table Framework
to Improve the Accuracy and Reliability of Energy Analysis.” *Applied
Energy*. <https://doi.org/10.1016/j.apenergy.2021.118413>.

</div>

<div id="ref-brandcorreaDevelopingInputOutputBased2017"
class="csl-entry">

Brand-Correa, Lina, Paul Brockway, Claire Copeland, Timothy Foxon, Anne
Owen, and Peter Taylor. 2017. “Developing an Input-Output Based Method
to Estimate a National-Level Energy Return on Investment (EROI).”
*Energies* 10 (4): 534. <https://doi.org/10.3390/en10040534>.

</div>

<div id="ref-brockwayEstimationGlobalFinalstage2019" class="csl-entry">

Brockway, Paul E., Anne Owen, Lina I. Brand-Correa, and Lukas Hardt.
2019. “Estimation of Global Final-Stage Energy-Return-on-Investment for
Fossil Fuels with Comparison to Renewable Energy Sources.” *Nature
Energy* 4 (7): 612–21. <https://doi.org/10.1038/s41560-019-0425-z>.

</div>

<div id="ref-Heun:2018" class="csl-entry">

Heun, Matthew Kuperus, Anne Owen, and Paul E. Brockway. 2018. “A
Physical Supply-Use Table Framework for Energy Analysis on the Energy
Conversion Chain.” *Applied Energy* 226 (September): 1134–62.
<https://doi.org/10.1016/j.apenergy.2018.05.109>.

</div>

</div>
