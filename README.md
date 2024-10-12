# detranli

DEtection of RANdom LIkert-type responses

## Description

Detects full (i.e. all items) content-nonresponsivity in Likert-type data, using the nonparametric unsupervised algorithm in [Ilagan & Falk (2023)](https://doi.org/10.3758/s13428-023-02246-7).
The algorithm has been extended to accommodate inventories whose items vary the number of response options.

## Installation

Download and install the package from the Github source code.

```
devtools::install_github("michaeljohnilagan/detranli")
```

## Getting started

The example dataset has 18 items, all on a scale of 1 to 5.

```
library("detranli")
data("cnrexample1")
head(cnrexample1)
set.seed(1234)
pvals = cnrdetect(cnrexample1, pointscales=rep(5, 18))
decisions = ifelse(pvals<0.05, "human", "bot")
head(data.frame(pval=pvals, decision=decisions))
```

## Training materials

This package was presented at a training workshop at the 2024 Annual National Convention of the Canadian Psychological Association (CPA).
The training materials are available online at OSF.  
[https://osf.io/vnuew](https://osf.io/vnuew)
