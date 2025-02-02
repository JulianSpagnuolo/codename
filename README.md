
# Generation of Code Names for Organizations, People, Projects, and Whatever Else

[![](https://www.r-pkg.org/badges/version/codename?color=green)](https://cran.r-project.org/package=codename)
[![](http://cranlogs.r-pkg.org/badges/grand-total/codename?color=green)](https://cran.r-project.org/package=codename)
[![](http://cranlogs.r-pkg.org/badges/last-month/codename?color=green)](https://cran.r-project.org/package=codename)
[![](http://cranlogs.r-pkg.org/badges/last-week/codename?color=green)](https://cran.r-project.org/package=codename)

`{codename}` allows for users to create short, pithy code names for
their organizations, their work projects, themselves, other people, or
whatever else. The core of this package is the eponymous `codename()`
function, which allows the user to create various types of code names.
The code names returned from this function, by in large, are a two-word
character vector of an attribute and an object. Right now, options
include “any” (which is default and incorporates a battery of adjectives
and nouns), “gods” (in which the object is the name of a deity or saint
from some religion), “ubuntu” (in which the code name is alliterative
but the object is always an animal), and “wu-tang” (in which the code
name is derived from the classic Wu-Tang Name Generator). `codename()`
also features the ability to use reproducible seeds, including character
seeds, for maximum transparency and reproducibility.

# Installation

You will ideally soon be able to install this on CRAN, as follows:

``` r
install.packages("codename")
```

Until then, you can install the development version of this package
through the `devtools` package.

``` r
devtools::install_github("svmiller/codename")
```

# Usage

There isn’t much to belabor here and usage should be self-explanatory.
Here is some sample output.

``` r
library(codename)

# Generate console message about package version.
# Successive updates may break the expected output of a reproducible seed.
# This just adds some layer of transparency/clarity.
codename_message()
#> code name generated by {codename} v.0.4.0

# defaults to any
codename()
#> [1] "moss green implication"
codename()
#> [1] "lavender blue snuggle"
codename()
#> [1] "warm purple ale"
codename()
#> [1] "pinkish train"
codename(seed = 8675309)
#> [1] "yellow orange twist"
codename(seed = "My Project Name: The Subtitle of It Too")
#> [1] "bright blue barbiturate"

codename(type = "gods")
#> [1] "apricot juras mate"
codename(type = "gods")
#> [1] "lighter green mokosh"
codename(type = "gods")
#> [1] "untried chernobog"
codename(type = "gods", seed = 8675309)
#> [1] "scholarly wakan tanka"

codename(type = "ubuntu")
#> [1] "cuddly cricket"
codename(type = "ubuntu")
#> [1] "capital cod"
codename(type = "ubuntu")
#> [1] "joint jaguar"
codename(type = "ubuntu", seed = 8675309)
#> [1] "moss mandrill"
codename(type = "ubuntu", seed = "My Ubuntu Release")
#> [1] "toupe tahr"

codename(type = "wu-tang")
#> [1] "Shriekin’ Wanderer"
codename(type = "wu-tang")
#> [1] "Bittah Knight"
codename(type = "wu-tang")
#> [1] "Zexy Dreamer"
codename(type = "wu-tang", seed = "Steven V. Miller")
#> [1] "X-cessive Observer"

variety_pack(seed = "A Reproducible Character Seed")
#> [1] "afraid patriarch"
#> [1] "meaty ausrine"
#> [1] "banana barnacle"
#> [1] "Scratchin’ Commander"
```

# Hall of Fame Entries

This is an incomplete and running list of some of my favorite returns
from this function. Because most of the sample output on the README is a
one-off return, these are prone to disappear every time the README is
updated. No matter, I want to preserve some of these, for posterity.

  - electric lime agenda
  - second-hand shovel
  - reckless azimuth
  - x-pert anesthesiology
  - sunny sunroom
  - improbable boris
  - criminal outlaw
  - gregarious denominator
  - lawn green insurgence
  - corrupt chickadee
  - realistic democrat
  - creamy escalator

# `{codename}` in the Wild

Here’s a running list of projects that make use of `{codename}`. If you
would like your project included, please [raise an
issue](https://github.com/svmiller/codename/issues) on the project’s
Github.

  - [**corrupt
    caterpillar**](https://github.com/andrewheiss/corrupt-caterpillar)
