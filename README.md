# languagermobile

## Overview

R-Shiny App to learn languages. Currently, the app in centered around French, with translations to English and German.

The App currently consists of three modules:

1.  Explorer - Sample expressions and view translations and examples
2.  Vocab - Test your vocabulary-knowledge by guessing sampled or user-supplied words.

## Usage

### Explorer and Vocab

The first two modules allow / require the use of custom .csv files that store the data. The app automatically handles such files if they are placed in the `data` directory. These files should contain columns for the expressions of each language, e.g. `FR` and `EN` for French and English expressions. Multiple such files can be supplied at the same time; the one to work with can be selected / switched while using the app.


## Installation

-   Make sure to have R, R-Studio and the shiny and golem packages installed.
-   Clone repository
-   Start the app `golem::run_dev()` in the R console (accept installation of required additional dependencies if prompted to do so).
