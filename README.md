# Languager

## Overview

R-Shiny App to learn languages. Currently, the app in centered around French, with translations to English and German.

The App currently consists of three modules:

1.  Explorer - Sample expressions and view translations and examples
2.  Vocab - Test your vocabulary-knowledge by guessing sampled or user-supplied words.
3.  Translate - Use a local LLM from [Helsinki-NLP](https://huggingface.co/Helsinki-NLP) to translate any expression.

## Usage

### Explorer and Vocab

The first two modules allow / require the use of custom .csv files that store the data. The app automatically handles such files if they are placed in the `data_inputs/` directory. These files should contain columns for the expressions of each language, e.g. `FR` and `EN` for French and English expressions. Multiple such files can be supplied at the same time; the one to work with can be selected / switched while using the app.

### Translate

The transformers library automatically checks if the required model and tokenizer for the selected language pair are available in the local cache (e.g. `~/.cache/huggingface/hub/`). If not, it downloads them from the Hugging Face Model Hub (e.g. [DE-FR](https://huggingface.co/Helsinki-NLP/opus-mt-de-fr) or [EN-FR](https://huggingface.co/Helsinki-NLP/opus-mt-en-fr)). This process may take a while when the app is first used.

## Installation

-   Make sure to have R, R-Studio and the shiny and golem packages installed.
-   Clone repository
-   Optional, to use Translate module:
    -   Make sure to have python and the transformers module (`pip install transformers`) installed. You may want to use a virtual environment for this.
    -   Create .Renviron file in to root folder of the repository. In this file, set RETICULATE_PYTHON="path", specifying the path to your python installation.
-   Start the app `golem::run_dev()` in the R console (accept installation of required additional dependencies if prompted to do so).
