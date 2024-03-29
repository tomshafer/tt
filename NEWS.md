# tt 0.5.0 (development)

- Refactored the `with_cache()` interface (`...` are no longer used).
- Provided an option `tt.cache.prefer_data_table` to return fst-backed cache objects as data.tables.

# tt 0.3.0

- Added `check_for_packages()` to guard against missing "Suggests" packages.
- Added `build_cmdstan()` for building a fast version on macOS.
- Added generics for `extract_divergences()` and `extract_treedepths()`.
- Added [renv](https://github.com/rstudio/renv)'s Git hooks for version numbers.

# tt 0.2.0

- Added `with_cache()` for caching R operations.
- Added a suite of Stan utilities from Michael Betancourt.

# tt 0.1.1

- Fix `theme_tmth()` docs to remove bad ggplot2 link.

# tt 0.1.0

- Added ggplot2 theme `theme_tmth()` that, along with `lemon::coord_capped_cart()`, reproduces key elements of the style advocated for in the book _Trees, maps, and theorems_.
- Added a vignette demonstrating plotting.
- (Apparently, in the past) updated RMarkdown template.

# tt 0.0.1

- Added `ts_pdf_document()` and a "LaTeX Article" R Markdown template
  to simplify PDF creation.

# tt 0.0.0.9001

- Added `setdiff3()` and `table_na()` functions.

# tt 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
- Added a `REAMDE.md` file.
