# Stanisław - very large Stan

![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

[Stan](https://mc-stan.org/) is a leading Bayesian computation framework
capable of fitting models with millions of parameters. This package
is a small collection of tools to help
[cmdstanr](https://mc-stan.org/cmdstanr/) users on Linux process such massive
models fast while conserving RAM.

The key functionality is the `stansummary` function to obtain posterior
summaries of parameter subsets using the `stansummary` command-line utility. 
For very large models this approach is dramatically
faster than the `summary` function provided by `cmdstanr` and requires less RAM.

## Installation

Install directly from github with `devtools`:

```r
devtools::install_github("https://github.com/huffyhenry/Stanislaw")
```
