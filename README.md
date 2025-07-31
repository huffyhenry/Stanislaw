# Stanisław - very large Stan

![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

[Stan](https://mc-stan.org/) is a leading Bayesian computation framework
capable of fitting models with millions of parameters. This package
is a small collection of tools to help
[cmdstanr](https://mc-stan.org/cmdstanr/) users on Linux process such massive
models fast while conserving RAM.

The key functionality is the `stansummary` function to obtain posterior
summaries of parameter subsets using the `stansummary` command-line utility
bundled with `CmdStan`. For very large models this approach is dramatically
faster than `cmdstanr::summary()` and requires less RAM.

## Installation

Install directly from github with `devtools` like so:

```r
devtools::install_github("https://github.com/huffyhenry/Stanislaw")
```
