# Stanisław - very large models in Stan

[Stan](https://mc-stan.org/) is an amazing Bayesian computation framework capable of fitting models with millions of parameters and beyond. This package is (intended to be) a collection of tools to help [cmdstanr](https://mc-stan.org/cmdstanr/) users on Linux process such massive models fast while conserving RAM.

The only functionality available currently is the `stansummary` function to obtain posterior summaries of parameter subsets using the `stansummary` command-line utility bundled with `CmdStan`. On very large models, this approach is dramatically faster than `cmdstanr::summary()` and requires little RAM.
