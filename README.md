# Estimate general intelligence (G-factor) from many tasks

The goal of this project is to check the idea of measuring general intelligence with cognitive paradigms. Modeling is done with latent variable models, such as factor analysis and structural equation modeling, with the aid of the R package {lavaan}.

## Outline

The project is pipelined with the help of R package {targets}. But a multiple-project setup is used, and these projects are interdependent. The naming convention is as follows:

* The targets project is named with prefix of `project_`.
* The targets data store is named with prefix of `_store_`.
* The targets pipeline script is named with prefix of `_script_`.

## Deprecation

The deprecated projects now live in the `deprecated` folder. And functions that are no longer used are moved to files starting with `deprecated-`.
