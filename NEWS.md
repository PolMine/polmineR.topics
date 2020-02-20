## v0.1.17

* A new argument `regex` has been added to the `docs()`-method. It can be used to limit the analysis to documents matched by the regular expression.
* A bug has been removed from the `cooccurrences()`-method that resulted in an overestimation of the cooccurrence of topics when topics had been renumbered.



## v0.1.14
  * The preliminary Rmarkdown template to use 'gensim' for topic modelling has been turned into a (somewhat) consolidated Rmarkdown template.
  * Functions `gensim_ldamodel_as_LDA_Gibbs()`, `gensim_ldamodel_load()` and `dtm_as_bow()` were added as functionality to get a gensim LDA model (prepared in Python) back to an R session.