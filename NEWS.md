topicanalysis 0.1.19
====================

* Substantial improvement of the documentation of the `cooccurrences()`-method.
* The `cooccurrences()`-method now accepts an argument `method` to determine the
statistical test to be applied.
* The total number of occurrences of topics a and b will now be reported in the
`data.table` resulting from the `cooccurrences()`-method.
* A flaw in the calculation of the number of total occurrences in the `cooccurrences()`-method
has been removed, when renumbering topics.


topicanalysis 0.1.18
====================

* The argument `topic_matrix` of the `cooccurrences()`-method has been discarded again. Instead,
the method is now defined for the `TopicModel` class and the `matrix` class. If multiple calculations are carried out, it may be much faster to generate the document-term-matrix once outside the `cooccurrences()`-method, and to apply the method on the matrix.


topicanalysis 0.1.17
====================

* A new argument `regex` has been added to the `docs()`-method. It can be used to limit the analysis to documents matched by the regular expression.
* A bug has been removed from the `cooccurrences()`-method that resulted in an overestimation of the cooccurrence of topics when topics had been renumbered.
* A new field `topics` has been introduced in the `Topicanalysis` class. A topics-document-matrix can #
be stored here. As retrieving the topics-document-matrix can be slow, subsequent operations on this matrix are faster now.
* The `cooccurrencs()`-method for the `TopicModel` class now has an argument `topic_matrix`, so that a topic-document-matrix prepared outside the method can be re-used. May be used for speeding up operations.
* Added new argument `verbose` to `Topicanalysis$cooccurrences()`-method, which is passed into S4 method `cooccurrences()`.

topicanalysis 0.1.14
====================

  * The preliminary Rmarkdown template to use 'gensim' for topic modelling has been turned into a (somewhat) consolidated Rmarkdown template.
  * Functions `gensim_ldamodel_as_LDA_Gibbs()`, `gensim_ldamodel_load()` and `dtm_as_bow()` were added as functionality to get a gensim LDA model (prepared in Python) back to an R session.