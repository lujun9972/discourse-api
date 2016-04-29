(discourse-categories (make-discourse-api))

(let* ((api (make-discourse-api))
       (category-array (discourse-categories api))
       (first-category (elt category-array 0))
       (topics (discourse-category-topics api first-category)))
  topics)

(let* ((api (make-discourse-api))
       (category-array (discourse-categories api))
       (first-category (elt category-array 0))
       (topics (discourse-category-latest-topics api first-category)))
  topics)

(let* ((api (make-discourse-api))
       (category-array (discourse-categories api))
       (first-category (elt category-array 0))
       (topics (discourse-category-new-topics api first-category)))
  topics)

(let* ((api (make-discourse-api))
       (category-array (discourse-categories api))
       (first-category (elt category-array 0))
       (topics (discourse-category-top-topics api first-category)))
  topics)

(discourse-category-create (make-discourse-api :url "http://forum.emacs-china.org"
                                               :api-key ""))

(discourse-latest-topics (make-discourse-api))
(discourse-top-topics (make-discourse-api))
(discourse-topic (make-discourse-api) 164)
