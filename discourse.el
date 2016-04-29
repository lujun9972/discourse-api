(require 'cl-lib)
(require 'request)
(require 's)

(cl-defstruct discourse-api
  (url "http://forum.emacs-china.org")
  api-key
  api-username)

(defun discourse--extract-response-data (response-data path)
  "Extract data from RESPONSE-DATA according PATH which is a string list"
  (if (null path)
      response-data
    (let* ((key (car path))
           (response-data (cdr (assoc key response-data)))
           (path (cdr path)))
      (discourse--extract-response-data response-data path))))

(cl-defun discourse--request-response-data (api url-template &key category-id topic-id username (type "GET") request-data extract-path)
  "使用request访问URL，并返回回应结果"
  (let* ((base-url (discourse-api-url api))
         (url-params-alist `(("category-id" . ,category-id)
                             ("topic-id" . ,topic-id)
                             ("username" . ,username)))
         (url (concat base-url (s-format url-template 'aget url-params-alist)))
         (response (request url
                            :type type
                            :data request-data
                            :parser (lambda ()
                                      (json-read-from-string (decode-coding-string (buffer-string) 'utf-8)))
                            :sync t
                            :success (cl-function (lambda (&key data &allow-other-keys)
                                                    data)))))
    (discourse--extract-response-data (request-response-data response) extract-path)))

(cl-defun discourse--select-from-alist (alist &optional (prompt ": "))
  ""
  (let* ((keys (mapcar #'car alist))
         (key (completing-read prompt keys)))
    (cdr (assoc-string key alist))))

(defun discourse-categories (api)
  "Get a list of categories"
  (discourse--request-response-data api "/categories.json" :extract-path '(category_list categories)))
;; (discourse-categories (make-discourse-api))

(defun discourse-get-id (data)
  "Return id from DATA which may be a category or a topic"
  (discourse--extract-response-data data '(id)))

(defun discourse-category-topics (api category)
  "List topics in a specific CATEGORY"
  (discourse--request-response-data api "/c/${category-id}.json" :category-id (discourse-get-id category) :extract-path '(topic_list topics)))

(defun discourse-category-latest-topics (api category)
  "List the latest topics in a specific CATEGORY"
  )

;; (discourse-category-topics-list 7)




