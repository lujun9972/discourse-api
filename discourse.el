(require 'cl-lib)
(require 'request)
(defvar discourse-url "http://forum.emacs-china.org")

(defun discourse--request-response-data (url)
  "使用request访问URL，并返回回应结果"
  (let ((response (request url
                           :parser (lambda ()
                                     (json-read-from-string (decode-coding-string (buffer-string) 'utf-8)))
                           :sync t
                           :success (cl-function (lambda (&key data &allow-other-keys)
                                                   data)))))
    (request-response-data response)))

(defun discourse--extract-response-data (response-data path)
  "Extract data from RESPONSE-DATA according PATH which is a string list"
  (if (null path)
      response-data
    (let* ((key (car path))
           (response-data (cdr (assoc key response-data)))
           (path (cdr path)))
      (discourse--extract-response-data response-data path))))

(cl-defun discourse--select-from-alist (alist &optional (prompt ": "))
  ""
  (let* ((keys (mapcar #'car alist))
         (key (completing-read prompt keys)))
    (cdr (assoc-string key alist))))

(cl-defun discourse-categories-list (&optional (url discourse-url))
  "Get a list of categories"
  (let* ((url (concat url "/categories.json"))
         (response-data (discourse--request-response-data url))
         (categories-array (discourse--extract-response-data response-data '(category_list categories))))
    (mapcar (lambda (category)
              (cons (discourse--extract-response-data category '(name))
                    (discourse--extract-response-data category '(id))))
            categories-array)))
;; (discourse--select-from-alist (discourse-categories-list))

(cl-defun discourse-category-topics-list (category-id &optional (url discourse-url))
  "List topics in a specific category"
  (let* ((url (concat url (format "/c/%s.json" category-id)))
         (response-data (discourse--request-response-data url))
         (topics-array (discourse--extract-response-data response-data '(topic_list topics))))
    topics-array))

;; (discourse-category-topics-list 7)




