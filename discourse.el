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

(cl-defun discourse--request-response-data (api url-template &key category-id topic-id username  request-data extract-path)
  "使用request访问URL，并返回回应结果"
  (let* ((base-url (discourse-api-url api))
         (url-params-alist `(("category-id" . ,category-id)
                             ("topic-id" . ,topic-id)
                             ("username" . ,username)))
         (api-key (discourse-api-api-key api))
         (api-username (discourse-api-api-username api))
         (type (if request-data
                   "POST"
                 "GET"))
         (url (concat base-url
                      (s-format url-template 'aget url-params-alist)
                      (if (and api-key api-username)
                          (format "?api_key=%s&api_username=%s"
                                  api-key api-username)
                        "")))
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

(defun discourse-get-id (data)
  "Return id from DATA which may be a category or a topic"
  (discourse--extract-response-data data '(id)))

(defun discourse-category-topics (api category)
  "List topics in a specific CATEGORY"
  (discourse--request-response-data api "/c/${category-id}.json" :category-id (discourse-get-id category) :extract-path '(topic_list topics)))

(defun discourse-category-latest-topics (api category)
  "List the latest topics in a specific CATEGORY"
  (discourse--request-response-data api "/c/${category-id}/l/latest.json" :category-id (discourse-get-id category) :extract-path '(topic_list topics)))

(defun discourse-category-new-topics (api category)
  "List new topics in a specific CATEGORY"
  (discourse--request-response-data api "/c/${category-id}/l/new.json" :category-id (discourse-get-id category) :extract-path '(topic_list topics)))

(defun discourse-category-top-topics (api category)
  "List top topics in a specific CATEGORY"
  (discourse--request-response-data api "/c/${category-id}/l/top.json" :category-id (discourse-get-id category) :extract-path '(topic_list topics)))

(cl-defun discourse-category-create (api name &key (color "3c3945") (text-color "ffffff"))
  "Create a category"
  (discourse--request-response-data api "/categories.json"
                                    :type "POST"
                                    :request-data `(("name" . ,name)
                                                    ("color" . ,color)
                                                    ("text_color" . text-color))
                                    :extract-path '(category_list categories)))
;; (discourse-category-topics-list 7)




;; Topics

(defun discourse-latest-topics (api)
  "Get the latest topics"
  (discourse--request-response-data api "/latest.json" :extract-path '(topic_list topics)))

(defun discourse-top-topics (api)
  "Get the top topics"
  (discourse--request-response-data api "/top.json" :extract-path '(topic_list topics)))

(defun discourse-topic (api topic-id)
  "Get the topic with TOPIC-ID"
  (discourse--request-response-data api "/t/${topic-id}.json" :topic-id topic-id ))

(defun discourse-topic-create (api title content)
  "Create Topic"
  (discourse--request-response-data api "/posts" :request-data `(("title" . ,title)
                                                                 ("raw" . ,content))))

(defun discourse-topic-update (api id new-id title category-id)
  "Update Topic"
  (discourse--request-response-data api "/t/${topic-id}" :topic-id id
                                    :request-data `(("topic_id" . ,new-id)
                                                    ("title" . ,title)
                                                    ("category_id" . ,category-id))))



;; Posts

(defun discourse-post-create (api topic-id content)
  "Create a post"
  (discourse--request-response-data api "/posts"
                                    :request-data `(("topic_id" . ,topic-id)
                                                    ("raw" . ,content))))


;; Notifications

(defun discourse-notifications (api)
  "List your notifications"
  (discourse--request-response-data api "/notifications.json"))

(defun discourse-notifications-mark-read (api)
  "Mark notifications read"
  (discourse--request-response-data api "/notifications/mark-read.json"))


;; Private Messages

(defun discourse-private-messages (api)
  "List private messages"
  (discourse--request-response-data api "/topics/private-messages/${username}.json" :username (discourse-api-api-username api)))
