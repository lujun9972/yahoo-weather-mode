(require 'url)
(require 'json)

(defgroup weather nil
  "Weather minor mode"
  :group 'emacs)

(defcustom weather-location "东莞"
  "location"
  :type 'string
  :group 'weather)

(defcustom weather-update-interval 60
  "Seconds after which the weather information will be updated."
  :type 'integer
  :group 'weather)

(defvar weather-info ""
  "weather information")

(defvar weather-env (url-hexify-string "store://datatables.org/alltableswithkeys"))

(defun weather-get-query-url (location env)
  "generate url that used to fetch weather information"
  (let ((yql_query (url-hexify-string (format "select * from weather.forecast where woeid in (select woeid from geo.places(1) where text='%s')" location)))
        (url (format 
              "https://query.yahooapis.com/v1/public/yql?q=%s&format=json&env=%s" yql_query env)))
    url))

(defun weather--extract-from-json-object (json-object extract-place-list)
  "extract data from JSON-OBJECT which responsed by yahoo weather"
  (let* ((place (car extract-place-list))
         (extract-place-list (cdr extract-place-list))
         (json-object (cdr (assoc place json-object))))
    (if extract-place-list
        (weather--extract-from-json-object json-object extract-place-list)
      json-object)))

(defun weather-update-info-cb (status &rest cbargs)
  (let (content)
    (goto-char (point-min))
    (when (search-forward-regexp "^$" nil t)
      (setq content (buffer-substring-no-properties (+ (point) 1) (point-max))))
    (kill-buffer)
    (let* ((json-object (json-read-from-string content))
           (temperature (weather--extract-from-json-object json-object '(query results channel item condition temp)))
           (text (weather--extract-from-json-object json-object '(query results channel item condition text))))
      (setq weather-info (format "%s %sF" text temperature))
      (run-at-time weather-update-interval nil #'weather-update-info))))

(defun weather-update-info ()
  "update weather information"
  (interactive)
  (let ((url (weather-get-query-url weather-location weather-env)))
    (url-retrieve url 'weather-update-info-cb nil t)))

(defcustom weather-mode-line
  '(:eval
    (concat " " weather-info))
  "Mode line lighter for weather-mode."
  :type 'sexp
  :group 'weather)

  (defvar weather-mode-line-string nil
    "String to display in the mode line.")
  (put 'weather-mode-line-string 'risky-local-variable t)

;;; Glboal Minor-mode

;;;###autoload
(define-minor-mode weather-mode
  "Toggle weather information display in mode line (weather information mode).
With a prefix argument ARG, enable weather mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t :group 'weather
  (setq weather-mode-line-string "")
  (unless global-mode-string
    (setq global-mode-string '("")))
  (if (not weather-mode)
      (setq global-mode-string
            (delq 'weather-mode-line-string global-mode-string))
    (add-to-list 'global-mode-string 'weather-mode-line-string t)
    (weather-update-info)
    (setq weather-mode-line-string
          weather-mode-line)))

(provide 'weather)
;;; weather.el ends here
