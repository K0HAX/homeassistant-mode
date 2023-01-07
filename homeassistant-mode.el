(defgroup homeassistant-helm nil
  "Home Assistant control for Helm"
  :group 'local)

(defcustom mje/homeassistant-url nil
           "The URL to Home Assistant"
           :group 'homeassistant-helm
           :type 'string)

(defcustom mje/homeassistant-api-token nil
           "Home Assistant API Bearer Token"
           :group 'homeassistant-helm
           :type 'string)

(defun mje/helm-homeassistant-format-data (data)
  (let ((tmp-data data))
    (setq sample-results tmp-data)))

(defun mje/helm-homeassistant-get-states ()
  (request
   (format "%s/api/states" mje/homeassistant-url)
   :parser 'json-read
   :success (cl-function
	     (lambda (&key data &allow-other-keys)
	       (mje/helm-homeassistant-format-data data)))
   :headers '(("Content-Type" . "application/json")("Authorization" . (format "Bearer %s" mje/homeassistant-api-token)))))

(defun mje/helm-homeassistant-turn-on (device)
  (cond
   ((string-match "^switch\." (cdr (assoc 'entity_id device)))
    (request
     (format "%s/api/services/switch/turn_on" mje/homeassistant-url)
     :type "POST"
     :data (json-encode `(("entity_id" . ,(cdr (assoc 'entity_id device)))))
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys)
		  (message "Got error: %S" error-thrown)))
     :headers '(("Content-Type" . "application/json")("Authorization" . (format "Bearer %s" mje/homeassistant-api-token))))
   ((string-match "^light\." (cdr (assoc 'entity_id device)))
    (request
     (format "%s/api/services/light/turn_on" mje/homeassistant-url)
     :type "POST"
     :data (json-encode `(("entity_id" . ,(cdr (assoc 'entity_id device)))))
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys)
		  (message "Got error: %S" error-thrown)))
     :headers '(("Content-Type" . "application/json")("Authorization" . (format "Bearer %s" mje/homeassistant-api-token))))
   ((string-match "^group\." (cdr (assoc 'entity_id device)))
    (request
     (format "%s/api/services/light/turn_on" mje/homeassistant-url)
     :type "POST"
     :data (json-encode `(("entity_id" . ,(cdr (assoc 'entity_id device)))))
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys)
		  (message "Got error: %S" error-thrown)))
     :headers '(("Content-Type" . "application/json")("Authorization" . (format "Bearer %s" mje/homeassistant-api-token)))))))))

(defun mje/helm-homeassistant-turn-off (device)
  (request
   (format "%s/api/services/light/turn_off" mje/homeassistant-url)
   :type "POST"
   :data (json-encode `(("entity_id" . ,(cdr (assoc 'entity_id device)))))
   :error
   (cl-function (lambda (&key error-thrown &allow-other-keys)
		(message "Got error: %S" error-thrown)))
   :headers '(("Content-Type" . "application/json")("Authorization" . (format "Bearer %s" mje/homeassistant-api-token)))))

(defun mje/helm-format-homeassistant-for-display (device)
  (cdr (assoc 'friendly_name (assoc 'attributes device))))

(defun mje/helm-homeassistant-search ()
  (mapcar (lambda (device)
	    (cons (mje/helm-format-homeassistant-for-display device)
		  device))
	  sample-results))

(defun mje/helm-homeassistant-actions (actions device)
  `((,(format "Turn on")  . mje/helm-homeassistant-turn-on)
    (,(format "Turn off") . mje/helm-homeassistant-turn-off)))

(defvar mje/helm-source-homeassistant
  '((name . "Home Assistant")
    (candidates-process . mje/helm-homeassistant-search)
    (action-transformer . mje/helm-homeassistant-actions)))

(defun helm-homeassistant ()
  (interactive)
  (mje/helm-homeassistant-get-states)
  (helm :sources '(mje/helm-source-homeassistant)))

(provide 'helm-homeassistant)

