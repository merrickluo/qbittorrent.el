;;; qbittorrent-api.el --- An api client for qBittorrent WebUI API -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 A.I.
;;
;; Author: A.I. <merrick@luois.me>
;; Maintainer: A.I. <merrick@luois.me>
;; Created: October 07, 2022
;; Modified: October 07, 2022
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3") (plz "0.2.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; API client for qBittorrent WebUI APIs.
;;
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'plz)

(cl-defstruct qbittorrent-api-session
  baseurl cookie)

(defun qbittorrent-api--signal-error (plz-err)
  "Signal an error for PLZ-ERR.
Borrowed from https://github.com/alphapapa/ement.el"
  (pcase-let* (((cl-struct plz-error response
                           (message plz-message) (curl-error `(,curl-exit-code . ,curl-message)))
                plz-err)
               (status (when (plz-response-p response)
                         (plz-response-status response)))
               (body (when (plz-response-p response)
                       (plz-response-body response)))
               (json-object (when body
                              (ignore-errors
                                (json-read-from-string body))))
               (error-message (format "%S: %s"
                                      (or curl-exit-code status)
                                      (or (when json-object
                                            (alist-get 'error json-object))
                                          curl-message
                                          plz-message))))

    (signal 'qbittorrent-api-error (list error-message))))

(defun qbittorrent-api-login (session &optional username password)
  "Login to a qBittorrent instance at SESSION.
USERNAME and PASSWORD can be null if in trusted LAN"
  (let* ((url (format "%s/api/v2/auth/login" (qbittorrent-api-session-baseurl session)))
         (body (format "\nusername=%s&password=%s\n" username password))
         (resp (plz 'post url
                 :headers '(("Content-Type" . "application/x-www-form-urlencoded; charset=utf-8"))
                 :as 'response
                 :body body
                 :else #'qbittorrent-api--signal-error)))
    (when resp
      (let ((set-cookie (alist-get 'set-cookie (plz-response-headers resp))))
        (if set-cookie
            (setf (qbittorrent-api-session-cookie session) (car (string-split set-cookie ";")))
          (error "Login failed, check your username and password"))))
    session))

(cl-defun qbittorrent-api (session path &key (method 'get) (then 'sync))
  "Call api at PATH in SESSION."
  (let* ((url (format "%s%s" (qbittorrent-api-session-baseurl session) path)))
    (plz method url
      :as 'json-read
      :headers `(("Cookie" . ,(qbittorrent-api-session-cookie session)))
      :else #'qbittorrent-api--signal-error
      :then then)))

(provide 'qbittorrent-api)
;;; qbittorrent-api.el ends here
