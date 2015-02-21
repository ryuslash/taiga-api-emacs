;;; taiga-api.el --- Taiga API client for Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Tom Willemse

;; Author: Tom Willemse <tom@ryuslash.org>
;; Keywords: comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Implementation of the Taiga API in Emacs Lisp.

;;; Code:

(require 'json)
(require 'url)

(defgroup taiga-api nil
  "Customization group for Taiga API"
  :group 'applications)

(defcustom taiga-api-url "https://api.taiga.io/"
  "The URL of your taiga instance."
  :group 'taiga-api
  :type 'string)

(define-error 'taiga-api-login-failed
  "Could not login to your Taiga instance.")

(cl-defstruct (taiga-error
               (:constructor taiga-error-from-alist
                             (alist
                              &aux
                              (type (alist-get '_error_type alist))
                              (message (alist-get '_error_message alist)))))
  type message)

(cl-defstruct (taiga-user
               (:constructor taiga-user-from-alist
                             (alist
                              &aux
                              (auth-token (alist-get 'auth_token alist))
                              (bio (alist-get 'bio alist))
                              (is-active (alist-get 'is_active alist))
                              (email (alist-get 'email alist))
                              (github-id (alist-get 'github_id alist))
                              (color (alist-get 'color alist))
                              (default-language (alist-get 'default_language alist))
                              (full-name-display (alist-get 'full_name_display alist))
                              (default-timezone (alist-get 'default_timezone alist))
                              (id (alist-get 'id alist))
                              (full-name (alist-get 'full_name alist))
                              (photo (alist-get 'photo alist))
                              (username (alist-get 'username alist))
                              (big-photo (alist-get 'big_photo alist)))))
  auth-token bio is-active email github-id color default-language
  full-name-display default-timezone id full-name photo username
  big-photo)

(defun taiga-api--get-object (constructor)
  "Use CONSTRUCTOR to build an object in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward "\n\n")
    (funcall constructor (json-read))))

(defun taiga-api-normal-login (username password)
  "Login a user USERNAME using PASSWORD."
  (let ((url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-method "POST")
        (url-request-data (json-encode `(("type" . "normal")
                                         ("username" . ,username)
                                         ("password" . ,password)))))
    (with-current-buffer
        (url-retrieve-synchronously (concat taiga-api-url "api/v1/auth"))
      (goto-char (point-min))
      (re-search-forward "^HTTP/.+ \\([0-9]+\\)")
      (if (string= (match-string 1) "200")
          (taiga-api--get-object #'taiga-user-from-alist)
        (signal 'taiga-api-login-failed
                (taiga-api--get-object #'taiga-error-from-alist))))))

(provide 'taiga-api)
;;; taiga-api.el ends here
