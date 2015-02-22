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
(require 'cl-lib)

(defgroup taiga-api nil
  "Customization group for Taiga API"
  :group 'applications)

(defcustom taiga-api-url "https://api.taiga.io/"
  "The URL of your taiga instance."
  :group 'taiga-api
  :type 'string)

(when (fboundp 'define-error)
  (define-error 'taiga-api-login-failed
    "Could not login to your Taiga instance")

  (define-error 'taiga-api-registration-failed
    "Could not register at your selected Taiga instance")

  (define-error 'taiga-api-throttled
    "Your API connection has been throttled"))

(unless (fboundp 'alist-get)
  ;; Copied from subr.el in Emacs 25.0.50.1 (from 2015-02-15)
  (defun alist-get (key alist &optional default remove)
    "Get the value associated to KEY in ALIST.
DEFAULT is the value to return if KEY is not found in ALIST.
REMOVE, if non-nil, means that when setting this element, we should
remove the entry if the new value is `eql' to DEFAULT."
    (ignore remove) ;;Silence byte-compiler.
    (let ((x (assq key alist)))
      (if x (cdr x) default))))

(cl-defstruct taiga-error type message)

(defun taiga-error-from-alist (alist)
  "Turn ALIST into a `taiga-error'."
  (make-taiga-error
   :type (alist-get '_error_type alist)
   :message (alist-get '_error_message alist)))

(cl-defstruct taiga-user
  auth-token bio is-active email github-id color default-language
  full-name-display default-timezone id full-name photo username
  big-photo)

(defun taiga-user-from-alist (alist)
  "Turn ALIST into a `taiga-user'."
  (make-taiga-user
   :auth-token (alist-get 'auth_token alist)
   :bio (alist-get 'bio alist)
   :is-active (alist-get 'is_active alist)
   :email (alist-get 'email alist)
   :github-id (alist-get 'github_id alist)
   :color (alist-get 'color alist)
   :default-language (alist-get 'default_language alist)
   :full-name-display (alist-get 'full_name_display alist)
   :default-timezone (alist-get 'default_timezone alist)
   :id (alist-get 'id alist)
   :full-name (alist-get 'full_name alist)
   :photo (alist-get 'photo alist)
   :username (alist-get 'username alist)
   :big-photo (alist-get 'big_photo alist)))

(defun taiga-api--get-object (constructor)
  "Use CONSTRUCTOR to build an object in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward "\n\n")
    (funcall constructor (json-read))))

(defun taiga-api--get-status-code ()
  "Get the HTTP status code in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^HTTP/.+ \\([0-9]+\\)" nil :noerror)
        (string-to-number (match-string 1)))))

(defun taiga-api-normal-login (username password)
  "Login a user USERNAME using PASSWORD."
  (let ((url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-method "POST")
        (url-request-data (json-encode `(("type" . "normal")
                                         ("username" . ,username)
                                         ("password" . ,password)))))
    (with-current-buffer
        (url-retrieve-synchronously (concat taiga-api-url "api/v1/auth"))
      (let ((status (taiga-api--get-status-code)))
        (unwind-protect
            (cl-case status
              (200
               (taiga-api--get-object #'taiga-user-from-alist))
              (400
               (signal 'taiga-api-login-failed
                       (taiga-api--get-object #'taiga-error-from-alist)))
              (429
               (signal 'taiga-api-throttled
                       (taiga-api--get-object #'taiga-error-from-alist))))
          (kill-buffer))))))

(defun taiga-api-github-login (code &optional token)
  "Login a user through github using CODE.

TOKEN can be used to accept an invitation to a project."
  (let ((params `(("type" . "github")
                  ("code" . ,code))))
    (when token (setq params (cons `("token" . ,token) params)))
    (let ((url-request-extra-headers '(("Content-Type" . "application/json")))
          (url-request-method "POST")
          (url-request-data (json-encode params)))
      (with-current-buffer
          (url-retrieve-synchronously (concat taiga-api-url "api/v1/auth"))
        (unwind-protect
            (let ((status (taiga-api--get-status-code)))
              (cl-ecase status
                (200 (taiga-api--get-object #'taiga-user-from-alist))
                (400
                 (signal 'taiga-api-login-failed
                         (taiga-api--get-object #'taiga-error-from-alist)))
                (429
                 (signal 'taiga-api-throttled
                         (taiga-api--get-object #'taiga-error-from-alist)))))
          (kill-buffer))))))

(defun taiga-api-register-public (username password email full-name)
  "Register a user without invitation on Taiga.

USERNAME is the username with which you would like to log in.
PASSWORD is the password you would like to use.  EMAIL is your
email address.  FULL-NAME is your full name."
  (let ((params `(("type" . "public")
                  ("username" . ,username)
                  ("password" . ,password)
                  ("email" . ,email)
                  ("full_name" . ,full-name))))
    (let ((url-request-extra-headers '(("Content-Type" . "application/json")))
          (url-request-method "POST")
          (url-request-data (json-encode params)))
      (with-current-buffer
          (url-retrieve-synchronously (concat taiga-api-url "api/v1/auth/register"))
        (unwind-protect
            (let ((status (taiga-api--get-status-code)))
              (cl-ecase status
                (201 (taiga-api--get-object #'taiga-user-from-alist))
                (400
                 (signal 'taiga-api-registration-failed
                         (taiga-api--get-object #'taiga-error-from-alist)))
                (429
                 (signal 'taiga-api-throttled
                         (taiga-api--get-object #'taiga-error-from-alist)))))
          (kill-buffer))))))

(provide 'taiga-api)
;;; taiga-api.el ends here
