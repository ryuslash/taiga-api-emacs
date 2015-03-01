;;; taiga-api.el --- Taiga API client for Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Tom Willemse

;; Author: Tom Willemse <tom@ryuslash.org>
;; Keywords: comm
;; Version: 0.1.0

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

(defvar *taiga-api--auth-token* ""
  "The auth token returned from Taiga.")

(when (fboundp 'define-error)
  (define-error 'taiga-api-login-failed
    "Could not login to your Taiga instance")

  (define-error 'taiga-api-registration-failed
    "Could not register at your selected Taiga instance")

  (define-error 'taiga-api-throttled
    "Your API connection has been throttled")

  (define-error 'taiga-api-unresolved
    "Could not resolve the object")

  (define-error 'taiga-api-unauthenticated
    "You forgot to login"))

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

(eval-when-compile
  (defun taiga-api--make-parameter-cons (param pvar)
    "Turn PARAM into a cons and join it to PVAR."
    (if (symbolp param)
        (let ((paramname
               (replace-regexp-in-string "-" "_" (symbol-name param))))
         `(when ,param
            (setq ,pvar (cons (cons ,paramname ,param) ,pvar))))
      `(setq ,pvar (cons ',param ,pvar))))

  (defun taiga-api--make-parameter-list (param pvar)
    "Turn PARAM into a list and join it to PVAR."
    (if (symbolp param)
        (let ((paramname
               (replace-regexp-in-string "-" "_" (symbol-name param))))
          `(when ,param
             (setq ,pvar (append ,pvar (list (list ,paramname ,param))))))
      `(setq ,pvar (append ,pvar (list (list ,(car param) ,(cdr param))))))))

(defmacro with-taiga-api-request (method endpoint &rest responses)
  "Prepare a request to Taiga using HTTP method METHOD to ENDPOINT.

RESPONSES is a list of `(code action)' pairs which dictate how to
respond to specific HTTP status codes."
  (declare (indent 3))
  (let ((svar (cl-gensym)))
    `(let ((url-request-extra-headers
            (append url-request-extra-headers
                    '(("Content-Type" . "application/json"))))
           (url-request-method ,method))
       (with-current-buffer
           (url-retrieve-synchronously
            (concat taiga-api-url "api/v1/" ,endpoint))
         (unwind-protect
             (let ((,svar (taiga-api--get-status-code)))
               (cl-ecase ,svar
                 ,@responses
                 (429
                  (signal 'taiga-api-throttled
                          (taiga-api--get-object #'taiga-error-from-alist)))))
           (kill-buffer))))))

(defmacro with-taiga-api-post-request (endpoint params &rest responses)
  "Prepare a POST request to Taiga using HTTP to ENDPOINT.

PARAMS is a list of parameter specifications.  They can either be
a symbol, in which case the parameter name will be derived from
that symbol, or a pair `(parameter-name . value)'.  RESPONSES is
a list of `(code action)' pairs which dictate how to respond to
specific HTTP status code."
  (declare (indent 2))
  (let ((pvar (cl-gensym)))
    `(let (,pvar)
       ,@(mapcar (lambda (param) (taiga-api--make-parameter-cons param pvar))
                 params)
       (let ((url-request-data (json-encode ,pvar)))
         (with-taiga-api-request "POST" ,endpoint ,@responses)))))

(defmacro with-taiga-api-get-request (endpoint params &rest responses)
  "Prepare a GET request to Taiga using HTTP to ENDPOINT.

PARAMS is a list of parameter specifiers.  They can be a symbol,
in which case the parameter name will be derived from that
symbol, or a pair `(parameter-name . value)'.  RESPONSES is a
list of `(code action)' pairs which dictate how to respond to
specific HTTP status codes."
  (declare (indent 2))
  (let ((pvar (cl-gensym)))
    `(let ((url-request-extra-headers
            `(("Authorization" . ,(concat "Bearer " *taiga-api--auth-token*))))
           ,pvar)
       ,@(mapcar (lambda (param) (taiga-api--make-parameter-list param pvar))
                 params)
       (with-taiga-api-request "GET"
           (concat ,endpoint "?" (url-build-query-string ,pvar))
           ,@responses))))

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

;;; Auth

(defun taiga-api-normal-login (username password)
  "Login a user USERNAME using PASSWORD."
  (with-taiga-api-post-request
      "auth" (("type" . "normal") username password)
    (200
     (let ((user (taiga-api--get-object#'taiga-user-from-alist)))
       (setq *taiga-api--auth-token* (taiga-user-auth-token user))
       user))
    (400 (signal 'taiga-api-login-failed
                 (taiga-api--get-object #'taiga-error-from-alist)))))

(defun taiga-api-github-login (code &optional token)
  "Login a user through github using CODE.

TOKEN can be used to accept an invitation to a project."
  (with-taiga-api-post-request
      "auth" (("type" . "github") code token)
    (200
     (let ((user (taiga-api--get-object #'taiga-user-from-alist)))
       (setq *taiga-api--auth-token* (taiga-user-auth-token user))
       user))
    (400 (signal 'taiga-api-login-failed
                 (taiga-api--get-object #'taiga-user-from-alist)))))

(defun taiga-api-register-public (username password email full-name)
  "Register a user without invitation on Taiga.

USERNAME is the username with which you would like to log in.
PASSWORD is the password you would like to use.  EMAIL is your
email address.  FULL-NAME is your full name."
  (with-taiga-api-post-request
      "auth/register"
      (("type" . "public") username password email full-name)
    (201
     (let ((user (taiga-api--get-object #'taiga-user-from-alist)))
       (setq *taiga-api--auth-token* (taiga-user-auth-token user))
       user))
    (400 (signal 'taiga-api-registration-failed
                 (taiga-api--get-object #'taiga-error-from-alist)))))

(defun taiga-api-register-private
    (existing token username password &optional email full-name)
  "Add a user to a project through an invitation.

EXISTING indicates whether or not the user is already a member on
the Taiga instance.  TOKEN is the token generated through the
invitation.  USERNAME is the user's username.  PASSWORD is the
user's password.  EMAIL is the user's email address, this is only
required if EXISTING is nil.  FULL-NAME is the user's full name
and also only required if EXISTING is nil."
  (with-taiga-api-post-request
      "auth/register"
      (("type" . "private")
       existing token username password email full-name)
    (201
     (let ((user (taiga-api--get-object #'taiga-user-from-alist)))
       (setq *taiga-api--auth-token* (taiga-user-auth-token user))
       user))
    (400 (signal 'taiga-api-registration-failed
                 (taiga-api--get-object #'taiga-error-from-alist)))))

;;; Resolver

(defun taiga-api-resolve-project (project)
  "Get the ID of a project identified by the slug PROJECT."
  (unless (not (string= *taiga-api--auth-token* ""))
    (signal 'taiga-api-unauthenticated nil))

  (with-taiga-api-get-request "resolver" (project)
    (200
     (goto-char (point-min))
     (search-forward "\n\n")
     (json-read))
    (404 (signal 'taiga-api-unresolved
                 (taiga-api--get-object #'taiga-error-from-alist)))))

(defun taiga-api-resolve-user-story (project us)
  "Search PROJECT for the id of a user story with number US.

PROJECT should be the slug of a project, US is the number of the
story within the project."
  (unless (not (string= *taiga-api--auth-token* ""))
    (signal 'taiga-api-unauthenticated nil))

  (with-taiga-api-get-request "resolver" (project us)
    (200
     (goto-char (point-min))
     (search-forward "\n\n")
     (json-read))
    (404 (signal 'taiga-api-unresolved
                 (taiga-api--get-object #'taiga-error-from-alist)))))

(defun taiga-api-resolve-issue (project issue)
  "Search PROJECT for the id of an issue with number ISSUE.

PROJECT should be the slug of a project, ISSUE is the number of
the issue within the project."
  (unless (not (string= *taiga-api--auth-token* ""))
    (signal 'taiga-api-unauthenticated nil))

  (with-taiga-api-get-request "resolver" (project issue)
    (200
     (goto-char (point-min))
     (search-forward "\n\n")
     (json-read))
    (404 (signal 'taiga-api-unresolved
                 (taiga-api--get-object #'taiga-error-from-alist)))))

(provide 'taiga-api)
;;; taiga-api.el ends here
