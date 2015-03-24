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

(defvar taiga-api--auth-token ""
  "The auth token returned from Taiga.")

(defvar taiga-api--cleanup-buffers t
  "Whether or not to cleanup buffers after calling API functions.")

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
    "You forgot to login")

  (define-error 'taiga-api-not-found
    "Could not find the specified resource"))

(cl-defstruct taiga-api-error type message)

(defun taiga-api-error-from-alist (alist)
  "Turn ALIST into a `taiga-api-error'."
  (make-taiga-api-error
   :type (cdr (assq '_error_type alist))
   :message (cdr (assq '_error_message alist))))

(cl-defstruct taiga-api-user-authentication
  auth-token bio is-active email github-id color default-language
  full-name-display default-timezone id full-name photo username
  big-photo)

(defun taiga-api-user-authentication-from-alist (alist)
  "Turn ALIST into a `taiga-api-user'."
  (make-taiga-api-user-authentication
   :auth-token (cdr (assq 'auth_token alist))
   :bio (cdr (assq 'bio alist))
   :is-active (cdr (assq 'is_active alist))
   :email (cdr (assq 'email alist))
   :github-id (cdr (assq 'github_id alist))
   :color (cdr (assq 'color alist))
   :default-language (cdr (assq 'default_language alist))
   :full-name-display (cdr (assq 'full_name_display alist))
   :default-timezone (cdr (assq 'default_timezone alist))
   :id (cdr (assq 'id alist))
   :full-name (cdr (assq 'full_name alist))
   :photo (cdr (assq 'photo alist))
   :username (cdr (assq 'username alist))
   :big-photo (cdr (assq 'big_photo alist))))

(cl-defstruct taiga-api-wiki-page
  html editions id version project slug content owner last-modified
  created-date modified-date watchers)

(defun taiga-api-wiki-page-from-alist (alist)
  "Turn ALIST into a `taiga-api-wiki-page'."
  (make-taiga-api-wiki-page
   :html (cdr (assq 'html alist))
   :editions (cdr (assq 'editions alist))
   :id (cdr (assq 'id alist))
   :version (cdr (assq 'version alist))
   :project (cdr (assq 'project alist))
   :slug (cdr (assq 'slug alist))
   :content (cdr (assq 'content alist))
   :owner (cdr (assq 'owner alist))
   :last-modified (cdr (assq 'last_modified alist))
   :created-date (cdr (assq 'created_date alist))
   :modified-date (cdr (assq 'modified_date alist))
   :watchers (cdr (assq 'watchers alist))))

(cl-defstruct taiga-api-user-story
  assigned-to backlog-order blocked-note blocked-note-html
  client-requirement comment created-date description description-html
  finish-date generated-from-issue id is-archived is-blocked is-closed
  kanban-order milestone milestone-name milestone-slug modified-date
  origin-issue owner points project ref sprint-order status subject tags
  team-requirement total-points version watchers)

(defun taiga-api-user-story-from-alist (alist)
  "Turn ALIST into a `taiga-api-user-story'."
  (make-taiga-api-user-story
   :assigned-to (cdr (assq 'assigned_to alist))
   :backlog-order (cdr (assq 'backlog_order alist))
   :blocked-note (cdr (assq 'blocked_note alist))
   :blocked-note-html (cdr (assq 'blocked_note_html alist))
   :client-requirement (not (eql (cdr (assq 'client_requirement alist)) :json-false))
   :comment (cdr (assq 'comment alist))
   :created-date (cdr (assq 'created_date alist))
   :description (cdr (assq 'description alist))
   :description-html (cdr (assq 'description_html alist))
   :finish-date (cdr (assq 'finish_date alist))
   :generated-from-issue (cdr (assq 'generated_from_issue alist))
   :id (cdr (assq 'id alist))
   :is-archived (not (eql (cdr (assq 'is_archived alist)) :json-false))
   :is-blocked (not (eql (cdr (assq 'is_blocked alist)) :json-false))
   :is-closed (not (eql (cdr (assq 'is_closed alist)) :json-false))
   :kanban-order (cdr (assq 'kanban_order alist))
   :milestone (cdr (assq 'milestone alist))
   :milestone-name (cdr (assq 'milestone_name alist))
   :milestone-slug (cdr (assq 'milestone_slug alist))
   :modified-date (cdr (assq 'modified_date alist))
   :origin-issue (cdr (assq 'origin_issue alist))
   :owner (cdr (assq 'owner alist))
   :points (cdr (assq 'points alist))
   :project (cdr (assq 'project alist))
   :ref (cdr (assq 'ref alist))
   :sprint-order (cdr (assq 'sprint_order alist))
   :status (cdr (assq 'status alist))
   :subject (cdr (assq 'subject alist))
   :tags (cdr (assq 'tags alist))
   :team-requirement (not (eql (cdr (assq 'team_requirement alist)) :json-false))
   :total-points (cdr (assq 'total_points alist))
   :version (cdr (assq 'version alist))
   :watchers (cdr (assq 'watchers alist))))

(cl-defstruct taiga-api-issue
  assigned-to blocked-note blocked-note-html comment created-date
  description description-html finish-date id is-blocked is-closed
  milestone modified-date finished-date owner project ref status severity
  priority type subject tags version watchers generated-user-stories
  votes neighbors)

(cl-defstruct taiga-api-neighbors next previous)

(cl-defstruct taiga-api-neighbor id ref subject)

(defun taiga-api-issue-from-alist (alist)
  "Turn ALIST into a `taiga-api-issue'."
  (make-taiga-api-issue
   :assigned-to (cdr (assq 'assigned_to alist))
   :blocked-note (cdr (assq 'blocked_note alist))
   :blocked-note-html (cdr (assq 'blocked_note_html alist))
   :comment (cdr (assq 'comment alist))
   :created-date (cdr (assq 'created_date alist))
   :description (cdr (assq 'description alist))
   :description-html (cdr (assq 'description_html alist))
   :finish-date (cdr (assq 'finish_date alist))
   :id (cdr (assq 'id alist))
   :is-blocked (not (eql (cdr (assq 'is_blocked alist)) :json-false))
   :is-closed (not (eql (cdr (assq 'is_closed alist)) :json-false))
   :milestone (cdr (assq 'milestone alist))
   :modified-date (cdr (assq 'modified_date alist))
   :finished-date (cdr (assq 'finished_date alist))
   :owner (cdr (assq 'owner alist))
   :project (cdr (assq 'project alist))
   :ref (cdr (assq 'ref alist))
   :status (cdr (assq 'status alist))
   :severity (cdr (assq 'severity alist))
   :priority (cdr (assq 'priority alist))
   :type (cdr (assq 'type alist))
   :subject (cdr (assq 'subject alist))
   :tags (cdr (assq 'tags alist))
   :version (cdr (assq 'version alist))
   :watchers (cdr (assq 'watchers alist))
   :generated-user-stories (cdr (assq 'generated_user_stories alist))
   :votes (cdr (assq 'votes alist))
   :neighbors (taiga-api-neighbors-from-alist (cdr (assq 'neighbors alist)))))

(defun taiga-api-neighbors-from-alist (alist)
  "Turn ALIST into a `taiga-api-neighbors'."
  (let ((next-alist (cdr (assq 'next alist)))
        (previous-alist (cdr (assq 'previous alist))))
    (make-taiga-api-neighbors
     :next (when next-alist (taiga-api-neighbor-from-alist next-alist))
     :previous (when previous-alist (taiga-api-neighbor-from-alist previous-alist)))))

(defun taiga-api-neighbor-from-alist (alist)
  "Turn ALIST into a `taiga-api-neighbor'."
  (make-taiga-api-neighbor
   :id (cdr (assq 'id alist))
   :ref (cdr (assq 'ref alist))
   :subject (cdr (assq 'subject alist))))

(cl-defstruct taiga-api-task
  assigned-to blocked-note blocked-note-html comment milestone-slug
  created-date description description-html id is-blocked is-closed
  milestone modified-date finished-date owner project user-story ref
  status subject tags us-order taskboard-order version is-iocaine
  external-reference watchers neighbors)

(defun taiga-api-task-from-alist (alist)
  "Turn ALIST into a `taiga-api-task'."
  (make-taiga-api-task
   :assigned-to (cdr (assq 'assigned_to alist))
   :blocked-note (cdr (assq 'blocked_note alist))
   :blocked-note-html (cdr (assq 'blocked_note_html alist))
   :comment (cdr (assq 'comment alist))
   :milestone-slug (cdr (assq 'milestone_slug alist))
   :created-date (cdr (assq 'created_date alist))
   :description (cdr (assq 'description alist))
   :description-html (cdr (assq 'description_html alist))
   :id (cdr (assq 'id alist))
   :is-blocked (not (eql (cdr (assq 'is_blocked alist)) :json-false))
   :is-closed (not (eql (cdr (assq 'is_closed alist)) :json-false))
   :milestone (cdr (assq 'milestone alist))
   :modified-date (cdr (assq 'modified_date alist))
   :finished-date (cdr (assq 'finished_date alist))
   :owner (cdr (assq 'owner alist))
   :project (cdr (assq 'project alist))
   :user-story (cdr (assq 'user_story alist))
   :ref (cdr (assq 'ref alist))
   :status (cdr (assq 'status alist))
   :subject (cdr (assq 'subject alist))
   :tags (cdr (assq 'tags alist))
   :us-order (cdr (assq 'us_order alist))
   :taskboard-order (cdr (assq 'taskboard_order alist))
   :version (cdr (assq 'version alist))
   :is-iocaine (not (eql (cdr (assq 'is_iocaine alist)) :json-false))
   :external-reference (cdr (assq 'external_reference alist))
   :watchers (cdr (assq 'watchers alist))
   :neighbors (taiga-api-neighbors-from-alist (cdr (assq 'neighbors alist)))))

(cl-defstruct taiga-api-search-result
  wikipages userstories issues tasks count)

(defun taiga-api-search-result-from-alist (alist)
  "Turn ALIST into a `taiga-api-search-result'."
  (make-taiga-api-search-result
   :wikipages (cl-coerce (mapcar #'taiga-api-wiki-page-from-alist
                                 (cdr (assq 'wikipages alist)))
                         'array)
   :userstories (cl-coerce (mapcar #'taiga-api-user-story-from-alist
                                   (cdr (assq 'userstories alist)))
                           'array)
   :issues (cl-coerce (mapcar #'taiga-api-issue-from-alist
                              (cdr (assq 'issues alist)))
                      'array)
   :tasks (cl-coerce (mapcar #'taiga-api-task-from-alist
                             (cdr (assq 'tasks alist)))
                     'array)
   :count (cdr (assq 'count alist))))

(cl-defstruct taiga-api-user-storage-data
  key value created-date modified-date)

(defun taiga-api-user-storage-data-from-alist (alist)
  "Turn ALIST into a `taiga-api-user-storage-data'."
  (make-taiga-api-user-storage-data
   :key (cdr (assq 'key alist))
   :value (cdr (assq 'value alist))
   :created-date (cdr (assq 'created_date alist))
   :modified-date (cdr (assq 'modified_date alist))))

(defun taiga-api-many-user-storage-data-from-array (array)
  "Turn ARRAY into a list of `taiga-api-user-storage-data'."
  (mapcar #'taiga-api-user-storage-data-from-alist array))

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

(defmacro taiga-api-with-request (method endpoint &rest responses)
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
                          (taiga-api--get-object #'taiga-api-error-from-alist)))))
           (when taiga-api--cleanup-buffers
             (kill-buffer)))))))

(defmacro taiga-api-with-non-get-request (method endpoint params &rest responses)
  "Prepare a request to Taiga using METHOD to ENDPOINT.

PARAMS is a list of parameter specifiers.  They can be a symbol,
in which case the parameter name will be derived from that
symbol, or a pair `(parameter-name . value)'.  RESPONSES is a
list of `(code action)' pairs which dictate how to respond to
specific HTTP status codes."
  (declare (indent 3))
  (let ((pvar (cl-gensym)))
    `(let (,pvar)
       ,@(mapcar (lambda (param) (taiga-api--make-parameter-cons param pvar))
                 params)
       (let ((url-request-data (json-encode ,pvar)))
         (taiga-api-with-request ,method ,endpoint ,@responses)))))

(defmacro taiga-api-with-post-request (endpoint params &rest responses)
  "Prepare a POST request to Taiga using HTTP to ENDPOINT.

For more information on the PARAMS and RESPONSES arguments see
`taiga-api-with-non-get-request'."
  (declare (indent 2))
  `(taiga-api-with-non-get-request "POST" ,endpoint ,params ,@responses))

(defmacro taiga-api-with-patch-request (endpoint params &rest responses)
  "Prepare a PATCH request to Taiga to ENDPOINT.

For more information on the PARAMS and RESPONSES arguments see
`taiga-api-with-non-get-request'."
  (declare (indent 2))
  `(taiga-api-with-non-get-request "PATCH" ,endpoint ,params ,@responses))

(defmacro taiga-api-with-delete-request (endpoint params &rest responses)
  "Prepare a DELETE request to Taiga using HTTP to ENDPOINT.

Form more information on the PARAMS and RESPONSES arguments see
`taiga-api-with-non-get-request'."
  (declare (indent 2))
  `(taiga-api-with-non-get-request "DELETE" ,endpoint ,params ,@responses))

(defmacro taiga-api-with-get-request (endpoint params &rest responses)
  "Prepare a GET request to Taiga using HTTP to ENDPOINT.

PARAMS is a list of parameter specifiers.  They can be a symbol,
in which case the parameter name will be derived from that
symbol, or a pair `(parameter-name . value)'.  RESPONSES is a
list of `(code action)' pairs which dictate how to respond to
specific HTTP status codes."
  (declare (indent 2))
  (let ((pvar (cl-gensym)))
    `(let ((url-request-extra-headers
            `(("Authorization" . ,(concat "Bearer " taiga-api--auth-token))))
           ,pvar)
       ,@(mapcar (lambda (param) (taiga-api--make-parameter-list param pvar))
                 params)
       (taiga-api-with-request "GET"
           (if ,pvar
               (concat ,endpoint "?" (url-build-query-string ,pvar))
             ,endpoint)
           ,@responses))))

(defun taiga-api--get-object (constructor)
  "Use CONSTRUCTOR to build an object in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward "\n\n")
    (funcall constructor (json-read))))

(defun taiga-api--check-authentication ()
  "Check that the user is authenticated, signal an error otherwise."
  (when (or (string= taiga-api--auth-token "")
            (null taiga-api--auth-token))
    (signal 'taiga-api-unauthenticated nil)))

(defun taiga-api--get-status-code ()
  "Get the HTTP status code in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^HTTP/.+ \\([0-9]+\\)" nil :noerror)
        (string-to-number (match-string 1)))))

;;; Auth

(defun taiga-api-normal-login (username password)
  "Login a user USERNAME using PASSWORD."
  (taiga-api-with-post-request
      "auth" (("type" . "normal") username password)
    (200
     (let ((user (taiga-api--get-object
                  #'taiga-api-user-authentication-from-alist)))
       (setq taiga-api--auth-token
             (taiga-api-user-authentication-auth-token user))
       user))
    (400 (signal 'taiga-api-login-failed
                 (taiga-api--get-object #'taiga-api-error-from-alist)))))

(defun taiga-api-github-login (code &optional token)
  "Login a user through github using CODE.

TOKEN can be used to accept an invitation to a project."
  (taiga-api-with-post-request
      "auth" (("type" . "github") code token)
    (200
     (let ((user (taiga-api--get-object
                  #'taiga-api-user-authentication-from-alist)))
       (setq taiga-api--auth-token
             (taiga-api-user-authentication-auth-token user))
       user))
    (400 (signal 'taiga-api-login-failed
                 (taiga-api--get-object #'taiga-api-error-from-alist)))))

(defun taiga-api-register-public (username password email full-name)
  "Register a user without invitation on Taiga.

USERNAME is the username with which you would like to log in.
PASSWORD is the password you would like to use.  EMAIL is your
email address.  FULL-NAME is your full name."
  (taiga-api-with-post-request
      "auth/register"
      (("type" . "public") username password email full-name)
    (201
     (let ((user (taiga-api--get-object
                  #'taiga-api-user-authentication-from-alist)))
       (setq taiga-api--auth-token
             (taiga-api-user-authentication-auth-token user))
       user))
    (400 (signal 'taiga-api-registration-failed
                 (taiga-api--get-object #'taiga-api-error-from-alist)))))

(defun taiga-api-register-private
    (existing token username password &optional email full-name)
  "Add a user to a project through an invitation.

EXISTING indicates whether or not the user is already a member on
the Taiga instance.  TOKEN is the token generated through the
invitation.  USERNAME is the user's username.  PASSWORD is the
user's password.  EMAIL is the user's email address, this is only
required if EXISTING is nil.  FULL-NAME is the user's full name
and also only required if EXISTING is nil."
  (taiga-api-with-post-request
      "auth/register"
      (("type" . "private")
       existing token username password email full-name)
    (201
     (let ((user (taiga-api--get-object
                  #'taiga-api-user-authentication-from-alist)))
       (setq taiga-api--auth-token
             (taiga-api-user-authentication-auth-token user))
       user))
    (400 (signal 'taiga-api-registration-failed
                 (taiga-api--get-object #'taiga-api-error-from-alist)))))

;;; Resolver

(defun taiga-api-resolve-project (project)
  "Get the ID of a project identified by the slug PROJECT."
  (taiga-api-resolve project))

(defun taiga-api-resolve-user-story (project us)
  "Search PROJECT for the id of a user story with number US.

PROJECT should be the slug of a project, US is the number of the
story within the project."
  (taiga-api-resolve project :us us))

(defun taiga-api-resolve-issue (project issue)
  "Search PROJECT for the id of an issue with number ISSUE.

PROJECT should be the slug of a project, ISSUE is the number of
the issue within the project."
  (taiga-api-resolve project :issue issue))

(defun taiga-api-resolve-task (project task)
  "Search PROJECT for the id of a task with number TASK.

PROJECT should be the slug of a project, TASK is the number of a
task within the project."
  (taiga-api-resolve project :task task))

(defun taiga-api-resolve-milestone (project milestone)
  "Search PROJECT for the id of a milestone with slug MILESTONE.

PROJECT and MILESTONE should be the slugs of a project and
milestone/sprint respectively."
  (taiga-api-resolve project :milestone milestone))

(defun taiga-api-resolve-wiki (project wikipage)
  "Search PROJECT for the id of a milestone with slug WIKIPAGE.

PROJECT and WIKIPAGE should be the slugs of a project and wiki
page respectively."
  (taiga-api-resolve project :wikipage wikipage))

(cl-defun taiga-api-resolve
    (project &key us issue task milestone wikipage)
  "Search PROJECT for any of the given parameters.

PROJECT should be the slug of a project.  US, ISSUE and TASK
should be the number of a user story, issue or task within the
project.  MILESTONE and WIKIPAGE should be slugs for a
milestone/sprint or wiki page."
  (taiga-api--check-authentication)
  (taiga-api-with-get-request "resolver"
      (project us issue task milestone wikipage)
    (200 (taiga-api--get-object #'identity))
    (404 (signal 'taiga-api-unresolved
                 (taiga-api--get-object #'taiga-api-error-from-alist)))))

;;; Search

(defun taiga-api-search (project text)
  "Search PROJECT for any story, issue or task containing TEXT."
  (taiga-api--check-authentication)
  (taiga-api-with-get-request "search" (project text)
    (200 (taiga-api--get-object #'taiga-api-search-result-from-alist))
    (404 (signal 'taiga-api-not-found
                 (taiga-api--get-object #'taiga-api-error-from-alist)))))

;;; User story

(defun taiga-api-list-user-storage ()
  "List all the user's stored values."
  (taiga-api--check-authentication)
  (taiga-api-with-get-request "user-storage" ()
    (200 (taiga-api--get-object #'taiga-api-many-user-storage-data-from-array))
    (404 (signal 'taiga-api-not-found
                 (taiga-api--get-object #'taiga-api-error-from-alist)))))

(defun taiga-api-create-user-storage (key value)
  "Create a new user storage with key KEY and value VALUE."
  (taiga-api--check-authentication)
  (let ((url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer " taiga-api--auth-token)))))
   (taiga-api-with-post-request "user-storage" (key value)
     (201 (taiga-api--get-object #'taiga-api-user-storage-data-from-alist)))))

(defun taiga-api-get-user-storage (key)
  "Get user storage data identified by KEY."
  (taiga-api--check-authentication)
  (let ((url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer " taiga-api--auth-token))))
        (endpoint (concat "user-storage/" (url-encode-url key))))
    (taiga-api-with-get-request endpoint ()
      (200 (taiga-api--get-object #'taiga-api-user-storage-data-from-alist))
      (404 (signal 'taiga-api-not-found
                   (taiga-api--get-object #'taiga-api-error-from-alist))))))

(defun taiga-api-edit-user-storage (key value)
  "Replace the user storage data identified by KEY with VALUE."
  (taiga-api--check-authentication)
  (let ((url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer " taiga-api--auth-token))))
        (endpoint (concat "user-storage/" (url-encode-url key))))
    (taiga-api-with-patch-request endpoint (value)
      (200 (taiga-api--get-object #'taiga-api-user-storage-data-from-alist))
      (404 (signal 'taiga-api-not-found
                   (taiga-api--get-object #'taiga-api-error-from-alist))))))

(defun taiga-api-delete-user-storage (key)
  "Delete the user storage data stored under KEY."
  (taiga-api--check-authentication)
  (let ((url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer " taiga-api--auth-token))))
        (endpoint (concat "user-storage/" (url-encode-url key))))
    (taiga-api-with-delete-request endpoint ()
      (204 t)
      (404 (signal 'taiga-api-not-found
                   (taiga-api--get-object #'taiga-api-error-from-alist))))))

(provide 'taiga-api)
;;; taiga-api.el ends here
