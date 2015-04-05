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
    "Could not find the specified resource")

  (define-error 'taiga-api-error
    "An error occurred"))

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

(cl-defstruct taiga-api-project-template
  default-options us-statuses points task-statuses issue-statuses
  issue-types priorities severities roles id name slug description
  created-date modified-date default-owner-role is-backlog-activated
  is-kanban-activated is-wiki-activated is-issues-activated
  videoconferences videoconferences-salt)

(cl-defstruct taiga-api-project-template-options
  us-status points priority severity task-status issue-type issue-status)

(cl-defstruct taiga-api-project-template-user-story-status
  wip-limit color name slug order is-closed)

(cl-defstruct taiga-api-project-template-point value name order)

(cl-defstruct taiga-api-project-template-status
  color name slug order is-closed)

(cl-defstruct taiga-api-project-template-thingy color name order)

(cl-defstruct taiga-api-project-template-role
  permissions order computable slug name)

(defun taiga-api-project-template-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-template'."
  (make-taiga-api-project-template
   :default-options (taiga-api-project-template-options-from-alist
                     (cdr (assq 'default_options alist)))
   :us-statuses (taiga-api-many-project-template-user-story-status-from-array
                 (cdr (assq 'us_statuses alist)))
   :points (taiga-api-many-project-template-point-from-array
            (cdr (assq 'points alist)))
   :task-statuses (taiga-api-many-project-template-status-from-array
                   (cdr (assq 'task_statuses alist)))
   :issue-statuses (taiga-api-many-project-template-status-from-array
                    (cdr (assq 'issue_statuses alist)))
   :issue-types (taiga-api-many-project-template-thingy-from-array
                 (cdr (assq 'issue_types alist)))
   :priorities (taiga-api-many-project-template-thingy-from-array
                (cdr (assq 'priorities alist)))
   :severities (taiga-api-many-project-template-thingy-from-array
                (cdr (assq 'severities alist)))
   :roles (taiga-api-many-project-template-role-from-array
           (cdr (assq 'roles alist)))
   :id (cdr (assq 'id alist))
   :name (cdr (assq 'name alist))
   :slug (cdr (assq 'slug alist))
   :description (cdr (assq 'description alist))
   :created-date (cdr (assq 'created_date alist))
   :modified-date (cdr (assq 'modified_date alist))
   :default-owner-role (cdr (assq 'default_owner_role alist))
   :is-backlog-activated (not (eql (cdr (assq 'is_backlog_activated alist)) :json-false))
   :is-kanban-activated (not (eql (cdr (assq 'is_kanban_activated alist)) :json-false))
   :is-wiki-activated (not (eql (cdr (assq 'is_wiki_activated alist)) :json-false))
   :is-issues-activated (not (eql (cdr (assq 'is_issues_activated alist)) :json-false))
   :videoconferences (cdr (assq 'videoconferences alist))
   :videoconferences-salt (cdr (assq 'videoconferences_salt alist))))

(defun taiga-api-many-project-template-from-array (array)
  "Turn ARRAY into a list of `taiga-api-project-template'."
  (mapcar #'taiga-api-project-template-from-alist array))

(defun taiga-api-project-template-options-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-template-options'."
  (make-taiga-api-project-template-options
   :us-status (cdr (assq 'us_status alist))
   :points (cdr (assq 'points alist))
   :priority (cdr (assq 'priority alist))
   :severity (cdr (assq 'severity alist))
   :task-status (cdr (assq 'task_status alist))
   :issue-type (cdr (assq 'issue_type alist))
   :issue-status (cdr (assq 'issue_status alist))))

(defun taiga-api-project-template-options-to-alist (options)
  "Turn OPTIONS into an alist."
  (list (cons 'us_status (taiga-api-project-template-options-us-status options))
        (cons 'points (taiga-api-project-template-options-points options))
        (cons 'priority (taiga-api-project-template-options-priority options))
        (cons 'severity (taiga-api-project-template-options-severity options))
        (cons 'task_status (taiga-api-project-template-options-task-status options))
        (cons 'issue_type (taiga-api-project-template-options-issue-type options))
        (cons 'issue_status (taiga-api-project-template-options-issue-status options))))

(defun taiga-api-project-template-user-story-status-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-template-user-story-status'."
  (make-taiga-api-project-template-user-story-status
   :wip-limit (cdr (assq 'wip_limit alist))
   :color (cdr (assq 'color alist))
   :name (cdr (assq 'name alist))
   :slug (cdr (assq 'slug alist))
   :order (cdr (assq 'order alist))
   :is-closed (not (eql (cdr (assq 'is_closed alist)) :json-false))))

(defun taiga-api-many-project-template-user-story-status-from-array (array)
  "Turn ARRAY into a list of `taiga-api-project-template-user-story-status'."
  (mapcar #'taiga-api-project-template-user-story-status-from-alist array))

(defun taiga-api-project-template-user-story-status-to-alist (status)
  "Turn STATUS into an alist."
  (list (cons 'wip_limit (taiga-api-project-template-user-story-status-wip-limit status))
        (cons 'color (taiga-api-project-template-user-story-status-color status))
        (cons 'name (taiga-api-project-template-user-story-status-name status))
        (cons 'slug (taiga-api-project-template-user-story-status-slug status))
        (cons 'order (taiga-api-project-template-user-story-status-order status))
        (cons 'is_closed (or (taiga-api-project-template-user-story-status-is-closed status) :json-false))))

(defun taiga-api-project-template-point-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-template-point'."
  (make-taiga-api-project-template-point
   :value (cdr (assq 'value alist))
   :name (cdr (assq 'name alist))
   :order (cdr (assq 'order alist))))

(defun taiga-api-many-project-template-point-from-array (array)
  "Turn ARRAY into a list of `taiga-api-project-template-point'."
  (mapcar #'taiga-api-project-template-point-from-alist array))

(defun taiga-api-project-template-point-to-alist (point)
  "Turn POINT into an alist."
  (list (cons 'value (taiga-api-project-template-point-value point))
        (cons 'name (taiga-api-project-template-point-name point))
        (cons 'order (taiga-api-project-template-point-order point))))

(defun taiga-api-project-template-status-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-template-status'."
  (make-taiga-api-project-template-status
   :color (cdr (assq 'color alist))
   :name (cdr (assq 'name alist))
   :slug (cdr (assq 'slug alist))
   :order (cdr (assq 'order alist))
   :is-closed (not (eql (cdr (assq 'is_closed alist)) :json-false))))

(defun taiga-api-many-project-template-status-from-array (array)
  "Turn ARRAY into a list of `taiga-api-project-template-status'."
  (mapcar #'taiga-api-project-template-status-from-alist array))

(defun taiga-api-project-template-status-to-alist (status)
  "Turn STATUS into an alist."
  (list (cons 'color (taiga-api-project-template-status-color status))
        (cons 'name (taiga-api-project-template-status-name status))
        (cons 'slug (taiga-api-project-template-status-slug status))
        (cons 'order (taiga-api-project-template-status-order status))
        (cons 'is_closed (or (taiga-api-project-template-status-is-closed status) :json-false))))

(defun taiga-api-project-template-thingy-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-template-thingy'."
  (make-taiga-api-project-template-thingy
   :color (cdr (assq 'color alist))
   :name (cdr (assq 'name alist))
   :order (cdr (assq 'order alist))))

(defun taiga-api-project-template-thingy-to-alist (thingy)
  "Turn THINGY into an alist."
  (list (cons 'color (taiga-api-project-template-thingy-color thingy))
        (cons 'name (taiga-api-project-template-thingy-name thingy))
        (cons 'order (taiga-api-project-template-thingy-order thingy))))

(defun taiga-api-many-project-template-thingy-from-array (array)
  "Turn ARRAY into a list of `taiga-api-project-template-thingy'."
  (mapcar #'taiga-api-project-template-thingy-from-alist array))

(defun taiga-api-project-template-role-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-template-role'."
  (make-taiga-api-project-template-role
   :permissions (cdr (assq 'permissions alist))
   :order (cdr (assq 'order alist))
   :computable (cdr (assq 'computable alist))
   :slug (cdr (assq 'slug alist))
   :name (cdr (assq 'name alist))))

(defun taiga-api-project-template-role-to-alist (role)
  "Turn ROLE into an alist."
  (list (cons 'permissions (taiga-api-project-template-role-permissions role))
        (cons 'order (taiga-api-project-template-role-order role))
        (cons 'computable (or (taiga-api-project-template-role-computable role) :json-false))
        (cons 'slug (taiga-api-project-template-role-slug role))
        (cons 'name (taiga-api-project-template-role-name role))))

(defun taiga-api-many-project-template-role-from-array (array)
  "Turn ARRAY into a list of `taiga-api-project-template-role'."
  (mapcar #'taiga-api-project-template-role-from-alist array))

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

;;; Project template

(defun taiga-api-list-project-template ()
  "List all the project templates defined."
  (taiga-api--check-authentication)
  (taiga-api-with-get-request "project-templates" ()
    (200 (taiga-api--get-object #'taiga-api-many-project-template-from-array))))

(defun taiga-api-create-project-template
    (name description default-owner-role &optional slug
          is-backlog-activated is-kanban-activated is-wiki-activated
          is-issues-activated videoconferences videoconferences-salt
          default-options us-statuses points task-statuses issue-statuses
          issue-types priorities severities roles)
  "Create a new project template named NAME.

NAME should be a string, it is the name for the new template.
DESCRIPTION should be a string.  DEFAULT-OWNER-ROLE should be a
string indicating one of the roles specified later.  SLUG should
be a string of only URL-safe characters, it is the URL where the
template can be found.  IS-BACKLOG-ACTIVATED should be a boolean
value indicating if the backlog should be enabled.
IS-KANBAN-ACTIVATED should be a boolean value indicating if the
Kanban board should be enabled.  IS-WIKI-ACTIVATED should be a
boolean value indicating if the project Wiki should be enabled.
IS-ISSUES-ACTIVATED should be a boolean value indicating if
project issues should be enabled.  VIDEOCONFERENCES should be
either \"talky\" or \"appear-in\" to select which service to use.
VIDEOCONFERENCES-SALT should be a string which is used to obscure
the URL of your videoconference rooms from the public.
DEFAULT-OPTIONS should be a `taiga-api-project-template-options'
instance.  US-STATUSES should be a list of
`taiga-api-project-template-user-story-status' instances.  POINTS
should be a list of `taiga-api-project-template-point' instances.
TASK-STATUSES and ISSUE-STATUSES should be lists of
`taiga-api-project-template-status' instances.  ISSUE-TYPES,
PRIORITIES and SEVERITIES should be lists of
`taiga-api-project-template-thingy' instances.  ROLES should be a
list of `taiga-api-project-template-role' instances."
  (taiga-api--check-authentication)

  (when roles
    (setq roles (mapcar #'taiga-api-project-template-role-to-alist roles)))
  (when severities
    (setq severities (mapcar #'taiga-api-project-template-thingy-to-alist severities)))
  (when priorities
    (setq priorities (mapcar #'taiga-api-project-template-thingy-to-alist priorities)))
  (when issue-types
    (setq issue-types (mapcar #'taiga-api-project-template-thingy-to-alist issue-types)))
  (when issue-statuses
    (setq issue-statuses (mapcar #'taiga-api-project-template-status-to-alist issue-statuses)))
  (when task-statuses
    (setq task-statuses (mapcar #'taiga-api-project-template-status-to-alist task-statuses)))
  (when points
    (setq points (mapcar #'taiga-api-project-template-point-to-alist points)))
  (when us-statuses
    (setq us-statuses (mapcar #'taiga-api-project-template-user-story-status-to-alist us-statuses)))
  (when default-options
    (setq default-options (taiga-api-project-template-options-to-alist default-options)))

  (unless is-backlog-activated (setq is-backlog-activated :json-false))
  (unless is-kanban-activated (setq is-kanban-activated :json-false))
  (unless is-issues-activated (setq is-issues-activated :json-false))
  (unless is-wiki-activated (setq is-wiki-activated :json-false))
  (unless videoconferences-salt (setq videoconferences-salt ""))

  (let ((url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer " taiga-api--auth-token)))))
    (taiga-api-with-post-request "project-templates"
        (name description default-owner-role slug is-backlog-activated
              is-kanban-activated is-wiki-activated is-issues-activated
              videoconferences videoconferences-salt default-options
              us-statuses points task-statuses issue-statuses issue-types
              priorities severities roles)
      (201 (taiga-api--get-object #'taiga-api-project-template-from-alist))
      (400 (signal 'taiga-api-error
                   (taiga-api--get-object #'taiga-api-error-from-alist))))))

(defun taiga-api-get-project-template (id)
  "Get a project template by ID."
  (taiga-api--check-authentication)
  (let ((url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer " taiga-api--auth-token))))
        (endpoint (concat "project-templates/"
                          (url-encode-url (number-to-string id)))))
    (taiga-api-with-get-request endpoint ()
      (200 (taiga-api--get-object #'taiga-api-project-template-from-alist))
      (404 (signal 'taiga-api-not-found
                   (taiga-api--get-object #'taiga-api-error-from-alist))))))

(cl-defun taiga-api-edit-project-template
    (id &key name description default-owner-role slug
        (is-backlog-activated nil backlog-specified)
        (is-kanban-activated nil kanban-specified)
        (is-wiki-activated nil wiki-specified)
        (is-issues-activated nil issues-specified)
        videoconferences
        (videoconferences-salt nil videoconferences-salt-specified)
        default-options us-statuses points task-statuses issue-statuses
        issue-types priorities severities roles)
  "Update a project template by ID.

NAME should be a string, it is the name of the template.
DESCRIPTION should be a string.  DEFAULT-OWNER-ROLE should be a
string indicating one of the roles in the template.  SLUG should
be a string of only URL-safe characters, it is the URL where the
template can be found.  IS-BACKLOG-ACTIVATED should be a boolean
value indicating if the backlog should be a boolean value
indicating if the backlog should be enabled.  IS-KANBAN-ACTIVATED
should be a boolean value indicating if the Kanban board should
be enabled.  IS-WIKI-ACTIVATED should be a boolean value
indicating if the project wiki should be enabled.
IS-ISSUES-ACTIVATED should be a boolean value indicating if
project issues should be enabled.  VIDEOCONFERENCES should be
either \"talky\" or \"appear-in\" to select which service to use.
VIDEOCONFERENCES-SALT should be a string which is used to obscure
the URL of your videoconference rooms from the public.
DEFAULT-OPTIONS should be a `taiga-api-project-template-options'
instance.  US-STATUSES should be a list of
`taiga-api-project-template-user-story-status' instances.  POINTS
should be a list of `taiga-api-project-template-point' instances.
TASK-STATUSES and ISSUE-STATUSES should should be lists of
`taiga-api-project-template-status' instances.  ISSUE-TYPES,
PRIORITIES annd SEVERITIES should be lists of
`taiga-api-project-template-thingy' instances.  ROLES should be a
list of `taiga-api-project-template-role' instances."
  (taiga-api--check-authentication)

  (when roles
    (setq roles (mapcar #'taiga-api-project-template-role-to-alist roles)))
  (when severities
    (setq severities (mapcar #'taiga-api-project-template-thingy-to-alist severities)))
  (when priorities
    (setq priorities (mapcar #'taiga-api-project-template-thingy-to-alist priorities)))
  (when issue-types
    (setq issue-types (mapcar #'taiga-api-project-template-thingy-to-alist issue-types)))
  (when issue-statuses
    (setq issue-statuses (mapcar #'taiga-api-project-template-status-to-alist issue-statuses)))
  (when task-statuses
    (setq task-statuses (mapcar #'taiga-api-project-template-status-to-alist task-statuses)))
  (when points
    (setq points (mapcar #'taiga-api-project-template-point-to-alist points)))
  (when us-statuses
    (setq us-statuses (mapcar #'taiga-api-project-template-user-story-status-to-alist us-statuses)))
  (when default-options
    (setq default-options (taiga-api-project-template-options-to-alist default-options)))

  (when (and backlog-specified (not is-backlog-activated))
    (setq is-backlog-activated :json-false))
  (when (and kanban-specified (not is-kanban-activated))
    (setq is-kanban-activated :json-false))
  (when (and issues-specified (not is-issues-activated))
    (setq is-issues-activated :json-false))
  (when (and wiki-specified (not is-wiki-activated))
    (setq is-wiki-activated :json-false))
  (when (and videoconferences-salt-specified (not videoconferences-salt))
    (setq videoconferences-salt ""))

  (let ((url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer " taiga-api--auth-token))))
        (endpoint (concat "project-templates/" (url-encode-url (number-to-string id)))))
    (taiga-api-with-patch-request endpoint
        (name description default-owner-role slug is-backlog-activated
              is-kanban-activated is-wiki-activated is-issues-activated
              videoconferences videoconferences-salt default-options
              us-statuses points task-statuses issue-statuses issue-types
              priorities severities roles)
      (200 (taiga-api--get-object #'taiga-api-project-template-from-alist))
      (404 (signal 'taiga-api-not-found
                   (taiga-api--get-object #'taiga-api-error-from-alist))))))

(provide 'taiga-api)
;;; taiga-api.el ends here
