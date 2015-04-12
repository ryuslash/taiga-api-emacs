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
(require 'eieio)

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

(defun taiga-api--slot->json (slot &optional prefix)
  "Turn SLOT into a JSON-understood name, possibly with PREFIX."
  (intern (concat (when prefix (symbol-name prefix))
                  (replace-regexp-in-string "-" "_" (symbol-name slot)))))

(defclass taiga-api-error ()
  ((type :accessor taiga-api-error-type :initarg :type)
   (message :accessor taiga-api-error-message :initarg :message)))

(defun taiga-api-error-from-alist (alist)
  "Turn ALIST into a `taiga-api-error'."
  (make-instance 'taiga-api-error :alist alist))

(defun taiga-api--plist-delete (plist prop)
  (let (lst)
    (while plist
      (unless (eql prop (car plist))
        (setq lst (cons (car plist) (cons (cadr plist) lst))))
      (setq plist (cddr plist)))
    lst))

(defun taiga-api--initialize-from-alist (obj alist &optional json-prefix)
  "Initialize OBJ from ALIST prepending JSON-PREFIX to the get."
  (if alist
      (mapc (lambda (slot)
              (setf (slot-value obj slot)
                    (cdr (assq (taiga-api--slot->json slot json-prefix) alist))))
            (mapcar #'eieio-slot-descriptor-name
                    (eieio-class-slots (eieio-object-class obj))))))

(defun taiga-api--read-bool (slot alist)
  (not (eql (cdr (assq slot alist)) :json-false)))

(defun taiga-api--set-bools (obj alist props)
  (when alist
    (mapc (lambda (slot)
            (setf (slot-value obj slot)
                  (taiga-api--read-bool (taiga-api--slot->json slot) alist)))
          props)))

(cl-defmethod shared-initialize ((obj taiga-api-error) slots)
  (cl-call-next-method obj (taiga-api--plist-delete slots :alist))
  (taiga-api--initialize-from-alist obj (plist-get slots :alist) '_error_))

(defclass taiga-api-object () ())

(cl-defmethod shared-initialize ((obj taiga-api-object) slots)
  (cl-call-next-method obj (taiga-api--plist-delete slots :alist))
  (taiga-api--initialize-from-alist obj (plist-get slots :alist)))

(defclass taiga-api-user-authentication (taiga-api-object)
  ((auth-token :accessor taiga-api-user-authentication-auth-token :initarg :auth-token)
   (bio :accessor taiga-api-user-authentication-bio :initarg :bio)
   (is-active :accessor taiga-api-user-authentication-is-active :initarg :is-active)
   (email :accessor taiga-api-user-authentication-email :initarg :email)
   (github-id :accessor taiga-api-user-authentication-github-id :initarg :github-id)
   (color :accessor taiga-api-user-authentication-color :initarg :color)
   (default-language :accessor taiga-api-user-authentication-default-language :initarg :default-language)
   (full-name-display :accessor taiga-api-user-authentication-full-name-display :initarg :full-name-display)
   (default-timezone :accessor taiga-api-user-authentication-default-timezone :initarg :default-timezone)
   (id :accessor taiga-api-user-authentication-id :initarg :id)
   (full-name :accessor taiga-api-user-authentication-full-name :initarg :full-name)
   (photo :accessor taiga-api-user-authentication-photo :initarg :photo)
   (username :accessor taiga-api-user-authentication-username :initarg :username)
   (big-photo :accessor taiga-api-user-authentication-big-photo :initarg :big-photo)))

(defun taiga-api-user-authentication-from-alist (alist)
  "Turn ALIST into a `taiga-api-user-authentication'."
  (make-instance 'taiga-api-user-authentication :alist alist))

(defclass taiga-api-wiki-page (taiga-api-object)
  ((html :accessor taiga-api-wiki-page-html :initarg :html)
   (editions :accessor taiga-api-wiki-page-editions :initarg :editions)
   (id :accessor taiga-api-wiki-page-id :initarg :id)
   (version :accessor taiga-api-wiki-page-version :initarg :version)
   (project :accessor taiga-api-wiki-page-project :initarg :project)
   (slug :accessor taiga-api-wiki-page-slug :initarg :slug)
   (content :accessor taiga-api-wiki-page-content :initarg :content)
   (owner :accessor taiga-api-wiki-page-owner :initarg :owner)
   (last-modified :accessor taiga-api-wiki-page-last-modified :initarg :last-modified)
   (created-date :accessor taiga-api-wiki-page-created-date :initarg :created-date)
   (modified-date :accessor taiga-api-wiki-page-modified-date :initarg :modified-date)
   (watchers :accessor taiga-api-wiki-page-watchers :initarg :watchers)))

(defun taiga-api-wiki-page-from-alist (alist)
  "Turn ALIST into a `taiga-api-wiki-page'."
  (make-instance 'taiga-api-wiki-page :alist alist))

(defclass taiga-api-user-story (taiga-api-object)
  ((assigned-to :accessor taiga-api-user-story-assigned-to :initarg :assigned-to)
   (backlog-order :accessor taiga-api-user-story-backlog-order :initarg :backlog-order)
   (blocked-note :accessor taiga-api-user-story-blocked-note :initarg :blocked-note)
   (blocked-note-html :accessor taiga-api-user-story-blocked-note-html :initarg :blocked-note-html)
   (client-requirement :accessor taiga-api-user-story-client-requirement :initarg :client-requirement)
   (comment :accessor taiga-api-user-story-comment :initarg :comment)
   (created-date :accessor taiga-api-user-story-created-date :initarg :created-date)
   (description :accessor taiga-api-user-story-description :initarg :description)
   (description-html :accessor taiga-api-user-story-description-html :initarg :description-html)
   (finish-date :accessor taiga-api-user-story-finish-date :initarg :finish-date)
   (generated-from-issue :accessor taiga-api-user-story-generated-from-issue :initarg :generated-from-issue)
   (id :accessor taiga-api-user-story-id :initarg :id)
   (is-archived :accessor taiga-api-user-story-is-archived :initarg :is-archived)
   (is-blocked :accessor taiga-api-user-story-is-blocked :initarg :is-blocked)
   (is-closed :accessor taiga-api-user-story-is-closed :initarg :is-closed)
   (kanban-order :accessor taiga-api-user-story-kanban-order :initarg :kanban-order)
   (milestone :accessor taiga-api-user-story-milestone :initarg :milestone)
   (milestone-name :accessor taiga-api-user-story-milestone-name :initarg :milestone-name)
   (milestone-slug :accessor taiga-api-user-story-milestone-slug :initarg :milestone-slug)
   (modified-date :accessor taiga-api-user-story-modified-date :initarg :modified-date)
   (origin-issue :accessor taiga-api-user-story-origin-issue :initarg :origin-issue)
   (owner :accessor taiga-api-user-story-owner :initarg :owner)
   (points :accessor taiga-api-user-story-points :initarg :points)
   (project :accessor taiga-api-user-story-project :initarg :project)
   (ref :accessor taiga-api-user-story-ref :initarg :ref)
   (sprint-order :accessor taiga-api-user-story-sprint-order :initarg :sprint-order)
   (status :accessor taiga-api-user-story-status :initarg :status)
   (subject :accessor taiga-api-user-story-subject :initarg :subject)
   (tags :accessor taiga-api-user-story-tags :initarg :tags)
   (team-requirement :accessor taiga-api-user-story-team-requirement :initarg :team-requirement)
   (total-points :accessor taiga-api-user-story-total-points :initarg :total-points)
   (version :accessor taiga-api-user-story-version :initarg :version)
   (watchers :accessor taiga-api-user-story-watchers :initarg :watchers)))

(cl-defmethod shared-initialize ((obj taiga-api-user-story) slots)
  (cl-call-next-method)

  (taiga-api--set-bools
   obj (plist-get slots :alist)
   '(client-requirement is-archived is-blocked is-closed team-requirement)))

(defun taiga-api-user-story-from-alist (alist)
  "Turn ALIST into a `taiga-api-user-story'."
  (make-instance 'taiga-api-user-story :alist alist))

(defclass taiga-api-issue (taiga-api-object)
  ((assigned-to :accessor taiga-api-issue-assigned-to :initarg :assigned-to)
   (blocked-note :accessor taiga-api-issue-blocked-note :initarg :blocked-note)
   (blocked-note-html :accessor taiga-api-issue-blocked-note-html :initarg :blocked-note-html)
   (comment :accessor taiga-api-issue-comment :initarg :comment)
   (created-date :accessor taiga-api-issue-created-date :initarg :created-date)
   (description :accessor taiga-api-issue-description :initarg :description)
   (description-html :accessor taiga-api-issue-description-html :initarg :description-html)
   (finish-date :accessor taiga-api-issue-finish-date :initarg :finish-date)
   (id :accessor taiga-api-issue-id :initarg :id)
   (is-blocked :accessor taiga-api-issue-is-blocked :initarg :is-blocked)
   (is-closed :accessor taiga-api-issue-is-closed :initarg :is-closed)
   (milestone :accessor taiga-api-issue-milestone :initarg :milestone)
   (modified-date :accessor taiga-api-issue-modified-date :initarg :modified-date)
   (finished-date :accessor taiga-api-issue-finished-date :initarg :finished-date)
   (owner :accessor taiga-api-issue-owner :initarg :owner)
   (project :accessor taiga-api-issue-project :initarg :project)
   (ref :accessor taiga-api-issue-ref :initarg :ref)
   (status :accessor taiga-api-issue-status :initarg :status)
   (severity :accessor taiga-api-issue-severity :initarg :severity)
   (priority :accessor taiga-api-issue-priority :initarg :priority)
   (type :accessor taiga-api-issue-type :initarg :type)
   (subject :accessor taiga-api-issue-subject :initarg :subject)
   (tags :accessor taiga-api-issue-tags :initarg :tags)
   (version :accessor taiga-api-issue-version :initarg :version)
   (watchers :accessor taiga-api-issue-watchers :initarg :watchers)
   (generated-user-stories :accessor taiga-api-issue-generated-user-stories :initarg :generated-user-stories)
   (votes :accessor taiga-api-issue-votes :initarg :votes)
   (neighbors :accessor taiga-api-issue-neighbors :initarg :neighbors)))

(cl-defmethod shared-initialize ((obj taiga-api-issue) slots)
  (cl-call-next-method)

  (let ((alist (plist-get slots :alist)))
    (taiga-api--set-bools obj alist '(is-blocked is-closed))
    (when alist
      (setf (slot-value obj 'neighbors)
            (taiga-api-neighbors-from-alist (cdr (assq 'neighbors alist)))))))

(defclass taiga-api-neighbors ()
  ((next :accessor taiga-api-neighbors-next :initarg :next)
   (previous :accessor taiga-api-neighbors-previous :initarg :previous)))

(cl-defmethod shared-initialize ((obj taiga-api-neighbors) slots)
  (cl-call-next-method obj (taiga-api--plist-delete slots :alist))

  (let* ((alist (plist-get slots :alist))
         (next-alist (cdr (assq 'next alist)))
         (previous-alist (cdr (assq 'previous alist))))
    (if alist
      (setf (slot-value obj 'next)
            (when next-alist (taiga-api-neighbor-from-alist next-alist)))
      (setf (slot-value obj 'previous)
            (when previous-alist (taiga-api-neighbor-from-alist previous-alist))))))

(defclass taiga-api-neighbor (taiga-api-object)
  ((id :accessor taiga-api-neighbor-id :initarg :id)
   (ref :accessor taiga-api-neighbor-ref :initarg :ref)
   (subject :accessor taiga-api-neighbor-subject :initarg :subject)))

(defun taiga-api-issue-from-alist (alist)
  "Turn ALIST into a `taiga-api-issue'."
  (make-instance 'taiga-api-issue :alist alist))

(defun taiga-api-neighbors-from-alist (alist)
  "Turn ALIST into a `taiga-api-neighbors'."
  (make-instance 'taiga-api-neighbors :alist alist))

(defun taiga-api-neighbor-from-alist (alist)
  "Turn ALIST into a `taiga-api-neighbor'."
  (make-instance 'taiga-api-neighbor :alist alist))

(defclass taiga-api-task (taiga-api-object)
  ((assigned-to :accessor taiga-api-task-assigned-to :initarg :assigned-to)
   (blocked-note :accessor taiga-api-task-blocked-note :initarg :blocked-note)
   (blocked-note-html :accessor taiga-api-task-blocked-note-html :initarg :blocked-note-html)
   (comment :accessor taiga-api-task-comment :initarg :comment)
   (milestone-slug :accessor taiga-api-task-milestone-slug :initarg :milestone-slug)
   (created-date :accessor taiga-api-task-created-date :initarg :created-date)
   (description :accessor taiga-api-task-description :initarg :description)
   (description-html :accessor taiga-api-task-description-html :initarg :description-html)
   (id :accessor taiga-api-task-id :initarg :id)
   (is-blocked :accessor taiga-api-task-is-blocked :initarg :is-blocked)
   (is-closed :accessor taiga-api-task-is-closed :initarg :is-closed)
   (milestone :accessor taiga-api-task-milestone :initarg :milestone)
   (modified-date :accessor taiga-api-task-modified-date :initarg :modified-date)
   (finished-date :accessor taiga-api-task-finished-date :initarg :finished-date)
   (owner :accessor taiga-api-task-owner :initarg :owner)
   (project :accessor taiga-api-task-project :initarg :project)
   (user-story :accessor taiga-api-task-user-story :initarg :user-story)
   (ref :accessor taiga-api-task-ref :initarg :ref)
   (status :accessor taiga-api-task-status :initarg :status)
   (subject :accessor taiga-api-task-subject :initarg :subject)
   (tags :accessor taiga-api-task-tags :initarg :tags)
   (us-order :accessor taiga-api-task-us-order :initarg :us-order)
   (taskboard-order :accessor taiga-api-task-taskboard-order :initarg :taskboard-order)
   (version :accessor taiga-api-task-version :initarg :version)
   (is-iocaine :accessor taiga-api-task-is-iocaine :initarg :is-iocaine)
   (external-reference :accessor taiga-api-task-external-reference :initarg :external-reference)
   (watchers :accessor taiga-api-task-watchers :initarg :watchers)
   (neighbors :accessor taiga-api-task-neighbors :initarg :neighbors)))

(cl-defmethod shared-initialize ((obj taiga-api-task) slots)
  (cl-call-next-method)

  (let ((alist (plist-get slots :alist)))
    (when alist
      (taiga-api--set-bools obj alist '(is-blocked is-closed is-iocaine))
      (setf (slot-value obj 'neighbors)
            (taiga-api-neighbors-from-alist (cdr (assq 'neighbors alist)))))))

(defun taiga-api-task-from-alist (alist)
  "Turn ALIST into a `taiga-api-task'."
  (make-instance 'taiga-api-task :alist alist))

(defclass taiga-api-search-result (taiga-api-object)
  ((wikipages :accessor taiga-api-search-result-wikipages :initarg :wikipages)
   (userstories :accessor taiga-api-search-result-userstories :initarg :userstories)
   (issues :accessor taiga-api-search-result-issues :initarg :issues)
   (tasks :accessor taiga-api-search-result-tasks :initarg :tasks)
   (count :accessor taiga-api-search-result-count :initarg :count)))

(defun taiga-api-search-result-from-alist (alist)
  "Turn ALIST into a `taiga-api-search-result'."
  (make-instance 'taiga-api-search-result
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

(defclass taiga-api-user-storage-data (taiga-api-object)
  ((key :accessor taiga-api-user-storage-data-key :initarg :key)
   (value :accessor taiga-api-user-storage-data-value :initarg :value)
   (created-date :accessor taiga-api-user-storage-data-created-date :initarg :created-date)
   (modified-date :accessor taiga-api-user-storage-data-modified-date :initarg :modified-date)))

(defun taiga-api-user-storage-data-from-alist (alist)
  "Turn ALIST into a `taiga-api-user-storage-data'."
  (make-instance 'taiga-api-user-storage-data :alist alist))

(defun taiga-api-many-user-storage-data-from-array (array)
  "Turn ARRAY into a list of `taiga-api-user-storage-data'."
  (mapcar #'taiga-api-user-storage-data-from-alist array))

(defclass taiga-api-project-template (taiga-api-object)
  ((default-options :accessor taiga-api-project-template-default-options :initarg :default-options)
   (us-statuses :accessor taiga-api-project-template-us-statuses :initarg :us-statuses)
   (points :accessor taiga-api-project-template-points :initarg :points)
   (task-statuses :accessor taiga-api-project-template-task-statuses :initarg :task-statuses)
   (issue-statuses :accessor taiga-api-project-template-issue-statuses :initarg :issue-statuses)
   (issue-types :accessor taiga-api-project-template-issue-types :initarg :issue-types)
   (priorities :accessor taiga-api-project-template-priorities :initarg :priorities)
   (severities :accessor taiga-api-project-template-severities :initarg :severities)
   (roles :accessor taiga-api-project-template-roles :initarg :roles)
   (id :accessor taiga-api-project-template-id :initarg :id)
   (name :accessor taiga-api-project-template-name :initarg :name)
   (slug :accessor taiga-api-project-template-slug :initarg :slug)
   (description :accessor taiga-api-project-template-description :initarg :description)
   (created-date :accessor taiga-api-project-template-created-date :initarg :created-date)
   (modified-date :accessor taiga-api-project-template-modified-date :initarg :modified-date)
   (default-owner-role :accessor taiga-api-project-template-default-owner-role :initarg :default-owner-role)
   (is-backlog-activated :accessor taiga-api-project-template-is-backlog-activated :initarg :is-backlog-activated)
   (is-kanban-activated :accessor taiga-api-project-template-is-kanban-activated :initarg :is-kanban-activated)
   (is-wiki-activated :accessor taiga-api-project-template-is-wiki-activated :initarg :is-wiki-activated)
   (is-issues-activated :accessor taiga-api-project-template-is-issues-activated :initarg :is-issues-activated)
   (videoconferences :accessor taiga-api-project-template-videoconferences :initarg :videoconferences)
   (videoconferences-salt :accessor taiga-api-project-template-videoconferences-salt :initarg :videoconferences-salt)))

(cl-defmethod shared-initialize ((obj taiga-api-project-template) slots)
  (cl-call-next-method)

  (let ((alist (plist-get slots :alist)))
    (when alist
      (taiga-api--set-bools
       obj alist
       '(is-backlog-activated is-kanban-activated is-wiki-activated is-issues-activated))
      (setf (slot-value obj 'default-options)
            (taiga-api-project-template-options-from-alist
             (cdr (assq 'default_options alist))))
      (setf (slot-value obj 'us-statuses)
            (taiga-api-many-project-template-user-story-status-from-array
             (cdr (assq 'us_statuses alist))))
      (setf (slot-value obj 'points)
            (taiga-api-many-project-template-point-from-array
             (cdr (assq 'points alist))))
      (setf (slot-value obj 'task-statuses)
            (taiga-api-many-project-template-status-from-array
             (cdr (assq 'task_statuses alist))))
      (setf (slot-value obj 'issue-statuses)
            (taiga-api-many-project-template-status-from-array
             (cdr (assq 'issue_statuses alist))))
      (setf (slot-value obj 'issue-types)
            (taiga-api-many-project-template-thingy-from-array
             (cdr (assq 'issue_types alist))))
      (setf (slot-value obj 'priorities)
            (taiga-api-many-project-template-thingy-from-array
             (cdr (assq 'priorities alist))))
      (setf (slot-value obj 'severities)
            (taiga-api-many-project-template-thingy-from-array
             (cdr (assq 'severities alist))))
      (setf (slot-value obj 'roles)
            (taiga-api-many-project-template-role-from-array
             (cdr (assq 'roles alist)))))))

(defun taiga-api-project-template-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-template'."
  (make-instance 'taiga-api-project-template :alist alist))

(defun taiga-api-many-project-template-from-array (array)
  "Turn ARRAY into a list of `taiga-api-project-template'."
  (mapcar #'taiga-api-project-template-from-alist array))

(defclass taiga-api-project-template-options (taiga-api-object)
  ((us-status :accessor taiga-api-project-template-options-us-status :initarg :us-status)
   (points :accessor taiga-api-project-template-options-points :initarg :points)
   (priority :accessor taiga-api-project-template-options-priority :initarg :priority)
   (severity :accessor taiga-api-project-template-options-severity :initarg :severity)
   (task-status :accessor taiga-api-project-template-options-task-status :initarg :task-status)
   (issue-type :accessor taiga-api-project-template-options-issue-type :initarg :issue-type)
   (issue-status :accessor taiga-api-project-template-options-issue-status :initarg :issue-status)))

(defun taiga-api-project-template-options-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-template-options'."
  (make-instance 'taiga-api-project-template-options :alist alist))

(defun taiga-api-project-template-options-to-alist (options)
  "Turn OPTIONS into an alist."
  (list (cons 'us_status (taiga-api-project-template-options-us-status options))
        (cons 'points (taiga-api-project-template-options-points options))
        (cons 'priority (taiga-api-project-template-options-priority options))
        (cons 'severity (taiga-api-project-template-options-severity options))
        (cons 'task_status (taiga-api-project-template-options-task-status options))
        (cons 'issue_type (taiga-api-project-template-options-issue-type options))
        (cons 'issue_status (taiga-api-project-template-options-issue-status options))))

(defclass taiga-api-project-template-user-story-status (taiga-api-object)
  ((wip-limit :accessor taiga-api-project-template-user-story-status-wip-limit :initarg :wip-limit)
   (color :accessor taiga-api-project-template-user-story-status-color :initarg :color)
   (name :accessor taiga-api-project-template-user-story-status-name :initarg :name)
   (slug :accessor taiga-api-project-template-user-story-status-slug :initarg :slug)
   (order :accessor taiga-api-project-template-user-story-status-order :initarg :order)
   (is-closed :accessor taiga-api-project-template-user-story-status-is-closed :initarg :is-closed)))

(cl-defmethod shared-initialize ((obj taiga-api-project-template-user-story-status) slots)
  (cl-call-next-method)

  (let ((alist (plist-get slots :alist)))
    (when alist
      (taiga-api--set-bools obj alist '(is-closed)))))

(defun taiga-api-project-template-user-story-status-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-template-user-story-status'."
  (make-instance 'taiga-api-project-template-user-story-status :alist alist))

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

(defclass taiga-api-project-template-point (taiga-api-object)
  ((value :accessor taiga-api-project-template-point-value :initarg :value)
   (name :accessor taiga-api-project-template-point-name :initarg :name)
   (order :accessor taiga-api-project-template-point-order :initarg :order)))

(defun taiga-api-project-template-point-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-template-point'."
  (make-instance 'taiga-api-project-template-point :alist alist))

(defun taiga-api-many-project-template-point-from-array (array)
  "Turn ARRAY into a list of `taiga-api-project-template-point'."
  (mapcar #'taiga-api-project-template-point-from-alist array))

(defun taiga-api-project-template-point-to-alist (point)
  "Turn POINT into an alist."
  (list (cons 'value (taiga-api-project-template-point-value point))
        (cons 'name (taiga-api-project-template-point-name point))
        (cons 'order (taiga-api-project-template-point-order point))))

(defclass taiga-api-project-template-status (taiga-api-object)
  ((color :accessor taiga-api-project-template-status-color :initarg :color)
   (name :accessor taiga-api-project-template-status-name :initarg :name)
   (slug :accessor taiga-api-project-template-status-slug :initarg :slug)
   (order :accessor taiga-api-project-template-status-order :initarg :order)
   (is-closed :accessor taiga-api-project-template-status-is-closed :initarg :is-closed)))

(cl-defmethod shared-initialize ((obj taiga-api-project-template-status) slots)
  (cl-call-next-method)
  (taiga-api--set-bools obj (plist-get slots :alist) '(is-closed)))

(defun taiga-api-project-template-status-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-template-status'."
  (make-instance 'taiga-api-project-template-status :alist alist))

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

(defclass taiga-api-project-template-thingy (taiga-api-object)
  ((color :accessor taiga-api-project-template-thingy-color :initarg :color)
   (name :accessor taiga-api-project-template-thingy-name :initarg :name)
   (order :accessor taiga-api-project-template-thingy-order :initarg :order)))

(defun taiga-api-project-template-thingy-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-template-thingy'."
  (make-instance 'taiga-api-project-template-thingy :alist alist))

(defun taiga-api-project-template-thingy-to-alist (thingy)
  "Turn THINGY into an alist."
  (list (cons 'color (taiga-api-project-template-thingy-color thingy))
        (cons 'name (taiga-api-project-template-thingy-name thingy))
        (cons 'order (taiga-api-project-template-thingy-order thingy))))

(defun taiga-api-many-project-template-thingy-from-array (array)
  "Turn ARRAY into a list of `taiga-api-project-template-thingy'."
  (mapcar #'taiga-api-project-template-thingy-from-alist array))

(defclass taiga-api-project-template-role (taiga-api-object)
  ((permissions :accessor taiga-api-project-template-role-permissions :initarg :permissions)
   (order :accessor taiga-api-project-template-role-order :initarg :order)
   (computable :accessor taiga-api-project-template-role-computable :initarg :computable)
   (slug :accessor taiga-api-project-template-role-slug :initarg :slug)
   (name :accessor taiga-api-project-template-role-name :initarg :name)))

(defun taiga-api-project-template-role-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-template-role'."
  (make-instance 'taiga-api-project-template-role :alist alist))

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

(cl-defstruct taiga-api-project-list-entry
  anon-permissions created-date creation-template default-issue-status
  default-issue-type default-points default-priority default-severity
  default-task-status default-us-status description i-am-owner id
  is-backlog-activated is-issues-activated is-kanban-activated is-private
  is-wiki-activated members modified-date my-permissions name owner
  public-permissions slug stars tags tags-colors total-milestones
  total-story-points users videoconferences videoconferences-salt)

(defun taiga-api-project-list-entry-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-list-entry'."
  (make-taiga-api-project-list-entry
   :anon-permissions (cdr (assq 'anon_permissions alist))
   :created-date (cdr (assq 'created_date alist))
   :creation-template (cdr (assq 'creation_template alist))
   :default-issue-status (cdr (assq 'default_issue_status alist))
   :default-issue-type (cdr (assq 'default_issue_type alist))
   :default-points (cdr (assq 'default_points alist))
   :default-priority (cdr (assq 'default_priority alist))
   :default-severity (cdr (assq 'default_severity alist))
   :default-task-status (cdr (assq 'default_task_status alist))
   :default-us-status (cdr (assq 'default_us_status alist))
   :description (cdr (assq 'description alist))
   :i-am-owner (not (eql (cdr (assq 'i_am_owner alist)) :json-false))
   :id (cdr (assq 'id alist))
   :is-backlog-activated (not (eql (cdr (assq 'is_backlog_activated alist)) :json-false))
   :is-issues-activated (not (eql (cdr (assq 'is_issues_activated alist)) :json-false))
   :is-kanban-activated (not (eql (cdr (assq 'is_kanban_activated alist)) :json-false))
   :is-private (not (eql (cdr (assq 'is_private alist)) :json-false))
   :is-wiki-activated (not (eql (cdr (assq 'is_wiki_activated alist)) :json-false))
   :members (cdr (assq 'members alist))
   :modified-date (cdr (assq 'modified_date alist))
   :my-permissions (cdr (assq 'my_permissions alist))
   :name (cdr (assq 'name alist))
   :owner (cdr (assq 'owner alist))
   :public-permissions (cdr (assq 'public_permissions alist))
   :slug (cdr (assq 'slug alist))
   :stars (cdr (assq 'stars alist))
   :tags (cdr (assq 'tags alist))
   :tags-colors (cdr (assq 'tags_colors alist))
   :total-milestones (cdr (assq 'total_milestones alist))
   :total-story-points (cdr (assq 'total_story_points alist))
   :users (mapcar #'taiga-api-user-detail-from-alist (cdr (assq 'users alist)))
   :videoconferences (cdr (assq 'videoconferences alist))
   :videoconferences-salt (cdr (assq 'videoconferences_salt alist))))

(defun taiga-api-many-project-list-entry-from-array (array)
  "Turn ARRAY into a list of `taiga-api-project-list-entry'."
  (mapcar #'taiga-api-project-list-entry-from-alist array))

(cl-defstruct taiga-api-user-detail
  big-photo bio color lang timezone email full-name full-name-display
  github-id id is-active photo username)

(defun taiga-api-user-detail-from-alist (alist)
  "Turn ALIST into a `taiga-api-user-detail'."
  (make-taiga-api-user-detail
   :big-photo (cdr (assq 'big_photo alist))
   :bio (cdr (assq 'bio alist))
   :color (cdr (assq 'color alist))
   :lang (cdr (assq 'lang alist))
   :timezone (cdr (assq 'timezone alist))
   :email (cdr (assq 'email alist))
   :full-name (cdr (assq 'full_name alist))
   :full-name-display (cdr (assq 'full_name_display alist))
   :github-id (cdr (assq 'github_id alist))
   :id (cdr (assq 'id alist))
   :is-active (not (eql (cdr (assq 'is_active alist)) :json-false))
   :photo (cdr (assq 'photo alist))
   :username (cdr (assq 'username alist))))

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

(defun taiga-api-delete-project-template (id)
  "Delete a project template by ID."
  (taiga-api--check-authentication)
  (let ((url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer " taiga-api--auth-token))))
        (endpoint (concat "project-templates/" (url-encode-url (number-to-string id)))))
    (taiga-api-with-delete-request endpoint ()
      (204 t)
      (404 (signal 'taiga-api-not-found
                   (taiga-api--get-object #'taiga-api-error-from-alist))))))

(defun taiga-api-list-project ()
  "List all the accessible projects."
  (taiga-api--check-authentication)
  (taiga-api-with-get-request "projects" ()
    (200 (taiga-api--get-object #'taiga-api-many-project-list-entry-from-array))))

(provide 'taiga-api)
;;; taiga-api.el ends here
