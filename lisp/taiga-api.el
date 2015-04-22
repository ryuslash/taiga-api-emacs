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

(defclass taiga-api-user-detail (taiga-api-object)
  ((big-photo :initarg :big-photo)
   (bio :initarg :bio)
   (color :initarg :color)
   (lang :initarg :lang)
   (timezone :initarg :timezone)
   (email :initarg :email)
   (full-name :initarg :full-name)
   (full-name-display :initarg :full-name-display)
   (github-id :initarg :github-id)
   (id :initarg :id)
   (is-active :initarg :is-active)
   (photo :initarg :photo)
   (username :initarg :username)))

(cl-defmethod shared-initialize ((obj taiga-api-user-detail) slots)
  (cl-call-next-method)
  (taiga-api--set-bools obj (plist-get slots :alist) '(is-active)))

(defun taiga-api-user-detail-from-alist (alist)
  "Turn ALIST into a `taiga-api-user-detail'."
  (make-instance 'taiga-api-user-detail :alist alist))

(defclass taiga-api-user-authentication-detail (taiga-api-user-detail)
  ((auth-token :initarg :auth-token)))

(defun taiga-api-user-authentication-detail-from-alist (alist)
  "Turn ALIST into a `taiga-api-user-authentication-detail'."
  (make-instance 'taiga-api-user-authentication-detail :alist alist))

(defclass taiga-api-wiki-page (taiga-api-object)
  ((html :initarg :html)
   (editions :initarg :editions)
   (id :initarg :id)
   (version :initarg :version)
   (project :initarg :project)
   (slug :initarg :slug)
   (content :initarg :content)
   (owner :initarg :owner)
   (last-modified :initarg :last-modified)
   (created-date :initarg :created-date)
   (modified-date :initarg :modified-date)
   (watchers :initarg :watchers)))

(defun taiga-api-wiki-page-from-alist (alist)
  "Turn ALIST into a `taiga-api-wiki-page'."
  (make-instance 'taiga-api-wiki-page :alist alist))

(defclass taiga-api-user-story (taiga-api-object)
  ((assigned-to :initarg :assigned-to)
   (backlog-order :initarg :backlog-order)
   (blocked-note :initarg :blocked-note)
   (blocked-note-html :initarg :blocked-note-html)
   (client-requirement :initarg :client-requirement)
   (comment :initarg :comment)
   (created-date :initarg :created-date)
   (description :initarg :description)
   (description-html :initarg :description-html)
   (finish-date :initarg :finish-date)
   (generated-from-issue :initarg :generated-from-issue)
   (id :initarg :id)
   (is-archived :initarg :is-archived)
   (is-blocked :initarg :is-blocked)
   (is-closed :initarg :is-closed)
   (kanban-order :initarg :kanban-order)
   (milestone :initarg :milestone)
   (milestone-name :initarg :milestone-name)
   (milestone-slug :initarg :milestone-slug)
   (modified-date :initarg :modified-date)
   (origin-issue :initarg :origin-issue)
   (owner :initarg :owner)
   (points :initarg :points)
   (project :initarg :project)
   (ref :initarg :ref)
   (sprint-order :initarg :sprint-order)
   (status :initarg :status)
   (subject :initarg :subject)
   (tags :initarg :tags)
   (team-requirement :initarg :team-requirement)
   (total-points :initarg :total-points)
   (version :initarg :version)
   (watchers :initarg :watchers)))

(cl-defmethod shared-initialize ((obj taiga-api-user-story) slots)
  (cl-call-next-method)

  (taiga-api--set-bools
   obj (plist-get slots :alist)
   '(client-requirement is-archived is-blocked is-closed team-requirement)))

(defun taiga-api-user-story-from-alist (alist)
  "Turn ALIST into a `taiga-api-user-story'."
  (make-instance 'taiga-api-user-story :alist alist))

(defclass taiga-api-issue (taiga-api-object)
  ((assigned-to :initarg :assigned-to)
   (blocked-note :initarg :blocked-note)
   (blocked-note-html :initarg :blocked-note-html)
   (comment :initarg :comment)
   (created-date :initarg :created-date)
   (description :initarg :description)
   (description-html :initarg :description-html)
   (finish-date :initarg :finish-date)
   (id :initarg :id)
   (is-blocked :initarg :is-blocked)
   (is-closed :initarg :is-closed)
   (milestone :initarg :milestone)
   (modified-date :initarg :modified-date)
   (finished-date :initarg :finished-date)
   (owner :initarg :owner)
   (project :initarg :project)
   (ref :initarg :ref)
   (status :initarg :status)
   (severity :initarg :severity)
   (priority :initarg :priority)
   (type :initarg :type)
   (subject :initarg :subject)
   (tags :initarg :tags)
   (version :initarg :version)
   (watchers :initarg :watchers)
   (generated-user-stories :initarg :generated-user-stories)
   (votes :initarg :votes)
   (neighbors :initarg :neighbors)))

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
  ((id :initarg :id)
   (ref :initarg :ref)
   (subject :initarg :subject)))

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
  ((assigned-to :initarg :assigned-to)
   (blocked-note :initarg :blocked-note)
   (blocked-note-html :initarg :blocked-note-html)
   (comment :initarg :comment)
   (milestone-slug :initarg :milestone-slug)
   (created-date :initarg :created-date)
   (description :initarg :description)
   (description-html :initarg :description-html)
   (id :initarg :id)
   (is-blocked :initarg :is-blocked)
   (is-closed :initarg :is-closed)
   (milestone :initarg :milestone)
   (modified-date :initarg :modified-date)
   (finished-date :initarg :finished-date)
   (owner :initarg :owner)
   (project :initarg :project)
   (user-story :initarg :user-story)
   (ref :initarg :ref)
   (status :initarg :status)
   (subject :initarg :subject)
   (tags :initarg :tags)
   (us-order :initarg :us-order)
   (taskboard-order :initarg :taskboard-order)
   (version :initarg :version)
   (is-iocaine :initarg :is-iocaine)
   (external-reference :initarg :external-reference)
   (watchers :initarg :watchers)
   (neighbors :initarg :neighbors)))

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
  ((wikipages :initarg :wikipages)
   (userstories :initarg :userstories)
   (issues :initarg :issues)
   (tasks :initarg :tasks)
   (count :initarg :count)))

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
  ((key :initarg :key)
   (value :initarg :value)
   (created-date :initarg :created-date)
   (modified-date :initarg :modified-date)))

(defun taiga-api-user-storage-data-from-alist (alist)
  "Turn ALIST into a `taiga-api-user-storage-data'."
  (make-instance 'taiga-api-user-storage-data :alist alist))

(defun taiga-api-many-user-storage-data-from-array (array)
  "Turn ARRAY into a list of `taiga-api-user-storage-data'."
  (mapcar #'taiga-api-user-storage-data-from-alist array))

(defclass taiga-api-project-template (taiga-api-object)
  ((default-options :initarg :default-options)
   (us-statuses :initarg :us-statuses)
   (points :initarg :points)
   (task-statuses :initarg :task-statuses)
   (issue-statuses :initarg :issue-statuses)
   (issue-types :initarg :issue-types)
   (priorities :initarg :priorities)
   (severities :initarg :severities)
   (roles :initarg :roles)
   (id :initarg :id)
   (name :initarg :name)
   (slug :initarg :slug)
   (description :initarg :description)
   (created-date :initarg :created-date)
   (modified-date :initarg :modified-date)
   (default-owner-role :initarg :default-owner-role)
   (is-backlog-activated :initarg :is-backlog-activated)
   (is-kanban-activated :initarg :is-kanban-activated)
   (is-wiki-activated :initarg :is-wiki-activated)
   (is-issues-activated :initarg :is-issues-activated)
   (videoconferences :initarg :videoconferences)
   (videoconferences-salt :initarg :videoconferences-salt)))

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
  ((us-status :initarg :us-status)
   (points :initarg :points)
   (priority :initarg :priority)
   (severity :initarg :severity)
   (task-status :initarg :task-status)
   (issue-type :initarg :issue-type)
   (issue-status :initarg :issue-status)))

(defun taiga-api-project-template-options-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-template-options'."
  (make-instance 'taiga-api-project-template-options :alist alist))

(defun taiga-api-project-template-options-to-alist (options)
  "Turn OPTIONS into an alist."
  (list (cons 'us_status (slot-value options 'us-status))
        (cons 'points (slot-value options 'points))
        (cons 'priority (slot-value options 'priority))
        (cons 'severity (slot-value options 'severity))
        (cons 'task_status (slot-value options 'task-status))
        (cons 'issue_type (slot-value options 'issue-type))
        (cons 'issue_status (slot-value options 'issue-status))))

(defclass taiga-api-project-template-user-story-status (taiga-api-object)
  ((wip-limit :initarg :wip-limit :initform nil)
   (color :initarg :color :initform nil)
   (name :initarg :name :initform nil)
   (slug :initarg :slug :initform nil)
   (order :initarg :order :initform nil)
   (is-closed :initarg :is-closed :initform nil)))

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
  (list (cons 'wip_limit (slot-value status 'wip-limit))
        (cons 'color (slot-value status 'color))
        (cons 'name (slot-value status 'name))
        (cons 'slug (slot-value status 'slug))
        (cons 'order (slot-value status 'order))
        (cons 'is_closed (or (slot-value status 'is-closed) :json-false))))

(defclass taiga-api-project-template-point (taiga-api-object)
  ((value :initarg :value :initform nil)
   (name :initarg :name :initform nil)
   (order :initarg :order :initform nil)))

(defun taiga-api-project-template-point-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-template-point'."
  (make-instance 'taiga-api-project-template-point :alist alist))

(defun taiga-api-many-project-template-point-from-array (array)
  "Turn ARRAY into a list of `taiga-api-project-template-point'."
  (mapcar #'taiga-api-project-template-point-from-alist array))

(defun taiga-api-project-template-point-to-alist (point)
  "Turn POINT into an alist."
  (list (cons 'value (slot-value point 'value))
        (cons 'name (slot-value point 'name))
        (cons 'order (slot-value point 'order))))

(defclass taiga-api-project-template-status (taiga-api-object)
  ((color :initarg :color :initform nil)
   (name :initarg :name :initform nil)
   (slug :initarg :slug :initform nil)
   (order :initarg :order :initform nil)
   (is-closed :initarg :is-closed :initform nil)))

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
  (list (cons 'color (slot-value status 'color))
        (cons 'name (slot-value status 'name))
        (cons 'slug (slot-value status 'slug))
        (cons 'order (slot-value status 'order))
        (cons 'is_closed (or (slot-value status 'is-closed) :json-false))))

(defclass taiga-api-project-template-thingy (taiga-api-object)
  ((color :initarg :color :initform nil)
   (name :initarg :name :initform nil)
   (order :initarg :order :initform nil)))

(defun taiga-api-project-template-thingy-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-template-thingy'."
  (make-instance 'taiga-api-project-template-thingy :alist alist))

(defun taiga-api-project-template-thingy-to-alist (thingy)
  "Turn THINGY into an alist."
  (list (cons 'color (slot-value thingy 'color))
        (cons 'name (slot-value thingy 'name))
        (cons 'order (slot-value thingy 'order))))

(defun taiga-api-many-project-template-thingy-from-array (array)
  "Turn ARRAY into a list of `taiga-api-project-template-thingy'."
  (mapcar #'taiga-api-project-template-thingy-from-alist array))

(defclass taiga-api-project-template-role (taiga-api-object)
  ((permissions :initarg :permissions :initarg nil)
   (order :initarg :order :initarg nil)
   (computable :initarg :computable :initarg nil)
   (slug :initarg :slug :initarg nil)
   (name :initarg :name :initarg nil)))

(defun taiga-api-project-template-role-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-template-role'."
  (make-instance 'taiga-api-project-template-role :alist alist))

(defun taiga-api-project-template-role-to-alist (role)
  "Turn ROLE into an alist."
  (list (cons 'permissions (slot-value role 'permissions))
        (cons 'order (slot-value role 'order))
        (cons 'computable (or (slot-value role 'computable) :json-false))
        (cons 'slug (slot-value role 'slug))
        (cons 'name (slot-value role 'name))))

(defun taiga-api-many-project-template-role-from-array (array)
  "Turn ARRAY into a list of `taiga-api-project-template-role'."
  (mapcar #'taiga-api-project-template-role-from-alist array))

(defclass taiga-api-project-list-entry (taiga-api-object)
  ((anon-permissions :initarg :anon-permissions)
   (created-date :initarg :created-date)
   (creation-template :initarg :creation-template)
   (default-issue-status :initarg :default-issue-status)
   (default-issue-type :initarg :default-issue-type)
   (default-points :initarg :default-points)
   (default-priority :initarg :default-priority)
   (default-severity :initarg :default-severity)
   (default-task-status :initarg :default-task-status)
   (default-us-status :initarg :default-us-status)
   (description :initarg :description)
   (i-am-owner :initarg :i-am-owner)
   (id :initarg :id)
   (is-backlog-activated :initarg :is-backlog-activated)
   (is-issues-activated :initarg :is-issues-activated)
   (is-kanban-activated :initarg :is-kanban-activated)
   (is-private :initarg :is-private)
   (is-wiki-activated :initarg :is-wiki-activated)
   (members :initarg :members)
   (modified-date :initarg :modified-date)
   (my-permissions :initarg :my-permissions)
   (name :initarg :name)
   (owner :initarg :owner)
   (public-permissions :initarg :public-permissions)
   (slug :initarg :slug)
   (stars :initarg :stars)
   (tags :initarg :tags)
   (tags-colors :initarg :tags-colors)
   (total-milestones :initarg :total-milestones)
   (total-story-points :initarg :total-story-points)
   (users :initarg :users)
   (videoconferences :initarg :videoconferences)
   (videoconferences-salt :initarg :videoconferences-salt)))

(cl-defmethod shared-initialize ((obj taiga-api-project-list-entry) slots)
  (cl-call-next-method)
  (let ((alist (plist-get slots :alist)))
    (taiga-api--set-bools
     obj alist
     '(i-am-owner is-backlog-activated is-issues-activated
                  is-kanban-activated is-private is-wiki-activated))
    (when alist
      (setf (slot-value obj 'users)
            (mapcar #'taiga-api-user-detail-from-alist
                    (cdr (assq 'users alist)))))))

(defun taiga-api-project-list-entry-from-alist (alist)
  "Turn ALIST into a `taiga-api-project-list-entry'."
  (make-instance 'taiga-api-project-list-entry :alist alist))

(defun taiga-api-many-project-list-entry-from-array (array)
  "Turn ARRAY into a list of `taiga-api-project-list-entry'."
  (mapcar #'taiga-api-project-list-entry-from-alist array))

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
                  #'taiga-api-user-authentication-detail-from-alist)))
       (setq taiga-api--auth-token (slot-value user 'auth-token))
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
                  #'taiga-api-user-authentication-detail-from-alist)))
       (setq taiga-api--auth-token (slot-value user 'auth-token))
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
                  #'taiga-api-user-authentication-detail-from-alist)))
       (setq taiga-api--auth-token (slot-value user 'auth-token))
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
                  #'taiga-api-user-authentication-detail-from-alist)))
       (setq taiga-api--auth-token (slot-value user 'auth-token))
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
