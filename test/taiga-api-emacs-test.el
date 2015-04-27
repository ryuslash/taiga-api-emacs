;;; taiga-api-emacs-test.el --- Tests for the Taiga API client implementation  -*- lexical-binding: t; -*-

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

;; Tests for the Taiga API client library implementation.

;;; Code:

(require 'taiga-api)

(defconst taiga-api-test--location
  (file-name-directory (or load-file-name
                           buffer-file-name)))

(ert-deftest taiga-api-error-from-alist ()
  "`taiga-api-error-from-alist' works properly."
  (let ((err (with-temp-buffer
               (insert "{\"_error_message\": \"foo\", \"_error_type\": \"bar\"}")
               (goto-char (point-min))
               (taiga-api-error-from-alist (json-read)))))
    (should (taiga-api-error-p err))
    (should (string= (taiga-api-error-message err) "foo"))
    (should (string= (taiga-api-error-type err) "bar"))))

(ert-deftest taiga-api-user-from-alist ()
  "`taiga-api-user-from-alist' works properly."
  (let ((detail (taiga-api-test--data
                 "user-authentication-detail"
                 #'taiga-api-user-authentication-detail-from-alist)))
    (should (taiga-api-user-authentication-detail-p detail))
    (should (string= (slot-value detail 'auth-token) "eyJ1c2VyX2F1dGhlbnRpY2F0aW9uX2lkIjo3fq:1XmPud:LKXVD9Z0rmHJjiyy0m4YaaHlQS1"))
    (should (string= (slot-value detail 'bio) ""))
    (should (string= (slot-value detail 'is-active) t))
    (should (string= (slot-value detail 'email) "beta.testing@taiga.io"))
    (should (null (slot-value detail 'github-id)))
    (should (string= (slot-value detail 'color) "#FC8EAC"))
    (should (string= (slot-value detail 'lang) ""))
    (should (string= (slot-value detail 'full-name-display) "Beta testing"))
    (should (string= (slot-value detail 'timezone) ""))
    (should (= (slot-value detail 'id) 7))
    (should (string= (slot-value detail 'full-name) "Beta testing"))
    (should (string= (slot-value detail 'photo) "//www.gravatar.com/avatar/4648b6d514c3ecece1b87136ceeda1d1?size=80"))
    (should (string= (slot-value detail 'username) "beta.tester"))
    (should (string= (slot-value detail 'big-photo) "//www.gravatar.com/avatar/4648b6d514c3ecece1b87136ceeda1d1?size=80"))))

(ert-deftest taiga-api-wiki-page-from-alist ()
  "`taiga-api-wiki-page-from-alist' works properly."
  (let ((page (taiga-api-test--data
               "wiki-page" #'taiga-api-wiki-page-from-alist)))
    (should (taiga-api-wiki-page-p page))
    (should (string= (slot-value page 'html) "<p>Lorem ipsum dolor.</p>"))
    (should (= (slot-value page 'editions) 1))
    (should (= (slot-value page 'id) 1))
    (should (= (slot-value page 'version) 1))
    (should (= (slot-value page 'project) 1))
    (should (string= (slot-value page 'slug) "home"))
    (should (string= (slot-value page 'content) "Lorem ipsum dolor."))
    (should (= (slot-value page 'owner) 11))
    (should (null (slot-value page 'last-modified)))
    (should (string= (slot-value page 'created-date) "2014-10-30T09:29:53+0000"))
    (should (string= (slot-value page 'modified-date) "2014-10-30T09:29:53+0000"))
    (should (equal (slot-value page 'watchers) []))))

(ert-deftest taiga-api-user-story-from-alist ()
  "Check that `taiga-api-user-story-from-alist' works properly."
  (let ((story (taiga-api-test--data
                "user-story" #'taiga-api-user-story-from-alist)))
    (should (taiga-api-user-story-p story))
    (should (= (slot-value story 'assigned-to) 19))
    (should (= (slot-value story 'backlog-order) 2))
    (should (string= (slot-value story 'blocked-note) ""))
    (should (string= (slot-value story 'blocked-note-html) ""))
    (should (null (slot-value story 'client-requirement)))
    (should (string= (slot-value story 'comment) ""))
    (should (string= (slot-value story 'created-date) "2014-07-29T07:15:50+0000"))
    (should (string= (slot-value story 'description) "Implement API CALL"))
    (should (string= (slot-value story 'description-html) "<p>Implement API CALL</p>"))
    (should (string= (slot-value story 'finish-date) "2014-09-16T17:17:16+0000"))
    (should (not (slot-value story 'generated-from-issue)))
    (should (= (slot-value story 'id) 1149))
    (should (not (slot-value story 'is-archived)))
    (should (not (slot-value story 'is-blocked)))
    (should (slot-value story 'is-closed))
    (should (= (slot-value story 'kanban-order) 37))
    (should (= (slot-value story 'milestone) 98))
    (should (string= (slot-value story 'milestone-name) "Sprint 03"))
    (should (string= (slot-value story 'milestone-slug) "sprint-03"))
    (should (string= (slot-value story 'modified-date) "2014-10-10T12:07:13+0000"))
    (should (null (slot-value story 'origin-issue)))
    (should (= (slot-value story 'owner) 19))
    (should (equal (slot-value story 'points)
                   '((\132 . 364)
                     (\131 . 361)
                     (\130 . 361)
                     (\129 . 361))))
    (should (= (slot-value story 'project) 31))
    (should (= (slot-value story 'ref) 31))
    (should (= (slot-value story 'sprint-order) 2))
    (should (= (slot-value story 'status) 83))
    (should (string= (slot-value story 'subject) "Customer personal data"))
    (should (equal (slot-value story 'tags) ["service catalog" "customer"]))
    (should (not (slot-value story 'team-requirement)))
    (should (= (slot-value story 'total-points) 1.0))
    (should (= (slot-value story 'version) 8))
    (should (equal (slot-value story 'watchers) []))))

(ert-deftest taiga-api-issue-from-alist ()
  "`taiga-api-issue-from-alist' works properly."
  (let ((issue (taiga-api-test--data "issue" #'taiga-api-issue-from-alist)))
    (should (taiga-api-issue-p issue))
    (should (= (slot-value issue 'assigned-to) 19))
    (should (string= (slot-value issue 'blocked-note) ""))
    (should (string= (slot-value issue 'blocked-note-html) ""))
    (should (string= (slot-value issue 'comment) ""))
    (should (string= (slot-value issue 'created-date) "2014-07-29T07:15:50+0000"))
    (should (string= (slot-value issue 'description) "Implement API CALL"))
    (should (string= (slot-value issue 'description-html) "<p>Implement API CALL</p>"))
    (should (string= (slot-value issue 'finish-date) "2014-09-16T17:17:16+0000"))
    (should (= (slot-value issue 'id) 1149))
    (should (not (slot-value issue 'is-blocked)))
    (should (slot-value issue 'is-closed))
    (should (= (slot-value issue 'milestone) 98))
    (should (string= (slot-value issue 'modified-date) "2014-10-10T12:07:13+0000"))
    (should (null (slot-value issue 'finished-date)))
    (should (= (slot-value issue 'owner) 19))
    (should (= (slot-value issue 'project) 31))
    (should (= (slot-value issue 'ref) 31))
    (should (= (slot-value issue 'status) 83))
    (should (= (slot-value issue 'severity) 2))
    (should (= (slot-value issue 'priority) 3))
    (should (= (slot-value issue 'type) 1))
    (should (string= (slot-value issue 'subject) "Customer personal data"))
    (should (equal (slot-value issue 'tags) ["service catalog" "customer"]))
    (should (= (slot-value issue 'version) 8))
    (should (arrayp (slot-value issue 'watchers)))
    (should (arrayp (slot-value issue 'generated-user-stories)))
    (should (null (slot-value issue 'votes)))
    (should (taiga-api-neighbors-p (slot-value issue 'neighbors)))))

(ert-deftest taiga-api-neighbors-from-alist ()
  "`taiga-api-neighbors-from-alist' works properly."
  (let ((neighbors (taiga-api-test--data
                    "neighbors" #'taiga-api-neighbors-from-alist)))
    (should (taiga-api-neighbors-p neighbors))
    (should (taiga-api-neighbor-p (taiga-api-neighbors-next neighbors)))
    (should (null (taiga-api-neighbors-previous neighbors)))))

(ert-deftest taiga-api-neighbor-from-alist ()
  "`taiga-api-neighbor-from-alist' works properly."
  (let ((neighbor (taiga-api-test--data
                   "neighbor" #'taiga-api-neighbor-from-alist)))
    (should (taiga-api-neighbor-p neighbor))
    (should (= (slot-value neighbor 'id) 16))
    (should (= (slot-value neighbor 'ref) 126))
    (should (string= (slot-value neighbor 'subject) "Support for bulk actions"))))

(ert-deftest taiga-api-task-from-alist ()
  "`taiga-api-task-from-alist' works properly."
  (let ((task (taiga-api-test--data "task" #'taiga-api-task-from-alist)))
    (should (taiga-api-task-p task))
    (should (= (slot-value task 'assigned-to) 19))
    (should (string= (slot-value task 'blocked-note) ""))
    (should (string= (slot-value task 'blocked-note-html) ""))
    (should (string= (slot-value task 'comment) ""))
    (should (string= (slot-value task 'milestone-slug) "sprint-2014-11-1"))
    (should (string= (slot-value task 'created-date) "2014-07-29T07:15:50+0000"))
    (should (string= (slot-value task 'description) "Implement API CALL"))
    (should (string= (slot-value task 'description-html) "<p>Implement API CALL</p>"))
    (should (= (slot-value task 'id) 1149))
    (should (not (slot-value task 'is-blocked)))
    (should (slot-value task 'is-closed))
    (should (= (slot-value task 'milestone) 98))
    (should (string= (slot-value task 'modified-date) "2014-10-10T12:07:13+0000"))
    (should (null (slot-value task 'finished-date)))
    (should (= (slot-value task 'owner) 19))
    (should (= (slot-value task 'project) 31))
    (should (= (slot-value task 'user-story) 17))
    (should (= (slot-value task 'ref) 31))
    (should (= (slot-value task 'status) 83))
    (should (string= (slot-value task 'subject) "Customer personal data"))
    (should (equal (slot-value task 'tags) ["service catalog" "customer"]))
    (should (= (slot-value task 'us-order) 1))
    (should (= (slot-value task 'taskboard-order) 1))
    (should (= (slot-value task 'version) 8))
    (should (not (slot-value task 'is-iocaine)))
    (should (null (slot-value task 'external-reference)))
    (should (arrayp (slot-value task 'watchers)))
    (should (taiga-api-neighbors-p (slot-value task 'neighbors)))))

(ert-deftest taiga-api-search-result-from-alist ()
  "`taiga-api-search-result-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "search-results" #'taiga-api-search-result-from-alist)))
    (should (taiga-api-search-result-p result))
    (mapc (lambda (page) (should (taiga-api-wiki-page-p page)))
          (slot-value result 'wikipages))
    (mapc (lambda (story) (should (taiga-api-user-story-p story)))
          (slot-value result 'userstories))
    (mapc (lambda (issue) (should (taiga-api-issue-p issue)))
          (slot-value result 'issues))
    (mapc (lambda (task) (should (taiga-api-task-p task)))
          (slot-value result 'tasks))
    (should (= (slot-value result 'count) 4))))

(ert-deftest taiga-api-user-storage-data-from-alist ()
  "`taiga-api-user-storage-data-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "user-storage-data"
                 #'taiga-api-user-storage-data-from-alist)))
    (should (taiga-api-user-storage-data-p result))
    (should (string= (slot-value result 'key) "favorite-forest"))
    (should (string= (slot-value result 'value) "Taiga"))
    (should (string= (slot-value result 'created-date) "2014-11-13T16:58:35+0000"))
    (should (string= (slot-value result 'modified-date) "2014-11-13T16:58:35+0000"))))

(ert-deftest taiga-api-many-user-storage-data-from-array ()
  "`taiga-api-many-user-storage-data-from-array' works properly."
  (let ((result (taiga-api-test--data
                 "user-storage-data-list"
                 #'taiga-api-many-user-storage-data-from-array)))
    (should (listp result))
    (mapc (lambda (stor)
            (should (taiga-api-user-storage-data-p stor))
            (should (string= (slot-value stor 'key) "favorite-forest"))
            (should (string= (slot-value stor 'value) "Taiga"))
            (should (string= (slot-value stor 'created-date) "2014-11-13T16:58:35+0000"))
            (should (string= (slot-value stor 'modified-date) "2014-11-13T16:58:35+0000")))
          result)))

(ert-deftest taiga-api-project-template-from-alist ()
  "`taiga-api-project-template-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "project-template"
                 #'taiga-api-project-template-from-alist)))
    (should (taiga-api-project-template-p result))
    (should (taiga-api-project-template-options-p
             (slot-value result 'default-options)))
    (let ((statuses (slot-value result 'us-statuses)))
      (should (listp statuses))
      (should (taiga-api-project-template-user-story-status-p (elt statuses 0))))
    (let ((points (slot-value result 'points)))
      (should (listp points))
      (should (taiga-api-project-template-point-p (elt points 0))))
    (let ((statuses (slot-value result 'task-statuses)))
      (should (listp statuses))
      (should (taiga-api-project-template-status-p (elt statuses 0))))
    (let ((statuses (slot-value result 'issue-statuses)))
      (should (listp statuses))
      (should (taiga-api-project-template-status-p (elt statuses 0))))
    (let ((types (slot-value result 'issue-types)))
      (should (listp types))
      (should (taiga-api-project-template-thingy-p (elt types 0))))
    (let ((priorities (slot-value result 'priorities)))
      (should (listp priorities))
      (should (taiga-api-project-template-thingy-p (elt priorities 0))))
    (let ((severities (slot-value result 'severities)))
      (should (listp severities))
      (should (taiga-api-project-template-thingy-p (elt severities 0))))
    (let ((roles (slot-value result 'roles)))
      (should (listp roles))
      (should (taiga-api-project-template-role-p (elt roles 0))))
    (should (= (slot-value result 'id) 2))
    (should (string= (slot-value result 'name) "Kanban"))
    (should (string= (slot-value result 'slug) "kanban"))
    (should (string= (slot-value result 'description)
                     "Sample description"))
    (should (string= (slot-value result 'created-date)
                     "2014-04-22T14:50:19+0000"))
    (should (string= (slot-value result 'modified-date)
                     "2014-07-25T13:11:42+0000"))
    (should (string= (slot-value result 'default-owner-role)
                     "product-owner"))
    (should (not (slot-value result 'is-backlog-activated)))
    (should (slot-value result 'is-kanban-activated))
    (should (not (slot-value result 'is-wiki-activated)))
    (should (not (slot-value result 'is-issues-activated)))
    (should (null (slot-value result 'videoconferences)))
    (should (string= (slot-value result 'videoconferences-salt) ""))))

(ert-deftest taiga-api-many-project-template-from-array ()
  "`taiga-api-many-project-template-from-array' works properly."
  (let ((result (taiga-api-test--data
                 "project-template-list"
                 #'taiga-api-many-project-template-from-array)))
    (should (listp result))
    (mapc (lambda (template)
            (should (taiga-api-project-template-p template)))
          result)))

(ert-deftest taiga-api-project-template-options-from-alist ()
  "`taiga-api-project-template-options-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "project-template-options"
                 #'taiga-api-project-template-options-from-alist)))
    (should (taiga-api-project-template-options-p result))
    (should (string= (slot-value result 'us-status) "New"))
    (should (string= (slot-value result 'points) "?"))
    (should (string= (slot-value result 'priority) "Normal"))
    (should (string= (slot-value result 'severity) "Normal"))
    (should (string= (slot-value result 'task-status) "New"))
    (should (string= (slot-value result 'issue-type) "Bug"))
    (should (string= (slot-value result 'issue-status) "New"))))

(ert-deftest taiga-api-project-template-options-to-alist ()
  "`taiga-api-project-template-options-to-alist' works properly."
  (let ((options (taiga-api-project-template-options-to-alist
                  (make-instance 'taiga-api-project-template-options
                   :us-status "New" :points "?" :priority "Normal"
                   :severity "Normal" :task-status "New"
                   :issue-type "Bug" :issue-status "New"))))
    (should (string= "New" (cdr (assq 'us_status options))))
    (should (string= "?" (cdr (assq 'points options))))
    (should (string= "Normal" (cdr (assq 'priority options))))
    (should (string= "Normal" (cdr (assq 'severity options))))
    (should (string= "New" (cdr (assq 'task_status options))))
    (should (string= "Bug" (cdr (assq 'issue_type options))))
    (should (string= "New" (cdr (assq 'issue_status options))))))

(ert-deftest taiga-api-project-template-user-story-status-from-alist ()
  "`taiga-api-project-template-user-story-status-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "project-template-user-story-status"
                 #'taiga-api-project-template-user-story-status-from-alist)))
    (should (taiga-api-project-template-user-story-status-p result))
    (should (null (slot-value result 'wip-limit)))
    (should (string= (slot-value result 'color) "#999999"))
    (should (string= (slot-value result 'name) "New"))
    (should (string= (slot-value result 'slug) "new"))
    (should (= (slot-value result 'order) 1))
    (should (not (slot-value result 'is-closed)))))

(ert-deftest taiga-api-many-project-template-user-story-status-from-array ()
  "`taiga-api-many-project-template-user-story-status-from-array' works properly."
  (let ((result (taiga-api-test--data
                 "project-template-user-story-status-list"
                 #'taiga-api-many-project-template-user-story-status-from-array)))
    (should (listp result))
    (mapc (lambda (status)
            (should (taiga-api-project-template-user-story-status-p status)))
          result)))

(ert-deftest taiga-api-project-template-user-story-status-to-alist ()
  "`taiga-api-project-template-user-story-status-to-alist' works properly."
  (let ((status (taiga-api-project-template-user-story-status-to-alist
                 (make-instance 'taiga-api-project-template-user-story-status
                  :wip-limit 3 :color "#ffffff" :name "Test" :slug "test"
                  :order 33 :is-closed nil))))
    (should (= 3 (cdr (assq 'wip_limit status))))
    (should (string= "#ffffff" (cdr (assq 'color status))))
    (should (string= "Test" (cdr (assq 'name status))))
    (should (string= "test" (cdr (assq 'slug status))))
    (should (= 33 (cdr (assq 'order status))))
    (should (eql :json-false (cdr (assq 'is_closed status))))))

(ert-deftest taiga-api-project-template-point-from-alist ()
  "`taiga-api-project-template-point-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "project-template-point"
                 #'taiga-api-project-template-point-from-alist)))
    (should (taiga-api-project-template-point-p result))
    (should (null (slot-value result 'value)))
    (should (string= (slot-value result 'name) "?"))
    (should (= (slot-value result 'order) 1))))

(ert-deftest taiga-api-many-project-template-point-from-array ()
  "`taiga-api-many-project-template-point-from-array' works properly."
  (let ((result (taiga-api-test--data
                 "project-template-point-list"
                 #'taiga-api-many-project-template-point-from-array)))
    (should (listp result))
    (mapc (lambda (point)
            (should (taiga-api-project-template-point-p point)))
          result)))

(ert-deftest taiga-api-project-template-point-to-alist ()
  "`taiga-api-project-template-point-to-alist' works properly."
  (let ((point (taiga-api-project-template-point-to-alist
                (make-instance 'taiga-api-project-template-point
                 :value 5 :name "key" :order 55))))
    (should (= 5 (cdr (assq 'value point))))
    (should (string= "key" (cdr (assq 'name point))))
    (should (= 55 (cdr (assq 'order point))))))

(ert-deftest taiga-api-project-template-status-from-alist ()
  "`taiga-api-project-template-point-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "project-template-status"
                 #'taiga-api-project-template-status-from-alist)))
    (should (taiga-api-project-template-status-p result))
    (should (string= (slot-value result 'color) "#999999"))
    (should (string= (slot-value result 'name) "New"))
    (should (string= (slot-value result 'slug) "new"))
    (should (= (slot-value result 'order) 1))
    (should (not (slot-value result 'is-closed)))))

(ert-deftest taiga-api-many-project-template-status-from-array ()
  "`taiga-api-many-project-template-status-from-array' works properly."
  (let ((result (taiga-api-test--data
                 "project-template-status-list"
                 #'taiga-api-many-project-template-status-from-array)))
    (should (listp result))
    (mapc (lambda (status)
            (should (taiga-api-project-template-status-p status)))
          result)))

(ert-deftest taiga-api-project-template-status-to-alist ()
  "`taiga-api-project-template-status-to-alist' works properly."
  (let ((status (taiga-api-project-template-status-to-alist
                 (make-instance 'taiga-api-project-template-status
                  :color "#ffffff" :name "Something" :slug "something"
                  :order 23 :is-closed nil))))
    (should (string= "#ffffff" (cdr (assq 'color status))))
    (should (string= "Something" (cdr (assq 'name status))))
    (should (string= "something" (cdr (assq 'slug status))))
    (should (= 23 (cdr (assq 'order status))))
    (should (eql :json-false (cdr (assq 'is_closed status))))))

(ert-deftest taiga-api-project-template-thingy-from-alist ()
  "`taiga-api-project-template-thingy-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "project-template-thingy"
                 #'taiga-api-project-template-thingy-from-alist)))
    (should (taiga-api-project-template-thingy-p result))
    (should (string= (slot-value result 'color) "#cc0000"))
    (should (string= (slot-value result 'name) "Bug"))
    (should (= (slot-value result 'order) 1))))

(ert-deftest taiga-api-many-project-template-thingy-from-alist ()
  "`taiga-api-many-project-template-thingy-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "project-template-thingy-list"
                 #'taiga-api-many-project-template-thingy-from-array)))
    (should (listp result))
    (mapc (lambda (status)
            (should (taiga-api-project-template-thingy-p status)))
          result)))

(ert-deftest taiga-api-project-template-thingy-to-alist ()
  "`taiga-api-project-template-thingy-to-alist' works properly."
  (let ((thingy (taiga-api-project-template-thingy-to-alist
                 (make-instance 'taiga-api-project-template-thingy
                  :color "#ffffff" :name "Something" :order 99))))
    (should (string= "#ffffff" (cdr (assq 'color thingy))))
    (should (string= "Something" (cdr (assq 'name thingy))))
    (should (= 99 (cdr (assq 'order thingy))))))

(ert-deftest taiga-api-project-template-role-from-alist ()
  "`taiga-api-project-template-role-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "project-template-role"
                 #'taiga-api-project-template-role-from-alist)))
    (should (taiga-api-project-template-role-p result))
    (should (arrayp (slot-value result 'permissions)))
    (should (string= (aref (slot-value result 'permissions) 0) "add_issue"))
    (should (= (slot-value result 'order) 10))
    (should (slot-value result 'computable))
    (should (string= (slot-value result 'slug) "ux"))
    (should (string= (slot-value result 'name) "UX"))))

(ert-deftest taiga-api-project-template-role-to-alist ()
  "`taiga-api-project-template-role-to-alist' works properly."
  (let ((role (taiga-api-project-template-role-to-alist
               (make-instance 'taiga-api-project-template-role
                :permissions ["add_issue" "modify_issue"]
                :order 1 :computable nil :slug "test" :name "Test"))))
    (should (string= "add_issue" (aref (cdr (assq 'permissions role)) 0)))
    (should (= 1 (cdr (assq 'order role))))
    (should (eql :json-false (cdr (assq 'computable role))))
    (should (string= "test" (cdr (assq 'slug role))))
    (should (string= "Test" (cdr (assq 'name role))))))

(ert-deftest taiga-api-many-project-template-role-from-array ()
  "`taiga-api-many-project-template-role-from-array' works properly."
  (let ((result (taiga-api-test--data
                 "project-template-role-list"
                 #'taiga-api-many-project-template-role-from-array)))
    (should (listp result))
    (mapc (lambda (role)
            (should (taiga-api-project-template-role-p role)))
          result)))

(ert-deftest taiga-api-project-list-entry-from-alist ()
  "`taiga-api-project-list-entry-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "project-list-entry"
                 #'taiga-api-project-list-entry-from-alist)))
    (should (taiga-api-project-list-entry-p result))
    (should (equal [] (slot-value result 'anon-permissions)))
    (should (string= "2014-09-16T15:39:49+0000" (slot-value result 'created-date)))
    (should (= 1 (slot-value result 'creation-template)))
    (should (= 574 (slot-value result 'default-issue-status)))
    (should (= 127 (slot-value result 'default-issue-type)))
    (should (= 977 (slot-value result 'default-points)))
    (should (= 245 (slot-value result 'default-priority)))
    (should (= 408 (slot-value result 'default-severity)))
    (should (= 411 (slot-value result 'default-task-status)))
    (should (= 352 (slot-value result 'default-us-status)))
    (should (string= "Taiga" (slot-value result 'description)))
    (should (slot-value result 'i-am-owner))
    (should (= 87 (slot-value result 'id)))
    (should (not (slot-value result 'is-backlog-activated)))
    (should (slot-value result 'is-issues-activated))
    (should (slot-value result 'is-kanban-activated))
    (should (not (slot-value result 'is-private)))
    (should (slot-value result 'is-wiki-activated))
    (should (equal [2 120 5 8766 16 121 8971]
                   (slot-value result 'members)))
    (should (string= "2014-10-29T07:35:38+0000" (slot-value result 'modified-date)))
    (should (equal ["admin_project_values" "view_tasks" "view_milestones"
                    "view_project" "delete_us" "modify_project"
                    "remove_member" "vote_issues" "add_wiki_link"
                    "add_issue" "add_task" "delete_wiki_page"
                    "delete_project" "add_us" "view_wiki_pages"
                    "delete_task" "delete_wiki_link" "view_wiki_links"
                    "modify_issue" "view_issues" "modify_wiki_link"
                    "add_wiki_page" "delete_milestone" "modify_us"
                    "modify_wiki_page" "admin_roles" "delete_issue"
                    "add_milestone" "modify_task" "add_member"
                    "modify_milestone" "view_us"]
                   (slot-value result 'my-permissions)))
    (should (string= "AIL" (slot-value result 'name)))
    (should (= 2 (slot-value result 'owner)))
    (should (equal [] (slot-value result 'public-permissions)))
    (should (string= "ail" (slot-value result 'slug)))
    (should (null (slot-value result 'stars)))
    (should (null (slot-value result 'tags)))
    (should (equal '((tags . "#edd400")
                     (notes . "#888a85")
                     (health . "#a40000")
                     (gestor . "#73d216")
                     (cuidador . "#204a87")
                     (api . "#ce5c00"))
                   (slot-value result 'tags-colors)))
    (should (= 3 (slot-value result 'total-milestones)))
    (should (= 20.0 (slot-value result 'total-story-points)))
    (should (taiga-api-user-detail-p (elt (slot-value result 'users) 0)))
    (should (string= "appear-in" (slot-value result 'videoconferences)))
    (should (null (slot-value result 'videoconferences-salt)))))

(ert-deftest taiga-api-many-project-list-entry-from-array ()
  "`taiga-api-many-project-list-entry-from-array' works properly."
  (let ((result (taiga-api-test--data
                 "project-list"
                 #'taiga-api-many-project-list-entry-from-array)))
    (should (listp result))
    (mapc (lambda (project)
            (should (taiga-api-project-list-entry-p project)))
          result)))

(ert-deftest taiga-api-user-detail-from-alist ()
  "`taiga-api-user-detail-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "user-detail" #'taiga-api-user-detail-from-alist)))
    (should (taiga-api-user-detail-p result))
    (should (string= "//www.gravatar.com/avatar/4648b6d514c3ecece1b87136ceeda1d1?size=80" (slot-value result 'big-photo)))
    (should (string= "" (slot-value result 'bio)))
    (should (string= "black" (slot-value result 'color)))
    (should (string= "" (slot-value result 'lang)))
    (should (string= "" (slot-value result 'timezone)))
    (should (string= "beta.testing@taiga.io" (slot-value result 'email)))
    (should (string= "Beta testing" (slot-value result 'full-name)))
    (should (string= "Beta testing" (slot-value result 'full-name-display)))
    (should (null (slot-value result 'github-id)))
    (should (= 1 (slot-value result 'id)))
    (should (slot-value result 'is-active))
    (should (string= "//www.gravatar.com/avatar/4648b6d514c3ecece1b87136ceeda1d1?size=80" (slot-value result 'photo)))
    (should (string= "beta.tester" (slot-value result 'username)))))

(ert-deftest taiga-api-issue-custom-attribute-detail-from-alist ()
  "`taiga-api-issue-custom-attribute-detail-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "issue-custom-attribute-detail"
                 #'taiga-api-issue-custom-attribute-detail-from-alist)))
    (should (taiga-api-issue-custom-attribute-detail-p result))
    (should (equal 1 (slot-value result 'id)))
    (should (equal "Duration" (slot-value result 'name)))
    (should (equal "Duration in minutes" (slot-value result 'description)))
    (should (equal 1 (slot-value result 'order)))
    (should (equal 9 (slot-value result 'project)))))

(ert-deftest taiga-api-issue-status-detail-from-alist ()
  "`taiga-api-issue-status-detail-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "issue-status-detail"
                 #'taiga-api-issue-status-detail-from-alist)))
    (should (taiga-api-issue-status-detail-p result))
    (should (equal "#669933" (slot-value result 'color)))
    (should (equal 143 (slot-value result 'id)))
    (should (equal nil (slot-value result 'is-closed)))
    (should (equal "Open" (slot-value result 'name)))
    (should (equal 1 (slot-value result 'order)))
    (should (equal 40 (slot-value result 'project)))))

(ert-deftest taiga-api-issue-type-detail-from-alist ()
  "`taiga-api-issue-type-detail-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "issue-type-detail"
                 #'taiga-api-issue-type-detail-from-alist)))
    (should (taiga-api-issue-type-detail-p result))
    (should (equal "#669933" (slot-value result 'color)))
    (should (equal 143 (slot-value result 'id)))
    (should (equal "Enhancement" (slot-value result 'name)))
    (should (equal 1 (slot-value result 'order)))
    (should (equal 40 (slot-value result 'project)))))

(ert-deftest taiga-api-membership-detail-from-alist ()
  "`taiga-api-membership-detail-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "membership-detail"
                 #'taiga-api-membership-detail-from-alist)))
    (should (taiga-api-membership-detail-p result))
    (should (equal "Front" (slot-value result 'role-name)))
    (should (equal "Alicia Diaz" (slot-value result 'full-name)))
    (should (equal "accusamus@doloribus.net" (slot-value result 'user-email)))
    (should (equal "#C0FF33" (slot-value result 'color)))
    (should (equal "//www.gravatar.com/avatar/e2cc11c9138524224f87aa72c1a343db?size=80" (slot-value result 'photo)))
    (should (equal "Project Example 0" (slot-value result 'project-name)))
    (should (equal "user6532909695705815086-project-example-0" (slot-value result 'project-slug)))
    (should (equal nil (slot-value result 'invited-by)))
    (should (equal 7 (slot-value result 'id)))
    (should (equal 8 (slot-value result 'user)))
    (should (equal 1 (slot-value result 'project)))
    (should (equal 3 (slot-value result 'role)))
    (should (equal t (slot-value result 'is-owner)))
    (should (equal "2014-11-17T16:19:04+0000" (slot-value result 'created-at)))
    (should (equal nil (slot-value result 'invitation-extra-text)))))

(ert-deftest taiga-api-point-detail-from-alist ()
  "`taiga-api-point-detail-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "point-detail"
                 #'taiga-api-point-detail-from-alist)))
    (should (taiga-api-point-detail-p result))
    (should (equal "#669933" (slot-value result 'color)))
    (should (equal 143 (slot-value result 'id)))
    (should (equal "Huge" (slot-value result 'name)))
    (should (equal 8 (slot-value result 'order)))
    (should (equal 40 (slot-value result 'value)))
    (should (equal 3 (slot-value result 'project)))))

(ert-deftest taiga-api-priority-detail-from-alist ()
  "`taiga-api-priority-detail-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "priority-detail"
                 #'taiga-api-priority-detail-from-alist)))
    (should (taiga-api-priority-detail-p result))
    (should (equal "#669933" (slot-value result 'color)))
    (should (equal 143 (slot-value result 'id)))
    (should (equal "High" (slot-value result 'name)))
    (should (equal 1 (slot-value result 'order)))
    (should (equal 40 (slot-value result 'project)))))

(ert-deftest taiga-api-project-role-from-alist ()
  "`taiga-api-project-role-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "project-role"
                 #'taiga-api-project-role-from-alist)))
    (should (taiga-api-project-role-p result))
    (should (equal t (slot-value result 'computable)))
    (should (equal 49 (slot-value result 'id)))
    (should (equal "UX" (slot-value result 'name)))
    (should (equal 10 (slot-value result 'order)))
    (should (equal "ux" (slot-value result 'slug)))))

;;; Auth

(ert-deftest taiga-api-unsuccessful-normal-login ()
  "An unsuccessful login signals `taiga-api-login-failed'."
  (with-taiga-api-synchronous-response
      400 nil (taiga-api--json-encoded-error)
    (taiga-api-test--ensure-token ""
      (should-error (taiga-api-normal-login "foo" "bar")
                    :type 'taiga-api-login-failed)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-successful-normal-login ()
  "A successful login returns a `taiga-api-user-authentication-detail'."
  (let (taiga-api--auth-token)
    (with-taiga-api-synchronous-response
        200 nil (json-encode '(("username" . "foobar")
                               ("auth_token" . "normaltoken")))
      (taiga-api-test--ensure-token "normaltoken"
        (should (taiga-api-user-authentication-detail-p
                 (taiga-api-normal-login "foo" "bar")))
        (should-not (buffer-live-p taiga-api-test-buffer))))))

(ert-deftest taiga-api-unsuccessful-github-login ()
  "An unsuccessful github login signals `taiga-api-login-failed'."
  (with-taiga-api-synchronous-response
      400 nil (taiga-api--json-encoded-error)
    (taiga-api-test--ensure-token ""
      (should-error (taiga-api-github-login "foo")
                    :type 'taiga-api-login-failed)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-successful-github-login ()
  "A successful github login returns a `taiga-api-user-authentication-detail'."
  (with-taiga-api-synchronous-response
      200 nil (json-encode '(("username" . "foobar")
                             ("auth_token" . "githubtoken")))
    (taiga-api-test--ensure-token "githubtoken"
      (should (taiga-api-user-authentication-detail-p
               (taiga-api-github-login "foo" "token")))
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(taiga-api-test-throttling (taiga-api-normal-login "foo" "bar"))
(taiga-api-test-throttling (taiga-api-github-login "foo" "token"))

(ert-deftest taiga-api-successful-public-registration ()
  "A successful public registration returns a `taiga-api-user-authentication-detail'."
  (with-taiga-api-synchronous-response
      201 nil (json-encode '(("username" . "foo")
                             ("auth_token" . "publictoken")))
    (taiga-api-test--ensure-token "publictoken"
      (should (taiga-api-user-authentication-detail-p
               (taiga-api-register-public
                "foo" "bar" "foo@example.com" "Foo Frobnicate")))
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-unsuccessful-public-registration ()
  "A successful public registration signals `taiga-api-registration-failed'."
  (with-taiga-api-synchronous-response
      400 nil (taiga-api--json-encoded-error)
    (taiga-api-test--ensure-token ""
      (should-error (taiga-api-register-public
                     "foo" "bar" "foo@example.com" "Foo Frobnicate")
                    :type 'taiga-api-registration-failed)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(taiga-api-test-throttling
  (taiga-api-register-public
   "foo" "bar" "foo@example.com" "Foo Frobnicate"))

(ert-deftest taiga-api-successful-private-registration ()
  "A successful private registration returns a `taiga-api-user-authentication-detail'."
  (let (taiga-api--auth-token)
    (with-taiga-api-synchronous-response
        201 nil (json-encode '(("username" . "foo")
                               ("auth_token" . "privatetoken")))
      (taiga-api-test--ensure-token "privatetoken"
        (should (taiga-api-user-authentication-detail-p
                 (taiga-api-register-private
                  t "token" "username" "password")))
        (should-not (buffer-live-p taiga-api-test-buffer))))))

(ert-deftest taiga-api-unsuccessful-private-registration ()
  "An unsuccessful private registration signals `taiga-api-registration-failed'."
  (with-taiga-api-synchronous-response
      400 nil (taiga-api--json-encoded-error)
    (taiga-api-test--ensure-token ""
      (should-error (taiga-api-register-private
                     nil "token" "username" "password" "email" "full-name")
                    :type 'taiga-api-registration-failed)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(taiga-api-test-throttling
  (taiga-api-register-private t "token" "username" "password"))

(ert-deftest taiga-api-normal-login-request ()
  "Request parameters for normal login are setup correctly."
  (let ((func-used 0))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= (json-encode '(("password" . "bar")
                                                 ("username" . "foo")
                                                 ("type" . "normal")))
                                  url-request-data))
                 (should (string= "https://api.taiga.io/api/v1/auth" url))
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-normal-login "foo" "bar"))
    (should (= 1 func-used))))

(ert-deftest taiga-api-github-login-request ()
  "Request parameters for github login are setup correctly."
  (let ((func-used 0))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= (json-encode '(("token" . "bar")
                                                 ("code" . "foo")
                                                 ("type" . "github")))
                                  url-request-data))
                 (should (string= "https://api.taiga.io/api/v1/auth" url))
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-github-login "foo" "bar"))
    (should (= 1 func-used))))

(ert-deftest taiga-api-public-registration-request ()
  "Request parameters for public registrations are setup correctly."
  (let ((func-used 0))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= (json-encode '(("full_name" . "Foo Bar")
                                                 ("email" . "foo@example.com")
                                                 ("password" . "bar")
                                                 ("username" . "foo")
                                                 ("type" . "public")))
                                  url-request-data))
                 (should (string= "https://api.taiga.io/api/v1/auth/register" url))
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 201 CREATED\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-register-public "foo" "bar" "foo@example.com" "Foo Bar"))
    (should (= 1 func-used))))

(ert-deftest taiga-api-private-registration-request ()
  "Request parameters for private registrations are setup correctly."
  (let ((func-used 0))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= (json-encode '(("password" . "bar")
                                                 ("username" . "foo")
                                                 ("token" . "token")
                                                 ("existing" . t)
                                                 ("type" . "private")))
                                  url-request-data))
                 (should (string= "https://api.taiga.io/api/v1/auth/register" url))
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 201 CREATED\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-register-private t "token" "foo" "bar"))
    (should (= 1 func-used))))

;;; Resolver

(ert-deftest taiga-api-successful-project-resolution ()
  "A successful project resolution returns an alist."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (json-encode '((project . 1)))
      (let ((result (taiga-api-resolve-project "project")))
        (should (= 1 (cdr (assq 'project result))))
        (should-not (buffer-live-p taiga-api-test-buffer))))))

(ert-deftest taiga-api-unsuccessful-project-resolution ()
  "An unsuccessful project resolution signals `taiga-api-unresolved'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve-project "project")
                    :type 'taiga-api-unresolved)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(taiga-api-test-unauthenticated (taiga-api-resolve-project "project"))
(taiga-api-test-throttling (taiga-api-resolve-project "project"))

(ert-deftest taiga-api-successful-user-story-resolution ()
  "A successful user story resolution returns an alist."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (json-encode '((us . 26) (project . 1)))
      (let ((result (taiga-api-resolve-user-story "project" "us")))
        (should (= 1 (cdr (assq 'project result))))
        (should (= 26 (cdr (assq 'us result))))
        (should-not (buffer-live-p taiga-api-test-buffer))))))

(ert-deftest taiga-api-unsuccessful-user-story-resolution ()
  "An unsuccessful user story resolution signals `taiga-api-unresolved'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve-user-story "project" "us")
                    :type 'taiga-api-unresolved)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(taiga-api-test-unauthenticated (taiga-api-resolve-user-story "project" "us"))
(taiga-api-test-throttling (taiga-api-resolve-user-story "project" "us"))

(ert-deftest taiga-api-successful-issue-resolution ()
  "A successful issue resolution returns an alist."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (json-encode '((issue . 5209) (project . 1)))
      (let ((result (taiga-api-resolve-issue "project" "issue")))
        (should (= 1 (cdr (assq 'project result))))
        (should (= 5209 (cdr (assq 'issue result))))
        (should-not (buffer-live-p taiga-api-test-buffer))))))

(ert-deftest taiga-api-unsuccessful-issue-resolution ()
  "An unsuccessful issue resolution signals `taiga-api-unresolved'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve-issue "project" "issue")
                    :type 'taiga-api-unresolved)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(taiga-api-test-unauthenticated (taiga-api-resolve-issue "project" "issue"))
(taiga-api-test-throttling (taiga-api-resolve-issue "project" "issue"))

(ert-deftest taiga-api-project-resolution-request ()
  "Request parameters for project resolution are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/resolver?project=some-project" url))
                 (should-have-auth-token "sometoken")
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-resolve-project "some-project"))
    (should (= 1 func-used))))

(ert-deftest taiga-api-user-story-resolution-request ()
  "Request parameters for user story resolution are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/resolver?project=some-project&us=5" url))
                 (should-have-auth-token "sometoken")
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-resolve-user-story "some-project" 5))
    (should (= 1 func-used))))

(ert-deftest taiga-api-issue-resolution-request ()
  "Request paramaters for issue resolution are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/resolver?project=some-project&issue=5" url))
                 (should-have-auth-token "sometoken")
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-resolve-issue "some-project" 5))
    (should (= 1 func-used))))

(ert-deftest taiga-api-successful-task-resolution ()
  "A successful task resolution returns an alist."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (json-encode '((task . 1336) (project . 1)))
      (let ((result (taiga-api-resolve-task "project" "task")))
        (should (= 1 (cdr (assq 'project result))))
        (should (= 1336 (cdr (assq 'task result))))
        (should-not (buffer-live-p taiga-api-test-buffer))))))

(ert-deftest taiga-api-unsuccessful-task-resolution ()
  "An unsuccessful task resolution signals `taiga-api-unresolved'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve-task "project" "task")
                    :type 'taiga-api-unresolved)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(taiga-api-test-unauthenticated (taiga-api-resolve-task "project" "task"))
(taiga-api-test-throttling (taiga-api-resolve-task "project" "task"))

(ert-deftest taiga-api-task-resolution-request ()
  "Request parameters for task resolution are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/resolver?project=some-project&task=5" url))
                 (should-have-auth-token "sometoken")
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-resolve-task "some-project" 5))
    (should (= 1 func-used))))

(ert-deftest taiga-api-successful-milestone-resolution ()
  "A successful milestone resolution returns an alist."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (json-encode '((milestone . 1) (project . 1)))
      (let ((result (taiga-api-resolve-milestone "project" "milestone")))
        (should (= 1 (cdr (assq 'project result))))
        (should (= 1 (cdr (assq 'milestone result))))
        (should-not (buffer-live-p taiga-api-test-buffer))))))

(ert-deftest taiga-api-unsuccessful-milestone-resolution ()
  "An unsuccessful milestone resolution signals `taiga-api-unresolved'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve-milestone "project" "milestone")
                    :type 'taiga-api-unresolved)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(taiga-api-test-unauthenticated
  (taiga-api-resolve-milestone "project" "milestone"))
(taiga-api-test-throttling
  (taiga-api-resolve-milestone "project" "milestone"))

(ert-deftest taiga-api-milestone-resolution-request ()
  "Request parameters for milestone resolution are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/resolver?project=some-project&milestone=some-milestone" url))
                 (should-have-auth-token "sometoken")
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-resolve-milestone "some-project" "some-milestone"))
    (should (= 1 func-used))))

(ert-deftest taiga-api-successful-wiki-resolution ()
  "A successful milestone resolution returns an alist."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (json-encode '((wikipage . 2) (project . 1)))
      (let ((result (taiga-api-resolve-wiki "project" "wikipage")))
        (should (= 1 (cdr (assq 'project result))))
        (should (= 2 (cdr (assq 'wikipage result))))
        (should-not (buffer-live-p taiga-api-test-buffer))))))

(ert-deftest taiga-api-unsuccessful-wiki-resolution ()
  "An unsuccessful wiki resolution raises `taiga-api-unresolved'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve-wiki "project" "wikipage")
                    :type 'taiga-api-unresolved)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(taiga-api-test-unauthenticated (taiga-api-resolve-wiki "project" "wikipage"))
(taiga-api-test-throttling (taiga-api-resolve-wiki "project" "wikipage"))

(ert-deftest taiga-api-wiki-resolution-request ()
  "Request parameters for wiki page resolution are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/resolver?project=some-project&wikipage=home" url))
                 (should-have-auth-token "sometoken")
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-resolve-wiki "some-project" "home"))
    (should (= 1 func-used))))

(ert-deftest taiga-api-successful-resolution ()
  "A successful resolution returns an alist."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (json-encode '((task . 1336)
                               (us . 26)
                               (wikipage . 2)
                               (project . 1)))
      (let ((result (taiga-api-resolve "project" :wikipage "home")))
        (should (= 1 (cdr (assq 'project result))))
        (should (= 1336 (cdr (assq 'task result))))
        (should (= 26 (cdr (assq 'us result))))
        (should (= 2 (cdr (assq 'wikipage result))))
        (should-not (buffer-live-p taiga-api-test-buffer))))))

(ert-deftest taiga-api-unsuccessful-resolution ()
  "An unsuccessful resolution signals `taiga-api-unresolved'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve "project" :task 3)
                    :type 'taiga-api-unresolved)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(taiga-api-test-unauthenticated
  (taiga-api-resolve "project" :milestone "sprint0"))
(taiga-api-test-throttling
  (taiga-api-resolve "project" :milestone "sprint0"))

(ert-deftest taiga-api-resolution-request ()
  "Request parameters for resolution are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/resolver?project=some-project&us=5&milestone=some-milestone" url))
                 (should-have-auth-token "sometoken")
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-resolve "some-project" :us 5 :milestone "some-milestone"))
    (should (= 1 func-used))))

;;; Search

(ert-deftest taiga-api-search-request ()
  "Request parameters for searches are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/search?project=1&text=design" url))
                 (should-have-auth-token "sometoken")
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-search 1 "design"))
    (should (= 1 func-used))))

(ert-deftest taiga-api-successful-search ()
  "A successful search returns a `taiga-api-search-result'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (taiga-api-test--read "search-results")
      (let ((result (taiga-api-search 1 "design")))
        (should (taiga-api-search-result-p result))
        (should (arrayp (slot-value result 'wikipages)))
        (should (arrayp (slot-value result 'userstories)))
        (should (arrayp (slot-value result 'issues)))
        (should (arrayp (slot-value result 'tasks)))
        (should (= 4 (slot-value result 'count)))))))

(taiga-api-test-unauthenticated (taiga-api-search 1 "design"))
(taiga-api-test-throttling (taiga-api-search 1 "design"))

;;; User storage

(ert-deftest taiga-api-list-user-storage-request ()
  "Request parameters for listing user storage are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/user-storage" url))
                 (should-have-auth-token "sometoken")
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "[{\"foo\": \"bar\"}]")
                   (current-buffer)))))
      (taiga-api-list-user-storage))
    (should (= 1 func-used))))

(ert-deftest taiga-api-successful-list-user-storage ()
  "A successful user storage listing returns a list of `taiga-api-user-storage'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (taiga-api-test--read "user-storage-data-list")
      (let ((result (taiga-api-list-user-storage)))
        (should (listp result))
        (mapc (lambda (stor)
                (should (taiga-api-user-storage-data-p stor))
                (should (string= "favorite-forest" (slot-value stor 'key)))
                (should (string= "Taiga" (slot-value stor 'value)))
                (should (string= "2014-11-13T16:58:35+0000" (slot-value stor 'created-date)))
                (should (string= "2014-11-13T16:58:35+0000" (slot-value stor 'modified-date))))
              result)))))

(taiga-api-test-unauthenticated (taiga-api-list-user-storage))
(taiga-api-test-throttling (taiga-api-list-user-storage))

(ert-deftest taiga-api-create-user-storage-request ()
  "Request parameters for creating user storage data are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/user-storage" url))
                 (should-have-auth-token "sometoken")
                 (should (string= (json-encode '(("value" . "bar")
                                                 ("key" . "foo")))
                                  url-request-data))
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 201 CREATED\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-create-user-storage "foo" "bar"))
    (should (= 1 func-used))))

(ert-deftest taiga-api-create-user-storage-success ()
  "Creating user storage successfully returns the object"
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        201 nil (taiga-api-test--read "user-storage-data")
      (let ((result (taiga-api-create-user-storage "foo" "bar")))
        (should (taiga-api-user-storage-data-p result))
        (should (string= (slot-value result 'key) "favorite-forest"))
        (should (string= (slot-value result 'value) "Taiga"))
        (should (string= (slot-value result 'created-date) "2014-11-13T16:58:35+0000"))
        (should (string= (slot-value result 'modified-date) "2014-11-13T16:58:35+0000"))))))

(taiga-api-test-unauthenticated (taiga-api-create-user-storage "foo" "bar"))
(taiga-api-test-throttling (taiga-api-create-user-storage "foo" "bar"))

(ert-deftest taiga-api-get-user-storage-request ()
  "Request parameters for getting user storage are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/user-storage/foo" url))
                 (should-have-auth-token "sometoken")
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-get-user-storage "foo"))
    (should (= 1 func-used))))

(ert-deftest taiga-api-unsuccessful-get-user-data ()
  "An unsuccessful user data fetch signals `taiga-api-not-found'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-get-user-storage "foo")
                    :type 'taiga-api-not-found)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-successful-get-user-data ()
  "A successful user data fetch returns a `taiga-api-user-storage-data'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (taiga-api-test--read "user-storage-data")
      (let ((result (taiga-api-get-user-storage "foo")))
        (should (taiga-api-user-storage-data-p result))
        (should (string= (slot-value result 'key) "favorite-forest"))
        (should (string= (slot-value result 'value) "Taiga"))
        (should (string= (slot-value result 'created-date) "2014-11-13T16:58:35+0000"))
        (should (string= (slot-value result 'modified-date) "2014-11-13T16:58:35+0000"))))))

(taiga-api-test-unauthenticated (taiga-api-get-user-storage "foo"))
(taiga-api-test-throttling (taiga-api-get-user-storage "foo"))

(ert-deftest taiga-api-edit-user-storage-request ()
  "Request parameters for editing user storage are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= url-request-method "PATCH"))
                 (should (string= "https://api.taiga.io/api/v1/user-storage/foo" url))
                 (should-have-auth-token "sometoken")
                 (should (string= (json-encode '(("value" . "bar")))
                                  url-request-data))
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-edit-user-storage "foo" "bar"))
    (should (= 1 func-used))))

(ert-deftest taiga-api-unsuccessful-edit-user-data ()
  "An unsuccessful user data edit signals `taiga-api-not-found'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-edit-user-storage "foo" "bar")
                    :type 'taiga-api-not-found)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-successful-edit-user-data ()
  "A successful user data edit returns the changed object."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (taiga-api-test--read "user-storage-data")
      (let ((result (taiga-api-edit-user-storage "foo" "bar")))
        (should (taiga-api-user-storage-data-p result))
        (should (string= (slot-value result 'key) "favorite-forest"))
        (should (string= (slot-value result 'value) "Taiga"))
        (should (string= (slot-value result 'created-date) "2014-11-13T16:58:35+0000"))
        (should (string= (slot-value result 'modified-date) "2014-11-13T16:58:35+0000"))))))

(taiga-api-test-unauthenticated (taiga-api-edit-user-storage "foo" "bar"))
(taiga-api-test-throttling (taiga-api-edit-user-storage "foo" "bar"))

(ert-deftest taiga-api-delete-user-storage-request ()
  "Request parameters for deleting user storage are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= url-request-method "DELETE"))
                 (should (string= "https://api.taiga.io/api/v1/user-storage/foo" url))
                 (should-have-auth-token "sometoken")
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 204 NO CONTENT\n"
                           "\n")
                   (current-buffer)))))
      (taiga-api-delete-user-storage "foo"))
    (should (= 1 func-used))))

(ert-deftest taiga-api-unsuccessful-delete-user-data ()
  "An unsuccessful user data delete signals `taiga-api-not-found'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-delete-user-storage "foo")
                    :type 'taiga-api-not-found)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-successful-delete-user-data ()
  "A successful user data delete returns t."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response 204 nil "\n"
      (should (taiga-api-delete-user-storage "foo")))))

(taiga-api-test-unauthenticated (taiga-api-delete-user-storage "foo"))
(taiga-api-test-throttling (taiga-api-delete-user-storage "foo"))

;;; Project template

(ert-deftest taiga-api-list-project-template-request ()
  "Request parameters for listing project templates are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/project-templates" url))
                 (should-have-auth-token "sometoken")
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "[{\"foo\": \"bar\"}]")
                   (current-buffer)))))
      (taiga-api-list-project-template))
    (should (= 1 func-used))))

(ert-deftest taiga-api-successful-list-project-template ()
  "A successful project template listing returns a list of `taiga-api-project-template'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (taiga-api-test--read "project-template-list")
      (let ((result (taiga-api-list-project-template)))
        (should (listp result))
        (mapc (lambda (template)
                (should (taiga-api-project-template-p template)))
              result)))))

(taiga-api-test-unauthenticated (taiga-api-list-project-template))
(taiga-api-test-throttling (taiga-api-list-project-template))

(ert-deftest taiga-api-create-project-template-request ()
  "Request parameters for creating project templates are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/project-templates" url))
                 (should-have-auth-token "sometoken")
                 (should (string= (json-encode '((roles
                                                  ((permissions "add_issue" "modify_issue")
                                                   (order . 20)
                                                   (computable . t)
                                                   (slug . "design")
                                                   (name . "Design")))
                                                 (severities
                                                  ((color . "#999999")
                                                   (name . "Wishlist")
                                                   (order . 1)))
                                                 (priorities
                                                  ((color . "#999999")
                                                   (name . "Low")
                                                   (order . 1)))
                                                 (issue_types
                                                  ((color . "#cc0000")
                                                   (name . "Bug")
                                                   (order . 1)))
                                                 (issue_statuses
                                                  ((color . "#999999")
                                                   (name . "New")
                                                   (slug . "new")
                                                   (order . 1)
                                                   (is_closed . :json-false)))
                                                 (task_statuses
                                                  ((color . "#999999")
                                                   (name . "New")
                                                   (slug . "new")
                                                   (order . 1)
                                                   (is_closed . :json-false)))
                                                 (points
                                                  ((value)
                                                   (name . "?")
                                                   (order . 1)))
                                                 (us_statuses
                                                  ((wip_limit)
                                                   (color . "#999999")
                                                   (name . "New")
                                                   (slug)
                                                   (order . 1)
                                                   (is_closed . :json-false)))
                                                 (default_options
                                                   (us_status . "New")
                                                   (points . "?")
                                                   (priority . "Normal")
                                                   (severity . "Normal")
                                                   (task_status . "New")
                                                   (issue_type . "Bug")
                                                   (issue_status . "New"))
                                                 (videoconferences_salt . "")
                                                 (is_issues_activated . :json-false)
                                                 (is_wiki_activated . :json-false)
                                                 (is_kanban_activated . t)
                                                 (is_backlog_activated . :json-false)
                                                 (slug . "kanban")
                                                 (default_owner_role . "product-owner")
                                                 (description . "Sample description")
                                                 (name . "Kanban")))
                                  url-request-data))
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 201 CREATED\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-create-project-template
       "Kanban" "Sample description" "product-owner" "kanban"
       nil t nil nil nil nil
       (make-instance 'taiga-api-project-template-options
        :us-status "New" :points "?" :priority "Normal"
        :severity "Normal" :task-status "New" :issue-type "Bug"
        :issue-status "New")
       (list (make-instance 'taiga-api-project-template-user-story-status
              :color "#999999" :name "New" :order 1))
       (list (make-instance 'taiga-api-project-template-point :name "?" :order 1))
       (list (make-instance 'taiga-api-project-template-status
              :color "#999999" :name "New" :slug "new" :order 1))
       (list (make-instance 'taiga-api-project-template-status
              :color "#999999" :name "New" :slug "new" :order 1))
       (list (make-instance 'taiga-api-project-template-thingy
              :color "#cc0000" :name "Bug" :order 1))
       (list (make-instance 'taiga-api-project-template-thingy
              :color "#999999" :name "Low" :order 1))
       (list (make-instance 'taiga-api-project-template-thingy
              :color "#999999" :name "Wishlist" :order 1))
       (list (make-instance 'taiga-api-project-template-role
              :permissions '("add_issue" "modify_issue") :order 20
              :computable t :slug "design" :name "Design"))))
    (should (= func-used 1))))

(ert-deftest taiga-api-successful-create-project-template ()
  "A successful project template creation returns a `taiga-api-project-template'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        201 nil (taiga-api-test--read "project-template")
      (let ((result (taiga-api-create-project-template
                     "Kanban" "Sample description" "product-owner" "kanban"
                     nil t nil nil nil nil
                     (make-instance 'taiga-api-project-template-options
                      :us-status "New" :points "?" :priority "Normal"
                      :severity "Normal" :task-status "New" :issue-type "Bug"
                      :issue-status "New")
                     (list (make-instance 'taiga-api-project-template-user-story-status
                            :color "#999999" :name "New" :order 1))
                     (list (make-instance 'taiga-api-project-template-point :name "?" :order 1))
                     (list (make-instance 'taiga-api-project-template-status
                            :color "#999999" :name "New" :slug "new" :order 1))
                     (list (make-instance 'taiga-api-project-template-status
                            :color "#999999" :name "New" :slug "new" :order 1))
                     (list (make-instance 'taiga-api-project-template-thingy
                            :color "#cc0000" :name "Bug" :order 1))
                     (list (make-instance 'taiga-api-project-template-thingy
                            :color "#999999" :name "Low" :order 1))
                     (list (make-instance 'taiga-api-project-template-thingy
                            :color "#999999" :name "Wishlist" :order 1))
                     (list (make-instance 'taiga-api-project-template-role
                            :permissions '("add_issue" "modify_issue") :order 20
                            :computable t :slug "design" :name "Design")))))
        (should (taiga-api-project-template-p result))))))

(ert-deftest taiga-api-unsuccessful-create-project-template ()
  "An unsuccessful project creation signals `taiga-api-error'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        400 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-create-project-template
                     "Kanban" "Sample description" "product-owner" "kanban"
                     nil t nil nil nil nil
                     (make-instance 'taiga-api-project-template-options
                      :us-status "New" :points "?" :priority "Normal"
                      :severity "Normal" :task-status "New" :issue-type "Bug"
                      :issue-status "New")
                     (list (make-instance 'taiga-api-project-template-user-story-status
                            :color "#999999" :name "New" :order 1))
                     (list (make-instance 'taiga-api-project-template-point :name "?" :order 1))
                     (list (make-instance 'taiga-api-project-template-status
                            :color "#999999" :name "New" :slug "new" :order 1))
                     (list (make-instance 'taiga-api-project-template-status
                            :color "#999999" :name "New" :slug "new" :order 1))
                     (list (make-instance 'taiga-api-project-template-thingy
                            :color "#cc0000" :name "Bug" :order 1))
                     (list (make-instance 'taiga-api-project-template-thingy
                            :color "#999999" :name "Low" :order 1))
                     (list (make-instance 'taiga-api-project-template-thingy
                            :color "#999999" :name "Wishlist" :order 1))
                     (list (make-instance 'taiga-api-project-template-role
                            :permissions '("add_issue" "modify_issue") :order 20
                            :computable t :slug "design" :name "Design")))
                    :type 'taiga-api-error)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(taiga-api-test-unauthenticated (taiga-api-create-project-template
                                 "Kanban" "Sample description" "product-owner" "kanban"
                                 nil t nil nil nil nil
                                 (make-instance 'taiga-api-project-template-options
                                  :us-status "New" :points "?" :priority "Normal"
                                  :severity "Normal" :task-status "New" :issue-type "Bug"
                                  :issue-status "New")
                                 (list (make-instance 'taiga-api-project-template-user-story-status
                                        :color "#999999" :name "New" :order 1))
                                 (list (make-instance 'taiga-api-project-template-point :name "?" :order 1))
                                 (list (make-instance 'taiga-api-project-template-status
                                        :color "#999999" :name "New" :slug "new" :order 1))
                                 (list (make-instance 'taiga-api-project-template-status
                                        :color "#999999" :name "New" :slug "new" :order 1))
                                 (list (make-instance 'taiga-api-project-template-thingy
                                        :color "#cc0000" :name "Bug" :order 1))
                                 (list (make-instance 'taiga-api-project-template-thingy
                                        :color "#999999" :name "Low" :order 1))
                                 (list (make-instance 'taiga-api-project-template-thingy
                                        :color "#999999" :name "Wishlist" :order 1))
                                 (list (make-instance 'taiga-api-project-template-role
                                        :permissions '("add_issue" "modify_issue") :order 20
                                        :computable t :slug "design" :name "Design"))))
(taiga-api-test-throttling (taiga-api-create-project-template
                            "Kanban" "Sample description" "product-owner" "kanban"
                            nil t nil nil nil nil
                            (make-instance 'taiga-api-project-template-options
                             :us-status "New" :points "?" :priority "Normal"
                             :severity "Normal" :task-status "New" :issue-type "Bug"
                             :issue-status "New")
                            (list (make-instance 'taiga-api-project-template-user-story-status
                                   :color "#999999" :name "New" :order 1))
                            (list (make-instance 'taiga-api-project-template-point :name "?" :order 1))
                            (list (make-instance 'taiga-api-project-template-status
                                   :color "#999999" :name "New" :slug "new" :order 1))
                            (list (make-instance 'taiga-api-project-template-status
                                   :color "#999999" :name "New" :slug "new" :order 1))
                            (list (make-instance 'taiga-api-project-template-thingy
                                   :color "#cc0000" :name "Bug" :order 1))
                            (list (make-instance 'taiga-api-project-template-thingy
                                   :color "#999999" :name "Low" :order 1))
                            (list (make-instance 'taiga-api-project-template-thingy
                                   :color "#999999" :name "Wishlist" :order 1))
                            (list (make-instance 'taiga-api-project-template-role
                                   :permissions '("add_issue" "modify_issue") :order 20
                                   :computable t :slug "design" :name "Design"))))

(ert-deftest taiga-api-get-project-template-request ()
  "Request parameters for getting a project template are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/project-templates/1" url))
                 (should-have-auth-token "sometoken")
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-get-project-template 1))
    (should (= 1 func-used))))

(ert-deftest taiga-api-unsuccessful-get-project-template ()
  "An unsuccessful project template fetch signals `taiga-api-not-found'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-get-project-template 5)
                    :type 'taiga-api-not-found)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-successful-get-project-template ()
  "A successful project template fetch returns a `taiga-api-project-template'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (taiga-api-test--read "project-template")
      (let ((result (taiga-api-get-project-template 2)))
        (should (taiga-api-project-template-p result))
        (should (taiga-api-project-template-options-p
                 (slot-value result 'default-options)))
        (should (listp (slot-value result 'us-statuses)))
        (should (listp (slot-value result 'points)))
        (should (listp (slot-value result 'task-statuses)))
        (should (listp (slot-value result 'issue-statuses)))
        (should (listp (slot-value result 'issue-types)))
        (should (listp (slot-value result 'priorities)))
        (should (listp (slot-value result 'severities)))
        (should (listp (slot-value result 'roles)))
        (should (= 2 (slot-value result 'id)))
        (should (string= "Kanban" (slot-value result 'name)))
        (should (string= "kanban" (slot-value result 'slug)))
        (should (string= "Sample description" (slot-value result 'description)))
        (should (string= "2014-04-22T14:50:19+0000" (slot-value result 'created-date)))
        (should (string= "2014-07-25T13:11:42+0000" (slot-value result 'modified-date)))
        (should (string= "product-owner" (slot-value result 'default-owner-role)))
        (should (not (slot-value result 'is-backlog-activated)))
        (should (slot-value result 'is-kanban-activated))
        (should (not (slot-value result 'is-wiki-activated)))
        (should (not (slot-value result 'is-issues-activated)))
        (should (null (slot-value result 'videoconferences)))
        (should (string= "" (slot-value result 'videoconferences-salt)))))))

(taiga-api-test-unauthenticated (taiga-api-get-project-template 1))
(taiga-api-test-throttling (taiga-api-get-project-template 1))

(ert-deftest taiga-api-edit-project-template-request ()
  "Request parameters for editing user storage are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= url-request-method "PATCH"))
                 (should (string= "https://api.taiga.io/api/v1/project-templates/1" url))
                 (should-have-auth-token "sometoken")
                 (should (string= (json-encode '(("description" . "New description")))
                                  url-request-data))
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-edit-project-template 1 :description "New description"))
    (should (= 1 func-used))))

(ert-deftest taiga-api-unsuccessful-edit-project-template ()
  "An unsuccessful project template edit signals `taiga-api-not-found'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-edit-project-template 1)
                    :type 'taiga-api-not-found)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-successful-edit-project-template ()
  "A successful project template edit returns the changed object."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (taiga-api-test--read "project-template")
      (let ((result (taiga-api-edit-project-template 1 :description "New description")))
        (should (taiga-api-project-template-p result))
        (should (taiga-api-project-template-options-p
                 (slot-value result 'default-options)))
        (should (listp (slot-value result 'us-statuses)))
        (should (listp (slot-value result 'points)))
        (should (listp (slot-value result 'task-statuses)))
        (should (listp (slot-value result 'issue-statuses)))
        (should (listp (slot-value result 'issue-types)))
        (should (listp (slot-value result 'priorities)))
        (should (listp (slot-value result 'severities)))
        (should (listp (slot-value result 'roles)))
        (should (= 2 (slot-value result 'id)))
        (should (string= "Kanban" (slot-value result 'name)))
        (should (string= "kanban" (slot-value result 'slug)))
        (should (string= "Sample description" (slot-value result 'description)))
        (should (string= "2014-04-22T14:50:19+0000" (slot-value result 'created-date)))
        (should (string= "2014-07-25T13:11:42+0000" (slot-value result 'modified-date)))
        (should (string= "product-owner" (slot-value result 'default-owner-role)))
        (should (not (slot-value result 'is-backlog-activated)))
        (should (slot-value result 'is-kanban-activated))
        (should (not (slot-value result 'is-wiki-activated)))
        (should (not (slot-value result 'is-issues-activated)))
        (should (null (slot-value result 'videoconferences)))
        (should (string= "" (slot-value result 'videoconferences-salt)))))))

(taiga-api-test-unauthenticated (taiga-api-edit-project-template 1 :description "New description"))
(taiga-api-test-throttling (taiga-api-edit-project-template 1 :description "New description"))

(ert-deftest taiga-api-delete-project-template-request ()
  "Request parameters for deleting a project template are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= url-request-method "DELETE"))
                 (should (string= "https://api.taiga.io/api/v1/project-templates/1" url))
                 (should-have-auth-token "sometoken")
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 204 NO CONTENT\n"
                           "\n")
                   (current-buffer)))))
      (taiga-api-delete-project-template 1))
    (should (= 1 func-used))))

(ert-deftest taiga-api-unsuccessful-delete-project-template ()
  "An unsuccessful project template deletion signals `taiga-api-not-found'."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-delete-project-template 1)
                    :type 'taiga-api-not-found)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-successful-delete-project-template ()
  "A successful project template deletion returns t."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response 204 nil "\n"
      (should (taiga-api-delete-project-template 1)))))

(taiga-api-test-unauthenticated (taiga-api-delete-project-template 1))
(taiga-api-test-throttling (taiga-api-delete-project-template 1))

(ert-deftest taiga-api-list-project-request ()
  "Request parameters for listing projects are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/projects" url))
                 (should-have-auth-token "sometoken")
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "[{\"foo\": \"bar\"}]")
                   (current-buffer)))))
      (taiga-api-list-project))
    (should (= 1 func-used))))

(ert-deftest taiga-api-successful-list-project ()
  "A successful project listing returns alist of `taiga-api-project-list-entry'."
  (let ((taiga-api--auth-token "sometokenn"))
    (with-taiga-api-synchronous-response
        200 nil (taiga-api-test--read "project-list")
      (let ((result (taiga-api-list-project)))
        (should (listp result))
        (mapc (lambda (project)
                (should (taiga-api-project-list-entry-p project)))
              result)))))

(taiga-api-test-unauthenticated (taiga-api-list-project))
(taiga-api-test-throttling (taiga-api-list-project))

(provide 'taiga-api-emacs-test)
;;; taiga-api-emacs-test.el ends here
