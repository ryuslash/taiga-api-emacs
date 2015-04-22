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
    (should (= (taiga-api-issue-assigned-to issue) 19))
    (should (string= (taiga-api-issue-blocked-note issue) ""))
    (should (string= (taiga-api-issue-blocked-note-html issue) ""))
    (should (string= (taiga-api-issue-comment issue) ""))
    (should (string= (taiga-api-issue-created-date issue)
                     "2014-07-29T07:15:50+0000"))
    (should (string= (taiga-api-issue-description issue)
                     "Implement API CALL"))
    (should (string= (taiga-api-issue-description-html issue)
                     "<p>Implement API CALL</p>"))
    (should (string= (taiga-api-issue-finish-date issue)
                     "2014-09-16T17:17:16+0000"))
    (should (= (taiga-api-issue-id issue) 1149))
    (should (not (taiga-api-issue-is-blocked issue)))
    (should (taiga-api-issue-is-closed issue))
    (should (= (taiga-api-issue-milestone issue) 98))
    (should (string= (taiga-api-issue-modified-date issue)
                     "2014-10-10T12:07:13+0000"))
    (should (null (taiga-api-issue-finished-date issue)))
    (should (= (taiga-api-issue-owner issue) 19))
    (should (= (taiga-api-issue-project issue) 31))
    (should (= (taiga-api-issue-ref issue) 31))
    (should (= (taiga-api-issue-status issue) 83))
    (should (= (taiga-api-issue-severity issue) 2))
    (should (= (taiga-api-issue-priority issue) 3))
    (should (= (taiga-api-issue-type issue) 1))
    (should (string= (taiga-api-issue-subject issue)
                     "Customer personal data"))
    (should (equal (taiga-api-issue-tags issue)
                   ["service catalog" "customer"]))
    (should (= (taiga-api-issue-version issue) 8))
    (should (arrayp (taiga-api-issue-watchers issue)))
    (should (arrayp (taiga-api-issue-generated-user-stories issue)))
    (should (null (taiga-api-issue-votes issue)))
    (should (taiga-api-neighbors-p (taiga-api-issue-neighbors issue)))))

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
    (should (= (taiga-api-neighbor-id neighbor) 16))
    (should (= (taiga-api-neighbor-ref neighbor) 126))
    (should (string= (taiga-api-neighbor-subject neighbor)
                     "Support for bulk actions"))))

(ert-deftest taiga-api-task-from-alist ()
  "`taiga-api-task-from-alist' works properly."
  (let ((task (taiga-api-test--data "task" #'taiga-api-task-from-alist)))
    (should (taiga-api-task-p task))
    (should (= (taiga-api-task-assigned-to task) 19))
    (should (string= (taiga-api-task-blocked-note task) ""))
    (should (string= (taiga-api-task-blocked-note-html task) ""))
    (should (string= (taiga-api-task-comment task) ""))
    (should (string= (taiga-api-task-milestone-slug task)
                     "sprint-2014-11-1"))
    (should (string= (taiga-api-task-created-date task)
                     "2014-07-29T07:15:50+0000"))
    (should (string= (taiga-api-task-description task)
                     "Implement API CALL"))
    (should (string= (taiga-api-task-description-html task)
                     "<p>Implement API CALL</p>"))
    (should (= (taiga-api-task-id task) 1149))
    (should (not (taiga-api-task-is-blocked task)))
    (should (taiga-api-task-is-closed task))
    (should (= (taiga-api-task-milestone task) 98))
    (should (string= (taiga-api-task-modified-date task)
                     "2014-10-10T12:07:13+0000"))
    (should (null (taiga-api-task-finished-date task)))
    (should (= (taiga-api-task-owner task) 19))
    (should (= (taiga-api-task-project task) 31))
    (should (= (taiga-api-task-user-story task) 17))
    (should (= (taiga-api-task-ref task) 31))
    (should (= (taiga-api-task-status task) 83))
    (should (string= (taiga-api-task-subject task)
                     "Customer personal data"))
    (should (equal (taiga-api-task-tags task)
                   ["service catalog" "customer"]))
    (should (= (taiga-api-task-us-order task) 1))
    (should (= (taiga-api-task-taskboard-order task) 1))
    (should (= (taiga-api-task-version task) 8))
    (should (not (taiga-api-task-is-iocaine task)))
    (should (null (taiga-api-task-external-reference task)))
    (should (arrayp (taiga-api-task-watchers task)))
    (should (taiga-api-neighbors-p (taiga-api-task-neighbors task)))))

(ert-deftest taiga-api-search-result-from-alist ()
  "`taiga-api-search-result-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "search-results" #'taiga-api-search-result-from-alist)))
    (should (taiga-api-search-result-p result))
    (mapc (lambda (page) (should (taiga-api-wiki-page-p page)))
          (taiga-api-search-result-wikipages result))
    (mapc (lambda (story) (should (taiga-api-user-story-p story)))
          (taiga-api-search-result-userstories result))
    (mapc (lambda (issue) (should (taiga-api-issue-p issue)))
          (taiga-api-search-result-issues result))
    (mapc (lambda (task) (should (taiga-api-task-p task)))
          (taiga-api-search-result-tasks result))
    (should (= (taiga-api-search-result-count result) 4))))

(ert-deftest taiga-api-user-storage-data-from-alist ()
  "`taiga-api-user-storage-data-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "user-storage-data"
                 #'taiga-api-user-storage-data-from-alist)))
    (should (taiga-api-user-storage-data-p result))
    (should (string= (taiga-api-user-storage-data-key result)
                     "favorite-forest"))
    (should (string= (taiga-api-user-storage-data-value result) "Taiga"))
    (should (string= (taiga-api-user-storage-data-created-date result)
                     "2014-11-13T16:58:35+0000"))
    (should (string= (taiga-api-user-storage-data-modified-date result)
                     "2014-11-13T16:58:35+0000"))))

(ert-deftest taiga-api-many-user-storage-data-from-array ()
  "`taiga-api-many-user-storage-data-from-array' works properly."
  (let ((result (taiga-api-test--data
                 "user-storage-data-list"
                 #'taiga-api-many-user-storage-data-from-array)))
    (should (listp result))
    (mapc (lambda (stor)
            (should (taiga-api-user-storage-data-p stor))
            (should (string= (taiga-api-user-storage-data-key stor) "favorite-forest"))
            (should (string= (taiga-api-user-storage-data-value stor) "Taiga"))
            (should (string= (taiga-api-user-storage-data-created-date stor) "2014-11-13T16:58:35+0000"))
            (should (string= (taiga-api-user-storage-data-modified-date stor) "2014-11-13T16:58:35+0000")))
          result)))

(ert-deftest taiga-api-project-template-from-alist ()
  "`taiga-api-project-template-from-alist' works properly."
  (let ((result (taiga-api-test--data
                 "project-template"
                 #'taiga-api-project-template-from-alist)))
    (should (taiga-api-project-template-p result))
    (should (taiga-api-project-template-options-p
             (taiga-api-project-template-default-options result)))
    (let ((statuses (taiga-api-project-template-us-statuses result)))
      (should (listp statuses))
      (should (taiga-api-project-template-user-story-status-p (elt statuses 0))))
    (let ((points (taiga-api-project-template-points result)))
      (should (listp points))
      (should (taiga-api-project-template-point-p (elt points 0))))
    (let ((statuses (taiga-api-project-template-task-statuses result)))
      (should (listp statuses))
      (should (taiga-api-project-template-status-p (elt statuses 0))))
    (let ((statuses (taiga-api-project-template-issue-statuses result)))
      (should (listp statuses))
      (should (taiga-api-project-template-status-p (elt statuses 0))))
    (let ((types (taiga-api-project-template-issue-types result)))
      (should (listp types))
      (should (taiga-api-project-template-thingy-p (elt types 0))))
    (let ((priorities (taiga-api-project-template-priorities result)))
      (should (listp priorities))
      (should (taiga-api-project-template-thingy-p (elt priorities 0))))
    (let ((severities (taiga-api-project-template-severities result)))
      (should (listp severities))
      (should (taiga-api-project-template-thingy-p (elt severities 0))))
    (let ((roles (taiga-api-project-template-roles result)))
      (should (listp roles))
      (should (taiga-api-project-template-role-p (elt roles 0))))
    (should (= (taiga-api-project-template-id result) 2))
    (should (string= (taiga-api-project-template-name result) "Kanban"))
    (should (string= (taiga-api-project-template-slug result) "kanban"))
    (should (string= (taiga-api-project-template-description result)
                     "Sample description"))
    (should (string= (taiga-api-project-template-created-date result)
                     "2014-04-22T14:50:19+0000"))
    (should (string= (taiga-api-project-template-modified-date result)
                     "2014-07-25T13:11:42+0000"))
    (should (string= (taiga-api-project-template-default-owner-role result)
                     "product-owner"))
    (should (not (taiga-api-project-template-is-backlog-activated result)))
    (should (taiga-api-project-template-is-kanban-activated result))
    (should (not (taiga-api-project-template-is-wiki-activated result)))
    (should (not (taiga-api-project-template-is-issues-activated result)))
    (should (null (taiga-api-project-template-videoconferences result)))
    (should (string= (taiga-api-project-template-videoconferences-salt result) ""))))

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
    (should (string= (taiga-api-project-template-options-us-status result) "New"))
    (should (string= (taiga-api-project-template-options-points result) "?"))
    (should (string= (taiga-api-project-template-options-priority result) "Normal"))
    (should (string= (taiga-api-project-template-options-severity result) "Normal"))
    (should (string= (taiga-api-project-template-options-task-status result) "New"))
    (should (string= (taiga-api-project-template-options-issue-type result) "Bug"))
    (should (string= (taiga-api-project-template-options-issue-status result) "New"))))

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
    (should (null (taiga-api-project-template-user-story-status-wip-limit result)))
    (should (string= (taiga-api-project-template-user-story-status-color result) "#999999"))
    (should (string= (taiga-api-project-template-user-story-status-name result) "New"))
    (should (string= (taiga-api-project-template-user-story-status-slug result) "new"))
    (should (= (taiga-api-project-template-user-story-status-order result) 1))
    (should (not (taiga-api-project-template-user-story-status-is-closed result)))))

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
    (should (null (taiga-api-project-template-point-value result)))
    (should (string= (taiga-api-project-template-point-name result) "?"))
    (should (= (taiga-api-project-template-point-order result) 1))))

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
    (should (string= (taiga-api-project-template-status-color result) "#999999"))
    (should (string= (taiga-api-project-template-status-name result) "New"))
    (should (string= (taiga-api-project-template-status-slug result) "new"))
    (should (= (taiga-api-project-template-status-order result) 1))
    (should (not (taiga-api-project-template-status-is-closed result)))))

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
    (should (string= (taiga-api-project-template-thingy-color result) "#cc0000"))
    (should (string= (taiga-api-project-template-thingy-name result) "Bug"))
    (should (= (taiga-api-project-template-thingy-order result) 1))))

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
    (should (arrayp (taiga-api-project-template-role-permissions result)))
    (should (string= (aref (taiga-api-project-template-role-permissions result) 0) "add_issue"))
    (should (= (taiga-api-project-template-role-order result) 10))
    (should (taiga-api-project-template-role-computable result))
    (should (string= (taiga-api-project-template-role-slug result) "ux"))
    (should (string= (taiga-api-project-template-role-name result) "UX"))))

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
    (should (equal [] (taiga-api-project-list-entry-anon-permissions result)))
    (should (string= "2014-09-16T15:39:49+0000" (taiga-api-project-list-entry-created-date result)))
    (should (= 1 (taiga-api-project-list-entry-creation-template result)))
    (should (= 574 (taiga-api-project-list-entry-default-issue-status result)))
    (should (= 127 (taiga-api-project-list-entry-default-issue-type result)))
    (should (= 977 (taiga-api-project-list-entry-default-points result)))
    (should (= 245 (taiga-api-project-list-entry-default-priority result)))
    (should (= 408 (taiga-api-project-list-entry-default-severity result)))
    (should (= 411 (taiga-api-project-list-entry-default-task-status result)))
    (should (= 352 (taiga-api-project-list-entry-default-us-status result)))
    (should (string= "Taiga" (taiga-api-project-list-entry-description result)))
    (should (taiga-api-project-list-entry-i-am-owner result))
    (should (= 87 (taiga-api-project-list-entry-id result)))
    (should (not (taiga-api-project-list-entry-is-backlog-activated result)))
    (should (taiga-api-project-list-entry-is-issues-activated result))
    (should (taiga-api-project-list-entry-is-kanban-activated result))
    (should (not (taiga-api-project-list-entry-is-private result)))
    (should (taiga-api-project-list-entry-is-wiki-activated result))
    (should (equal [2 120 5 8766 16 121 8971]
                   (taiga-api-project-list-entry-members result)))
    (should (string= "2014-10-29T07:35:38+0000" (taiga-api-project-list-entry-modified-date result)))
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
                   (taiga-api-project-list-entry-my-permissions result)))
    (should (string= "AIL" (taiga-api-project-list-entry-name result)))
    (should (= 2 (taiga-api-project-list-entry-owner result)))
    (should (equal [] (taiga-api-project-list-entry-public-permissions result)))
    (should (string= "ail" (taiga-api-project-list-entry-slug result)))
    (should (null (taiga-api-project-list-entry-stars result)))
    (should (null (taiga-api-project-list-entry-tags result)))
    (should (equal '((tags . "#edd400")
                     (notes . "#888a85")
                     (health . "#a40000")
                     (gestor . "#73d216")
                     (cuidador . "#204a87")
                     (api . "#ce5c00"))
                   (taiga-api-project-list-entry-tags-colors result)))
    (should (= 3 (taiga-api-project-list-entry-total-milestones result)))
    (should (= 20.0 (taiga-api-project-list-entry-total-story-points result)))
    (should (taiga-api-user-detail-p (elt (taiga-api-project-list-entry-users result) 0)))
    (should (string= "appear-in" (taiga-api-project-list-entry-videoconferences result)))
    (should (null (taiga-api-project-list-entry-videoconferences-salt result)))))

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
        (should (arrayp (taiga-api-search-result-wikipages result)))
        (should (arrayp (taiga-api-search-result-userstories result)))
        (should (arrayp (taiga-api-search-result-issues result)))
        (should (arrayp (taiga-api-search-result-tasks result)))
        (should (= 4 (taiga-api-search-result-count result)))))))

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
                (should (string= "favorite-forest" (taiga-api-user-storage-data-key stor)))
                (should (string= "Taiga" (taiga-api-user-storage-data-value stor)))
                (should (string= "2014-11-13T16:58:35+0000" (taiga-api-user-storage-data-created-date stor)))
                (should (string= "2014-11-13T16:58:35+0000" (taiga-api-user-storage-data-modified-date stor))))
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
        (should (string= (taiga-api-user-storage-data-key result) "favorite-forest"))
        (should (string= (taiga-api-user-storage-data-value result) "Taiga"))
        (should (string= (taiga-api-user-storage-data-created-date result)
                         "2014-11-13T16:58:35+0000"))
        (should (string= (taiga-api-user-storage-data-modified-date result)
                         "2014-11-13T16:58:35+0000"))))))

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
        (should (string= (taiga-api-user-storage-data-key result) "favorite-forest"))
        (should (string= (taiga-api-user-storage-data-value result) "Taiga"))
        (should (string= (taiga-api-user-storage-data-created-date result)
                         "2014-11-13T16:58:35+0000"))
        (should (string= (taiga-api-user-storage-data-modified-date result)
                         "2014-11-13T16:58:35+0000"))))))

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
        (should (string= (taiga-api-user-storage-data-key result) "favorite-forest"))
        (should (string= (taiga-api-user-storage-data-value result) "Taiga"))
        (should (string= (taiga-api-user-storage-data-created-date result)
                         "2014-11-13T16:58:35+0000"))
        (should (string= (taiga-api-user-storage-data-modified-date result)
                         "2014-11-13T16:58:35+0000"))))))

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
                 (taiga-api-project-template-default-options result)))
        (should (listp (taiga-api-project-template-us-statuses result)))
        (should (listp (taiga-api-project-template-points result)))
        (should (listp (taiga-api-project-template-task-statuses result)))
        (should (listp (taiga-api-project-template-issue-statuses result)))
        (should (listp (taiga-api-project-template-issue-types result)))
        (should (listp (taiga-api-project-template-priorities result)))
        (should (listp (taiga-api-project-template-severities result)))
        (should (listp (taiga-api-project-template-roles result)))
        (should (= 2 (taiga-api-project-template-id result)))
        (should (string= "Kanban" (taiga-api-project-template-name result)))
        (should (string= "kanban" (taiga-api-project-template-slug result)))
        (should (string= "Sample description" (taiga-api-project-template-description result)))
        (should (string= "2014-04-22T14:50:19+0000" (taiga-api-project-template-created-date result)))
        (should (string= "2014-07-25T13:11:42+0000" (taiga-api-project-template-modified-date result)))
        (should (string= "product-owner" (taiga-api-project-template-default-owner-role result)))
        (should (not (taiga-api-project-template-is-backlog-activated result)))
        (should (taiga-api-project-template-is-kanban-activated result))
        (should (not (taiga-api-project-template-is-wiki-activated result)))
        (should (not (taiga-api-project-template-is-issues-activated result)))
        (should (null (taiga-api-project-template-videoconferences result)))
        (should (string= "" (taiga-api-project-template-videoconferences-salt result)))))))

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
                 (taiga-api-project-template-default-options result)))
        (should (listp (taiga-api-project-template-us-statuses result)))
        (should (listp (taiga-api-project-template-points result)))
        (should (listp (taiga-api-project-template-task-statuses result)))
        (should (listp (taiga-api-project-template-issue-statuses result)))
        (should (listp (taiga-api-project-template-issue-types result)))
        (should (listp (taiga-api-project-template-priorities result)))
        (should (listp (taiga-api-project-template-severities result)))
        (should (listp (taiga-api-project-template-roles result)))
        (should (= 2 (taiga-api-project-template-id result)))
        (should (string= "Kanban" (taiga-api-project-template-name result)))
        (should (string= "kanban" (taiga-api-project-template-slug result)))
        (should (string= "Sample description" (taiga-api-project-template-description result)))
        (should (string= "2014-04-22T14:50:19+0000" (taiga-api-project-template-created-date result)))
        (should (string= "2014-07-25T13:11:42+0000" (taiga-api-project-template-modified-date result)))
        (should (string= "product-owner" (taiga-api-project-template-default-owner-role result)))
        (should (not (taiga-api-project-template-is-backlog-activated result)))
        (should (taiga-api-project-template-is-kanban-activated result))
        (should (not (taiga-api-project-template-is-wiki-activated result)))
        (should (not (taiga-api-project-template-is-issues-activated result)))
        (should (null (taiga-api-project-template-videoconferences result)))
        (should (string= "" (taiga-api-project-template-videoconferences-salt result)))))))

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
