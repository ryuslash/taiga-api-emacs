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
  "Check that `taiga-api-error-from-alist' works properly."
  (let ((err (with-temp-buffer
               (insert "{\"_error_message\": \"foo\", \"_error_type\": \"bar\"}")
               (goto-char (point-min))
               (taiga-api-error-from-alist (json-read)))))
    (should (taiga-api-error-p err))
    (should (string= (taiga-api-error-message err) "foo"))
    (should (string= (taiga-api-error-type err) "bar"))))

(ert-deftest taiga-api-user-from-alist ()
  "Check that `taiga-api-user-from-alist' works properly."
  (let ((detail (taiga-api-test--data
                 "user-authentication-detail"
                 #'taiga-api-user-authentication-from-alist)))
    (should (taiga-api-user-authentication-p detail))
    (should (string= (taiga-api-user-authentication-auth-token detail)
                     "eyJ1c2VyX2F1dGhlbnRpY2F0aW9uX2lkIjo3fq:1XmPud:LKXVD9Z0rmHJjiyy0m4YaaHlQS1"))
    (should (string= (taiga-api-user-authentication-bio detail) ""))
    (should (string= (taiga-api-user-authentication-is-active detail) t))
    (should (string= (taiga-api-user-authentication-email detail) "beta.testing@taiga.io"))
    (should (null (taiga-api-user-authentication-github-id detail)))
    (should (string= (taiga-api-user-authentication-color detail) "#FC8EAC"))
    (should (string= (taiga-api-user-authentication-default-language detail) ""))
    (should (string= (taiga-api-user-authentication-full-name-display detail) "Beta testing"))
    (should (string= (taiga-api-user-authentication-default-timezone detail) ""))
    (should (= (taiga-api-user-authentication-id detail) 7))
    (should (string= (taiga-api-user-authentication-full-name detail) "Beta testing"))
    (should (string= (taiga-api-user-authentication-photo detail)
                     "//www.gravatar.com/avatar/4648b6d514c3ecece1b87136ceeda1d1?size=80"))
    (should (string= (taiga-api-user-authentication-username detail) "beta.tester"))
    (should (string= (taiga-api-user-authentication-big-photo detail)
                     "//www.gravatar.com/avatar/4648b6d514c3ecece1b87136ceeda1d1?size=80"))))

(ert-deftest taiga-api-wiki-page-from-alist ()
  "Check that `taiga-api-wiki-page-from-alist' works properly."
  (let ((page (taiga-api-test--data
               "wiki-page" #'taiga-api-wiki-page-from-alist)))
    (should (taiga-api-wiki-page-p page))
    (should (string= (taiga-api-wiki-page-html page)
                     "<p>Lorem ipsum dolor.</p>"))
    (should (= (taiga-api-wiki-page-editions page) 1))
    (should (= (taiga-api-wiki-page-id page) 1))
    (should (= (taiga-api-wiki-page-version page) 1))
    (should (= (taiga-api-wiki-page-project page) 1))
    (should (string= (taiga-api-wiki-page-slug page) "home"))
    (should (string= (taiga-api-wiki-page-content page)
                     "Lorem ipsum dolor."))
    (should (= (taiga-api-wiki-page-owner page) 11))
    (should (null (taiga-api-wiki-page-last-modified page)))
    (should (string= (taiga-api-wiki-page-created-date page)
                     "2014-10-30T09:29:53+0000"))
    (should (string= (taiga-api-wiki-page-modified-date page)
                     "2014-10-30T09:29:53+0000"))
    (should (equal (taiga-api-wiki-page-watchers page) []))))

(ert-deftest taiga-api-user-story-from-alist ()
  "Check that `taiga-api-user-story-from-alist' works properly."
  (let ((story (taiga-api-test--data
                "user-story" #'taiga-api-user-story-from-alist)))
    (should (taiga-api-user-story-p story))
    (should (= (taiga-api-user-story-assigned-to story) 19))
    (should (= (taiga-api-user-story-backlog-order story) 2))
    (should (string= (taiga-api-user-story-blocked-note story) ""))
    (should (string= (taiga-api-user-story-blocked-note-html story) ""))
    (should (null (taiga-api-user-story-client-requirement story)))
    (should (string= (taiga-api-user-story-comment story) ""))
    (should (string= (taiga-api-user-story-created-date story)
                     "2014-07-29T07:15:50+0000"))
    (should (string= (taiga-api-user-story-description story)
                     "Implement API CALL"))
    (should (string= (taiga-api-user-story-description-html story)
                     "<p>Implement API CALL</p>"))
    (should (string= (taiga-api-user-story-finish-date story)
                     "2014-09-16T17:17:16+0000"))
    (should (not (taiga-api-user-story-generated-from-issue story)))
    (should (= (taiga-api-user-story-id story) 1149))
    (should (not (taiga-api-user-story-is-archived story)))
    (should (not (taiga-api-user-story-is-blocked story)))
    (should (taiga-api-user-story-is-closed story))
    (should (= (taiga-api-user-story-kanban-order story) 37))
    (should (= (taiga-api-user-story-milestone story) 98))
    (should (string= (taiga-api-user-story-milestone-name story) "Sprint 03"))
    (should (string= (taiga-api-user-story-milestone-slug story) "sprint-03"))
    (should (string= (taiga-api-user-story-modified-date story)
                     "2014-10-10T12:07:13+0000"))
    (should (null (taiga-api-user-story-origin-issue story)))
    (should (= (taiga-api-user-story-owner story) 19))
    (should (equal (taiga-api-user-story-points story)
                   '((\132 . 364)
                     (\131 . 361)
                     (\130 . 361)
                     (\129 . 361))))
    (should (= (taiga-api-user-story-project story) 31))
    (should (= (taiga-api-user-story-ref story) 31))
    (should (= (taiga-api-user-story-sprint-order story) 2))
    (should (= (taiga-api-user-story-status story) 83))
    (should (string= (taiga-api-user-story-subject story)
                     "Customer personal data"))
    (should (equal (taiga-api-user-story-tags story)
                   ["service catalog" "customer"]))
    (should (not (taiga-api-user-story-team-requirement story)))
    (should (= (taiga-api-user-story-total-points story) 1.0))
    (should (= (taiga-api-user-story-version story) 8))
    (should (equal (taiga-api-user-story-watchers story) []))))

(ert-deftest taiga-api-issue-from-alist ()
  "Check that `taiga-api-issue-from-alist' works properly."
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
  "Check that `taiga-api-neighbors-from-alist' works properly."
  (let ((neighbors (taiga-api-test--data
                    "neighbors" #'taiga-api-neighbors-from-alist)))
    (should (taiga-api-neighbors-p neighbors))
    (should (taiga-api-neighbor-p (taiga-api-neighbors-next neighbors)))
    (should (null (taiga-api-neighbors-previous neighbors)))))

(ert-deftest taiga-api-neighbor-from-alist ()
  "Check that `taiga-api-neighbor-from-alist' works properly."
  (let ((neighbor (taiga-api-test--data
                   "neighbor" #'taiga-api-neighbor-from-alist)))
    (should (taiga-api-neighbor-p neighbor))
    (should (= (taiga-api-neighbor-id neighbor) 16))
    (should (= (taiga-api-neighbor-ref neighbor) 126))
    (should (string= (taiga-api-neighbor-subject neighbor)
                     "Support for bulk actions"))))

(ert-deftest taiga-api-task-from-alist ()
  "Check that `taiga-api-task-from-alist' works properly."
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
  "Check that `taiga-api-search-result-from-alist' works properly."
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
  "Check that `taiga-api-user-storage-data-from-alist' works properly."
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
  "Check that `taiga-api-many-user-storage-data-from-array' works properly."
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
;;; Auth

(ert-deftest taiga-api-unsuccessful-normal-login ()
  "Check that an unsuccessful login signals an error."
  (with-taiga-api-synchronous-response
      400 nil (taiga-api--json-encoded-error)
    (taiga-api-test--ensure-token ""
      (should-error (taiga-api-normal-login "foo" "bar")
                    :type 'taiga-api-login-failed)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-successful-normal-login ()
  "Check that a successful login returns a user object."
  (let (taiga-api--auth-token)
    (with-taiga-api-synchronous-response
        200 nil (json-encode '(("username" . "foobar")
                               ("auth_token" . "normaltoken")))
      (taiga-api-test--ensure-token "normaltoken"
        (should (taiga-api-user-authentication-p
                 (taiga-api-normal-login "foo" "bar")))
        (should-not (buffer-live-p taiga-api-test-buffer))))))

(ert-deftest taiga-api-unsuccessful-github-login ()
  "Check that an unsuccessful github login signals an error."
  (with-taiga-api-synchronous-response
      400 nil (taiga-api--json-encoded-error)
    (taiga-api-test--ensure-token ""
      (should-error (taiga-api-github-login "foo")
                    :type 'taiga-api-login-failed)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-successful-github-login ()
  "Check that a successful github login returns a user object."
  (with-taiga-api-synchronous-response
      200 nil (json-encode '(("username" . "foobar")
                             ("auth_token" . "githubtoken")))
    (taiga-api-test--ensure-token "githubtoken"
      (should (taiga-api-user-authentication-p
               (taiga-api-github-login "foo" "token")))
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-throttled-normal-login ()
  "Check that a throttled login signals the proper error."
  (with-taiga-api-synchronous-response
      429 nil (taiga-api--json-encoded-error)
    (taiga-api-test--ensure-token ""
      (should-error (taiga-api-normal-login "foo" "bar")
                    :type 'taiga-api-throttled)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-throttled-github-login ()
  "Check that a throttled github login signals the proper error."
  (with-taiga-api-synchronous-response
      429 nil (taiga-api--json-encoded-error)
    (taiga-api-test--ensure-token ""
      (should-error (taiga-api-github-login "foo" "token")
                    :type 'taiga-api-throttled)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-successful-public-registration ()
  "Check that a successful public registration returns a user object."
  (with-taiga-api-synchronous-response
      201 nil (json-encode '(("username" . "foo")
                             ("auth_token" . "publictoken")))
    (taiga-api-test--ensure-token "publictoken"
      (should (taiga-api-user-authentication-p
               (taiga-api-register-public
                "foo" "bar" "foo@example.com" "Foo Frobnicate")))
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-unsuccessful-public-registration ()
  "Check that a successful public registration signals an error."
  (with-taiga-api-synchronous-response
      400 nil (taiga-api--json-encoded-error)
    (taiga-api-test--ensure-token ""
      (should-error (taiga-api-register-public
                     "foo" "bar" "foo@example.com" "Foo Frobnicate")
                    :type 'taiga-api-registration-failed)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-throttled-public-registration ()
  "Check that a throttled public registration signals an error."
  (with-taiga-api-synchronous-response
      429 nil (taiga-api--json-encoded-error)
    (taiga-api-test--ensure-token ""
      (should-error (taiga-api-register-public
                     "foo" "bar" "foo@example.com" "Foo Frobnicate")
                    :type 'taiga-api-throttled)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-successful-private-registration ()
  "Check that a successful private registration returns a user object."
  (let (taiga-api--auth-token)
    (with-taiga-api-synchronous-response
        201 nil (json-encode '(("username" . "foo")
                               ("auth_token" . "privatetoken")))
      (taiga-api-test--ensure-token "privatetoken"
        (should (taiga-api-user-authentication-p
                 (taiga-api-register-private
                  t "token" "username" "password")))
        (should-not (buffer-live-p taiga-api-test-buffer))))))

(ert-deftest taiga-api-unsuccessful-private-registration ()
  "Check that an unsuccessful private registration signals an error."
  (with-taiga-api-synchronous-response
      400 nil (taiga-api--json-encoded-error)
    (taiga-api-test--ensure-token ""
      (should-error (taiga-api-register-private
                     nil "token" "username" "password" "email" "full-name")
                    :type 'taiga-api-registration-failed)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-throttled-private-registration ()
  "Check that a throttled private registration signals an error."
  (with-taiga-api-synchronous-response
      429 nil (taiga-api--json-encoded-error)
    (taiga-api-test--ensure-token ""
      (should-error (taiga-api-register-private
                     t "token" "username" "password")
                    :type 'taiga-api-throttled)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-normal-login-request ()
  "Check that request parameters for normal login are setup correctly."
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
  "Check that request parameters for github login are setup correctly."
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
  "Check that request parameters for public registrations are setup correctly."
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
  "Check that request parameters for private registrations are setup correctly."
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

(ert-deftest taiga-api-unauthenticated-project-resolution ()
  "Check that an unauthenticated project resolution signals an error."
  (taiga-api-test--ensure-token ""
    (should-error (taiga-api-resolve-project "project")
                  :type 'taiga-api-unauthenticated)))

(ert-deftest taiga-api-successful-project-resolution ()
  "Check that a successful project resolution returns an alist."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (json-encode '((project . 1)))
      (let ((result (taiga-api-resolve-project "project")))
        (should (= 1 (cdr (assq 'project result))))
        (should-not (buffer-live-p taiga-api-test-buffer))))))

(ert-deftest taiga-api-unsuccessful-project-resolution ()
  "Check that an unsuccessful project resolution signals an error."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve-project "project")
                    :type 'taiga-api-unresolved)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-throttled-project-resolution ()
  "Check that a throttled project resolution signals an error."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        429 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve-project "project")
                    :type 'taiga-api-throttled)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-unauthenticated-user-story-resolution ()
  "Check that an unauthenticated user story resolution signals an error."
  (taiga-api-test--ensure-token ""
    (should-error (taiga-api-resolve-user-story "project" "us")
                  :type 'taiga-api-unauthenticated)))

(ert-deftest taiga-api-successful-user-story-resolution ()
  "Check that a successful user story resolution returns an alist."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (json-encode '((us . 26) (project . 1)))
      (let ((result (taiga-api-resolve-user-story "project" "us")))
        (should (= 1 (cdr (assq 'project result))))
        (should (= 26 (cdr (assq 'us result))))
        (should-not (buffer-live-p taiga-api-test-buffer))))))

(ert-deftest taiga-api-unsuccessful-user-story-resolution ()
  "Check that an unsuccessful user story resolution signals an error."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve-user-story "project" "us")
                    :type 'taiga-api-unresolved)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-throttled-user-story-resolution ()
  "Check that a throttled user story resolution signals an error."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        429 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve-user-story "project" "us")
                    :type 'taiga-api-throttled)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-unauthenticated-issue-resolution ()
  "Check that an unauthenticated issue resolution signals an error."
  (taiga-api-test--ensure-token ""
    (should-error (taiga-api-resolve-issue "project" "issue")
                  :type 'taiga-api-unauthenticated)))

(ert-deftest taiga-api-successful-issue-resolution ()
  "Check that a successful issue resolution returns an alist."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (json-encode '((issue . 5209) (project . 1)))
      (let ((result (taiga-api-resolve-issue "project" "issue")))
        (should (= 1 (cdr (assq 'project result))))
        (should (= 5209 (cdr (assq 'issue result))))
        (should-not (buffer-live-p taiga-api-test-buffer))))))

(ert-deftest taiga-api-unsuccessful-issue-resolution ()
  "Check that an unsuccessful issue resolution signals an error."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve-issue "project" "issue")
                    :type 'taiga-api-unresolved)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-throttled-issue-resolution ()
  "Check that a throttled issue resolution signals an error."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        429 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve-issue "project" "issue")
                    :type 'taiga-api-throttled)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-project-resolution-request ()
  "Check that request parameters for project resolution are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/resolver?project=some-project" url))
                 (should (string= "Bearer sometoken" (cdr (assoc "Authorization" url-request-extra-headers))))
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-resolve-project "some-project"))
    (should (= 1 func-used))))

(ert-deftest taiga-api-user-story-resolution-request ()
  "Check that request parameters for user story resolution are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/resolver?project=some-project&us=5" url))
                 (should (string= "Bearer sometoken" (cdr (assoc "Authorization" url-request-extra-headers))))
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-resolve-user-story "some-project" 5))
    (should (= 1 func-used))))

(ert-deftest taiga-api-issue-resolution-request ()
  "Check that request paramaters for issue resolution are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/resolver?project=some-project&issue=5" url))
                 (should (string= "Bearer sometoken" (cdr (assoc "Authorization" url-request-extra-headers))))
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-resolve-issue "some-project" 5))
    (should (= 1 func-used))))

(ert-deftest taiga-api-unauthenticated-task-resolution ()
  "Check that an unauthenticated task resolution signals an error."
  (taiga-api-test--ensure-token ""
    (should-error (taiga-api-resolve-task "project" "task")
                  :type 'taiga-api-unauthenticated)))

(ert-deftest taiga-api-successful-task-resolution ()
  "Check that a successful task resolution returns an alist."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (json-encode '((task . 1336) (project . 1)))
      (let ((result (taiga-api-resolve-task "project" "task")))
        (should (= 1 (cdr (assq 'project result))))
        (should (= 1336 (cdr (assq 'task result))))
        (should-not (buffer-live-p taiga-api-test-buffer))))))

(ert-deftest taiga-api-unsuccessful-task-resolution ()
  "Check that an unsuccessful task resolution signals an error."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve-task "project" "task")
                    :type 'taiga-api-unresolved)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-throttled-task-resolution ()
  "Check that a throttled task resolution signals an error."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        429 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve-task "project" "task")
                    :type 'taiga-api-throttled)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-task-resolution-request ()
  "Check that request parameters for task resolution are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/resolver?project=some-project&task=5" url))
                 (should (string= "Bearer sometoken" (cdr (assoc "Authorization" url-request-extra-headers))))
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-resolve-task "some-project" 5))
    (should (= 1 func-used))))

(ert-deftest taiga-api-unauthenticated-milestone-resolution ()
  "Check that an unauthenticated milestone resolution signals an error."
  (taiga-api-test--ensure-token ""
    (should-error (taiga-api-resolve-milestone "project" "milestone")
                  :type 'taiga-api-unauthenticated)))

(ert-deftest taiga-api-successful-milestone-resolution ()
  "Check that a successful milestone resolution returns an alist."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (json-encode '((milestone . 1) (project . 1)))
      (let ((result (taiga-api-resolve-milestone "project" "milestone")))
        (should (= 1 (cdr (assq 'project result))))
        (should (= 1 (cdr (assq 'milestone result))))
        (should-not (buffer-live-p taiga-api-test-buffer))))))

(ert-deftest taiga-api-unsuccessful-milestone-resolution ()
  "Check that an unsuccessful milestone resolution signals an error."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve-milestone "project" "milestone")
                    :type 'taiga-api-unresolved)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-throttled-milestone-resolution ()
  "Check that a throttled milestone resolution signals an error."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        429 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve-milestone "project" "milestone")
                    :type 'taiga-api-throttled)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-milestone-resolution-request ()
  "Check that request parameters for milestone resolution are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/resolver?project=some-project&milestone=some-milestone" url))
                 (should (string= "Bearer sometoken" (cdr (assoc "Authorization" url-request-extra-headers))))
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-resolve-milestone "some-project" "some-milestone"))
    (should (= 1 func-used))))

(ert-deftest taiga-api-unauthenticated-wiki-resolution ()
  "Check that an unauthenticated wiki resolution signals an error."
  (taiga-api-test--ensure-token ""
    (should-error (taiga-api-resolve-wiki "project" "wikipage")
                  :type 'taiga-api-unauthenticated)))

(ert-deftest taiga-api-successful-wiki-resolution ()
  "Check that a successful milestone resolution returns an alist."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (json-encode '((wikipage . 2) (project . 1)))
      (let ((result (taiga-api-resolve-wiki "project" "wikipage")))
        (should (= 1 (cdr (assq 'project result))))
        (should (= 2 (cdr (assq 'wikipage result))))
        (should-not (buffer-live-p taiga-api-test-buffer))))))

(ert-deftest taiga-api-unsuccessful-wiki-resolution ()
  "Check that a successful wiki resolution returns an alist."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve-wiki "project" "wikipage")
                    :type 'taiga-api-unresolved)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-throttled-wiki-resolution ()
  "Check that a throttled wiki resolution signals an error."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        429 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve-wiki "project" "wikipage")
                    :type 'taiga-api-throttled)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-wiki-resolution-request ()
  "Check that request parameters for wiki page resolution are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/resolver?project=some-project&wikipage=home" url))
                 (should (string= "Bearer sometoken" (cdr (assoc "Authorization" url-request-extra-headers))))
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-resolve-wiki "some-project" "home"))
    (should (= 1 func-used))))

(ert-deftest taiga-api-unauthenticated-resolution ()
  "Check that an unauthenticated resolution signals an error."
  (taiga-api-test--ensure-token ""
    (should-error (taiga-api-resolve "project" :us 1)
                  :type 'taiga-api-unauthenticated)))

(ert-deftest taiga-api-successful-resolution ()
  "Check that a successful resolution returns an alist."
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
  "Check that an unsuccessful resolution signals an error."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        404 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve "project" :task 3)
                    :type 'taiga-api-unresolved)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-throttled-resolution ()
  "Check that a throttled resolution signals an error."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        429 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-resolve "project" :milestone "sprint0")
                    :type 'taiga-api-throttled)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-resolution-request ()
  "Check that request parameters for resolution are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/resolver?project=some-project&us=5&milestone=some-milestone" url))
                 (should (string= "Bearer sometoken" (cdr (assoc "Authorization" url-request-extra-headers))))
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-resolve "some-project" :us 5 :milestone "some-milestone"))
    (should (= 1 func-used))))

;;; Search

(ert-deftest taiga-api-search-request ()
  "Check that request parameters for searches are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/search?project=1&text=design" url))
                 (should (string= "Bearer sometoken" (cdr (assoc "Authorization" url-request-extra-headers))))
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-search 1 "design"))
    (should (= 1 func-used))))

(ert-deftest taiga-api-unauthenticated-search ()
  "Check that an unauthenticated search signals an error."
  (taiga-api-test--ensure-token ""
    (should-error (taiga-api-search 1 "design")
                  :type 'taiga-api-unauthenticated)))

(ert-deftest taiga-api-successful-search ()
  "Check that a successful search returns an alist."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (with-temp-buffer
                  (insert-file-contents (concat taiga-api-test--location "files/search-results.json"))
                  (buffer-substring-no-properties (point-min) (point-max)))
      (let ((result (taiga-api-search 1 "design")))
        (should (taiga-api-search-result-p result))
        (should (arrayp (taiga-api-search-result-wikipages result)))
        (should (arrayp (taiga-api-search-result-userstories result)))
        (should (arrayp (taiga-api-search-result-issues result)))
        (should (arrayp (taiga-api-search-result-tasks result)))
        (should (= 4 (taiga-api-search-result-count result)))))))

(ert-deftest taiga-api-throttled-search ()
  "Check that a throttled search signals an error."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        429 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-search 1 "design")
                    :type 'taiga-api-throttled)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

;;; User storage

(ert-deftest taiga-api-list-user-storage-request ()
  "Check that request parameters for listing user storage are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/user-storage" url))
                 (should (string= "Bearer sometoken" (cdr (assoc "Authorization" url-request-extra-headers))))
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 200 OK\n"
                           "\n"
                           "[{\"foo\": \"bar\"}]")
                   (current-buffer)))))
      (taiga-api-list-user-storage))
    (should (= 1 func-used))))

(ert-deftest taiga-api-unauthenticated-list-user-storage ()
  "Check that an unauthenticated search signals an error."
  (taiga-api-test--ensure-token ""
    (should-error (taiga-api-list-user-storage)
                  :type 'taiga-api-unauthenticated)))

(ert-deftest taiga-api-successful-list-user-storage ()
  "Check that a successful user storage listing returns an array."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        200 nil (with-temp-buffer
                  (insert-file-contents (concat taiga-api-test--location "files/user-storage-data-list.json"))
                  (buffer-substring-no-properties (point-min) (point-max)))
      (let ((result (taiga-api-list-user-storage)))
        (should (listp result))
        (mapc (lambda (stor)
                (should (taiga-api-user-storage-data-p stor))
                (should (string= "favorite-forest" (taiga-api-user-storage-data-key stor)))
                (should (string= "Taiga" (taiga-api-user-storage-data-value stor)))
                (should (string= "2014-11-13T16:58:35+0000" (taiga-api-user-storage-data-created-date stor)))
                (should (string= "2014-11-13T16:58:35+0000" (taiga-api-user-storage-data-modified-date stor))))
              result)))))

(ert-deftest taiga-api-throttled-list-user-storage ()
  "Check that a throttled user storage listing signals an error."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        429 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-list-user-storage)
                    :type 'taiga-api-throttled)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(ert-deftest taiga-api-create-user-storage-request ()
  "Check chat request parameters for listing user storage are setup correctly."
  (let ((func-used 0)
        (taiga-api--auth-token "sometoken"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest args)
                 (cl-incf func-used)
                 (should (string= "https://api.taiga.io/api/v1/user-storage" url))
                 (should (string= "Bearer sometoken" (cdr (assoc "Authorization" url-request-extra-headers))))
                 (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                   (insert "HTTP/1.1 201 CREATED\n"
                           "\n"
                           "{\"foo\": \"bar\"}")
                   (current-buffer)))))
      (taiga-api-create-user-storage "foo" "bar"))
    (should (= 1 func-used))))

(ert-deftest taiga-api-create-user-storage-unauthenticated ()
  "Check that creating user storage unauthenticated fails."
  (taiga-api-test--ensure-token ""
    (should-error (taiga-api-create-user-storage "foo" "bar")
                  :type 'taiga-api-unauthenticated)))

(ert-deftest taiga-api-create-user-storage-success ()
  "Check that creating user storage successfully returns the object"
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        201 nil (with-temp-buffer
                  (insert-file-contents (concat taiga-api-test--location "files/user-storage-data.json"))
                  (buffer-substring-no-properties (point-min) (point-max)))
      (let ((result (taiga-api-create-user-storage "foo" "bar")))
        (should (taiga-api-user-storage-data-p result))
        (should (string= (taiga-api-user-storage-data-key result) "favorite-forest"))
        (should (string= (taiga-api-user-storage-data-value result) "Taiga"))
        (should (string= (taiga-api-user-storage-data-created-date result)
                         "2014-11-13T16:58:35+0000"))
        (should (string= (taiga-api-user-storage-data-modified-date result)
                         "2014-11-13T16:58:35+0000"))))))

(ert-deftest taiga-api-create-user-storage-throttled ()
  "Check that creating user storage being throttled signals an error."
  (let ((taiga-api--auth-token "sometoken"))
    (with-taiga-api-synchronous-response
        429 nil (taiga-api--json-encoded-error)
      (should-error (taiga-api-create-user-storage "foo" "bar")
                    :type 'taiga-api-throttled)
      (should-not (buffer-live-p taiga-api-test-buffer)))))

(provide 'taiga-api-emacs-test)
;;; taiga-api-emacs-test.el ends here
