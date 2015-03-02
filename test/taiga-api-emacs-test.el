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

(ert-deftest taiga-api-error-from-alist ()
  "Check that `taiga-api-error-from-alist' works properly."
  (let ((err (taiga-api-error-from-alist '((_error_message . "foo")
                                       (_error_type . "bar")))))
    (should (taiga-api-error-p err))
    (should (string= (taiga-api-error-message err) "foo"))
    (should (string= (taiga-api-error-type err) "bar"))))

(ert-deftest taiga-api-user-from-alist ()
  "Check that `taiga-api-user-from-alist' works properly."
  (let ((detail (taiga-api-user-from-alist
                 '((auth_token . "abcdef")
                   (bio . "foo")
                   (is_active . t)
                   (email . "foo@example.com")
                   (github_id)
                   (color . "#FC8EAC")
                   (default_language . "en")
                   (id . 7)
                   (full_name . "Foo Bar")
                   (photo . "//www.gravatar.com/avatar/12345")
                   (username . "foobar")
                   (big_photo . "//www.gravatar.com/avatar/12346")))))
    (should (taiga-api-user-p detail))
    (should (string= (taiga-api-user-auth-token detail) "abcdef"))
    (should (string= (taiga-api-user-bio detail) "foo"))
    (should (string= (taiga-api-user-is-active detail) t))
    (should (string= (taiga-api-user-email detail) "foo@example.com"))
    (should (null (taiga-api-user-github-id detail)))
    (should (string= (taiga-api-user-color detail) "#FC8EAC"))
    (should (string= (taiga-api-user-default-language detail) "en"))
    (should (= (taiga-api-user-id detail) 7))
    (should (string= (taiga-api-user-full-name detail) "Foo Bar"))
    (should (string= (taiga-api-user-photo detail) "//www.gravatar.com/avatar/12345"))
    (should (string= (taiga-api-user-username detail) "foobar"))
    (should (string= (taiga-api-user-big-photo detail) "//www.gravatar.com/avatar/12346"))))

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
        (should (taiga-api-user-p (taiga-api-normal-login "foo" "bar")))
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
      (should (taiga-api-user-p
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
      (should (taiga-api-user-p
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
        (should (taiga-api-user-p
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

(provide 'taiga-api-emacs-test)
;;; taiga-api-emacs-test.el ends here
