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

(ert-deftest taiga-error-from-alist ()
  "Check that `taiga-error-from-alist' works properly."
  (let ((err (taiga-error-from-alist '((_error_message . "foo")
                                       (_error_type . "bar")))))
    (should (taiga-error-p err))
    (should (string= (taiga-error-message err) "foo"))
    (should (string= (taiga-error-type err) "bar"))))

(ert-deftest taiga-user-from-alist ()
  "Check that `taiga-user-from-alist' works properly."
  (let ((detail (taiga-user-from-alist
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
    (should (taiga-user-p detail))
    (should (string= (taiga-user-auth-token detail) "abcdef"))
    (should (string= (taiga-user-bio detail) "foo"))
    (should (string= (taiga-user-is-active detail) t))
    (should (string= (taiga-user-email detail) "foo@example.com"))
    (should (null (taiga-user-github-id detail)))
    (should (string= (taiga-user-color detail) "#FC8EAC"))
    (should (string= (taiga-user-default-language detail) "en"))
    (should (= (taiga-user-id detail) 7))
    (should (string= (taiga-user-full-name detail) "Foo Bar"))
    (should (string= (taiga-user-photo detail) "//www.gravatar.com/avatar/12345"))
    (should (string= (taiga-user-username detail) "foobar"))
    (should (string= (taiga-user-big-photo detail) "//www.gravatar.com/avatar/12346"))))

(ert-deftest taiga-api-unsuccessful-normal-login ()
  "Check that an unsuccessful login signals an error."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest args)
               (ignore args)
               (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                 (erase-buffer)
                 (insert "HTTP/1.1 400 BAD REQUEST\n"
                         "\n"
                         "{\"_error_type\": \"taiga.base.exceptions.WrongArguments\", \"_error_message\": \"Username or password does not matches user.\"}")
                 (current-buffer)))))
    (should-error (taiga-api-normal-login "foo" "bar")
                  :type 'taiga-api-login-failed)))

(ert-deftest taiga-api-successful-normal-login ()
  "Check that a successful login returns a user object."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest args)
               (ignore args)
               (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                 (erase-buffer)
                 (insert "HTTP/1.1 200 OK\n"
                         "\n"
                         "{\"username\": \"foobar\"}")
                 (current-buffer)))))
    (should (taiga-user-p
             (taiga-api-normal-login "foo" "bar")))))

(ert-deftest taiga-api-unsuccessful-github-login ()
  "Check that an unsuccessful github login signals an error."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest args)
               (ignore args)
               (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                 (erase-buffer)
                 (insert "HTTP/1.1 400 BAD REQUEST\n"
                         "\n"
                         "{\"_error_type\": \"taiga.base.exceptions.WrongArguments\", \"_error_message\": \"Username or password does not matches user.\"}")
                 (current-buffer)))))
    (should-error (taiga-api-github-login "foo")
                  :type 'taiga-api-login-failed)))

(ert-deftest taiga-api-successful-github-login ()
  "Check that a successful github login returns a user object."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest args)
               (ignore args)
               (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                 (erase-buffer)
                 (insert "HTTP/1.1 200 OK\n"
                         "\n"
                         "{\"username\": \"foobar\"}")
                 (current-buffer)))))
    (should (taiga-user-p
             (taiga-api-github-login "foo" "token")))))

(ert-deftest taiga-api-throttled-normal-login ()
  "Check that a throttled login signals the proper error."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest args)
               (ignore args)
               (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                 (erase-buffer)
                 (insert "HTTP/1.1 429 TOO MANY REQUESTS\n"
                         "\n"
                         "{\"_error_type\": \"foo\", \"_error_message\": \"bar\"}")
                 (current-buffer)))))
    (should-error (taiga-api-normal-login "foo" "bar")
                  :type 'taiga-api-throttled)))

(ert-deftest taiga-api-throttled-github-login ()
  "Check that a throttled github login signals the proper error."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest args)
               (ignore args)
               (with-current-buffer (generate-new-buffer "taiga-api-http-test")
                 (erase-buffer)
                 (insert "HTTP/1.1 429 TOO MANY REQUESTS\n"
                         "\n"
                         "{\"_error_type\": \"foo\", \"_error_message\": \"bar\"}")
                 (current-buffer)))))
    (should-error (taiga-api-github-login "foo" "token")
                  :type 'taiga-api-throttled)))

(provide 'taiga-api-emacs-test)
;;; taiga-api-emacs-test.el ends here
