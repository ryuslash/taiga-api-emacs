;; -*- lexical-binding: t; -*-

(require 'taiga-api)

(describe "Reading an error from an alist"
  (let ((err (with-temp-buffer
               (insert "{\"_error_message\": \"foo\", \"_error_type\": \"bar\"}")
               (goto-char (point-min))
               (taiga-api-error-from-alist (json-read)))))
    (it "yields an error" (expect (taiga-api-error-p err) :to-be-truthy))
    (it "includes the message" (expect (taiga-api-error-message err) :to-equal "foo"))
    (it "includes the type" (expect (taiga-api-error-type err) :to-equal "bar"))))

(describe "Reading a user from an alist"
  (with-read-data (detail "user-authentication-detail")
    (with-subject detail
      (it "yields a user authentication detail" (expect (taiga-api-user-authentication-detail-p detail) :to-be-truthy))
      (it "includes the auth token" (expect (its 'auth-token) :to-equal "eyJ1c2VyX2F1dGhlbnRpY2F0aW9uX2lkIjo3fq:1XmPud:LKXVD9Z0rmHJjiyy0m4YaaHlQS1"))
      (it "includes the bio" (expect (its 'bio) :to-equal ""))
      (it "includes the active state" (expect (its 'is-active) :to-be-truthy))
      (it "includes the email" (expect (its 'email) :to-equal "beta.testing@taiga.io"))
      (it "includes the github id" (expect (its 'github-id) :to-be nil))
      (it "includes the color" (expect (its 'color) :to-equal "#FC8EAC"))
      (it "includes the language" (expect (its 'lang) :to-equal ""))
      (it "includes how to display the full name" (expect (its 'full-name-display) :to-equal "Beta testing"))
      (it "includes the time zone" (expect (its 'timezone) :to-equal ""))
      (it "includes the identification number" (expect (its 'id) :to-be 7))
      (it "includes the full name of the user" (expect (its 'full-name) :to-equal "Beta testing"))
      (it "includes the user's photo URL" (expect (its 'photo) :to-equal "//www.gravatar.com/avatar/4648b6d514c3ecece1b87136ceeda1d1?size=80"))
      (it "includes the username" (expect (its 'username) :to-equal "beta.tester"))
      (it "includes the user's big photo URL" (expect (its 'big-photo) :to-equal "//www.gravatar.com/avatar/4648b6d514c3ecece1b87136ceeda1d1?size=80")))))

(describe "Reading a wiki page from an alist"
  (with-read-data (page "wiki-page")
    (with-subject page
      (it "yields a wiki page" (expect (taiga-api-wiki-page-p page) :to-be-truthy))
      (it "includes the HTML contents" (expect (its 'html) :to-equal "<p>Lorem ipsum dolor.</p>"))
      (it "includes the number of editions" (expect (its 'editions) :to-be 1))
      (it "includes the identification number" (expect (its 'id) :to-be 1))
      (it "includes the version" (expect (its 'version) :to-be 1))
      (it "includes the project id" (expect (its 'project) :to-be 1))
      (it "includes the slug" (expect (its 'slug) :to-equal "home"))
      (it "includes the content" (expect (its 'content) :to-equal "Lorem ipsum dolor."))
      (it "includes the owner id" (expect (its 'owner) :to-be 11))
      (it "includes the last modified date" (expect (its 'last-modified) :to-be nil))
      (it "includes the created date" (expect (its 'created-date) :to-equal "2014-10-30T09:29:53+0000"))
      (it "includes the modified date" (expect (its 'modified-date) :to-equal "2014-10-30T09:29:53+0000"))
      (it "includes the watchers" (expect (its 'watchers) :to-equal [])))))
