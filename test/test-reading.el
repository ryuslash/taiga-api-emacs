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

(describe "Reading a user story from an alist"
  (with-read-data (story "user-story")
    (with-subject story
      (it "yields a user story" (expect (taiga-api-user-story-p story) :to-be-truthy))
      (it "includes the assigned user's id" (expect (its 'assigned-to) :to-be 19))
      (it "includes the place in the backlog" (expect (its 'backlog-order) :to-be 2))
      (it "includes the reason why it's blocked" (expect (its 'blocked-note) :to-equal ""))
      (it "includes the raw html of the reason why it's blocked" (expect (its 'blocked-note-html) :to-equal ""))
      (it "includes whether it's a client requirement" (expect (its 'client-requirement) :to-be nil))
      (it "includes a comment" (expect (its 'comment) :to-equal ""))
      (it "includes the date on which it was created" (expect (its 'created-date) :to-equal "2014-07-29T07:15:50+0000"))
      (it "includes a description" (expect (its 'description) :to-equal "Implement API CALL"))
      (it "includes the description's raw HTML" (expect (its 'description-html) :to-equal "<p>Implement API CALL</p>"))
      (it "includes the date it was finished" (expect (its 'finish-date) :to-equal "2014-09-16T17:17:16+0000"))
      (it "includes whether it came from an issue" (expect (its 'generated-from-issue) :not :to-be-truthy))
      (it "includes its id" (expect (its 'id) :to-be 1149))
      (it "includes whether it's been archived" (expect (its 'is-archived) :not :to-be-truthy))
      (it "includes whether it's being blocked" (expect (its 'is-blocked) :not :to-be-truthy))
      (it "includes whether it's been closed" (expect (its 'is-closed) :to-be-truthy))
      (it "includes the place in the kanban board" (expect (its 'kanban-order) :to-be 37))
      (it "includes the id of the containing milestone" (expect (its 'milestone) :to-be 98))
      (it "includes the containing milestone's name" (expect (its 'milestone-name) :to-equal "Sprint 03"))
      (it "includes the containing milestone's slug" (expect (its 'milestone-slug) :to-equal "sprint-03"))
      (it "includes the modified date" (expect (its 'modified-date) :to-equal "2014-10-10T12:07:13+0000"))
      (it "includes the id of the originating issue" (expect (its 'origin-issue) :to-be nil))
      (it "includes the id of its owner" (expect (its 'owner) :to-be 19))
      (it "includes a list of its points"
        (expect (its 'points) :to-equal '((\132 . 364)
                                          (\131 . 361)
                                          (\130 . 361)
                                          (\129 . 361))))
      (it "includes the id of the containing project" (expect (its 'project) :to-be 31))
      (it "includes a reference id" (expect (its 'ref) :to-be 31))
      (it "includes the place in the sprint" (expect (its 'sprint-order) :to-be 2))
      (it "includes the id of its status" (expect (its 'status) :to-be 83))
      (it "includes a  subject" (expect (its 'subject) :to-equal "Customer personal data"))
      (it "includes tags" (expect (its 'tags) :to-equal ["service catalog" "customer"]))
      (it "includes whether it is a team requirement" (expect (its 'team-requirement) :not :to-be-truthy))
      (it "includes the total points" (expect (its 'total-points) :to-equal 1.0))
      (it "insludes the version" (expect (its 'version) :to-be 8))
      (it "insludes the watchers' ids" (expect (its 'watchers) :to-equal [])))))

(describe "Reading an issue from an alist"
  (with-read-data (issue "issue")
    (with-subject issue
      (it "yields an issue" (expect (taiga-api-issue-p issue) :to-be-truthy))
      (it "includes the assigned user's id" (expect (its 'assigned-to) :to-be 19))
      (it "includes a reason why it was blocked" (expect (its 'blocked-note) :to-equal ""))
      (it "includes the raw HTML of the reason why it was blocked"
        (expect (its 'blocked-note-html) :to-equal ""))
      (it "includes a comment" (expect (its 'comment) :to-equal ""))
      (it "includes its created date" (expect (its 'created-date) :to-equal "2014-07-29T07:15:50+0000"))
      (it "includes a description" (expect (its 'description) :to-equal "Implement API CALL"))
      (it "includes the raw HTML of the description" (expect (its 'description-html) :to-equal "<p>Implement API CALL</p>"))
      (it "includes the date it was finished" (expect (its 'finish-date) :to-equal "2014-09-16T17:17:16+0000"))
      (it "includes its identification number" (expect (its 'id) :to-equal 1149))
      (it "includes its blocked status" (expect (its 'is-blocked) :not :to-be-truthy))
      (it "includes its closed status" (expect (its 'is-closed) :to-be-truthy))
      (it "includes the containing milestone's id" (expect (its 'milestone) :to-be 98))
      (it "includes the last date it was modified" (expect (its 'modified-date) :to-equal "2014-10-10T12:07:13+0000"))
      (it "includes the date it was finished" (expect (its 'finished-date) :to-be nil))
      (it "includes the user id of its owner" (expect (its 'owner) :to-be 19))
      (it "includes the id of its project" (expect (its 'project) :to-be 31))
      (it "includes a reference id" (expect (its 'ref) :to-be 31))
      (it "includes a status id" (expect (its 'status) :to-be 83))
      (it "includes a severity id" (expect (its 'severity) :to-be 2))
      (it "includes a priority id" (expect (its 'priority) :to-be 3))
      (it "includes a type id" (expect (its 'type) :to-be 1))
      (it "includes a subject" (expect (its 'subject) :to-equal "Customer personal data"))
      (it "includes some tags" (expect (its 'tags) :to-equal ["service catalog" "customer"]))
      (it "includes a version number" (expect (its 'version) :to-be 8))
      (it "includes a list of watchers" (expect (arrayp (its 'watchers)) :to-be-truthy))
      (it "includes a list of user stories generated by it"
        (expect (arrayp (its 'generated-user-stories)) :to-be-truthy))
      (it "includes its votes" (expect (its 'votes) :to-be nil))
      (it "includes its neighbours"
        (expect (taiga-api-neighbors-p (its 'neighbors)) :to-be-truthy)))))

(describe "Reading neighbours from an alist"
  (with-read-data (neighbors "neighbors")
    (it "yields neighbours" (expect (taiga-api-neighbors-p neighbors) :to-be-truthy))
    (it "includes the next neighbor"
      (expect (taiga-api-neighbor-p (taiga-api-neighbors-next neighbors)) :to-be-truthy))
    (it "includes the previous neighbor"
      (expect (taiga-api-neighbor-p (taiga-api-neighbors-previous neighbors)) :to-be nil))))

(describe "Reading a neighbor from an alist"
  (with-read-data (neighbor "neighbor")
    (with-subject neighbor
      (it "yields a neighbor" (expect (taiga-api-neighbor-p neighbor) :to-be-truthy))
      (it "includes its id" (expect (its 'id) :to-be 16))
      (it "includes a reference" (expect (its 'ref) :to-be 126))
      (it "includes a subject" (expect (its 'subject) :to-equal "Support for bulk actions")))))
