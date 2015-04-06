(require 'ert)

(defvar taiga-api-test-buffer nil)

(eval-when-compile
  (defun taiga-api--status-name (code)
    (let ((status (assoc code '((200 . "OK")
                                (201 . "CREATED")
                                (204 . "NO CONTENT")
                                (400 . "BAD REQUEST")
                                (404 . "NOT FOUND")
                                (429 . "TOO MANY REQUESTS")))))
      (if status (cdr status))))

  (defun taiga-api--write-header (header)
    (format "%s: %s\n" (car header) (cdr header))))

(defmacro with-taiga-api-synchronous-response
    (status headers content &rest body)
  "Create a custom response for `url-retrieve-synchronously'.

STATUS is the status code that should be returned.  HEADERS are
extra headers that should be added to the response.  CONTENT is
the body of the response.

This macro creates a mock for `url-retrieve-synchronously' and
runs BODY.  It also creates a variable `taiga-api-test-buffer'
which is bound to the buffer the response is written to.  Use
this to inspect the contents of the buffer."
  (declare (indent 3))
  `(let ((taiga-api-test-buffer
          (generate-new-buffer "taiga-api-http-test")))
     (cl-letf (((symbol-function 'url-retrieve-synchronously)
                (lambda (&rest args)
                  (ignore args)
                  (with-current-buffer taiga-api-test-buffer
                    (insert "HTTP/1.1 " ,(number-to-string status)
                            " " ,(taiga-api--status-name status) "\n"
                            ,@(mapcar #'taiga-api--write-header headers)
                            "\n"
                            ,content)
                    (current-buffer)))))
       ,@body)))

(defun taiga-api--json-encoded-error ()
  "A json-encoded error to test with."
  (json-encode
   '(("_error_type" . "taiga.base.exceptions.WrongArguments")
     ("_error_message" . "Username or password does not matches user."))))

(defmacro taiga-api-test--ensure-token (token &rest body)
  "Test that the `taiga-api--auth-token' equals TOKEN after running BODY."
  (declare (indent 1))
  `(let ((taiga-api--auth-token ""))
     ,@body
     (should (string= taiga-api--auth-token ,token))))

(defun taiga-api-test--data (name func)
  "Read test data from a file with name `files/NAME.json' using FUNC."
  (with-temp-buffer
    (insert-file-contents
     (concat taiga-api-test--location "files/" name ".json"))
    (goto-char (point-min))
    (funcall func (json-read))))

(defun taiga-api-test--read (name)
  "Read test data from a file named `files/NAME.json'."
  (with-temp-buffer
    (insert-file-contents
     (concat taiga-api-test--location "files/" name ".json"))
    (goto-char (point-min))
    (buffer-substring-no-properties (point-min) (point-max))))

(defmacro taiga-api-test-throttling (form)
  "Define a test for a throttled request of FORM."
  (let* ((name (car form))
         (test-name (intern (concat (symbol-name name) "-throttled"))))
    `(ert-deftest ,test-name ()
       ,(concat "`" (symbol-name name)
                "' signals `taiga-api-throttled' when throttled.")
       (let ((taiga-api--auth-token "sometoken"))
         (with-taiga-api-synchronous-response
             429 nil (taiga-api--json-encoded-error)
           (should-error ,form :type 'taiga-api-throttled))))))

(defmacro taiga-api-test-unauthenticated (form)
  "Define a test for an unauthenticated request of FORM."
  (let* ((name (car form))
         (test-name
          (intern (concat (symbol-name name) "-unauthenticated"))))
    `(ert-deftest ,test-name ()
       ,(concat "`" (symbol-name name)
                "' signals `taiga-api-unauthenticated' when unauthenticated.")
       (let ((taiga-api--auth-token ""))
         (should-error ,form :type 'taiga-api-unauthenticated)))))

(defun should-have-auth-token (token)
  "Check that the URL request headers contain the auth token."
  (should (string= (concat "Bearer " token)
                   (cdr (assoc "Authorization" url-request-extra-headers)))))
