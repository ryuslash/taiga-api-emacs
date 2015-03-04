(require 'ert)

(if (require 'undercover nil :noerror)
    (undercover "lisp/*.el")
  (warn "Couldn't load undercover"))

(defvar taiga-api-test-buffer nil)

(eval-when-compile
  (defun taiga-api--status-name (code)
    (let ((status (assoc code '((200 . "OK")
                                (201 . "CREATED")
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
  `(let ((taiga-api-test-buffer (generate-new-buffer "taiga-api-http-test")))
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
  (json-encode '(("_error_type" . "taiga.base.exceptions.WrongArguments")
                 ("_error_message" . "Username or password does not matches user."))))

(defmacro taiga-api-test--ensure-token (token &rest body)
  "Test that the `taiga-api--auth-token' equals TOKEN after running BODY."
  (declare (indent 1))
  `(let ((taiga-api--auth-token ""))
     ,@body
     (should (string= taiga-api--auth-token ,token))))
