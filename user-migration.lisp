;; -*- Lisp -*-

(defpackage :user-migration
  (:use :cl :alexandria))

(in-package :user-migration)

(defparameter *db-params* `("user-migration" ,(sb-posix:getenv "USER") nil :unix))
(defparameter *ml-list-url* "http://common-lisp.net/mailman/lists")
(defparameter *sudo* "/usr/bin/sudo")
(defparameter *mailman-add-members* "/usr/sbin/add_members")

(defun connect-db ()
  (apply #'pomo:connect-toplevel *db-params*))

(defclass subscription ()
  ((email-address :col-type string :initarg :email-address :reader email-address)
   (mailing-list :col-type string :initarg :mailing-list :reader mailing-list))
  (:metaclass postmodern:dao-class)
  (:keys email-address mailing-list))

(pomo:deftable subscription
  (pomo:!dao-def)
  (pomo:!index 'email-address)
  (pomo:!index 'mailing-list))

(defclass token ()
  ((email-address :col-type string :initarg :email-address :reader email-address)
   (token :col-type string :initarg :token :reader token)
   (usedp :col-type boolean :initarg :usedp :reader usedp))
  (:default-initargs :usedp nil)
  (:metaclass postmodern:dao-class)
  (:keys email-address))

(pomo:deftable token
  (pomo:!dao-def))

(defclass active-list ()
  ((mailing-list :col-type string :initarg :mailing-list :reader mailing-list)
   (owner :col-type string :initarg :string :reader owner))
  (:metaclass postmodern:dao-class)
  (:keys mailing-list))

(pomo:deftable active-list
  (pomo:!dao-def))

(defclass shell-user ()
  ((login-name :col-type string :initarg :login-name :reader login-name)
   (full-name :col-type string :initarg :full-name :reader full-name)
   (email-address :col-type (or s-sql:db-null string) :initarg :email-address :reader email-address)
   (ssh-authorized-keys :col-type (or s-sql:db-null string) :initarg :ssh-authorized-keys :reader ssh-authorized-keys))
  (:metaclass postmodern:dao-class)
  (:keys login-name))

(pomo:deftable shell-user
  (pomo:!dao-def)
  (pomo:!index 'email-address))

(defclass project ()
  ((name :col-type string :initarg :name :reader name))
  (:metaclass postmodern:dao-class)
  (:keys name))

(pomo:deftable project
  (pomo:!dao-def))

(defclass project-membership ()
  ((project-name :col-type string :initarg :project-name :read project-name)
   (member-login-name :col-type string :initarg :member-login-name :reader member-login-name))
  (:metaclass postmodern:dao-class))

(pomo:deftable project-membership
  (pomo:!dao-def)
  (pomo:!unique '(project-name member-login-name))
  (pomo:!foreign 'project 'project-name 'name)
  (pomo:!foreign 'shell-user 'member-login-name 'login-name))

(defun read-line-skipping-comments (stream)
  (loop for line = (read-line stream nil)
        while line
        unless (cl-ppcre:scan "^\\s*($|#.*)" line)
          do (return line)))

(defun maybe-read-file (pathname)
  (when (probe-file pathname)
    (alexandria:read-file-into-string pathname)))

(defun read-dot-forward (pathname)
  (alexandria:when-let (file-contents (maybe-read-file pathname))
    (dolist (line (cl-ppcre:split #\Newline file-contents))
      (when (cl-ppcre:scan #\@ line)
        (return line)))))

(defun import-shell-user (login-name full-name home-directory)
  (let ((*default-pathname-defaults* (merge-pathnames (format nil "~A/" (subseq home-directory 1)))))
    (pomo:make-dao 'shell-user
                   :login-name login-name
                   :full-name full-name
                   :ssh-authorized-keys (or (maybe-read-file ".ssh/authorized_keys") :null)
                   :email-address (or (read-dot-forward ".forward") :null))))

(defun import-shell-users (backup-directory-pathname)
  (pomo:execute "delete from shell_user")
  (let ((*default-pathname-defaults* (merge-pathnames (cl-fad:pathname-as-directory backup-directory-pathname))))
    (with-open-file (passwd "etc/passwd")
      (loop
        (destructuring-bind (login-name password uid gid full-name home-directory &optional shell)
            (cl-ppcre:split #\: (or (read-line passwd nil)
                                    (return)))
          (declare (ignore password gid shell))
          (when (>= (parse-integer uid) 1000)
            (import-shell-user login-name full-name home-directory)))))))

(defun import-project (project-name members)
  (pomo:make-dao 'project
                 :name project-name)
  (dolist (member members)
    (when (pomo:query (:select '*
                       :from 'shell-user
                       :where (:= 'login-name member)))
      (pomo:make-dao 'project-membership
                     :project-name project-name
                     :member-login-name member))))

(defun import-projects (backup-directory-pathname)
  (pomo:execute "delete from project_membership")
  (pomo:execute "delete from project")
  (let ((*default-pathname-defaults* (merge-pathnames (cl-fad:pathname-as-directory backup-directory-pathname))))
    (with-open-file (group "etc/group")
      (loop
        (destructuring-bind (group-name password gid &optional members)
            (cl-ppcre:split #\: (or (read-line-skipping-comments group)
                                    (return)))
          (declare (ignore password))
          (setf members (cl-ppcre:split #\, members))
          (when (and members
                     (not (member group-name members :test #'equal))
                     (>= (parse-integer gid) 1000))
            (import-project group-name members)))))))

(defun import-subscriptions (base-path)
  (pomo:with-transaction ()
    (pomo:execute "delete from subscription;")
    (let* ((*default-pathname-defaults* (merge-pathnames (pathname base-path)))
           (mailing-list-path "**/subscribers.d/*"))
      (dolist (subscribers-pathname (or (directory mailing-list-path)
                                        (error "no mailing lists found in ~A" (merge-pathnames mailing-list-path))))
        (let ((list-name (cl-ppcre:regex-replace "/.*" (enough-namestring subscribers-pathname) "")))
          (with-open-file (f subscribers-pathname)
            (loop
              (pomo:make-dao 'subscription
                             :email-address (or (read-line f nil) (return))
                             :mailing-list list-name))))))))

(defun make-tokens ()
  (pomo:with-transaction ()
    (pomo:execute "delete from token;")
    (dolist (email-address (pomo:query (:select 'email-address :distinct :from 'subscription) :column))
      (pomo:make-dao 'token
                     :email-address email-address
                     :token (format nil "~36,8,'0R" (random most-positive-fixnum))))))

#+(or)
(defun initialize-db (&key ((base-path *default-pathname-defaults*) #P"/clo-backup/2014-03-30/"))
  (pomo:execute "drop table if exists subscription")
  (pomo:execute "drop table if exists token")
  (pomo:execute "drop table if exists project_membership")
  (pomo:execute "drop table if exists shell_user")
  (pomo:execute "drop table if exists project")
  (pomo:execute "drop table if exists active_list")
  (pomo:create-all-tables)
  (import-subscriptions "var/spool/mlmmj/")
  (make-tokens))

(defun token-valid-p (email-address token)
  (pomo:query (:select '*
               :from 'token
               :where (:and (:= 'email-address email-address)
                            (:= 'token token)))))

(defun token-used-p (email-address token)
  (pomo:query (:select 'usedp
               :from 'token
               :where (:and (:= 'email-address email-address)
                            (:= 'token token)))))

(defun (setf token-used-p) (new-value email-address token)
  (pomo:query (:update 'token
               :set 'usedp new-value
               :where (:and (:= 'email-address email-address)
                            (:= 'token token)))))

(defun lists-subscribed-by (email-address)
  (pomo:query
   (:order-by
    (:select 'mailing-list
     :from 'subscription
     :where (:= 'email-address email-address))
    'mailing-list)
   :column))

(defun subscribe-to-list (email-address list-name)
  (with-input-from-string (s (format nil "~A~%" email-address))
    ;; potential deadlock when the subprocess creates too much output to be buffered in the pipe
    (let ((process (sb-ext:run-program *sudo*  `(,*mailman-add-members* "--regular-members=-" "--welcome-msg=n" "--admin-notify=n" ,list-name)
                                       :input s :output :stream :error :output)))
      (unless (zerop (sb-ext:process-exit-code process))
        (error "could not subscribe ~S to mailing list ~S:~%~A~%"
               email-address list-name
               (with-output-to-string (s)
                 (alexandria:copy-stream (sb-ext:process-output process) s)))))))

(defun read-legacy-list-owners ()
  (alist-hash-table (mapcar (lambda (owner-file)
                              (cons (cl-ppcre:regex-replace ".*mlmmj/(.*)/control/owner" (namestring owner-file) "\\1")
                                    (cl-ppcre:regex-replace "(?s)\\n.*" (read-file-into-string owner-file) "")))
                            (or (directory #P"var/spool/mlmmj/*/cotrol/owner")
                                (error "no lists found")))
                    :test #'equal))

(defun get-active-lists ()
  (pomo:query (:select 'mailing-list :from 'active-list) :column))

(defun update-list-owners ()
  (let ((list-owners (read-legacy-list-owners)))
    (dolist (list (get-active-lists))
      (pomo:query (:update 'active-list
                   :set 'owner (gethash list list-owners)
                   :where (:= 'mailing-list list))))))

(defvar *server* nil)

(defun start (&key (port 4242))
  (when *server*
    (hunchentoot:stop *server*))
  (setf *server* (make-instance 'hunchentoot:easy-acceptor :port port))
  (hunchentoot:start *server*))

(defun abort-request (status text)
  (setf (hunchentoot:return-code*) status)
  (hunchentoot:abort-request-handler text))

(defmacro with-html (() &body body)
  `(with-output-to-string (s)
     (xhtml-generator:with-xhtml (s)
       ,@body)))

(defmacro html (&body body)
  `(xhtml-generator:html ,@body))

(defmacro with-page (title &body body)
  `(progn
     (setf (hunchentoot:content-type*) "text/html")
     (with-html ()
       (:html
         (:head
          (:title ,title))
         (:body ,@body)))))
     
(hunchentoot:define-easy-handler (confirm-subscriptions :uri "/migrate/confirm-subscriptions") (email-address token)

  (pomo:with-connection *db-params*

    (unless (and email-address token)
      (abort-request hunchentoot:+http-bad-request+ "Missing parameters"))

    (unless (token-valid-p email-address token)
      (abort-request hunchentoot:+http-forbidden+ "Invalid token"))

    (when (token-used-p email-address token)
      (abort-request hunchentoot:+http-ok+
                     (with-page "You already confirmed your old subscriptions"
                       (:p "Your previous subscriptions have already been confirmed.  Please visit "
                           ((:a :href *ml-list-url* :target "_new")
                            " the main mailing list page")
                           " to subscribe to more lists"))))

    (ecase (hunchentoot:request-method*)
      (:GET
       (with-page "Confirm your mailing list subscriptions"
         (:p "Please select all mailing lists on common-lisp.net that you still want to be subscribed to")
         ((:form :method "POST")
          (:ul
           (dolist (list-name (lists-subscribed-by email-address))
             (html
               (:li
                ((:input :type "checkbox" :checked "checked" :name list-name))
                (:princ list-name)))))
          ((:button :type "submit") "Confirm subscription to selected lists"))))

      (:POST
       (setf (token-used-p email-address token) t)

       (with-page "Your mailing list subscriptions have been confirmed"
         (:p "Your subscription to the following list(s) has been confirmed")
         (:ul
          (dolist (list-name (mapcar #'car (hunchentoot:post-parameters*)))
            (subscribe-to-list email-address list-name)
            (html
              (:li (:princ list-name))))))))))
