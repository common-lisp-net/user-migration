;; -*- Lisp -*-

(defpackage :ml-migration
  (:use :cl))

(in-package :ml-migration)

(defparameter *db-params* `("ml-migration" ,(sb-posix:getenv "USER") nil :unix))

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

(defun import-paths (base-path)
  (pomo:with-transaction ()
    (pomo:execute "delete from subscription;")
    (let ((*default-pathname-defaults* (pathname base-path)))
      (dolist (subscribers-pathname (directory "**/subscribers.d/*"))
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

(defun initialize-db (&key (base-path #P"/clo-backup/2014-01-25/var/spool/mlmmj/"))
  (pomo:execute "drop table subscription")
  (pomo:execute "drop table token")
  (pomo:create-all-tables)
  (import-paths base-path)
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
                            (:= 'token token)))
              :single!))

(defun lists-subscribed-by (email-address)
  (pomo:query
   (:order-by
    (:select 'mailing-list
     :from 'subscription
     :where (:= 'email-address email-address))
    'mailing-list)
   :column))

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
  `(with-html ()
     (:html
       (:head
        (:title ,title))
       (:body ,@body))))
     
(hunchentoot:define-easy-handler (confirm-subscriptions :uri "/migrate/confirm-subscriptions") (email-address token)

  (pomo:with-connection *db-params*

    (unless (and email-address token)
      (abort-request hunchentoot:+http-bad-request+ "Missing parameters"))

    (unless (token-valid-p email-address token)
      (abort-request hunchentoot:+http-forbidden+ "Invalid token"))

    (setf (hunchentoot:content-type*) "text/html")

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
       (with-page "Your mailing list subscriptions have been confirmed"
         (:p "Your subscription to the following list(s) has been confirmed")
         (:ul
          (dolist (list-name (mapcar #'car (hunchentoot:post-parameters*)))
            (html
              (:li (:princ list-name))))))))))
