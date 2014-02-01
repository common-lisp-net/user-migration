;; -*- Lisp -*-

(defpackage :ml-migration
  (:use :cl))

(in-package :ml-migration)

(defclass subscription ()
  ((email-address :col-type string :initarg :email-address :reader email-address)
   (mailing-list :col-type string :initarg :mailing-list :reader mailing-list))
  (:metaclass postmodern:dao-class)
  (:keys email-address mailing-list))

(defun connect-db ()
  (pomo:connect-toplevel "ml-migration" "hhubner" nil :unix))

(pomo:deftable subscription
  (pomo:!dao-def)
  (pomo:!index 'email-address)
  (pomo:!index 'mailing-list))

(defclass token ()
  ((email-address :col-type string :initarg :email-address :reader email-address)
   (token :col-type string :initarg :token :reader token))
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
          (format t "processing ~A~%" subscribers-pathname)
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
     
(hunchentoot:define-easy-handler (confirm-subscriptions :uri "/migrate/confirm-subscriptions") (email-address token)
  (format nil "hello ~A ~A~%" email-address token)

  (unless (and email-address token)
    (abort-request hunchentoot:+http-bad-request+ "Missing parameters"))

  (unless (pomo:query (:select '*
                       :from 'token
                       :where (:and (:= 'email-address email-address)
                                    (:= 'token token))))
    (abort-request hunchentoot:+http-forbidden+ "Invalid token"))

  (setf (hunchentoot:content-type*) "text/html")

  (with-html ()
    (:html
     (:head
      (:title "Confirm your mailing list subscriptions"))
     (:body
      (:p "Please select all mailing lists on common-lisp.net that you still want to be subscribed to")
      ((:form :method "POST")
       (:ul
        (dolist (list-name (pomo:query
                            (:order-by
                             (:select 'mailing-list
                              :from 'subscription
                              :where (:= 'email-address email-address))
                             'mailing-list)
                            :column))
          (html
            (:li
             ((:input :type "checkbox" :checked "checked"))
             (:princ list-name)))))
       ((:button :type "submit") "Confirm subscription to selected lists"))))))
