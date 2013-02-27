(defpackage :wookie-util
  (:use :cl :wookie-config)
  (:export #:wlog
           #:map-plist
           #:camel-case
           #:querystringp
           #:map-querystring
           #:body-to-string
           #:getf-reverse
           #:generate-tmp-file-name
           #:lookup-status-text))
(in-package :wookie-util)

(defvar *tmp-file-counter* 0
  "Holds a value that is incremented for each temporary file generated.")

(defun wlog (level format-string &rest format-args)
  "Wookie's logging function. simple for now, just sends to STDOUT."
  (when (<= (getf +log-levels+ level) (getf +log-levels+ *log-level*))
    (let ((output (if *log-output*
                      *log-output*
                      t)))
      (apply #'format (append (list output format-string) format-args)))))
  
(defun map-plist (plist fn)
  "Iterate over a plist"
  (let ((result nil))
    (dotimes (i (/ (length plist) 2))
      (let (header value)
        (setf header (car plist)
              plist (cdr plist)
              value (car plist)
              plist (cdr plist))
        (let ((res (funcall fn header value)))
          (when res (push res result)))))
    (reverse result)))

(defun camel-case (keyword)
  "Camel case anything that can be converted to a string (string, keyword,
   symbol, etc)."
  (let* ((lower (string-downcase (string keyword))))
    (loop for i from 0
          for c across lower do
      (let ((char-before (when (< 0 i)
                           (aref lower (1- i)))))
        (when (or (= i 0)
                  (eq char-before #\-)
                  (eq char-before #\space)
                  (eq char-before #\tab))
          (setf (aref lower i) (aref (string-upcase (string c)) 0)))))
    lower))

(defparameter *scanner-querystring-p*
  (cl-ppcre:create-scanner "^([a-z-_]+(=[^&]+)?(&|$))+" :case-insensitive-mode t)
  "Detects a querystring.")

(defun querystringp (querystring)
  "Detects if the given string is an HTTP querystring."
  (cl-ppcre:scan *scanner-querystring-p* querystring))

(defun map-querystring (querystring function)
  "Map a function that takes key and value args across a querystring."
  (let ((last-split 0))
    (loop for search-pos = (position #\& querystring :start last-split) do
      (let* ((entry (subseq querystring last-split search-pos))
             (equal-pos (position #\= entry))
             (key (subseq entry 0 equal-pos))
             (value (if equal-pos
                        (subseq entry (1+ equal-pos))
                        ""))
             (value (do-urlencode:urldecode value :lenientp t)))
        (unless (or (null key)
                    (string= key ""))
          (funcall function key value)))
      (unless search-pos (return))
      (setf last-split (1+ search-pos)))))

(defun body-to-string (body-bytes content-type-header)
  "Given a byte vector of HTTP body data and the value of a Content-Type header,
   convert the body to a string via the charset provided in the header. If a
   character encoding is not provided, go with the HTTP default, ISO-8859-1."
  (let* ((charset-pos (search "charset=" content-type-header))
         (charset (if charset-pos
                      (intern (string-upcase (subseq content-type-header
                                                     (+ charset-pos 8))) :keyword)
                      :iso-8859-1)))
    (handler-case
      (babel:octets-to-string body-bytes :encoding charset)
      (t ()
        (babel:octets-to-string body-bytes :encoding :iso-8859-1))))) 

(defun getf-reverse (plist key)
  "Like getf, except the VALUE comes before the KEY:
     '(:value1 :key1 :value2 :key2)
   Allows reverse lookups in plists without duplicating structures."
  (dotimes (i (length plist))
    (when (eq key (cadr plist))
      (return-from getf-reverse (car plist)))
    (setf plist (cddr plist))))

(defun generate-tmp-file-name ()
  "Generate the a full path/filename for a temporary file that does not exist
   already in the tmp directory."
  (format nil "~atmp~a" (namestring *tmp-file-store*) (incf *tmp-file-counter*)))

(defun lookup-status-text (status-code)
  "Get the HTTP standard text that goes along with a status code."
  (case status-code
    (100 "Continue")
    (101 "Switching Protocols")
    (102 "Processing")

    (200 "OK")
    (201 "Created")
    (202 "Accepted")
    (203 "Non-Authoratative Information")
    (204 "No Content")
    (205 "Reset Content")
    (206 "Partial Content")
    (207 "Multi-Status")
    (208 "Already Reported")
    (226 "IM Used")

    (300 "Multiple Choices")
    (301 "Moved Permanently")
    (302 "Found")
    (303 "See Other")
    (304 "Not Modified")
    (305 "Use Proxy")
    (306 "Switch Proxy")
    (307 "Temporary Redirect")
    (308 "Permanent Redirect")

    (400 "Bad Request")
    (401 "Unauthorized")
    (402 "Payment Required")
    (403 "Forbidden")
    (404 "Not Found")
    (405 "Method Not Allowed")
    (406 "Not Acceptable")
    (407 "Proxy Authentication Required")
    (408 "Request Timeout")
    (409 "Conflict")
    (410 "Gone")
    (411 "Length Required")
    (412 "Precondition Failed")
    (413 "Request Entity Too Large")
    (414 "Request-URI Too Long")
    (415 "Unsupported Media Type")
    (416 "Requested Range Not Satisfiable")
    (417 "Expectation Failed")
    (418 "I'm a teapot")
    (420 "Enhance Your Calm")
    (422 "Unprocessable Entity")
    (423 "Locked")
    (424 "Failed Dependency")
    (425 "Unordered Collection")
    (426 "Upgrade Required")
    (428 "Precondition Required")
    (429 "Too Many Requests")
    (431 "Request Header Fields Too Large")
    (444 "No Response")
    (449 "Retry With")
    (450 "Blocked by Windows Parental Controls")
    (451 "Unavailable For Legal Reasons")
    (494 "Request Header Too Large")
    (495 "Cert Error")
    (496 "No Cert")
    (497 "HTTP to HTTPS")
    (499 "Client Closed Request")

    (500 "Internal Server Error")
    (501 "Not Implemented")
    (502 "Bad Gateway")
    (503 "Service Unavailable")
    (504 "Gateway Timeout")
    (505 "HTTP Version Not Supported")
    (506 "Variant Also Negotiates")
    (507 "Insufficient Storage")
    (508 "Loop Detected")
    (509 "Bandwidth Limit Exceeded")
    (510 "Not Extended")
    (511 "Network Authentication Required")
    (598 "Network Read Timeout Error")
    (599 "Network Connect Timeout Error")
    (t "Unknown Status")))

