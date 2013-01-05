(defpackage :wookie-plugin
  (:use :cl :wookie)
  (:export :register-plugin
           :set-plugin-request-data
           :get-plugin-request-data
           :set-plugin-folder
           :load-plugins)
  (:import-from :wookie))
(in-package :wookie-plugin)

(defvar *plugins* nil
  "A hash table holding all registered Wookie plugins.")
(defvar *plugin-config* nil
  "A hash table holding configuration values for all plugins.") 
(defvar *plugin-folder* "./wookie-plugins/"
  "thedirectory Wookie plugins are loaded from.") 

(defun register-plugin (plugin-name meta init-fn)
  "Register a plugin in the Wookie plugin system. Generally this is called from
   a plugin.lisp file, but can also be called elsewhere in the plugin. The
   plugin-name argument must be a unique keyword, meta is a plist of information
   about the plugin (name, author, description, etc), and init-fn is the
   initialization function called that loads the plugin (called only once, on
   register)."
  (setf (gethash plugin-name *plugins*)
        (list :name plugin-name :meta meta))
  (funcall init-fn))

(defun plugin-config (plugin-name)
  "Return the configuration for a plugin. Setfable."
  (unless (hash-table-p *plugin-config*)
    (setf *plugin-config* (make-hash-table :test #'eq)))
  (gethash plugin-name *plugin-config*))

(defun (setf plugin-config) (config plugin-name)
  "Allow setting of plugin configuration via setf."
  (unless (hash-table-p *plugin-config*)
    (setf *plugin-config* (make-hash-table :test #'eq)))
  (setf (gethash plugin-name *plugin-config*) config))

(defun set-plugin-request-data (plugin-name request data)
  "When a plugin wants to store data available to the main app, it can do so by
   storing the data into the request's plugin data. This function allows this by
   taking the plugin-name (keyword), request object passed into the route, and
   the data to store."
  (unless (hash-table-p (request-plugin-data request))
    (setf (request-plugin-data request) (make-hash-table :test #'eq)))
  (setf (gethash plugin-name (request-plugin-data request)) data)) 

(defun get-plugin-request-data (plugin-name request)
  "Retrieve the data stored into a request object for the plugin-name (keyword)
   plugin."
  (let ((data (request-plugin-data request)))
    (when (hash-table-p data)
      (gethash plugin-name data))))

(defun set-plugin-folder (folder)
  "Set the directory to load Wookie plugins from."
  (setf *plugin-folder* folder))

(defun load-plugins (&key compile)
  "Load all plugins under the *plugin-folder* fold (set with set-plugin-folder).
   There is also the option to compile the plugins (default nil)."
  (setf *plugins* (make-hash-table :test #'eq))
  (let ((scan (concatenate 'string (namestring *plugin-folder*) "*")))
    (dolist (dir (directory scan))
      (let* ((dirstr (namestring dir))
             (last-char (aref dirstr (1- (length dirstr)))))
        (when (or (eq last-char #\/)
                  (eq last-char #\\))
          (let ((plugin-file (concatenate 'string dirstr
                                          "plugin.lisp")))
            (when (probe-file plugin-file)
              (when compile
                (setf plugin-file (compile-file plugin-file)))
              (load plugin-file))))))))

