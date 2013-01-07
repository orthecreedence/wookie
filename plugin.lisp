;; wookie-plugin-export provides a shared namespace for plugins to provide
;; their public symbols to. apps can :use this package to gain access to
;; the shared plugin namespace.
(defpackage :wookie-plugin-export
  (:use :cl))

(defpackage :wookie-plugin
  (:use :cl :wookie)
  (:export :register-plugin
           :set-plugin-request-data
           :get-plugin-request-data
           :*plugin-folders*
           :*enabled-plugins*
           :load-plugins)
  (:import-from :wookie))
(in-package :wookie-plugin)

(defvar *plugins* nil
  "A hash table holding all registered Wookie plugins.")
(defvar *plugin-config* nil
  "A hash table holding configuration values for all plugins.")
(defvar *plugin-folders* '("./wookie-plugins/")
  "A list of directories where Wookie plugins can be found.")
(defvar *enabled-plugins* nil
  "A list of (keyword) names of enabled plugins.")

(defun register-plugin (plugin-name meta init-function unload-function)
  "Register a plugin in the Wookie plugin system. Generally this is called from
   a plugin.lisp file, but can also be called elsewhere in the plugin. The
   plugin-name argument must be a unique keyword, meta is a plist of information
   about the plugin (name, author, description, etc), and init-fn is the
   initialization function called that loads the plugin (called only once, on
   register)."
  (when (and (find plugin-name *enabled-plugins*)    ; make sure it's enabled
             (not (gethash plugin-name *plugins*)))  ; make sure it's not loaded already
    (setf (gethash plugin-name *plugins*)
          (list :name plugin-name :meta meta :unload-function unload-function))
    (funcall init-function)))

(defun unload-plugin (plugin-name)
  "Unload a plugin from the wookie system. If it's currently registered, its
   unload-function will be called."
  (let ((plugin (gethash plugin-name *plugins*)))
    (when plugin
      (funcall (getf plugin :unload-function (lambda ())))
      (remhash plugin-name *plugins*))))

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

(defun load-plugins (&key compile)
  "Load all plugins under the *plugin-folder* fold (set with set-plugin-folder).
   There is also the option to compile the plugins (default nil)."
  (unless *plugins*
    (setf *plugins* (make-hash-table :test #'eq)))
  ;; unload current plugins
  (loop for name being the hash-keys of *plugins* do
    (unload-plugin name))
  (dolist (plugin-folder *plugin-folders*)
    (let ((scan (concatenate 'string (namestring plugin-folder) "*")))
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
                (load plugin-file)))))))))

