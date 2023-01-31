;; wookie-plugin-export provides a shared namespace for plugins to provide
;; their public symbols to. apps can :use this package to gain access to
;; the shared plugin namespace.
(defpackage :wookie-plugin-export
  (:use :cl))

(in-package :wookie)

(defvar *plugin-folders* (list "./wookie-plugins/"
                               (asdf:system-relative-pathname :wookie #P"core-plugins/"))
  "A list of directories where Wookie plugins can be found.")

(defvar *available-plugins* nil
  "A plist (generated by load-plugins) that holds a mapping of plugin <--> ASDF
   systems for the plugins. Reset on each load-plugins run.")

(defun register-plugin (plugin-name init-function unload-function)
  "Register a plugin in the Wookie plugin system. Generally this is called from
   a plugin.lisp file, but can also be called elsewhere in the plugin. The
   plugin-name argument must be a unique keyword, and init-fn is the
   initialization function called that loads the plugin (called only once, on
   register)."
  (vom:debug1 "(plugin) Register plugin ~s" plugin-name)

  ;; Add to plugin to *available-plugins*.
  (match-plugin-asdf (read-from-string (format nil ":~a" (file-namestring (package-name *package*))))
		     (read-from-string (format nil ":~a" (package-name *package*))))

  (let ((plugin-entry (list :name plugin-name
                            :init-function init-function
                            :unload-function unload-function)))
    ;; if enabled (And not already loaded), load it
    (when (and (find plugin-name *enabled-plugins*)
               (not (gethash plugin-name (wookie-state-plugins *state*))))
      (setf (gethash plugin-name (wookie-state-plugins *state*)) plugin-entry)
      (funcall init-function))))

(defun unload-plugin (plugin-name)
  "Unload a plugin from the wookie system. If it's currently registered, its
   unload-function will be called.

   Also unloads any current plugins that depend on this plugin. Does this
   recursively so all depencies are always resolved."
  (vom:debug1 "(plugin) Unload plugin ~s" plugin-name)
  ;; unload the plugin
  (let ((plugin (gethash plugin-name (wookie-state-plugins *state*))))
    (when plugin
      (funcall (getf plugin :unload-function (lambda ())))
      (remhash plugin-name (wookie-state-plugins *state*))))

  (let ((asdf (getf *available-plugins* plugin-name)))
    (when asdf
      (let* ((tmp-deps (asdf:component-depends-on
                            'asdf:load-op
                            (asdf:find-system asdf)))
             (plugin-deps (mapcar (lambda (asdf)
                                    (intern (asdf:component-name (asdf:find-system asdf)) :keyword))
                                  (cdadr tmp-deps)))
             (plugin-systems (loop for system in *available-plugins*
                                   for i from 0
                                   when (oddp i)
                                     collect (intern (string system) :keyword)))
             (to-unload (intersection plugin-deps plugin-systems)))
        (vom:debug1 "(plugin) Unload deps for ~s ~s" plugin-name to-unload)
        (dolist (asdf to-unload)
          (let ((plugin-name (getf-reverse *available-plugins* asdf)))
            (unload-plugin plugin-name)))))))

(defun plugin-config (plugin-name)
  "Return the configuration for a plugin. Setfable."
  (unless (hash-table-p (wookie-state-plugin-config *state*))
    (setf (wookie-state-plugin-config *state*) (make-hash-table :test #'eq)))
  (gethash plugin-name (wookie-state-plugin-config *state*)))

(defun (setf plugin-config) (config plugin-name)
  "Allow setting of plugin configuration via setf."
  (unless (hash-table-p (wookie-state-plugin-config *state*))
    (setf (wookie-state-plugin-config *state*) (make-hash-table :test #'eq)))
  (setf (gethash plugin-name (wookie-state-plugin-config *state*)) config))

(defun plugin-request-data (plugin-name request)
  "Retrieve the data stored into a request object for the plugin-name (keyword)
   plugin. This function is setfable."
  (let ((data (request-plugin-data request)))
    (when (hash-table-p data)
      (gethash plugin-name data))))

(defun (setf plugin-request-data) (data plugin-name request)
  "When a plugin wants to store data available to the main app, it can do so by
   storing the data into the request's plugin data. This function allows this by
   taking the plugin-name (keyword), request object passed into the route, and
   the data to store."
  (vom:debug1 "(plugin) Set plugin data ~s: ~a" plugin-name data)
  (unless (hash-table-p (request-plugin-data request))
    (setf (request-plugin-data request) (make-hash-table :test #'eq)))
  (setf (gethash plugin-name (request-plugin-data request)) data))

(defun match-plugin-asdf (plugin-name asdf-system)
  "Match a plugin and an ASDF system toeach other."
  (setf (getf *available-plugins* plugin-name) asdf-system))

(defun pkg-symbol (sym pkg)
  (and pkg (find-symbol (if (stringp sym) sym (symbol-name sym)) pkg)))

(defun load-system (system &key use-quicklisp)
  ;; FUCK the system
  (let* ((pkg (find-package :ql))
         (quickload-sym (pkg-symbol '#:quickload pkg)))
    (if (and use-quicklisp pkg)
        (if quickload-sym
            (funcall quickload-sym system)
            (error "Symbol ~A is missing from package ~A(!)" '#:quickload pkg))
        (asdf:oos 'asdf:load-op system))))

(defun load-plugins (&key ignore-loading-errors (use-quicklisp t))
  "Load default plugins. Deprecated: Please list needed plugins in your ASDF system's
   :depends-on instead."
  (declare (ignore ignore-loading-errors))
  (vom:debug "(plugin) Load plugins ~s" *enabled-plugins*)
  (unless (wookie-state-plugins *state*)
    (setf (wookie-state-plugins *state*) (make-hash-table :test #'eq)))
  ;; unload current plugins
  (loop for name being the hash-keys of (wookie-state-plugins *state*) do
    (unload-plugin name))
  (setf *available-plugins* nil)
  (loop for plugin in *enabled-plugins*
	for plugin-system = (format nil
				    "wookie/wookie-plugin/~a"
				    (string-downcase (string plugin)))
	do (load-system plugin-system :use-quicklisp use-quicklisp)))

(defmacro defplugin (&rest asdf-defsystem-args)
  "Simple wrapper around asdf:defsystem that maps a plugin-name (hopefully in
   *current-plugin-name*) to the ASDF system the plugin defines."
  `(progn
     (asdf:defsystem ,@asdf-defsystem-args)
     (wookie::match-plugin-asdf wookie::*current-plugin-name*
                                ,(intern (string-upcase (string (car asdf-defsystem-args)))
                                         :keyword))))

(defmacro defplugfun (name args &body body)
  "Define a plugin function that is auto-exported to the :wookie-plugin-export
   package."
  `(progn
     (defun ,name ,args ,@body)
     (shadowing-import ',name :wookie-plugin-export)
     (export ',name :wookie-plugin-export)))
