#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defimpl core
  "Core module to provide API interface.")

(defimpl dispatcher
  "Primary dispatcher module that propagates page calls to triggers."
  (dispatch (request) "Dispatch a new webserver call.")
  (register (hook module uri) "Register a hook to dispatch to on the given URI.")
  (unregister (uri) "Free a given URI.")
  (effective-trigger (uri) "Return the trigger and URI that would be called on the given request URI.")
  (dispatch-default (request) "The standard method to invoke when no specific handler has been found."))

(defimpl (user)
  "Defines a very basic user class that allows tying arbitrary data to a user."
  (user-get (username) "Returns the user object of an existing user or creates a new hull instance.")
  (user-field ((field string) &key value) "Set or get a user data field.")
  (user-save () "Save the user to the database.")
  (user-saved-p () "Returns T if the user is not a hull instance, otherwise NIL.")
  (user-check (branch) "Checks if the user has access to that permissions branch")
  (user-grant (branch) "Give permission to a certain branch.")
  (user-prohibit (branch) "Reclaim/Prohibit permission to a certain branch."))

(defimpl auth
  "Handles one or more methods for authentication of a user."
  (authenticate () "Authenticate the current user using whatever method applicable. Returns the user object.")
  (auth-page-login (&key redirect) "Returns an URL to the login page of the auth system. If redirect is provided, the user will be taken to that page afterwards.")
  (auth-page-logout (&key redirect) "Returns an URL to the logout page of the auth system. If redirect is provided, the user will be taken to that page afterwards.")
  (auth-page-register (&key redirect) "Returns an URL to the registration page of the auth system. If redirect is provided, the user will be taken to that page afterwards.")
  (auth-page-options (&key target) "Either displays a full options page or inserts all necessary things into the target if provided."))

(defimpl (session)
  "Session instances track whether a given user is still logged in or not."
  (session-get ((uuid string)) "Returns the session for the given UUID or NIL if no session is found.")
  (session-start ((username string)) "Creates a new session object for the given user.")
  (session-start ((user user)) "Creates a new session object for the given user.")
  (session-start-temp () "Creates a temporary session without a bound user.")
  (session-uuid () "Returns the uuid for this session.")
  (session-user () "Returns the user associated with this session.")
  (session-field ((field string) &key value) "Set or get a session data field.")
  (session-end () "Finalizes the session object and in effect logs the user out.")
  (session-active-p () "Returns T if the session is still active, otherwise NIL.")
  (session-temp-p () "Returns T if the session is only temporary, otherwise NIL."))

(defimpl (profile)
  "Extension for the user class to get common things such as display name, avatar and fields and provide a UI."
  (profile-field ((user user) (name string) &key value default) "Retrieves or sets a custom user field. If the field is unset or does not exist, the default value is returned.")
  (profile-avatar ((user user) (size fixnum)) "Returns an URL to the avatar of the user, in the closest available size to the one requested.")
  (profile-name ((user user)) "Returns the displayable name of the user.")
  (profile-page-settings ((user user)) "Returns the URL to the settings page for the user.")
  (profile-page-user (user) "Returns the URL to the user's profile page."))

(defimpl database
  "Base database interface. Tries to be as abstract as possible while still providing all essential functionality.
Manipulating data directly through this is discouraged and the data-model class should be used instead."
  (db-connect (dbname &key (host (config-tree :database :host))
                      (port (config-tree :database :port))
                      (user (config-tree :database :user))
                      (pass (config-tree :database :pass)))
              "Connects to the database given the information in the arguments.")
  (db-disconnect () "Disconnects the database")
  (db-connected-p () "Returns T if the database is connected, otherwise NIL.")
  (db-collections () "Returns a list of all existing collections.")
  (db-create ((collection string) fields &key indices)
             "Create a new collection with an optional list of indexed fields.")
  (db-select ((collection string) query &key (skip 0) (limit 0) sort) 
             "Retrieve data from the collection. Query should be constructed with the query macro.")
  (db-iterate ((collection string) query function &key (skip 0) (limit 0) sort) 
              "Iterate over data in the collection. Query should be constructed with the query macro. Might be faster than db-select.")
  (db-insert ((collection string) data) 
             "Insert the data into the collection. Data is a list of alists.")
  (db-remove ((collection string) query &key (skip 0) (limit 0)) 
             "Delete data from the collection. Query should be constructed with the query macro.")
  (db-update ((collection string) query data &key (skip 0) (limit 0) replace) 
             "Update data in the collection. Query should be constructed with the query macro and data is a list of alists.")
  (db-apropos ((collection string)) "Returns a list of all available fields and their type or NIL if any field is possible."))

(defimpl (data-model)
  "Abstract data object for easier database interaction."
  (model-field ((field string) &key value)
               "Returns the value of a field. Is setf-able.") 
  (model-get ((collection string) query &key (skip 0) (limit 0) sort)
             "Returns a list of model instances built from the query result.")
  (model-get-one ((collection string) query &key (skip 0) sort)
                 "Returns the model instance of the first query result.")
  (model-hull ((collection string))
              "Returns an empty model hull that can be used to insert data.")
  (model-hull-p () "Returns T if the model is a hull, otherwise NIL.")
  (model-save () "Updates the model in the database or throws an error if it does not exist.")
  (model-delete () "Deletes the model from the database.")
  (model-insert () "Inserts the model into the database."))

(defmacro with-fields ((&rest field-spec) model &body body)
  "Lets you access fields directly by name. This is similar to with-accessors.
Each field-spec can either be a symbol depicting the variable and field to bind or a list of a symbol
and a string, denoting variable name and field name respectively."
  (let ((vargens (gensym "MODEL")))
    `(let ((,vargens ,model))
       (symbol-macrolet
           ,(loop for field in field-spec 
               for varname = (if (listp field) (first field) field)
               for fieldname = (if (listp field) (second field) (string-downcase (symbol-name field)))
               collect `(,varname (model-field ,vargens ,fieldname)))
         ,@body))))

(defmacro with-model (model-spec (collection query &key (skip 0) sort save) &body body)
  "Allows easy access to a single model. Model-spec can be either just the model variable's name, or a list
starting with the model's name, followed by field specifiers like in with-fields. If save is not-NIL, a
model-save is executed after the body. The return value of this is always the last statement in the body,
even if save is non-NIL."
  (let* ((returngens (gensym "RETURN"))
         (modelname (if (listp model-spec) (car model-spec) model-spec))
         (modelfields (if (listp model-spec) (cdr model-spec) NIL)))
    (if save (setf body `((let ((,returngens (progn ,@body))) (model-save ,modelname) ,returngens))))
    (if modelfields (setf body `((with-fields ,modelfields ,modelname ,@body))))
    `(let ((,modelname (model-get-one T ,collection ,query :skip ,skip :sort ,sort)))
       (when ,modelname
         ,@body))))

(defmacro define-query-parts (&rest parts)
  `(progn ,@(loop for func in parts
               collect `(defun ,(car func) ,(cdr func)
                          "Stub for database query functions- Should be redefined by the db implemenntation"
                          (declare (ignore ,@(if (eq (second func) '&rest)
                                                 (cddr func) (cdr func))))
                          (error "~a not implemented!" ,(car func))))))

(define-query-parts (:and &rest args) (:or &rest args) (:not &rest args)
                    (:> a b) (:< a b) (:= a b) (:<= a b) (:>= a b)
                    (:in a b) (:!in a b) (:matches a b))


(defmethod getdf ((model data-model) field)
  (model-field model field))

(defmethod getdf ((model user) field)
  (user-field model field))

(defmethod getdf ((model session) field)
  (session-field model field))

(defimpl admin
  "Administration panel."
  (site () "Handles the admin page rendering.")
  (admin-category (name) "Defines a new menu category.")
  (admin-panel (name category function &key funcargs) "Defines a new function that will create the panel."))
