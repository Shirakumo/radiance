#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defimpl dispatcher
    "Primary dispatcher module that propagates page calls to triggers."
  (dispatch (request) "Dispatch a new webserver call.")
  (register (trigger) "Register a trigger to dispatch to."))

(defimpl (user)
    "Defines a very basic user class that allows tying arbitrary data to a user."
  (user-get (username) "Returns the user object of an existing user or creates a new hull instance.")
  (user-field (field &key value) "Set or get a user data field.")
  (user-save () "Save the user to the database.")
  (user-saved-p () "Returns T if the user is not a hull instance, otherwise NIL.")
  (check (&rest branch) "Checks if the user has access to that permissions branch"))

(defimpl auth
    "Handles one or more methods for authentication of a user."
  (authenticate () "Authenticate the current user using whatever method applicable. Returns the user object.")
  (authenticated-p ((user user)) "Returns T if the user has been authenticated successfully, otherwise NIL.")
  (show-login-page (&key target redirect) "Shows the login page or inserts it into the target and handles the authentication process. If redirect is provided, the user will be taken to that page afterwards.")
  (show-logout-page (&key target redirect) "Shows the logout page or inserts it into the target and handles the session termination. If redirect is provided, the user will be taken to that page afterwards.")
  (show-option-page (&key target) "Either displays a full options page or inserts all necessary things into the target if provided. Handles authentication registration."))

(defimpl (session)
    "Session instances track whether a given user is still logged in or not."
  (session-start ((user user)) "Creates a new session object for the given user or continues an existing one if applicable.")
  (session-user () "Returns the user associated with this session.")
  (session-end () "Finalizes the session object and in effect logs the user out."))

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
  (db-create ((collection string) &key indices)
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
  (model-field ((field string))
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

(defmacro with-model-fields ((&rest fields) model &rest body)
  "Stub for the with-model-fields macro. Should be redefined by the database implementation."
  (declare (ignore fields) (ignore model) (ignore body))
  (error "With-model-fields macro not implemented!"))

(defmacro query (&rest forms)
  "Stub for the query macro. Should be redefined by the database implementation."
  (declare (ignore forms))
  (error "Query macro not implemented!"))
