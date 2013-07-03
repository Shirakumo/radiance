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

(defimpl user
    "Defines a very basic user class that allows tying arbitrary data to a user."
  (user-get (username) "Returns the user object of an existing user or creates a new hull instance.")
  (user-field (field &key value) "Set or get a user data field.")
  (user-save () "Save the user to the database.")
  (user-saved-p () "Returns T if the user is not a hull instance, otherwise NIL."))

(defimpl auth
    "Handles one or more methods for authentication of a user."
  (authenticate ((user user)) "Authenticate the given user using whatever method applicable. Additional arguments may be necessary or read from the user object. Returns NIL on failure, user on success.")
  (authenticated-p ((user user)) "Returns T if the user has been authenticated successfully, otherwise NIL."))

(defimpl session
    "Session instances track whether a given user is still logged in or not."
  (session-start ((user user)) "Creates a new session object for the given user or continues an existing one if applicable..")
  (session-end () "Finalizes the session object and in effect logs the user out."))

(defimpl database
    "Base database interface. Tries to be as abstract as possible while still providing all essential functionality."
  (db-connect (dbname &key (host (config-tree :database :host))
                      (port (config-tree :database :port))
                      (user (config-tree :database :user))
                      (pass (config-tree :database :pass)))
              "Connects to the database given the information in the arguments.")
  (db-disconnect () "Disconnects the database")
  (db-connected-p () "Returns T if the database is connected, otherwise NIL.")
  (db-collections () "Returns a list of all existing collections.")
  (db-create ((collection collection) &key indices)
             "Create a new collection with an optional list of indexed fields.")
  (db-select ((collection collection) query &key (skip 0) (limit 0) sort) 
             "Retrieve data from the collection. Query should be constructed with the query macro.")
  (db-iterate ((collection collection) query function &key (skip 0) (limit 0) sort) 
              "Iterate over data in the collection. Query should be constructed with the query macro. Might be faster than db-select.")
  (db-insert ((collection collection) data) 
             "Insert the data into the collection. Data is a list of alists.")
  (db-remove ((collection collection) query &key (skip 0) (limit 0)) 
             "Delete data from the collection. Query should be constructed with the query macro.")
  (db-update ((collection collection) query data &key (skip 0) (limit 0)) 
             "Update data in the collection. Query should be constructed with the query macro and data is a list of alists.")
  (db-apropos ((collection collection)) "Returns a list of all available fields and their type or NIL if any field is possible."))

(defmacro query (&rest forms)
  "Stub for the query macro. Should be redefined by the database implementation."
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
  (error "Query macro not implemented!"))
