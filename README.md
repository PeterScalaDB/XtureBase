Database specification
=======================

Classes
--------

Database records are seen as object instances. The structure of the database object is described in classes. A class describes what fields and methods an object can contain. Classes can inherit their characteristics from superclasses.
There is a simple version management for different versions of a class descriptions, to cover changes in the data structure during the livetime of a database.

Fields
--------

Database fields can not only contain simple values, but composite terms consisting of values, operators and variables, similar do a spreadsheet. Both term and result value are stored in the db. The terms can contain variables that refer to other database fields, even in other object instances of other classes.
That way the database can be used for complex calculations.

Structure
-----------

An Object can have fields that are a list of dependend sub-object. The super-object is seen as the owner of the sub-objects. When the super-object is deleted, also the sub-objects are deleted. When the super-object is cloned to another place, also als sub-objects are cloned, and so on.
That way database elements can be used to create a hierachical structure of data that represent the natural structure of real world objects. The main organizational structure of the database is that of a directory, comparable to the directory e.g. of a file system or the windows registry.

Indexing and storage
---------------------

In most cases, complex and costly queries are not necessary, because the index keys of all needed sub-objects are already known by the super-objects. For each class there is an index file, containing the index keys of all object instances of that class, in ascending order.
The index is searched by a simple binary-search algorithm.
The actual data is stored in a container file. Every change of a record is appended to the container file. All changes are logged in a transaction log file. Transactions can be retracted, so there is a global undo-feature in the database