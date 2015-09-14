/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2000-2015, University of Amsterdam
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(db,
	  [ db_open/4,			% +File, +Mode, -Handle, +Options
	    db_close/1,			% +Handle
	    db_closeall/0,		%
	    db_current/1,		% -DB
	    db_put/3,			% +DB, +Key, +Value
	    db_del/3,			% +DB, +Key, ?Value
	    db_delall/3,		% +DB, +Key, +Value
	    db_enum/3,			% +DB, -Key, -Value
	    db_get/3,			% +DB, +Key, -Value
	    db_getall/3,		% +DB, +Key, -ValueList
	    db_init/1,			% +Options
	    db_transaction/1,		% :Goal
	    db_atom/3			% +DB, ?Atom, ?Id
	  ]).
:- use_foreign_library(foreign(db4pl)).
:- meta_predicate
	db_transaction(0).

/** <module> Berkeley DB interface

This  package  realises  a  binding  to  _Berkeley  DB_,  originally  by
[Sleepycat   Software](http://www.sleepycat.com/),   now    managed   by
[Oracle](http://www.oracle.com/technetwork/database/database-technologies/berkeleydb/downloads/index.html).
The DB library implements modular support  for   the  bottom layers of a
database. In can be configured  for   single-threaded  access to a file,
multi-threaded access with  transactions,  remote   access  as  well  as
database replication.

Berkeley DB is an _embedded_ database. This implies the library provides
access to a file containing one or more database tables. The Berkeley DB
database tables are always _binary_, mapping a   _key_ to a _value_. The
SWI-Prolog interface to Berkeley DB allows for fast storage of arbitrary
Prolog terms including cycles and constraints in the database.

Accessing a database consists of four steps:

    1. Initialise the DB environment using db_init/1. This step is
       optional, providing simple non-transactional file access when
       omitted.
    2. Open a database using db_open/4, returning a handle to the
       database.
    3. Accessing the data using db_put/3, db_get/3, etc.
    4. Closing a database using db_close/1. When omitted, all open
       databases are closed on program halt (see at_halt/1).
*/

%%	db_init(+Options) is det.
%
%	Initialise the default  DB  _environment_.   This  must  be done
%	before the first call  to  db_open/4   and  at  maximum once. If
%	db_open/4  is  called   without    calling   db_init/1,  default
%	initialisation is used, which is suitable for using a plain file
%	as a database that is  accessed   from  a  single Prolog thread.
%	Options is a list of options.   The  currently supported options
%	are listed below. The name of   the  boolean options are derived
%	from the DB  flags  by  dropping   the  =DB_=  prefix  and using
%	lowercase, e.g. =DB_INIT_LOCK= becomes `init_lock`. For details,
%	please refer to the DB manual.
%
%	  - create(+Bool)
%	    If `true`, create any underlying file as required. By
%	    default, no new files are created. This option should be
%	    set for prograns that create new databases.
%	  - failchk(+Bool)
%	  - home(+Home)
%	    Specify the DB home directory, the directory holding the
%	    database files.
%	  - init_lock(+Bool)
%	    Enable locking (=DB_INIT_LOCK=).  Implied if transactions
%	    are used.
%	  - init_log(+Bool)
%	    Enable logging the DB modifications (=DB_INIT_LOG=). Logging
%	    enables recovery of databases in case of system failure.
%	    Normally it is used in combination with transactions.
%	  - init_mpool(+Bool)
%	    Initialize memory pool.  Impicit if mp_size(+Size) or
%	    mp_mmapsize(+Size) is specified.
%	  - init_rep(+Bool)
%	    Init database replication.	The rest of the replication
%	    logic is not yet supported.
%	  - init_txn(+Bool)
%	    Init transactions.  Implies init_log(true).
%	  - lockdown(+Bool)
%	  - mp_size(+Integer)
%	  - mp_mmapsize(+Integer)
%	    Control memory pool handling (=DB_INIT_MPOOL=). The
%	    `mp_size` option sets the memory-pool used for
%	    caching, while the `mp_mmapsize` controls the maximum size
%	    of a DB file mapped entirely into memory.
%	  - private(+Bool)
%	  - recover(+Bool)
%	    Perform recovery before opening the database.
%	  - recover_fatal(+Bool)
%	    Perform fatal recovery before opening the database.
%	  - register(+Bool)
%	  - server(+Host, [+ServerOptions])
%	    Initialise the DB package for accessing a remote
%	    database. Host specifies the name of the machine running
%	    `berkeley_db_svc`. Optionally additional options may be
%	    specified:
%	    - server_timeout(+Seconds)
%	      Specify the timeout time the server uses to determine
%	      that the client has gone. This implies the server will
%	      terminate the connection to this client if this client
%	      does not issue any requests for the indicated time.
%	    - client_timeout(+Seconds)
%	      Specify the time the client waits for the server to
%	      handle a request.
%	  - system_mem(+Bool)
%	  - transactions(+Bool)
%	    Enable transactions, providing atomicy of changes and
%	    security. Implies logging and locking. See
%	    db_transaction/1.
%	  - thread(+Bool)
%	    Make the environment accessible from multiple threads.
%	  - use_environ(+Bool)
%	  - use_environ_root(+Bool)
%	  - config(+ListOfConfig)
%	    Specify a list of configuration options, each option is of
%	    the form Name(Value).  Currently unused.

%%	db_open(+File, +Mode, -DB, +Options) is det.
%
%	Open File holding a database. Mode   is one of `read`, providing
%	read-only  access  or  `update`,  providing  read/write  access.
%	Options is a list of options. Supported options are:
%
%	  - duplicates(+Boolean)
%	    Do/do not allow for duplicate values on the same key.
%	    Default is not to allow for duplicates.
%	  - database(+Name)
%	    If File contains multiple databases, address the named
%	    database in the file. A DB file can only consist of multiple
%	    databases if the db_open/4 call that created it specified
%	    this argument. Each database in the file has its own
%	    characteristics.
%	  - key(+Type)
%	  - value(+Type)
%	    Specify the type of the key or value. Allowed values are:
%	    - term
%	      Key/Value is a Prolog term (default). This type allows for
%	      representing arbitrary Prolog data in both keys and value.
%	      The representation is space-efficient, but Prolog
%	      specific. See PL_record_external() in the SWI-Prolog
%	      Reference Manual for details on the representation. The
%	      other representations are more neutral. This implies they
%	      are more stable and sharing the DB with other languages is
%	      feasible.
%	    - atom
%	      Key/Value is an atom. The text is represented as a
%	      UTF-8 string and its length.
%	    - c_blob
%	      Key/Value is a blob (sequence of bytes).  On output,
%	      a Prolog string is used.  The input is either a Prolog
%	      string or an atom holding only characters in the range
%	      [0..255].
%	    - c_string
%	      Key/Value is an atom. The text is represented as a C
%	      0-terminated UTF-8 string.
%	    - c_long
%	      Key/Value is an integer. The value is represented as a
%	      native C long in machine byte-order.
%
%	@arg DB is unified with a _blob_ of type `db`. Database handles
%	are subject to atom garbage collection.

%%	db_close(+DB) is det.
%
%	Close BerkeleyDB database indicated by DB. DB becomes invalid
%	after this operation.  An attempt to access a closed database
%	is detected reliably and results in a permission_error
%	exception.

%%	db_put(+DB, +Key, +Value) is det.
%
%	Add a new key-value pair to the   database. If the database does
%	not allow for duplicates the   possible previous associated with
%	Key is replaced by Value.

%%	db_del(+DB, ?Key, ?Value) is nondet.
%
%	Delete the first matching key-value pair   from the database. If
%	the  database  allows  for   duplicates,    this   predicate  is
%	non-deterministic, otherwise it is   _semidet_.  The enumeration
%	performed by this predicate is  the   same  as for db_get/3. See
%	also db_delall/3.

%%	db_delall(+DB, +Key, ?Value) is det.
%
%	Delete all matching key-value  pairs   from  the  database. With
%	unbound Value the key and all values are removed efficiently.

db_delall(DB, Key, Value) :-
	var(Value), !,
	db_del(DB, Key).		% this is much faster
db_delall(DB, Key, Value) :-
	(   db_del(DB, Key, Value),
	    fail
	;   true
	).

%%	db_get(+DB, ?Key, -Value) is nondet.
%
%	Query the database. If the database   allows for duplicates this
%	predicate is non-deterministic, otherwise it  is _semidet_. Note
%	that if Key is  a  term  this   matches  stored  keys  that  are
%	_variants_ of Key, *not*  unification.   See  =@=/2. Thus, after
%	db_put(DB, f(X), 42), we get the following query results:
%
%	  - db_get(DB, f(Y), V) binds Value to `42`, while `Y` is left
%	    unbound.
%	  - db_get(DB, f(a), V) _fails_.
%	  - db_enum(DB, f(a), V) succeeds, but does not perform any
%	    indexing, i.e., it enumerates all key-value pairs and
%	    performs the unification.

%%	db_enum(+DB, -Key, -Value)
%
%	Enumerate the whole database, unifying   the  key-value pairs to
%	Key and Value.  Though  this  predicate   can  be  used  with an
%	instantiated Key to enumerate only the   keys unifying with Key,
%	no indexing is used by db_enum/3.

%%	db_getall(+DB, +Key, -Values) is semidet.
%
%	Get all values associated with Key. Fails   if  the key does not
%	exist (as bagof/3).

%%	db_current(?DB) is nondet.
%
%	True when DB is a handle to a currently open database.

db_current(DB) :-
	current_blob(DB, db),
	db_is_open(DB).

%%	db_closeall is det.
%
%	Close all currently open databases. This is called automatically
%	after  loading  this  library  on  process  terminatation  using
%	at_halt/1.

db_closeall :-
	forall(db_current(DB),
	       catch(db_close(DB),
		     E,
		     print_message(warning, E))).

terminate_db :-
	(   current_predicate(db_exit/0)	% library was loaded ok
	->  db_closeall,
	    catch(db_exit, E, print_message(warning, E))
	;   true
	).

:- at_halt(terminate_db).

%%	db_transaction(:Goal)
%
%	Start a transaction, execute Goal and terminate the transaction.
%	Only if Goal succeeds, the  transaction   is  commited.  If Goal
%	fails or raises an exception,  the   transaction  is aborted and
%	db_transaction/1 either fails or  rethrows   the  exception.  Of
%	special interest is the exception
%
%	  ==
%	  error(package(db, deadlock), _)
%	  ==
%
%	This exception indicates a deadlock was raised  by one of the DB
%	predicates. Deadlocks may arise if multiple processes or threads
%	access  the  same  keys   in   a    different   order.   The  DB
%	infra-structure causes one of  the   processes  involved  in the
%	deadlock to abort its transaction. This   process  may choose to
%	restart the transaction.
%
%	For example, a DB application  may   define  `{Goal}` to realise
%	transactions and restart these automatically   is  a deadlock is
%	raised:
%
%	  ==
%	  {Goal} :-
%	      catch(db_transaction(Goal), E, true),
%	      (   var(E)
%	      ->  true
%	      ;   E = error(package(db, deadlock), _)
%	      ->  {Goal}
%	      ;   throw(E)
%	      ).
%	  ==

		 /*******************************
		 *	       MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(error(package(db, Code), context(_, Message))) -->
	[ 'DB: Error ~w: ~w'-[Code, Message] ].
