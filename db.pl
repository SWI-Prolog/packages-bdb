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

/** <module> Berkeley DB interface

This package realised external storage  of   Prolog  terms  based on the
Berkeley          DB          library           from          [Sleepycat
Software](http://www.sleepycat.com/). The DB library  implements modular
support for the bottom layers of a   database.  The database itself maps
unconstrained keys onto values. Both key and value are binary blobs.

The SWI-Prolog interface for  DB  allows   for  fast  storage of general
Prolog terms in the database.
*/

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

%%	db_put(+DB, +Key, +Value) is semidet.
%
%	Add a new key-value pair to the database. If the database allows
%	for duplicates this will always succeed,   unless a system error
%	occurs.

%%	db_del(+DB, ?Key, ?Value).
%
%	Delete the first matching key-value pair   from the database. If
%	the  database  allows  for   duplicates,    this   predicate  is
%	non-deterministic. The enumeration performed   by this predicate
%	is the same as for db_get/3. See also db_delall/3.

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

%%	db_get(+DB, ?Key, -Value)
%
%	Query the database. If the database   allows for duplicates this
%	predicate is non-deterministic. Note that if  Key is a term this
%	matches stored keys that are _variants_ of Key. See =@=/2.

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

		 /*******************************
		 *	       MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(error(package(db, Code), context(_, Message))) -->
	[ 'DB: Error ~w: ~w'-[Code, Message] ].
