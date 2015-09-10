/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(test_db,
	  [ test_db/0
	  ]).

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module(library(db)).
:- use_module(library(plunit)).
:- use_module(library(lists)).

test_db :-
	run_tests([ db
		  ]).

data(var,	     _).
data(int,	     42).
data(bigint,	     343786473836435678).
data(atom,	     'aap').
data(atom_unicode,   'aa\u0410p').
data(atom_nul,	     'aa\u0000p').
data(string_unicode, "aa\u0410p").
data(string_nul,     "aa\u0000p").
data(list,	     [aap, noot, mies]).
data(compound,	     f(a)).
data(vars_shared,    f(A,A)).
data(vars_nshared,   f(_,_)).
data(dict,	     d{x:42, y:20}).

delete_existing_file(File) :-
	exists_file(File), !,
	delete_file(File).
delete_existing_file(_).

:- begin_tests(db).

test(loop, PairsOut =@= PairsIn) :-
	DBFile = 'test.db',
	delete_existing_file(DBFile),
	setof(Type-Data, data(Type, Data), PairsIn),
	db_open(DBFile, update, DB, []),
	forall(member(Type-Data, PairsIn),
	       db_put(DB, Type, Data)),
	setof(Type-Data, db_enum(DB, Type, Data), PairsOut),
	db_close(DB),
	delete_existing_file(DBFile).

:- end_tests(db).
