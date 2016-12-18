/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(test_bdb,
          [ test_bdb/0
          ]).

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module(library(bdb)).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(lists)).

test_bdb :-
    run_tests([ bdb
              ]).

data(var,            _).
data(int,            42).
data(bigint,         343786473836435678).
data(atom,           'aap').
data(atom_unicode,   'aa\u0410p').
data(atom_nul,       'aa\u0000p').
data(string_unicode, "aa\u0410p").
data(string_nul,     "aa\u0000p").
data(list,           [aap, noot, mies]).
data(compound,       f(a)).
data(vars_shared,    f(A,A)).
data(vars_nshared,   f(_,_)).
data(term_unicode,   'aa\u0410p'(1)).
data(cycle,          X) :- X = f(X).
data(dict,           d{x:42, y:20}).

delete_existing_file(File) :-
    exists_file(File),
    !,
    delete_file(File).
delete_existing_file(_).

:- begin_tests(bdb).

test(loop, PairsOut =@= PairsIn) :-
    DBFile = 'test.db',
    delete_existing_file(DBFile),
    setof(Type-Data, data(Type, Data), PairsIn),
    bdb_open(DBFile, update, DB, []),
    forall(member(Type-Data, PairsIn),
           bdb_put(DB, Type, Data)),
    setof(Type-Data, bdb_enum(DB, Type, Data), PairsOut),
    bdb_close(DB),
    delete_existing_file(DBFile).
test(no_duplicates, Mies == mies) :-
    DBFile = 'test.db',
    delete_existing_file(DBFile),
    bdb_open(DBFile, update, DB, [duplicates(false)]),
    bdb_put(DB, aap, noot),
    bdb_put(DB, aap, mies),
    bdb_get(DB, aap, Mies),
    bdb_close(DB),
    delete_existing_file(DBFile).
test(duplicates, Out == [1,2,3,4,5,6,7,8,9,10]) :-
    DBFile = 'test.db',
    delete_existing_file(DBFile),
    bdb_open(DBFile, update, DB, [duplicates(true)]),
    forall(between(1, 10, X),
           forall(between(1, 10, Y),
                  bdb_put(DB, X, Y))),
    bdb_getall(DB, 5, Out),
    bdb_close(DB),
    delete_existing_file(DBFile).

:- end_tests(bdb).
