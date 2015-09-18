/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2000-2015, University of Amsterdam
			      VU University Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef DB4PL_H_INCLUDED
#define DB4PL_H_INCLUDED

#include <SWI-Prolog.h>
#include <db.h>

/* Consider anything >= DB4.3 as DB43 */
#if DB_VERSION_MAJOR >= 4
#if DB_VERSION_MAJOR > 4 || DB_VERSION_MINOR >= 3
#define DB43 1
#endif
#endif

/* Consider anything >= DB4.1 as DB41 */
#if DB_VERSION_MAJOR >= 4
#if DB_VERSION_MAJOR > 4 || DB_VERSION_MINOR >= 1
#define DB41 1
#endif
#endif

#define DBH_MAGIC 277484232		/* magic for validation */
#define DBH_ENVMAGIC 6560701		/* magic for validation */

typedef enum
{ D_TERM,				/* a Prolog term */
  D_ATOM,				/* an atom (length+cahsr) */
  D_CBLOB,				/* a C-blob (bytes) */
  D_CSTRING,				/* a C-string (0-terminated) */
  D_CLONG				/* a C-long */
} dtype;

typedef struct
{ DB_ENV       *env;			/* the database environment */

  atom_t	symbol;			/* <bdb_env>(...)  */
  int		magic;			/* DBH_MAGIC */
  u_int32_t	flags;			/* flags used to create the env */
  int		thread;			/* associated thread */
  char	       *home;			/* Directory */
} dbenvh;

typedef struct
{ DB	       *db;			/* the database */

  atom_t	symbol;			/* <bdb>(...)  */
  int		magic;			/* DBH_MAGIC */
  u_int32_t	flags;			/* flags used to open the database */
  dtype		key_type;		/* type of the key */
  dtype		value_type;		/* type of the data */
  dbenvh       *env;			/* associated environment */
} dbh;

#endif /*DB4PL_H_INCLUDED*/
