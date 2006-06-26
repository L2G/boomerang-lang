/**
 * GENEPI is the GENEric Presburger programming Interface.
 *
 * Copyright (C) 2006 Jerome Leroux (coordinator), Sebastien Bardin, 
 * Gerald Point and LaBRI, CNRS UMR 5800, Universite Bordeaux 1, ENSEIRB.
 *
 * GENEPI is free software; you can redistribute it and/or modify it under the 
 * terms of the GNU General Public License as published by the Free Software 
 * Foundation; either version 2, or (at your option) any later version.
 *
 * GENEPI  is distributed in the hope that it will be useful, but WITHOUT ANY 
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more 
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * GENEPI; see the file COPYING.  If not, write to the Free Software 
 * Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* $Id: genepi.h,v 1.1 2006/03/17 11:11:33 point Exp $ */
#ifndef __GENEPI_H__
# define __GENEPI_H__

# include <stdio.h>

#ifndef EXTERN
# ifdef __cplusplus 
#  define EXTERN extern "C"
# else
#  define EXTERN extern
# endif 
#endif

typedef struct genepi_set_st genepi_set;

# define GENEPI_SHOW_STATISTICS (0x01<<0)
# define GENEPI_SHOW_TIMERS     (0x01<<1)

			/* --------------- */

typedef struct {
  const char *name;
  void (*init)(void);
  void (*terminate)(int show_statistics);
  genepi_set *(*add_reference)(genepi_set *X);
  void (*del_reference)(genepi_set *X);
  genepi_set *(*linear_equality)(int *alpha, int alpha_size, int c);
  genepi_set *(*set_union)(genepi_set *X1, genepi_set *X2);
  genepi_set *(*set_intersection)(genepi_set *X1, genepi_set *X2);
  genepi_set *(*set_complement)(genepi_set *X);
  genepi_set *(*project)(genepi_set *X, int *selection, int size);
  genepi_set *(*invproject)(genepi_set *X, int *selection, int size);
  genepi_set *(*apply)(genepi_set *R, genepi_set *A);
  genepi_set *(*applyinv)(genepi_set *R, genepi_set *A);
  int (*is_empty)(genepi_set *X);
  int (*is_full)(genepi_set *X);
  int (*is_included_in)(genepi_set *X1, genepi_set *X2);
  int (*is_finite)(genepi_set *X);
  void (*get_solutions)(genepi_set *X, int ***psolutions, int *psize, int max);
  void (*display_all_solutions)(genepi_set *X, FILE *output);
  void (*display_data_structure)(genepi_set *X, FILE *output);
  int (*get_width)(genepi_set *X);
  int (*get_data_structure_size)(genepi_set *X);
  genepi_set *(*top)(int n);
  genepi_set *(*bot)(int n);
  int (*equal)(genepi_set *X1, genepi_set *X2);
} genepi_engine;

			/* --------------- */

EXTERN void
genepi_set_init(void);
EXTERN void
genepi_set_terminate(int flags);
EXTERN genepi_set *
genepi_set_add_reference(genepi_set *X);
EXTERN void
genepi_set_del_reference(genepi_set *X);
EXTERN genepi_set *
genepi_set_linear_equality(int *alpha, int alpha_size, int cst);
EXTERN genepi_set *
genepi_set_union(genepi_set *X1, genepi_set *X2);
EXTERN genepi_set *
genepi_set_intersection(genepi_set *X1, genepi_set *X2);
EXTERN genepi_set *
genepi_set_complement(genepi_set *X);
EXTERN genepi_set *
genepi_set_project(genepi_set *X, int *sel, int selsize);
EXTERN genepi_set *
genepi_set_invproject(genepi_set *X, int *sel, int selsize);
EXTERN genepi_set *
genepi_set_apply(genepi_set *R, genepi_set *A);
EXTERN genepi_set *
genepi_set_applyinv(genepi_set *R, genepi_set *A);
EXTERN int
genepi_set_is_empty(genepi_set *X);
EXTERN int 
genepi_set_is_full(genepi_set *X);
EXTERN int
genepi_set_is_included_in(genepi_set *X1, genepi_set *X2);
EXTERN int 
genepi_set_is_finite(genepi_set *X);
EXTERN void
genepi_set_get_solutions(genepi_set *X, int ***psolutionss, int *psize, int max);
EXTERN void
genepi_set_display_all_solutions(genepi_set *X, FILE *output);
EXTERN void
genepi_set_display_data_structure(genepi_set *X, FILE *output);
EXTERN int
genepi_set_get_width(genepi_set *X);
EXTERN int
genepi_set_get_data_structure_size(genepi_set *X);
EXTERN genepi_set *
genepi_set_top(int width);
EXTERN genepi_set *
genepi_set_bot(int width);
EXTERN int
genepi_set_equal(genepi_set *X1, genepi_set *X2);

			/* --------------- */

EXTERN int
genepi_autotest(void);

			/* --------------- */

EXTERN void
genepi_set_engine(const genepi_engine *engine);

EXTERN const genepi_engine *
genepi_get_engine(void);

#endif /* __GENEPI_H__ */
