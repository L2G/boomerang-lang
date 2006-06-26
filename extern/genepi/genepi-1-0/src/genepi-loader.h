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

/* $Id: genepi-loader.h,v 1.2 2006/03/17 13:57:01 point Exp $ */
#ifndef __GENEPI_LOADER_H__
# define __GENEPI_LOADER_H__

# include <genepi.h>

EXTERN void
genepi_loader_init(void);

EXTERN void
genepi_loader_terminate(void);

EXTERN void
genepi_loader_load_engine(const char *filename);

EXTERN int
genepi_loader_load_directory(const char *dirname);

EXTERN genepi_engine **
genepi_loader_get_engines(int *psize);


#endif /* __GENEPI_LOADER_H__ */
