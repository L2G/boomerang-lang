/**
 * FAST Enhanced Release, an accelerated symbolic model-checker. 
 * Copyright (C) 2005-2006 Jerome Leroux (coordinator), Sebastien Bardin, 
 * Gerald Point and LaBRI, CNRS UMR 5800, Universite Bordeaux 1, ENSEIRB.
 *
 * FAST is free software; you can redistribute it and/or modify it under the 
 * terms of the GNU General Public License as published by the Free Software 
 * Foundation; either version 2, or (at your option) any later version.
 *
 * FAST  is distributed in the hope that it will be useful, but WITHOUT ANY 
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more 
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 *
 * FAST; see the file COPYING.  If not, write to the Free Software Foundation, 
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */


/* $Id: prestaf2fast-cache.h,v 1.3 2006/03/17 13:41:41 point Exp $ */
#ifndef __PRESTAF2FAST_CACHE_H__
# define __PRESTAF2FAST_CACHE_H__

# include <sataf-msa.h>

CCL_EXTERN void
genepi_set_cache_init(int cache_size);
CCL_EXTERN void
genepi_set_cache_terminate(void);

			/* --------------- */

CCL_EXTERN sataf_msa
genepi_set_cache_get_linear_equality(const int alpha_size, const int *alpha, 
				   const int c);
CCL_EXTERN void
genepi_set_cache_put_linear_equality(const int alpha_size, const int *alpha, 
				   const int c, sataf_msa R);

			/* --------------- */

CCL_EXTERN sataf_msa
genepi_set_cache_get_project(const sataf_msa X, const int width, 
			   const int selsize, const int *sel);
CCL_EXTERN void
genepi_set_cache_put_project(const sataf_msa X, const int width, 
			   const int selsize, const int *sel, sataf_msa R);

			/* --------------- */

CCL_EXTERN sataf_msa
genepi_set_cache_get_invproject(const sataf_msa X, const int width, 
			      const int selsize, const int *sel);
CCL_EXTERN void
genepi_set_cache_put_invproject(const sataf_msa X, const int width, 
			      const int selsize, const int *sel, 
			      sataf_msa R);

			/* --------------- */

CCL_EXTERN sataf_msa
genepi_set_cache_get_apply(const sataf_msa Rel, const sataf_msa A);

CCL_EXTERN void
genepi_set_cache_put_apply(const sataf_msa Rel, const sataf_msa A, sataf_msa R);

			/* --------------- */

CCL_EXTERN sataf_msa
genepi_set_cache_get_applyinv(const sataf_msa Rel, const sataf_msa A);

CCL_EXTERN void
genepi_set_cache_put_applyinv(const sataf_msa Rel, const sataf_msa A, 
			    sataf_msa R);

			/* --------------- */

CCL_EXTERN void
genepi_set_cache_log_statistics(ccl_log_type log);

#endif /* __PRESTAF2FAST_CACHE_H__ */
