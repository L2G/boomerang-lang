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
#include "prestaf2fast-cache.h"

typedef struct {
  int requests;
  int collisions;
  int success;
  int number_of_entries;
  int size;
  ccl_delete_proc del;
  void *records[1];
} cache;

			/* --------------- */

typedef struct linear_equality_st {
  sataf_msa R;
  int alpha_size;
  int c;
  int alpha[1];
} linear_equality_record;

typedef struct projection_st {
  sataf_msa R;
  sataf_msa X;
  int width;
  int selsize;
  int sel[1];
} project_record;

			/* --------------- */

typedef struct apply_record_st {
  sataf_msa R;
  sataf_msa Rel;
  sataf_msa A;
} apply_record;

			/* --------------- */

static cache *CACHES[5];

			/* --------------- */
#define NB_CACHES (sizeof(CACHES)/sizeof(CACHES[0]))
#define LINEAR_EQUALITY_CACHE (CACHES[0])
#define PROJECT_CACHE (CACHES[1])
#define INVPROJECT_CACHE (CACHES[2])
#define APPLY_CACHE (CACHES[3])
#define APPLYINV_CACHE (CACHES[4])

			/* --------------- */

static const int PRIME_TABLE[] = {
   1, 3, 7, 17, 37, 79, 163, 331, 673, 1361,
   2729, 5471, 10949, 21911, 43853,  87719, 175447, 350899, 701819, 1403641,
   2807303, 5614657, 11229331
};

# define PRIME_TABLE_SIZE (sizeof(PRIME_TABLE)/PRIME_TABLE[0])

static cache *
s_new_cache(int size, ccl_delete_proc del)
{
  cache *res = ccl_calloc(sizeof(cache)+sizeof(void*)*size,1);

  res->requests = 0;
  res->collisions = 0;
  res->success = 0;
  res->number_of_entries = 0;
  res->size = size;
  res->del = del;

  return res;
}

			/* --------------- */

static void
s_delete_leq(void *leq)
{
  sataf_msa_del_reference(((linear_equality_record *)leq)->R);
  ccl_delete(leq);
}

			/* --------------- */

static void
s_delete_project(void *p)
{
  sataf_msa_del_reference(((project_record *)p)->X);
  sataf_msa_del_reference(((project_record *)p)->R);
  ccl_delete(p);
}

			/* --------------- */

static void
s_delete_apply(void *p)
{
  sataf_msa_del_reference(((apply_record *)p)->Rel);
  sataf_msa_del_reference(((apply_record *)p)->A);
  sataf_msa_del_reference(((apply_record *)p)->R);
  ccl_delete(p);
}

			/* --------------- */

void
genepi_set_cache_init(int cache_size)
{
  LINEAR_EQUALITY_CACHE = s_new_cache(cache_size,s_delete_leq);
  PROJECT_CACHE = s_new_cache(cache_size,s_delete_project);
  INVPROJECT_CACHE = s_new_cache(cache_size,s_delete_project);
  APPLY_CACHE = s_new_cache(cache_size,s_delete_apply);
  APPLYINV_CACHE = s_new_cache(cache_size,s_delete_apply);
}

			/* --------------- */

void
genepi_set_cache_terminate(void)
{
  int i, j;

  for(i = 0; i < NB_CACHES; i++)
    {
      for(j = 0; j < CACHES[i]->size; j++)
	{
	  if( CACHES[i]->records[j] )
	    CACHES[i]->del(CACHES[i]->records[j]);
	}
      ccl_delete(CACHES[i]);
    }
}

			/* --------------- */

static uint32_t
s_linear_equality_index(const int alpha_size, const int *alpha, const int c)
{
  int i = alpha_size;
  uint32_t result = 175447*alpha_size+10949*c;

  while( i-- )
    result = 175447*result+PRIME_TABLE[i%PRIME_TABLE_SIZE]**(alpha++);

  result = result % LINEAR_EQUALITY_CACHE->size;

  return result;
}

			/* --------------- */

static int
s_equal_linear_equality(const int alpha_size, const int *alpha, 
			const int c, const linear_equality_record *rec)
{
  if( alpha_size != rec->alpha_size ) return 0;
  if( c != rec->c ) return 0;

  return memcmp(alpha,rec->alpha,alpha_size*sizeof(int)) == 0;
}

			/* --------------- */

sataf_msa
genepi_set_cache_get_linear_equality(const int alpha_size, const int *alpha, 
				   const int c)
{
  uint32_t index = s_linear_equality_index(alpha_size,alpha,c);
  linear_equality_record *rec = LINEAR_EQUALITY_CACHE->records[index];

   LINEAR_EQUALITY_CACHE->requests++;
  if( rec != NULL && s_equal_linear_equality(alpha_size,alpha,c,rec) )
    {
      LINEAR_EQUALITY_CACHE->success++;
      return sataf_msa_add_reference(rec->R);
    }

  return NULL;
}

			/* --------------- */

void
genepi_set_cache_put_linear_equality(const int alpha_size, const int *alpha, 
				   const int c, sataf_msa R)
{
  uint32_t index = s_linear_equality_index(alpha_size,alpha,c);
  linear_equality_record *rec = LINEAR_EQUALITY_CACHE->records[index];

  if( rec == NULL )
    {
      LINEAR_EQUALITY_CACHE->number_of_entries++;
      rec = ccl_malloc(sizeof(linear_equality_record)+
		       (alpha_size-1)*sizeof(int));
      LINEAR_EQUALITY_CACHE->records[index] = rec;
    }
  else
    {
      LINEAR_EQUALITY_CACHE->collisions++;
      sataf_msa_del_reference(rec->R);
      if( rec->alpha_size != alpha_size )
	{
	  rec = ccl_realloc(rec,sizeof(linear_equality_record)+
			    (alpha_size-1)*sizeof(int));
	  LINEAR_EQUALITY_CACHE->records[index] = rec;
	}
    }

  ccl_assert( alpha_size >= 1 );

  rec->alpha_size = alpha_size;
  rec->c = c;
  rec->R = sataf_msa_add_reference(R);
  memmove(rec->alpha,alpha,sizeof(int)*alpha_size);
}

			/* --------------- */

static uint32_t 
s_project_index(const sataf_msa X, int width, const int selsize, 
		const int *sel, const int table_size)
{
  int i = selsize;
  uint32_t result = 175447*(uint32_t)X+10949*width+2807303*selsize;

  while( i-- )
    result = 21911*result+PRIME_TABLE[i%PRIME_TABLE_SIZE]**(sel++);

  result = result % table_size;

  return result;
}

			/* --------------- */

static int 
s_equal_project(const sataf_msa X, const int width, const int selsize, 
		const int *sel, const project_record *rec)
{
  if( X != rec->X ) return 0;
  if( width != rec->width ) return 0;
  if( selsize != rec->selsize ) return 0;
  
  return memcmp(sel,rec->sel,selsize*sizeof(int)) == 0;
}
			/* --------------- */

static sataf_msa
s_get_project_invproject(cache *cache, const sataf_msa X, const int width, 
			 const int selsize, const int *sel)
{
  uint32_t index = s_project_index(X,width,selsize,sel,cache->size);
  const project_record *rec = cache->records[index];

  cache->requests++;
  if( rec != NULL && s_equal_project(X,width,selsize,sel,rec) )
    {
      cache->success++;
      return sataf_msa_add_reference(rec->R);
    }
  return NULL;
 
}

			/* --------------- */

static void
s_put_project_invproject(cache *cache, const sataf_msa X, const int width, 
			 const int selsize, const int *sel, 
			 sataf_msa R)
{
  uint32_t index = s_project_index(X,width,selsize,sel,cache->size);
  project_record *rec = cache->records[index];

  if( rec == NULL )
    {
      cache->number_of_entries++;
      rec = ccl_malloc(sizeof(project_record)+(selsize-1)*sizeof(int));
      cache->records[index] = rec;
    }
  else
    {
      cache->collisions++;
      sataf_msa_del_reference(rec->R);
      sataf_msa_del_reference(rec->X);
      if( rec->selsize != selsize )
	{
	  rec = ccl_realloc(rec,sizeof(project_record)+
			    (selsize-1)*sizeof(int));
	  cache->records[index] = rec;
	}
    }

  ccl_assert( selsize >= 1 );

  rec->X = sataf_msa_add_reference(X);
  rec->R = sataf_msa_add_reference(R);
  rec->width = width;
  rec->selsize = selsize;
  memmove(rec->sel,sel,sizeof(int)*selsize);
}

			/* --------------- */

sataf_msa
genepi_set_cache_get_project(const sataf_msa X, const int width, 
			   const int selsize, const int *sel)
{
  return s_get_project_invproject(PROJECT_CACHE,X,width,selsize,sel);
}

			/* --------------- */

void
genepi_set_cache_put_project(const sataf_msa X, const int width, 
			   const int selsize, const int *sel, 
			   sataf_msa R)
{
  s_put_project_invproject(PROJECT_CACHE,X,width,selsize,sel,R);
}

			/* --------------- */

sataf_msa
genepi_set_cache_get_invproject(const sataf_msa X, const int width, 
			      const int selsize, const int *sel)
{
  return s_get_project_invproject(INVPROJECT_CACHE,X,width,selsize,sel);
}

			/* --------------- */

void
genepi_set_cache_put_invproject(const sataf_msa X, const int width, 
			      const int selsize, const int *sel, 
			      sataf_msa R)
{
  s_put_project_invproject(INVPROJECT_CACHE,X,width,selsize,sel,R);
}

			/* --------------- */

static uint32_t
s_apply_index(const sataf_msa Rel, const sataf_msa A)
{
  return (175447*(uint32_t)Rel+10949*(uint32_t)A)% APPLY_CACHE->size;
}

			/* --------------- */

static int
s_equal_apply(const sataf_msa Rel, const sataf_msa A, const apply_record *rec)
{
  return rec->Rel == Rel && rec->A == A;
}

			/* --------------- */

static sataf_msa
s_cache_get_apply(cache *C, const sataf_msa Rel, const sataf_msa A)
{
  uint32_t index = s_apply_index(Rel,A);
  apply_record *rec = C->records[index];

  C->requests++;
  if( rec != NULL && s_equal_apply(Rel,A,rec) )
    {
      C->success++;
      return sataf_msa_add_reference(rec->R);
    }

  return NULL;
}

			/* --------------- */

static void
s_cache_put_apply(cache *C, const sataf_msa Rel, const sataf_msa A, 
		  sataf_msa R)
{
  uint32_t index = s_apply_index(R,A);
  apply_record *rec = C->records[index];

  if( rec == NULL )
    {
      C->number_of_entries++;
      rec = ccl_malloc(sizeof(apply_record));
      C->records[index] = rec;
    }
  else
    {
      C->collisions++;
      sataf_msa_del_reference(rec->R);
      sataf_msa_del_reference(rec->A);
      sataf_msa_del_reference(rec->Rel);
    }

  rec->Rel = sataf_msa_add_reference(Rel);
  rec->A = sataf_msa_add_reference(A);
  rec->R = sataf_msa_add_reference(R);
}

			/* --------------- */

sataf_msa
genepi_set_cache_get_apply(const sataf_msa Rel, const sataf_msa A)
{
  return s_cache_get_apply(APPLY_CACHE,Rel,A);
}

			/* --------------- */

void
genepi_set_cache_put_apply(const sataf_msa Rel, const sataf_msa A, sataf_msa R)
{
  s_cache_put_apply(APPLY_CACHE,Rel,A,R);
}

			/* --------------- */

sataf_msa
genepi_set_cache_get_applyinv(const sataf_msa Rel, const sataf_msa A)
{
  return s_cache_get_apply(APPLYINV_CACHE,Rel,A);
}

			/* --------------- */

void
genepi_set_cache_put_applyinv(const sataf_msa Rel, const sataf_msa A, 
			    sataf_msa R)
{
  s_cache_put_apply(APPLYINV_CACHE,Rel,A,R);
}

			/* --------------- */

static void
s_log_cache_stats(const char *fmt, const char *name, 
		  ccl_log_type log, cache *c)
{
  ccl_log(log,fmt,
	  name,
	  c->size,
	  /*c->number_of_entries, */
	  100.0*(float)c->number_of_entries/(float)c->size,
	  c->requests,
	  /*c->success, */
	  100.0*(float)c->success/(float)c->requests,
	  c->collisions);
}

			/* --------------- */

void
genepi_set_cache_log_statistics(ccl_log_type log)
{	  
  const char *title_fmt = "%18s %10s %7s %8s %7s %10s\n";
  const char  *line_fmt = "%18s %10d %6.2f%% %8d %6.2f%% %10d\n";


  ccl_log(log,"-----------------------------------------------------------------------------\n");
  ccl_log(log,title_fmt,
	  "cache name",
	  "size",
	  "entries",
	  "requests",
	  "success",
	  "collisions");
  ccl_log(log,"-----------------------------------------------------------------------------\n");
  s_log_cache_stats(line_fmt,"Linear equalities",log,LINEAR_EQUALITY_CACHE);
  s_log_cache_stats(line_fmt,"Projections",log,PROJECT_CACHE);
  s_log_cache_stats(line_fmt,"InvProjections",log,INVPROJECT_CACHE);
  s_log_cache_stats(line_fmt,"Apply",log,APPLY_CACHE);
  s_log_cache_stats(line_fmt,"ApplyInv",log,APPLYINV_CACHE);
  ccl_log(log,"-----------------------------------------------------------------------------\n");
}
