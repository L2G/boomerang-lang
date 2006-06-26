/* $Id: quantifier-automaton.c,v 1.5 2006/02/08 07:47:40 point Exp $ */
#include <ccl-list.h>
# include <shared-automaton.h>
#include "quantifier-automaton.h"

			/* --------------- */

#undef PRE_COMPUTE_HVAL 
#define USE_MSA


#ifdef USE_MSA
# define AUTO sataf_msa
# define SUCC(a,l,r) do { (r) = sataf_msa_succ(a,l); } while(0)
# define IS_ZERO(a) sataf_msa_is_zero(a)
# define IS_ONE(a) sataf_msa_is_one(a)
# define CLEAR(a) sataf_msa_del_reference(a)
# define IS_EQUAL(a1,a2) ((a1) == (a2))
# define IS_LT(a1,a2) ((a1) < (a2))
# define IS_FINAL(a) sataf_msa_is_final((a))
# define HASH(a) ((uint32_t)(a))
#else
typedef struct marked_automaton_st {
  sataf_sa sa;
  uint32_t initial;  
} marked_automaton;

# define AUTO marked_automaton
# define SUCC(m,l,r)							\
do {									\
  uint32_t succ = exit_automaton_successor((m).sa->automaton,(m).initial,(l));\
  uint32_t index = exit_automaton_decode_succ_state(succ);		\
									\
  if( exit_automaton_is_local_state(succ) )				\
    {									\
      (r).sa = shared_automaton_add_reference((m).sa);			\
      (r).initial = index;						\
    }									\
  else									\
    {									\
      (r).sa = shared_automaton_add_reference((m).sa->bind[index]->A);	\
      (r).initial = (m).sa->bind[index]->initial;			\
    }									\
} while(0)

# define IS_ZERO(m) shared_automaton_is_zero((m).sa)
# define IS_ONE(m)  shared_automaton_is_one((m).sa)
# define CLEAR(m) shared_automaton_del_reference((m).sa)
# define IS_EQUAL(a1,a2) ((a1).sa == (a2).sa && (a1).initial == (a2).initial)
# define IS_LT(a1,a2) (((a1).sa < (a2).sa) || \
		       (((a1).sa == (a2).sa) && (a1).initial < (a2).initial))
# define IS_FINAL(a) ((a).sa->automaton->is_final[(a).initial])
# define HASH(a) (((19*(uint32_t)((a).sa))<<3)+13*((a).initial))
#endif

typedef struct qaut_st {
  struct sataf_automaton_st super;
  uint16_t i;
  uint16_t n;
#ifdef PRE_COMPUTE_HVAL
  uint32_t hval_nb_autos; /* nb_autos == first byte */
#else
  uint8_t nb_autos;
#endif
  AUTO *autos;
} *qaut;

			/* --------------- */

static int 
s_cmp_msa(const void *p1, const void *p2);

static void
s_qaut_destroy(sataf_automaton self);

static uint32_t
s_qaut_get_alphabet_size(sataf_automaton self);

#define s_qaut_to_string NULL

static sataf_automaton
s_qaut_succ_exists(sataf_automaton self, uint32_t letter);

static sataf_automaton
s_qaut_succ_forall(sataf_automaton self, uint32_t letter);

static int
s_qaut_is_final_forall(sataf_automaton self);

static int
s_qaut_is_final_exists(sataf_automaton self);

static int
s_qaut_is_equal_to(sataf_automaton self, sataf_automaton other);

static uint32_t
s_qaut_actual_hashcode(qaut a);

static uint32_t
s_qaut_hashcode(sataf_automaton self);

# define s_qaut_to_dot NULL

			/* --------------- */

static const struct sataf_automaton_methods_st QAUT_FORALL_METHODS = {
  "SA-QAUT-FA",
  sizeof(struct qaut_st),
  s_qaut_destroy,
  s_qaut_get_alphabet_size,
  s_qaut_to_string,
  s_qaut_succ_forall,
  s_qaut_is_final_forall,
  s_qaut_is_equal_to,
  s_qaut_hashcode,
  s_qaut_to_dot
};

			/* --------------- */

static const struct sataf_automaton_methods_st QAUT_EXISTS_METHODS = {
  "SA-QAUT-EX",
  sizeof(struct qaut_st),
  s_qaut_destroy,
  s_qaut_get_alphabet_size,
  s_qaut_to_string,
  s_qaut_succ_exists,
  s_qaut_is_final_exists,
  s_qaut_is_equal_to,
  s_qaut_hashcode,
  s_qaut_to_dot
};

			/* --------------- */

static qaut
s_create_qaut(uint32_t i, uint32_t n, AUTO *autos, int nb_autos,
	      sataf_automaton_methods methods);
	
static sataf_automaton
s_create_quantifier(uint32_t i, uint32_t n, int nb_autos, AUTO *autos, 
		    sataf_automaton_methods methods);

			/* --------------- */

sataf_automaton
quantifier_automaton_create(int forall, uint32_t i, uint32_t n, sataf_msa fset)
{
  sataf_automaton    R;

  
  if( sataf_msa_is_zero(fset) )
    R = sataf_automaton_create_zero(2);
  else if( sataf_msa_is_one(fset) )
    R = sataf_automaton_create_one(2);
  else if( n < 2 )
    {
      if( forall )
	R = sataf_automaton_create_zero(2);
      else
	R = sataf_automaton_create_one(2);
    }
  else
    {
      AUTO *autos = ccl_new_array(AUTO,1);

      sataf_automaton_methods methods = forall
	? &QAUT_FORALL_METHODS
	: &QAUT_EXISTS_METHODS;
#ifdef USE_MSA
      *autos = sataf_msa_add_reference(fset);
#else
      autos[0].sa = shared_automaton_add_reference(fset->A);
      autos[0].initial = fset->initial;
#endif
      R = s_create_quantifier(i,n,1,autos,methods);
    }

  return R;
}

			/* --------------- */

static sataf_automaton
s_create_quantifier(uint32_t i, uint32_t n, int nb_autos, AUTO *autos, 
		    sataf_automaton_methods methods)
{
  sataf_automaton R;
  int forall = (methods == &QAUT_FORALL_METHODS);
  ccl_pre( n > 1 );

  if( nb_autos == 0 )
    {
      ccl_zdelete(ccl_delete,autos);
      if( forall )
	R = sataf_automaton_create_zero(2);
      else
	R = sataf_automaton_create_one(2);
    }
  else if( i == 0 )
    {
      int a, j, k, l = 0;
      int len = nb_autos<<1;
      AUTO tmp, msa, *new_array = ccl_new_array(AUTO,len);

      for(j = 0; j < nb_autos; j++)
	{
	  for(a = 0; a < 2; a++)
	    {
	      SUCC(autos[j],a,msa);
	      
	      if( (IS_ZERO(msa) && forall ) || (IS_ONE(msa) && ! forall) )
		{
		  CLEAR(msa);
		  for(; j < nb_autos; j++)
		    CLEAR(autos[j]);
		  ccl_delete(autos);
		  while( l-- )
		    CLEAR(new_array[l]);
		  ccl_delete(new_array);
		  if( forall )
		    return sataf_automaton_create_zero(2);
		  else
		    return sataf_automaton_create_one(2);
		} 

	      for(k = 0; k < l; k++)
		{
		  if( IS_LT(msa,new_array[k]) )
		    {
		      tmp = new_array[k];
		      new_array[k] = msa;
		      msa = tmp;
		    }
		  else if( IS_EQUAL(msa,new_array[k]) )
		    {
		      CLEAR(msa);
		      break;
		    }
		}
		  
	      if( k == l )
		new_array[l++] = msa;
	    }
	  CLEAR(autos[j]);
	}
      ccl_delete(autos);

      ccl_assert( l <= len );
      R = s_create_quantifier(n-1,n,l,new_array,methods);
    }
  else 
    {      
      R = (sataf_automaton)s_create_qaut(i,n,autos,nb_autos,methods);
    }
  
  return R;
}

			/* --------------- */


static void
s_qaut_destroy(sataf_automaton self)
{
  qaut a = (qaut)self;
  AUTO *m = a->autos;
#ifdef PRE_COMPUTE_HVAL
  int i = a->hval_nb_autos&0xFF;
#else
  int i = a->nb_autos;
#endif

  ccl_pre( i > 0 );

  while( i-- )
    {
      CLEAR(*m);
      m++;
    }

  ccl_delete(a->autos);
}

			/* --------------- */

static uint32_t
s_qaut_get_alphabet_size(sataf_automaton self)
{
  qaut a = (qaut)self;

  return 2;
}

			/* --------------- */

static sataf_automaton
s_qaut_succ_exists(sataf_automaton self, uint32_t letter)
{
  int i, k, l = 0;
  qaut a = (qaut)self; 
#ifdef PRE_COMPUTE_HVAL 
  int len = a->hval_nb_autos&0xFF;
#else
  int len = a->nb_autos;
#endif
  AUTO *succs = ccl_new_array(AUTO,len);
  sataf_automaton R;
  AUTO msa,tmp;

  ccl_assert( a->i > 0 );

  for(i = 0; i < len; i++)
    {
      SUCC(a->autos[i],letter,msa);
	  
      if( IS_ONE(msa) )
	{
	  CLEAR(msa);
	  while( l-- )
	    CLEAR(succs[l]);
	  ccl_delete(succs);
	  return sataf_automaton_create_one(2);
	}
	  
      for(k = 0; k < l; k++)
	{
	  if( IS_LT(msa,succs[k]) )
	    {
	      tmp = succs[k];
	      succs[k] = msa;
	      msa = tmp;
	    }
	  else if( IS_EQUAL(msa,succs[k]) )
	    {
	      CLEAR(msa);
	      break;
	    }
	}
	  
      if( k == l )
	succs[l++] = msa;
    }

  ccl_assert( l <= len );

  R = (sataf_automaton)
    s_create_quantifier(a->i-1,a->n,l,succs,a->super.methods);

  return R;
}

			/* --------------- */

static sataf_automaton
s_qaut_succ_forall(sataf_automaton self, uint32_t letter)
{
  int i, k, l = 0;
  qaut a = (qaut)self;  
#ifdef PRE_COMPUTE_HVAL
  int len = a->hval_nb_autos&0xFF;
#else
  int len = a->nb_autos;
#endif
  AUTO *succs = ccl_new_array(AUTO,len);
  sataf_automaton R;
  AUTO msa,tmp;

  ccl_assert( a->i > 0 );

  for(i = 0; i < len; i++)
    {
      SUCC(a->autos[i],letter,msa);
	  
      if( IS_ZERO(msa) )
	{
	  CLEAR(msa);
	  while( l-- )
	    CLEAR(succs[l]);
	  ccl_delete(succs);
	  return sataf_automaton_create_zero(2);
	}
	  
      for(k = 0; k < l; k++)
	{
	  if( IS_LT(msa,succs[k]) )
	    {
	      tmp = succs[k];
	      succs[k] = msa;
	      msa = tmp;
	    }
	  else if( IS_EQUAL(msa,succs[k]) )
	    {
	      CLEAR(msa);
	      break;
	    }
	}
	  
      if( k == l )
	succs[l++] = msa;
    }

  ccl_assert( l <= len );

  R = (sataf_automaton)
    s_create_quantifier(a->i-1,a->n,l,succs,a->super.methods);

  return R;
}

			/* --------------- */

static int
s_qaut_is_final_forall(sataf_automaton self)
{
  int i;
  qaut a = (qaut)self;
#ifdef PRE_COMPUTE_HVAL
  int max = a->hval_nb_autos&0xFF;
#else
  int max = a->nb_autos;
#endif

  for(i = 0; i < max; i++)
    if( ! IS_FINAL(a->autos[i]) )
      return 0;
  return 1;
}

			/* --------------- */

static int
s_qaut_is_final_exists(sataf_automaton self)
{
  int i;
  qaut a = (qaut)self;
#ifdef PRE_COMPUTE_HVAL
  int max = a->hval_nb_autos&0xFF;
#else
  int max = a->nb_autos;
#endif

  for(i = 0; i < max; i++)
    if( IS_FINAL(a->autos[i]) )
      return 1;
  return 0;
}

			/* --------------- */

static int
s_qaut_is_equal_to(sataf_automaton self, sataf_automaton other)
{
  int i;
  qaut a1 = (qaut)self;
  qaut a2 = (qaut)other;

#ifdef PRE_COMPUTE_HVAL
  if( ! ( a1->hval_nb_autos == a2->hval_nb_autos && a1->i == a2->i && 
	  a1->n == a2->n ) )
    return 0;
  return memcmp(a1->autos,a2->autos,
		sizeof(AUTO)*(0xFF&a1->hval_nb_autos)) == 0;
#else
  if( ! ( a1->i == a2->i && a1->n == a2->n && a1->nb_autos == a2->nb_autos ) )
    return 0;
  return memcmp(a1->autos,a2->autos,sizeof(AUTO)*a1->nb_autos) == 0;
#endif
}

			/* --------------- */

static const int PRIME_TABLE[] = {
   1, 3, 7, 17, 37, 79, 163, 331, 673, 1361,
   2729, 5471, 10949, 21911, 43853,  87719, 175447, 350899, 701819, 1403641,
   2807303, 5614657, 11229331
};

# define PRIME_TABLE_SIZE (sizeof(PRIME_TABLE)/PRIME_TABLE[0])

static uint32_t
s_qaut_actual_hashcode(qaut a)
{
  int i;
#ifdef PRE_COMPUTE_HVAL
  int max = 0xFF&a->hval_nb_autos;
#else
  int max = a->nb_autos;
#endif
  uint32_t result = 175447*a->i+43853*a->n+350899*max;

  for(i = 0; i < max; i++)
    result = 175447*result+
      PRIME_TABLE[i%PRIME_TABLE_SIZE]*HASH(a->autos[i]);

  return result;
}

			/* --------------- */

static uint32_t
s_qaut_hashcode(sataf_automaton self)
{
  qaut a = (qaut)self;

#ifdef PRE_COMPUTE_HVAL
  return a->hval_nb_autos; 
#else
  return s_qaut_actual_hashcode(a); 
#endif
}

			/* --------------- */

static qaut
s_create_qaut(uint32_t i, uint32_t n, AUTO *autos, int nb_autos,
	      sataf_automaton_methods methods)
{  
  int k;
  qaut R = (qaut)sataf_automaton_create(sizeof(struct qaut_st),methods);

  R->i = i;
  R->n = n;
  R->autos = autos;
#ifdef PRE_COMPUTE_HVAL
  R->hval_nb_autos = nb_autos;
  R->hval_nb_autos = (s_qaut_actual_hashcode(R)<<8)+nb_autos;
#else
  R->nb_autos = nb_autos;
#endif

  return R;
}
			/* --------------- */

static int 
s_cmp_msa(const void *p1, const void *p2)
{
  const sataf_msa *m1 = p1;
  const sataf_msa *m2 = p2;

  if( *m1 < *m2 ) return -1;
  if( *m1 == *m2 ) return 0;
  return 1;
}

			/* --------------- */
