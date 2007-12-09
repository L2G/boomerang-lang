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


/* $Id: mona2fast.c,v 1.3 2006/03/17 13:41:40 point Exp $ */
# include <dfa.h>
# include <stdlib.h>
# include "mona2fast.h"

struct genepi_set_st
{
  DFA *a;
  int number_reference;
  int dim;
};

			/* --------------- */

static genepi_set *set_plus; /* x[0] + x[1] = x[2] */
static genepi_set *set_eq;   /* x[0] = x[1] */

			/* --------------- */

static void
mona_genepi_set_del_reference(genepi_set *X);
static genepi_set *
mona_genepi_set_intersection(genepi_set *X1, genepi_set *X2);
static genepi_set *
mona_genepi_set_complement(genepi_set *X);
static genepi_set *
mona_genepi_set_invproject(genepi_set *X, int *selection, int size);
static genepi_set *
mona_genepi_set_project(genepi_set *X, int *selection, int size);

			/* --------------- */

static void
mona_genepi_set_init(void)
{
  bdd_init();  
  //Construction de set_plus
  int indices[3];
  indices[0]=0;
  indices[1]=1;
  indices[2]=2;
  dfaSetup(4,3,indices);
  dfaAllocExceptions(0);
  dfaStoreState(1);
  dfaAllocExceptions(4);
  dfaStoreException(1,"000");
  dfaStoreException(1,"011");
  dfaStoreException(1,"101");
  dfaStoreException(3,"110");
  dfaStoreState(2);
  dfaAllocExceptions(0);
  dfaStoreState(2);
  dfaAllocExceptions(4);
  dfaStoreException(3,"111");
  dfaStoreException(3,"100");
  dfaStoreException(3,"010");
  dfaStoreException(1,"001");
  dfaStoreState(2);
  set_plus=malloc(sizeof(genepi_set));
  set_plus->a=dfaBuild("0+--");
  set_plus->number_reference=1;
  set_plus->dim=3;
  
  //Construction de DFAeq
  indices[0]=0;
  indices[1]=1;
  dfaSetup(3,2,indices);
  dfaAllocExceptions(0);
  dfaStoreState(1);
  dfaAllocExceptions(2);
  dfaStoreException(1,"00");
  dfaStoreException(1,"11");
  dfaStoreState(2);
  dfaAllocExceptions(0);
  dfaStoreState(2);  
  set_eq=malloc(sizeof(genepi_set));
  set_eq->a=dfaBuild("0+-");
  set_eq->number_reference=1;
  set_eq->dim=2;
}

			/* --------------- */

static void
mona_genepi_set_terminate(int show_statistics)
{
  mona_genepi_set_del_reference(set_plus);
  mona_genepi_set_del_reference(set_eq);
}

			/* --------------- */

static genepi_set *
mona_genepi_set_add_reference(genepi_set *X)
{
  X->number_reference++;
  return X;
}

			/* --------------- */

static void
mona_genepi_set_del_reference(genepi_set *X)
{
  X->number_reference--;
  if (X->number_reference==0)
    {
      dfaFree(X->a);
      free(X);
    }
}

			/* --------------- */

// return the set that defines the predicate a.p_0=p_1 where a>0
static genepi_set *
mona_genepi_set_linear_multiply_simple(int a)
{
  if (a<=0){
    printf("Internal error in linear equality");
    exit(0);      
  }
  if (a==1){
    mona_genepi_set_add_reference(set_eq);
    return set_eq;
  }
  int a1,a2;
  if ((a%2)==0){
    a1=a/2;
    a2=a/2;
  } else{
    a1=a-1;
    a2=1;
  }
  int proj[4];
  genepi_set *X = mona_genepi_set_linear_multiply_simple(a1);
  proj[0]=0;
  proj[1]=0;
  proj[2]=1;
  proj[3]=1;
  genepi_set *X1 = mona_genepi_set_invproject(X,proj,4);
  mona_genepi_set_del_reference(X);
  X = mona_genepi_set_linear_multiply_simple(a2);
  proj[0]=0;
  proj[1]=1;
  proj[2]=0;
  proj[3]=1;
  genepi_set *X2 = mona_genepi_set_invproject(X,proj,4);
  mona_genepi_set_del_reference(X);
  genepi_set *X3 = mona_genepi_set_intersection(X1,X2);
  mona_genepi_set_del_reference(X1);
  mona_genepi_set_del_reference(X2);
  proj[0]=1;
  proj[1]=0;
  proj[2]=0;
  proj[3]=0;
  genepi_set *X4 = mona_genepi_set_invproject(set_plus,proj,4);
  genepi_set *X5 = mona_genepi_set_intersection(X3,X4);
  mona_genepi_set_del_reference(X3);
  mona_genepi_set_del_reference(X4);
  proj[0]=0;
  proj[1]=1;
  proj[2]=1;
  proj[3]=0;
  X = mona_genepi_set_project(X5,proj,4);
  mona_genepi_set_del_reference(X5);
  return X;
}

			/* --------------- */

//We return the set that defines the predicate a.p_0+p_1=p_2 where a>0
static genepi_set *
mona_genepi_set_linear_multiply(int a)
{
  if (a<=0){
    printf("Internal error in linear equality");
    exit(0);      
  }
  genepi_set *X = mona_genepi_set_linear_multiply_simple(a);
  int proj[4];
  proj[0]=0;
  proj[1]=0;
  proj[2]=1;
  proj[3]=1;
  genepi_set *X0= mona_genepi_set_invproject(X,proj,4);
  mona_genepi_set_del_reference(X);
  proj[0]=1;
  proj[1]=0;
  proj[2]=0;
  proj[3]=0;
  genepi_set *X1= mona_genepi_set_invproject(set_plus,proj,4);
  genepi_set *X2= mona_genepi_set_intersection(X0,X1);
  mona_genepi_set_del_reference(X0);
  mona_genepi_set_del_reference(X1);
  proj[0]=0;
  proj[1]=1;
  proj[2]=0;
  proj[3]=0;
  X= mona_genepi_set_project(X2,proj,4);
  mona_genepi_set_del_reference(X2);
  return X;
}

			/* --------------- */


//returns the 1-dim set representing x[0]=c where c>=0
static genepi_set *
mona_genepi_set_constante(int c)
{
  if (c<0){
    printf("internal error in constante");
    exit(0);
  }
  genepi_set *R=malloc(sizeof(genepi_set));
  R->a=dfaPresbConst(0,c);
  R->number_reference=1;
  R->dim=1;
  return R;
}



static genepi_set *
mona_genepi_set_linear_equality(int *alpha, int size, int c)
{
  // We first compute the (n+2)-dim set X that defines 
  // a[0].x[0]+...a[n-1].x[n-1]+x[n]=x[n+1]
  // where a[0]=...=a[n-1]=0 and n=size
  //FILE *f; 
  //genepi_set_display_data_structure(set_plus,f);
  //genepi_set_display_data_structure(set_eq,f);
  genepi_set *X;
  int proj[(size+3)];
  int i;
  for(i=0;i<size;i++)
    proj[i]=1;
  proj[size]=0;
  proj[size+1]=0;
  X= mona_genepi_set_invproject(set_eq,proj,size+2);    

  int k;
  for(k=0;k<size;k++){
    // We replace in X the coef a[k] by a[k]+alpha[k]
    //genepi_set_display_data_structure(X,f);
    if (alpha[k]>0){
      genepi_set *Y0;
      int i;
      for(i=0;i<size+3;i++)
	proj[i]=0;
      proj[size]=1;
      Y0= mona_genepi_set_invproject(X,proj,size+3);
      mona_genepi_set_del_reference(X);
      genepi_set *Y1;
      for(i=0;i<size+3;i++)
	proj[i]=1;
      proj[k]=0;
      proj[size]=0;
      proj[size+1]=0;
      genepi_set *Z= mona_genepi_set_linear_multiply(alpha[k]);
      Y1= mona_genepi_set_invproject(Z,proj,size+3);
      mona_genepi_set_del_reference(Z);
      genepi_set *Y2= mona_genepi_set_intersection(Y0,Y1);
      mona_genepi_set_del_reference(Y0);
      mona_genepi_set_del_reference(Y1);
      for(i=0;i<size+3;i++)
	proj[i]=0;
      proj[size+1]=1;
      X= mona_genepi_set_project(Y2,proj,size+3); 
      mona_genepi_set_del_reference(Y2);
    }
    if (alpha[k]<0){
      genepi_set *Y0;
      int i;
      for(i=0;i<size+3;i++)
	proj[i]=0;
      proj[size+1]=1;
      Y0= mona_genepi_set_invproject(X,proj,size+3);
      mona_genepi_set_del_reference(X);
      genepi_set *Y1;
      for(i=0;i<size+3;i++)
	proj[i]=1;
      proj[k]=0;
      proj[size+1]=0;
      proj[size+2]=0;
      genepi_set *Z= mona_genepi_set_linear_multiply(-alpha[k]);
      Y1= mona_genepi_set_invproject(Z,proj,size+3);
      mona_genepi_set_del_reference(Z);
      //genepi_set_display_data_structure(Y0,f);
      //genepi_set_display_data_structure(Y1,f);
      genepi_set *Y2= mona_genepi_set_intersection(Y0,Y1);
      mona_genepi_set_del_reference(Y0);
      mona_genepi_set_del_reference(Y1);
      for(i=0;i<size+3;i++)
	proj[i]=0;
      proj[size+2]=1;
      X= mona_genepi_set_project(Y2,proj,size+3); 
      mona_genepi_set_del_reference(Y2);
    }
  }
  //genepi_set_display_data_structure(X,f);
  
  int an,ap;
  if (c>=0){
    an=0;
    ap=c;
  }
  else{
    an=-c;
    ap=0;
  }
  
  
  for(i=0;i<size+2;i++)
    proj[i]=1;
  //proj is full of 1.
  proj[size]=0;
  genepi_set *Rn= mona_genepi_set_constante(an);
  genepi_set *Ran= mona_genepi_set_invproject(Rn,proj,size+2);
  mona_genepi_set_del_reference(Rn);
  proj[size]=1;
  proj[size+1]=0;
  genepi_set *Rp= mona_genepi_set_constante(ap);
  genepi_set *Rap= mona_genepi_set_invproject(Rp,proj,size+2);
  mona_genepi_set_del_reference(Rp);
  proj[size+1]=1;
  genepi_set *Rconst= mona_genepi_set_intersection(Ran,Rap);
  mona_genepi_set_del_reference(Ran);
  mona_genepi_set_del_reference(Rap);
  //genepi_set_display_data_structure(Rconst,f);
  genepi_set *XX= mona_genepi_set_intersection(X,Rconst);
  mona_genepi_set_del_reference(X);
  mona_genepi_set_del_reference(Rconst);
  for(i=0;i<size+2;i++)
    proj[i]=0;
  proj[size]=1;
  proj[size+1]=1;
  X= mona_genepi_set_project(XX,proj,size+2);
  mona_genepi_set_del_reference(XX);
  return X;
}

			/* --------------- */

static genepi_set *
mona_genepi_set_union(genepi_set *X1, genepi_set *X2)
{
  if (X1->dim!=X2->dim){
    printf("Dimension not equal in union");
    exit(0);
  }
  genepi_set *R;
  R=malloc(sizeof(genepi_set));
  DFA *a=dfaProduct(X1->a,X2->a,dfaOR);
  R->a=dfaMinimize(a);
  dfaFree(a);
  R->number_reference=1;
  R->dim=X1->dim;
  return R;
}

			/* --------------- */

static genepi_set *
mona_genepi_set_intersection(genepi_set *X1, genepi_set *X2)
{
  if (X1->dim!=X2->dim){
    printf("Dimension not equal in intersection");
    exit(0);
  }
  genepi_set *R;
  R=malloc(sizeof(genepi_set));
  R->number_reference=1;
  R->dim=X1->dim;
  DFA *a=dfaProduct(X1->a,X2->a,dfaAND);
  R->a=dfaMinimize(a);
  dfaFree(a);
  return R;
}

			/* --------------- */

static genepi_set *
mona_genepi_set_complement(genepi_set *X)
{
  genepi_set *R;
  R=malloc(sizeof(genepi_set));
  R->a=dfaCopy(X->a);
  dfaNegation(R->a);
  R->number_reference=1;
  R->dim=X->dim;
  return R;
}

			/* --------------- */

static genepi_set *
mona_genepi_set_project(genepi_set *X, int *selection, int size)
{
  if (X->dim!=size){
    printf("Dimension not equal in projection");
    exit(0);
  }
  DFA *a=dfaCopy(X->a);
  int map[size];
  int pos=0;
  int i;
  for(i=0;i<size;i++)
    if (selection[i]==1){
      dfaRightQuotient(a,i);
      DFA* b = dfaProject(a, i);
      dfaFree(a);
      a=dfaMinimize(b);
      dfaFree(b);
    } else{
      map[i]=pos;
      pos++;
    }
  dfaReplaceIndices(a,map); 
  genepi_set *R;
  R=malloc(sizeof(genepi_set));
  R->a=dfaMinimize(a);
  dfaFree(a);
  R->number_reference=1;
  R->dim=pos;
  return R;
}

			/* --------------- */

static genepi_set *
mona_genepi_set_invproject(genepi_set *X, int *selection, int size)
{
  int dim;
  int i;
  dim=0;
  for(i=0;i<size;i++)
    if (selection[i]==0)
      dim++;
  if (X->dim!=dim){
    printf("Dimension not equal in projection");
    exit(0);
  }
  int map[X->dim];
  dim=0;
  for(i=0;i<size;i++)
    if (selection[i]==0)
      {
	map[dim]=i;
	dim++;
      }
  genepi_set *R;
  R=malloc(sizeof(genepi_set));
  R->a=dfaCopy(X->a);
  dfaReplaceIndices(R->a,map);
  R->number_reference=1;
  R->dim=size;
  return R;  
}

			/* --------------- */

#define mona_genepi_set_apply NULL

#define mona_genepi_set_applyinv NULL

static int
mona_genepi_set_is_empty(genepi_set *X)
{
  int i;
  for(i=1;i<X->a->ns;i++)
    if (X->a->f[i]==1)
      return 0;
  return 1;
}

			/* --------------- */

static int
mona_genepi_set_is_full(genepi_set *X)
{
  int i;
  for(i=1;i<X->a->ns;i++)
    if (X->a->f[i]!=1)
      return 0;
  return 1;
}

			/* --------------- */

static int
mona_genepi_set_is_included_in(genepi_set *X1, genepi_set *X2)
{
  if (X1->dim!=X2->dim){
    printf("Dimension not equal in inclusion");
    exit(0);
  }
  genepi_set *X2bar = mona_genepi_set_complement(X2);
  genepi_set *X1capX2bar = mona_genepi_set_intersection(X1,X2bar);
  int r = mona_genepi_set_is_empty(X1capX2bar);
  mona_genepi_set_del_reference(X1capX2bar);
  mona_genepi_set_del_reference(X2bar);
  return r;
}

			/* --------------- */

static int
mona_genepi_set_is_finite(genepi_set *X)
{
  //Non yet implemented
  return 0;
}

			/* --------------- */

static void
mona_genepi_set_get_solutions(genepi_set *X, int ***psolutions, int *psize, 
			    int max)
{
  //Non yet implemented
}

			/* --------------- */

static void
mona_genepi_set_display_all_solutions(genepi_set *X, FILE *output)
{
  // Non yet implemented
}

			/* --------------- */

static void
mona_genepi_set_display_data_structure(genepi_set *X, FILE *output)
{
  // Non yet implemented
  unsigned int indices[X->dim];
  int i;
  for(i=0;i<X->dim;i++)
    indices[i]=i;
  dfaPrintGraphviz(X->a,X->dim,indices);
}

			/* --------------- */

static int
mona_genepi_set_get_width(genepi_set *X)
{
  return X->dim;
}


static int
mona_genepi_set_get_data_structure_size(genepi_set *X)
{
  return bdd_size(X->a->bddm);
}

			/* --------------- */

static genepi_set *
mona_genepi_set_top(int n)
{
  genepi_set *R=malloc(sizeof(genepi_set));
  R->a=dfaTrue();
  R->number_reference=1;
  R->dim=n;
  return R;
}

			/* --------------- */

static genepi_set *
mona_genepi_set_bot(int n)
{
  genepi_set *R=malloc(sizeof(genepi_set));
  R->a=dfaFalse();
  R->number_reference=1;
  R->dim=n;
  return R;
}

			/* --------------- */

static int
mona_genepi_set_equal(genepi_set *X1, genepi_set *X2)
{
  if (X1->dim!=X2->dim){
    printf("Dimension not equal in equal");
    exit(0);
  }
  genepi_set *R;
  R=malloc(sizeof(genepi_set));
  R->number_reference=1;
  R->dim=X1->dim;
  R->a=dfaProduct(X1->a,X2->a,dfaBIIMPL);
  int r = mona_genepi_set_is_full(R);
  mona_genepi_set_del_reference(R);
  return r;
}



			/* --------------- */

static genepi_engine MONA2FAST = {
  "mona",
  mona_genepi_set_init,
  mona_genepi_set_terminate,
  mona_genepi_set_add_reference,
  mona_genepi_set_del_reference,
  mona_genepi_set_linear_equality,
  mona_genepi_set_union,
  mona_genepi_set_intersection,
  mona_genepi_set_complement,
  mona_genepi_set_project,
  mona_genepi_set_invproject,
  mona_genepi_set_apply,
  mona_genepi_set_applyinv,
  mona_genepi_set_is_empty,
  mona_genepi_set_is_full,
  mona_genepi_set_is_included_in,
  mona_genepi_set_is_finite,
  mona_genepi_set_get_solutions,
  mona_genepi_set_display_all_solutions,
  mona_genepi_set_display_data_structure,
  mona_genepi_set_get_width,
  mona_genepi_set_get_data_structure_size,
  mona_genepi_set_top,
  mona_genepi_set_bot,
  mona_genepi_set_equal
};

			/* --------------- */

genepi_engine *
genepi_plugin_init(void)
{
  return &MONA2FAST;
}
