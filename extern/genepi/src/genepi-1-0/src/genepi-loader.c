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

/* $Id: genepi-loader.c,v 1.2 2006/03/17 13:57:01 point Exp $ */
#include <dlfcn.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <stdlib.h>
#include "genepi.h"

			/* --------------- */

static void **lib_handlers = NULL;
static genepi_engine **engines = NULL;
static int nb_engines = 0;

			/* --------------- */

void
genepi_loader_init(void)
{
  engines = (genepi_engine **)calloc(sizeof(genepi_engine *),1);
  lib_handlers = (void **)calloc(sizeof(void *),1);
}

			/* --------------- */

void
genepi_loader_terminate(void)
{
  int i;

  if( engines == NULL )
    return;
  for(i = 0; i < nb_engines; i++)
    dlclose(lib_handlers[i]);
  free(engines);
  free(lib_handlers);
}

			/* --------------- */

void
genepi_loader_load_engine(const char *filename)
{  
  char *err;
  genepi_engine *(*init)(void) = NULL;
  void *hdl = dlopen(filename,RTLD_LAZY|RTLD_LOCAL);

  if( (err = dlerror()) != NULL )
    {
      fprintf(stderr,"%s: error: %s\n",filename,err);
      return;
    }

  init = (genepi_engine *(*)(void))dlsym(hdl,"genepi_plugin_init");
  if( (err = dlerror()) != NULL )
    {
      fprintf(stderr,"%s: error: %s\n",filename,err);
      dlclose(hdl);
      return;
    }
  
  lib_handlers = (void **)realloc(lib_handlers,sizeof(void *)*(nb_engines+1));
  lib_handlers[nb_engines] = hdl;
  
  engines = (genepi_engine **)
    realloc(engines,sizeof(genepi_engine *)*(nb_engines+1));
  engines[nb_engines] = init();

  nb_engines++;
}

			/* --------------- */

int
genepi_loader_load_directory(const char *dirname)
{
  struct dirent *e;
  char *module_name;
  int module_name_size, dnlen;
  DIR *dir = opendir(dirname);

  if( dir == NULL )   
    return 0;
    
   dnlen = strlen(dirname);
  module_name_size = dnlen+1;
  module_name = (char *)calloc(sizeof(char),module_name_size);
  
  while( (e=readdir(dir)) != NULL )
    {
      int sz = dnlen+1;
      if( strcmp(e->d_name,".") == 0 || strcmp(e->d_name,"..") == 0 )
	continue;
      
      if( (dnlen += strlen(e->d_name)) > module_name_size )
	{
	  module_name = (char *)realloc(module_name,sizeof(char)*dnlen);
	  module_name_size = dnlen;
	}
      sprintf(module_name,"%s/%s",dirname,e->d_name);
      genepi_loader_load_engine(module_name);
    }
  free(module_name);

  closedir(dir);

  return 1;
}

			/* --------------- */

genepi_engine **
genepi_loader_get_engines(int *psize)
{
  *psize = nb_engines;
  return engines;
}

			/* --------------- */


