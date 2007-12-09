/**
 * Copyright (c) 2005 LaBRI, Universite Bordeaux I / CNRS UMR 5800
 *
 * All rights reserved.
 *
 * @author  : $Author: point $
 * @version : $Revision: 1.1.1.1 $
 * @date    : $Date: 2005/09/26 12:11:04 $
 */


#define THE_m ((int) 2147483647)
#define THE_a ((int) 16807)
#define THE_q ((int) 127773)
#define THE_r ((int) 2836)

static int THE_SEED=1234567891;

int 
rnd_modulo(int modvalue)
{
  THE_SEED=THE_a*(THE_SEED % THE_q) - THE_r*(THE_SEED / THE_q);
  if (THE_SEED<0) THE_SEED+=THE_m;
  return( THE_SEED%modvalue );
}

			/* --------------- */
double
rnd_probability(void)
{
  double result;

  THE_SEED=THE_a*(THE_SEED % THE_q) - THE_r*(THE_SEED / THE_q);
  if (THE_SEED<0) THE_SEED+=THE_m;
  result = ( ((double) THE_SEED) / ((double) THE_m) );

  return result;
}

			/* --------------- */

void
rnd_set_seed(int seed)
{
  THE_SEED = seed;
}

			/* --------------- */

int
rnd_get_seed(void)
{
  return THE_SEED;
}

			/* --------------- */
