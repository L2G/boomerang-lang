/**
 * Copyright (c) 2005 LaBRI, Universite Bordeaux I / CNRS UMR 5800
 *
 * All rights reserved.
 *
 * @author  : $Author: point $
 * @version : $Revision: 1.1.1.1 $
 * @date    : $Date: 2005/09/26 12:11:04 $
 */

#ifndef __RND_H__
# define __RND_H__

extern int rnd_modulo(int modvalue);
extern double rnd_probability(void);
extern void rnd_set_seed(int seed);
extern int rnd_get_seed(void);

#endif /* __RND_H__ */
