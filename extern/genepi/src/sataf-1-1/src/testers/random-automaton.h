/**
 * Copyright (c) 2005 LaBRI, Universite Bordeaux I / CNRS UMR 5800
 *
 * All rights reserved.
 *
 * @author  : $Author: point $
 * @version : $Revision: 1.1.1.1 $
 * @date    : $Date: 2005/09/26 12:11:04 $
 */

#ifndef __RANDOM_AUTOMATON_H__
# define __RANDOM_AUTOMATON_H__

# include <exit-automaton.h>
# include <sataf-automaton.h>


extern sataf_automaton
random_automaton_create(uint32_t size, uint32_t alphabet_size);

extern exit_automaton
random_exit_automaton_create(uint32_t size, uint32_t alphabet_size);


#endif /* __RANDOM_AUTOMATON_H__ */
