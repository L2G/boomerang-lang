/**
 * Copyright (c) 2005 LaBRI, Universite Bordeaux I / CNRS UMR 5800
 *
 * All rights reserved.
 *
 * @author  : $Author: point $
 * @version : $Revision: 1.2 $
 * @date    : $Date: 2006/01/23 13:32:17 $
 */

#ifndef __HOMOMORPHISM_CHECKING_H__
# define __HOMOMORPHISM_CHECKING_H__

# include "shared-automaton.h"

/**
 * check if the shared automaton 'sa' is homomorphic to its first bound
 * marked_shared_automaton M. 'src' and 'letter' are such that 
 * succ(src,letter) yields to the exit state mapped to M.
 * 
 * The function returns a non-null value if an homomorphism is found.
 *
 * The size of tmp and h must be sa->nb_local_states
 */

extern uint32_t 
check_homomorphism(exit_automaton ea, shared_automaton *bind_sa, 
		   uint32_t *bind_initial, uint32_t src, uint32_t letter,
		   uint32_t *tmp, uint32_t *h);

extern uint32_t 
check_homomorphism1(exit_automaton ea, shared_automaton *bind_sa, 
		   uint32_t *bind_initial, uint32_t letter,
		   uint32_t *tmp, uint32_t *h);
   
#endif /* __HOMOMORPHISM_CHECKING_H__ */
