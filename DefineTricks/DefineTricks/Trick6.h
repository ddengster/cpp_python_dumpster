
#ifndef TRICK6_H
#define TRICK6_H

/********
Some default macros:
__LINE__ //line number this define was called on
__FILE__ //file this define was called on
__DATE__ //date when it was compiled
__TIME__ //time when it was compiled
//Useful for debugging/memory tracking
********/

#include <string>
#include <assert.h>

#define assertdebug(a,b) assert(a && b)

#define _QUOTE(x) #x
#define QUOTE(x) _QUOTE(x)
#define __FILELINE__ __FILE__ "(" QUOTE(__LINE__) ")"

void Trick6()
{
	std::cout << "Trick6() Called at " << __FILELINE__ << std::endl;
	//assertdebug(0,__FILELINE__);
}
;
#endif