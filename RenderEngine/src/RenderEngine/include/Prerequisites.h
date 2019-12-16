
#ifndef _PREREQUISITES_H
#define _PREREQUISITES_H

/** Caused by Math stuff **/
// disable: "conversion from 'double' to 'float', possible loss of data
#   pragma warning (disable : 4244)
// disable: "truncation from 'double' to 'float'
#   pragma warning (disable : 4305)



#include "stdint.h"
#include "Defines.h"

//Mark for debug?
#include <iostream>
#include <exception>
#include <assert.h>

#include <string>


typedef std::string String;

#endif
