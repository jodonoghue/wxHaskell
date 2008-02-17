#ifndef __EWXW_DEF_H
#define __EWXW_DEF_H

#ifdef FOREIGN_RELIGION
#include "../contrib/contrib.h"
#endif

#ifdef __WATCOMC__
  #include <windows.h>
  #define EWXWEXPORT(TYPE,FUNC_NAME) TYPE __export FUNC_NAME
#else
  #ifdef __GNUWIN32__
    #define EWXWEXPORT(TYPE,FUNC_NAME) __declspec(dllexport) TYPE FUNC_NAME
  #else
    #define EWXWEXPORT(TYPE,FUNC_NAME) TYPE FUNC_NAME
  #endif /* #ifdef __GNUWIN32__ */
  #ifndef _cdecl
    #define _cdecl
  #endif
#endif /* #ifdef __WATCOMC__ */

#endif /* #ifndef __EWXW_DEF_H */
