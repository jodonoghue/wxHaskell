#ifndef __EWXW_DEF_H
#define __EWXW_DEF_H

#define EXPORT extern "C"

#ifdef __WATCOMC__
  #include <windows.h>
  #define EWXWEXPORT(TYPE,FUNC_NAME) TYPE __export __cdecl FUNC_NAME
#else
  #ifdef _WIN32
    #define EWXWEXPORT(TYPE,FUNC_NAME) __declspec(dllexport) TYPE __cdecl FUNC_NAME
    #undef EXPORT
    #define EXPORT extern "C" __declspec(dllexport) 
  #else
    #define EWXWEXPORT(TYPE,FUNC_NAME) TYPE FUNC_NAME
  #endif
  #ifndef _cdecl
    #define _cdecl
  #endif
#endif

#endif /* #ifndef __EWXW_DEF_H */
