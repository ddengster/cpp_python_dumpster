
#ifndef _DEFINES_H
#define _DEFINES_H

/** Dll stuff **/
#ifdef _USRDLL
   #define DLLDIR __declspec(dllexport)   // export DLL information
#else
   #define DLLDIR __declspec(dllimport)   // import DLL information
#endif

#ifdef USING_DLL
	#define SHARED DLLDIR
#else
	#define SHARED
#endif

#ifdef _MSC_VER
#	define FORCEINLINE __forceinline
#else
#	define FORCEINLINE inline
#endif

/** Math stuff **/
typedef float Real;
#define PI  3.14159265358979323846

/** Table sizes **/
#define TABLE_SMALL_SIZE 128 // 2 << 7
#define TABLE_AVERAGE_SIZE 512 // 2 << 9
#define TABLE_LARGE_SIZE 1024 // 2 << 10
#define TABLE_HUGE_SIZE 8192 //2 << 13

/** Deletion macros **/
#if !defined(SAFE_DELETE)
#define SAFE_DELETE(p) if(p) delete p; p=NULL;
#endif
#if !defined(SAFE_DELETE_ARRAY)
#define SAFE_DELETE_ARRAY(p) if(p) delete [] p; p=NULL;
#endif
#if !defined(SAFE_RELEASE)
#define SAFE_RELEASE(p) if(p) p->Release(); p=NULL;
#endif
#if !defined(SAFE_FREE)
#define SAFE_FREE(p) if(p) free(p); p=NULL;
#endif

/** Namespace macros **/
#define DECLARE_ENGINE_NAMESPACE namespace ddengine_RenderEngine {
#define END_ENGINE_NAMESPACE  }

#endif
