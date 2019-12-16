
#ifndef _GLHEADERS_H
#define _GLHEADERS_H

#ifdef	_WIN32
	#include <glee.h>
	#include <gl/gl.h>
	#include <gl/glu.h>
//	#include <gl/glext.h>
//   #include<OpenGLExtensions.h>
#else
	#ifdef _MAC
	   #include<Carbon/Carbon.h>
	   #include<agl/agl.h>
	   #include<OpenGL/gl.h>
	   #include<OpenGL/glu.h>
	   #include<OpenGL/glext.h>
	#else
	   #include<GL/glx.h>
	   #include<GL/gl.h>
	   #include<GL/glu.h>
	   #include<GL/glext.h>
	   #include<GL/glxext.h>
	   #include<OpenGLExtensions.h>
	   #include<X11/extensions/xf86vmode.h>
	   #include<X11/keysym.h>
	#endif
#endif

#endif
