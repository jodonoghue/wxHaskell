#include <memory.h>
#include <string.h>

#include "wrapper.h"

extern "C" 
{

/* bitmap/image helpers */
EWXWEXPORT(wxBitmap*, wxBitmap_CreateFromImage)( wxImage* image, int depth )
{
  return new wxBitmap(*image,depth);	
}


EWXWEXPORT(wxImage*, wxImage_CreateFromDataEx)(int width, int height, void* data, int isStaticData)
{
  return new wxImage(width, height, (unsigned char*)data, isStaticData != 0);
}


EWXWEXPORT(void, wxImage_Delete)( wxImage* image )
{
  delete image;
}

/* basic (zbuffer) pixel manipulation */
EWXWEXPORT(void, wxcSetPixel)( unsigned char* buffer, int width, int x, int y, int r, int g, int b )
{
  int indexR = 3*(width*y + x);
  buffer[indexR]   = r;
  buffer[indexR+1] = g;
  buffer[indexR+2] = b;
}

EWXWEXPORT(void, wxcSetPixelRGB)( unsigned char* buffer, int width, int x, int y, int rgb )
{
  int indexR = 3*(width*y + x);
  buffer[indexR]   = rgb >> 16;
  buffer[indexR+1] = rgb >>  8;
  buffer[indexR+2] = rgb;
}

EWXWEXPORT(int, wxcGetPixelRGB)( unsigned char* buffer, int width, int x, int y )
{
  int indexR = 3*(width*y + x);
  int r,g,b;
  r = buffer[indexR];
  g = buffer[indexR+1];
  b = buffer[indexR+2];
  return ((r << 16) | (g << 8) | b);
}

EWXWEXPORT(void, wxcSetZPixel)( unsigned char* buffer, int* zbuffer, int width, int x, int y, int z, int r, int g, int b )
{
  int index  = width*y + x;
  if (z < zbuffer[index]) {
    int indexR = 3*index;
    zbuffer[index]   = z;
    buffer[indexR]   = r;
    buffer[indexR+1] = g;
    buffer[indexR+2] = b;
  }
}

EWXWEXPORT(void, wxcSetZPixelRGB)( unsigned char* buffer, int* zbuffer, int width, int x, int y, int z, int rgb )
{
  int index  = width*y + x;
  if (z < zbuffer[index]) {
    int indexR = 3*index;
    zbuffer[index]   = z;
    buffer[indexR]   = rgb >> 16;
    buffer[indexR+1] = rgb >>  8;
    buffer[indexR+2] = rgb;
  }
}

EWXWEXPORT(int,  wxcGetZValue)( int* zbuffer, int width, int x, int y )
{
  return zbuffer[width*y + x];
}

EWXWEXPORT(void, wxcSetZValue)( int* zbuffer, int width, int x, int y, int z )
{
  zbuffer[width*y+x] = z;
}

EWXWEXPORT(int,  wxcUpdateZValue)( int* zbuffer, int width, int x, int y, int z )
{
  int index = width*y + x;
  if (z < zbuffer[index]) {
    zbuffer[index] = z;
    return TRUE;
  }
  else return FALSE;
}

/* buffer helpers */

EWXWEXPORT(void*, wxcMalloc)( int size )
{
  return malloc(size);
}

EWXWEXPORT(void*, wxcMallocInts)( int count )
{
  return malloc(count * sizeof(int));
}

EWXWEXPORT(void, wxcFree)( void* p )
{
  if (p) { free(p); }
}


EWXWEXPORT(int, wxcPeekByte)( unsigned char* buffer, int index )
{
  return buffer[index];
}

EWXWEXPORT(void, wxcPokeByte)( unsigned char* buffer, int index, int i )
{
  buffer[index] = i;
}

EWXWEXPORT(int, wxcPeekInt)( int* buffer, int index )
{
  return buffer[index];
}

EWXWEXPORT(void, wxcPokeInt)( int* buffer, int index, int i )
{
  buffer[index] = i;
}


EWXWEXPORT(void, wxcPokeBytes)( unsigned char* buffer, int index, int count, int i )
{
  memset( &buffer[index], i, count );
}

EWXWEXPORT(void, wxcPokeInts)( int* buffer, int index, int count, int i )
{
  if (i >= 0 && i <= 255) {
    memset( &buffer[index], i, count*sizeof(int) );
  }
  else {
    int n;
    for (n = 0; n < count; n++) {
      buffer[index+n] = i;
    }
  }
}

}