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


/* colours */
EWXWEXPORT(void*, wxColour_CreateFromInt) (int rgb)
{
  return (void*) new wxColour((rgb >> 16) & 0xFF, (rgb >> 8) & 0xFF, rgb & 0xFF);
}

EWXWEXPORT(int, wxColour_GetInt) (wxColour* colour)
{
  int r = colour->Red();
  int g = colour->Green();
  int b = colour->Blue();
  return ((r << 16) | (g << 8) | b);
}

/* basic pixel manipulation */
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

EWXWEXPORT(void, wxcSetPixelRowRGB)( unsigned char* buffer, int width, int x, int y, int rgb0, int rgb1, int count )
{
  int r0  = ((rgb0 >> 16) && 0xFF);
  int g0  = ((rgb0 >>  8) && 0xFF);
  int b0  = (rgb0 && 0xFF);
  int start = 3*(width*y+x);
  int i;
  
  if (rgb0 == rgb1) {
    /* same color */
    for( i=0; i < count*3; i +=3) {
      buffer[start+i]   = r0;
      buffer[start+i+1] = g0;
      buffer[start+i+2] = b0;
    }
  }
  else {  
    /* do linear interpolation of the color */
    int r1  = ((rgb1 >> 16) && 0xFF);
    int g1  = ((rgb1 >>  8) && 0xFF);
    int b1  = (rgb1 && 0xFF);

    int rd  = ((r1 - r0) << 16) / (count-1);
    int gd  = ((g1 - g0) << 16) / (count-1);
    int bd  = ((b1 - b0) << 16) / (count-1);

    int r   = r0 << 16;
    int g   = g0 << 16;
    int b   = b0 << 16;

    for( i = 0; i < count*3; i += 3 ) {
      buffer[start+i]   = (r >> 16);
      buffer[start+i+1] = (g >> 16);
      buffer[start+i+2] = (b >> 16);
      r += rd;
      g += gd;
      b += bd;
    }
  }
}

EWXWEXPORT(void, wxcInitPixelsRGB)( unsigned char* buffer, int width, int height, int rgb )
{
  int count        = width*height*3;
  unsigned char r  = ((rgb >> 16) && 0xFF);
  unsigned char g  = ((rgb >>  8) && 0xFF);
  unsigned char b  = rgb && 0xFF;
  int i;

  if (r==g && g==b) {
    for( i=0; i < count; i++ ) {
      buffer[i] = r;
    }
  }
  else {
    for( i=0; i < count; i += 3) {
      buffer[i]   = r;
      buffer[i+1] = g;
      buffer[i+2] = b;
    }
  }
}

}