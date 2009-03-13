#include "wx/wx.h"
#include "wx/string.h"

int main(int argc,char **argv)
{
  wxString s = wxString::FromAscii(argv[1]);
  int tok;
  tok = s.Find('.');
  while (tok != wxNOT_FOUND) {
    wxPrintf(_T("#include \"src/%s.cpp\"\n"),s.Mid(4,tok-4).c_str());
    s = s.Mid(tok+3);
    tok = s.Find('.');
  }
}
