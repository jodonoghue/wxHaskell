#include "wrapper.h"
#include "wx/fontenum.h"
#include "wx/fontmap.h"
#include "wx/encconv.h"

extern "C"
{

typedef int _cdecl (*TTextEnum) (void* _obj, void* _txt);

}

class ELJFontEnumerator : public wxFontEnumerator
{
	private:
		TTextEnum func;
		void*     EiffelObject;
	public:
		ELJFontEnumerator (void* _obj, void* _fnc) : wxFontEnumerator()
		{
			func = (TTextEnum)_fnc;
			EiffelObject = _obj;
		}
		
	    virtual bool OnFacename(const wxString& facename)
        { 
			return func(EiffelObject, (void*)facename.c_str()) != 0;
		}
		virtual bool OnFontEncoding(const wxString& WXUNUSED(facename), const wxString& encoding)
        {
			return func(EiffelObject, (void*)encoding.c_str()) != 0;
		}

};

extern "C"
{

EWXWEXPORT(void*, wxFont_Create) (int pointSize, int family, int style, int weight, int underlined, char* face, int enc)
{
	return new wxFont (pointSize, family, style, weight, underlined != 0, face, (wxFontEncoding)enc);
}

EWXWEXPORT(void*, wxFont_CreateDefault)()
{
	return new wxFont ();
}

EWXWEXPORT(void, wxFont_Delete)(void* _obj)
{
	delete ((wxFont*)_obj);
}

EWXWEXPORT(int, wxFont_Ok)(void* _obj)
{
	return (int)((wxFont*)_obj)->Ok();
}
	
EWXWEXPORT(int, wxFont_GetPointSize)(void* _obj)
{
	return ((wxFont*)_obj)->GetPointSize();
}
	
EWXWEXPORT(int, wxFont_GetFamily)(void* _obj)
{
	return ((wxFont*)_obj)->GetFamily();
}
	
EWXWEXPORT(int, wxFont_GetStyle)(void* _obj)
{
	return ((wxFont*)_obj)->GetStyle();
}
	
EWXWEXPORT(int, wxFont_GetWeight)(void* _obj)
{
	return ((wxFont*)_obj)->GetWeight();
}
	
EWXWEXPORT(int, wxFont_GetUnderlined)(void* _obj)
{
	return (int)((wxFont*)_obj)->GetUnderlined();
}
	
EWXWEXPORT(int, wxFont_GetFaceName)(void* _obj, void* _buf)
{
	wxString result = ((wxFont*)_obj)->GetFaceName();
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}
	
EWXWEXPORT(int, wxFont_GetEncoding)(void* _obj)
{
	return (int)((wxFont*)_obj)->GetEncoding();
}
	
EWXWEXPORT(void, wxFont_SetPointSize)(void* _obj, int pointSize)
{
	((wxFont*)_obj)->SetPointSize(pointSize);
}
	
EWXWEXPORT(void, wxFont_SetFamily)(void* _obj, int family)
{
	((wxFont*)_obj)->SetFamily(family);
}
	
EWXWEXPORT(void, wxFont_SetStyle)(void* _obj, int style)
{
	((wxFont*)_obj)->SetStyle(style);
}
	
EWXWEXPORT(void, wxFont_SetWeight)(void* _obj, int weight)
{
	((wxFont*)_obj)->SetWeight(weight);
}
	
EWXWEXPORT(void, wxFont_SetFaceName)(void* _obj, char* faceName)
{
	((wxFont*)_obj)->SetFaceName(faceName);
}
	
EWXWEXPORT(void, wxFont_SetUnderlined)(void* _obj, int underlined)
{
	((wxFont*)_obj)->SetUnderlined(underlined != 0);
}
	
EWXWEXPORT(void, wxFont_SetEncoding)(void* _obj, int encoding)
{
	((wxFont*)_obj)->SetEncoding((wxFontEncoding)encoding);
}
	
EWXWEXPORT(int, wxFont_GetFamilyString)(void* _obj, void* _buf)
{
	wxString result = ((wxFont*)_obj)->GetFamilyString();
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}
	
EWXWEXPORT(int, wxFont_GetStyleString)(void* _obj, void* _buf)
{
	wxString result = ((wxFont*)_obj)->GetStyleString();
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}
	
EWXWEXPORT(int, wxFont_GetWeightString)(void* _obj, void* _buf)
{
	wxString result = ((wxFont*)_obj)->GetWeightString();
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}
	
EWXWEXPORT(int, wxFont_GetDefaultEncoding)(void* _obj)
{
	return (int)((wxFont*)_obj)->GetDefaultEncoding();
}
	
EWXWEXPORT(void, wxFont_SetDefaultEncoding)(void* _obj, int encoding)
{
	((wxFont*)_obj)->SetDefaultEncoding((wxFontEncoding) encoding);
}
	

EWXWEXPORT(void*, wxFontEnumerator_Create)(void* _obj, void* _fnc)
{
	return (void*) new ELJFontEnumerator(_obj, _fnc);
}

EWXWEXPORT(void, wxFontEnumerator_Delete)(void* _obj)
{
	delete ((ELJFontEnumerator*)_obj);
}

EWXWEXPORT(int,wxFontEnumerator_EnumerateFacenames)(void* _obj, int encoding, int fixedWidthOnly)
{
	return (int)((ELJFontEnumerator*)_obj)->EnumerateFacenames((wxFontEncoding)encoding, fixedWidthOnly != 0);
}
	
EWXWEXPORT(int,wxFontEnumerator_EnumerateEncodings)(void* _obj, void* facename)
{
	return (int)((ELJFontEnumerator*)_obj)->EnumerateEncodings((const char*)facename);
}
	

EWXWEXPORT(void*, wxFontMapper_Create)()
{
	return wxTheFontMapper;
}

EWXWEXPORT(int,wxFontMapper_GetAltForEncoding)(void* _obj, int encoding, void* alt_encoding, void* _buf)
{
	return (int)((wxFontMapper*)_obj)->GetAltForEncoding((wxFontEncoding)encoding, (wxFontEncoding*)alt_encoding, (const char*)_buf, false);
}
	
EWXWEXPORT(int,wxFontMapper_IsEncodingAvailable)(void* _obj, int encoding, void* _buf)
{
	return (int)((wxFontMapper*)_obj)->IsEncodingAvailable((wxFontEncoding)encoding, (const char*)_buf);
}
	

EWXWEXPORT(void*, wxEncodingConverter_Create)()
{
	return (void*) new wxEncodingConverter();
}

EWXWEXPORT(void, wxEncodingConverter_Delete)(void* _obj)
{
	delete (wxEncodingConverter*)_obj;
}

EWXWEXPORT(int,wxEncodingConverter_Init)(void* _obj, int input_enc, int output_enc, int method)
{
	return (int)((wxEncodingConverter*)_obj)->Init((wxFontEncoding)input_enc, (wxFontEncoding)output_enc, method);
}
	
EWXWEXPORT(void,wxEncodingConverter_Convert)(void* _obj, void* input, void* output)
{
	((wxEncodingConverter*)_obj)->Convert((const char*)input, (char*)output);
}

EWXWEXPORT(int,wxEncodingConverter_GetPlatformEquivalents)(void* _obj, int enc, int platform, void* _lst)
{
	wxFontEncodingArray arr = ((wxEncodingConverter*)_obj)->GetPlatformEquivalents((wxFontEncoding)enc, platform);
	if (_lst)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((int*)_lst)[i] = (int)arr.Item(i);
	}
	return (int)arr.GetCount();
}

EWXWEXPORT(int,wxEncodingConverter_GetAllEquivalents)(void* _obj, int enc, void* _lst)
{
	wxFontEncodingArray arr = ((wxEncodingConverter*)_obj)->GetAllEquivalents((wxFontEncoding)enc);
	if (_lst)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((int*)_lst)[i] = (int)arr.Item(i);
	}
	return (int)arr.GetCount();
}

}
