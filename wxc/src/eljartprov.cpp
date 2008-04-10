#include "wrapper.h"
#if wxVERSION_NUMBER >= 2400
#include "wx/artprov.h"

extern "C"
{
typedef void* _cdecl (*TCreateBmp)(void* _obj, void* id, void* clt, int w, int h);
}


class ELJArtProv: public wxArtProvider
{
	private:
		void* EiffelObject;
		TCreateBmp cb;
	protected:
    	virtual wxBitmap CreateBitmap(const wxArtID& id, const wxArtClient& client, const wxSize& size)
		{
			if (EiffelObject)
			{
				void* res = cb (EiffelObject, (void*)id.c_str(), (void*)client.c_str(), size.GetWidth(), size.GetHeight());
				
				if (res)
					return (*((wxBitmap*)res));
				else
					return wxNullBitmap;
			}
			return wxNullBitmap;
		}
	public:
		ELJArtProv (void* obj, void* clb){EiffelObject = obj; cb = (TCreateBmp)clb;};
		void Release(){EiffelObject = NULL; cb = NULL;};
};

extern "C"
{

EWXWEXPORT(void*,ELJArtProv_Create)(void* _obj, void* _clb)
{
	return (void*)new ELJArtProv(_obj, _clb);
}
	
EWXWEXPORT(void,ELJArtProv_Release)(void* _obj)
{
	((ELJArtProv*)_obj)->Release();
	delete (ELJArtProv*)_obj;
}
	
EWXWEXPORT(void,PushProvider)(void* provider)
{
#if WXWIN_COMPATIBILITY_2_6
	wxArtProvider::PushProvider((wxArtProvider*)provider);
#else
  wxArtProvider::Push((wxArtProvider*)provider);
#endif
}

EWXWEXPORT(int,PopProvider)()
{
#if WXWIN_COMPATIBILITY_2_6
	return (int)wxArtProvider::PopProvider();
#else
  return wxArtProvider::Pop();
#endif
}

EWXWEXPORT(int,RemoveProvider)(void* provider)
{
#if WXWIN_COMPATIBILITY_2_6
	return (int)wxArtProvider::RemoveProvider((wxArtProvider*)provider);
#else
  return wxArtProvider::Remove((wxArtProvider*)provider);
#endif
}

}
#endif
