#include "wrapper.h"
#include "wx/docview.h"

extern "C"
{

typedef int _cdecl (*TGetResp) (void* _obj, int _und);

}

class ELJCommand : public wxCommand
{
	private:
		TGetResp func;
		void*    EiffelObject;
	
	public:
		ELJCommand (bool _und, const wxString& _nme, void* _obj, void* _clb) : wxCommand(_und, _nme)
		{
			func = (TGetResp)_clb;
			EiffelObject = _obj;
		}
		
		bool Do()
		{return func (EiffelObject, 0) != 0;}

		bool Undo()
		{return func (EiffelObject, 1) != 0;}
};

extern "C"
{

EWXWEXPORT(void*,ELJCommand_Create)(int _und, void* _nme, void* _obj, void* _clb)
{
	return (void*)new ELJCommand(_und != 0, (const char*)_nme, _obj, _clb);
}

EWXWEXPORT(void,ELJCommand_Delete)(void* _obj)
{
	delete (ELJCommand*)_obj;
}

EWXWEXPORT(int,ELJCommand_GetName)(void* _obj, void* _buf)
{
	wxString tmp = ((ELJCommand*)_obj)->GetName();
	if (_buf) memcpy (_buf, tmp.c_str(), tmp.Length());
	return tmp.Length();
}

EWXWEXPORT(int,ELJCommand_CanUndo)(void* _obj)
{
	return (int)((ELJCommand*)_obj)->CanUndo();
}
	

EWXWEXPORT(void*,wxCommandProcessor_wxCommandProcessor)(int maxCommands)
{
	return (void*)new wxCommandProcessor(maxCommands);
}
	
EWXWEXPORT(void,wxCommandProcessor_Delete)(void* _obj)
{
	delete (wxCommandProcessor*)_obj;
}

EWXWEXPORT(int,wxCommandProcessor_Submit)(void* _obj, void* command, int storeIt)
{
	return (int)((wxCommandProcessor*)_obj)->Submit((wxCommand*)command, storeIt != 0);
}
	
EWXWEXPORT(int,wxCommandProcessor_Undo)(void* _obj)
{
	return (int)((wxCommandProcessor*)_obj)->Undo();
}
	
EWXWEXPORT(int,wxCommandProcessor_Redo)(void* _obj)
{
	return (int)((wxCommandProcessor*)_obj)->Redo();
}
	
EWXWEXPORT(int,wxCommandProcessor_CanUndo)(void* _obj)
{
	return (int)((wxCommandProcessor*)_obj)->CanUndo();
}
	
EWXWEXPORT(int,wxCommandProcessor_CanRedo)(void* _obj)
{
	return (int)((wxCommandProcessor*)_obj)->CanRedo();
}
	
EWXWEXPORT(void,wxCommandProcessor_SetEditMenu)(void* _obj, void* menu)
{
	((wxCommandProcessor*)_obj)->SetEditMenu((wxMenu*)menu);
}
	
EWXWEXPORT(void*,wxCommandProcessor_GetEditMenu)(void* _obj)
{
	return (void*)((wxCommandProcessor*)_obj)->GetEditMenu();
}
	
EWXWEXPORT(void,wxCommandProcessor_SetMenuStrings)(void* _obj)
{
	((wxCommandProcessor*)_obj)->SetMenuStrings();
}
	
EWXWEXPORT(void,wxCommandProcessor_Initialize)(void* _obj)
{
	((wxCommandProcessor*)_obj)->Initialize();
}
	
EWXWEXPORT(int,wxCommandProcessor_GetCommands)(void* _obj, void* _ref)
{
	wxList lst = ((wxCommandProcessor*)_obj)->GetCommands();
	if (_ref)
	{
		for (unsigned int i = 0; i < lst.GetCount(); i++)
			((void**)_ref)[i] = (void*)lst.Item(i);
	}
	
	return lst.GetCount();
}
	
EWXWEXPORT(int,wxCommandProcessor_GetMaxCommands)(void* _obj)
{
	return ((wxCommandProcessor*)_obj)->GetMaxCommands();
}
	
EWXWEXPORT(void,wxCommandProcessor_ClearCommands)(void* _obj)
{
	((wxCommandProcessor*)_obj)->ClearCommands();
}
	
}
