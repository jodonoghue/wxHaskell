#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void, wxSystemSettings_GetSystemColour)(int index, void* _ref)
{
	*((wxColour*)_ref) = wxSystemSettings::GetSystemColour(index);
}

EWXWEXPORT(void, wxSystemSettings_GetSystemFont)(int index, void* _ref)
{
	*((wxFont*)_ref) = wxSystemSettings::GetSystemFont(index);
}

EWXWEXPORT(int, wxSystemSettings_GetSystemMetric)(int index)
{
	return wxSystemSettings::GetSystemMetric(index);
}

}
