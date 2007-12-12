#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void, wxSystemSettings_GetSystemColour)(wxSystemColour index, void* _ref)
{
	*((wxColour*)_ref) = wxSystemSettings::GetColour(index);
}

EWXWEXPORT(void, wxSystemSettings_GetSystemFont)(wxSystemFont index, void* _ref)
{
	*((wxFont*)_ref) = wxSystemSettings::GetFont(index);
}

EWXWEXPORT(int, wxSystemSettings_GetSystemMetric)(wxSystemMetric index)
{
	return wxSystemSettings::GetMetric(index);
}

}
