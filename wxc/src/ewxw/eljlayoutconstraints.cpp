#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxLayoutConstraints_left)(void* _obj)
{
	return (void*)(&((wxLayoutConstraints*)_obj)->left);
}
	
EWXWEXPORT(void*, wxLayoutConstraints_top)(void* _obj)
{
	return (void*)(&((wxLayoutConstraints*)_obj)->top);
}
	
EWXWEXPORT(void*, wxLayoutConstraints_right)(void* _obj)
{
	return (void*)(&((wxLayoutConstraints*)_obj)->right);
}
	
EWXWEXPORT(void*, wxLayoutConstraints_bottom)(void* _obj)
{
	return (void*)(&((wxLayoutConstraints*)_obj)->bottom);
}
	
EWXWEXPORT(void*, wxLayoutConstraints_width)(void* _obj)
{
	return (void*)(&((wxLayoutConstraints*)_obj)->width);
}
	
EWXWEXPORT(void*, wxLayoutConstraints_height)(void* _obj)
{
	return (void*)(&((wxLayoutConstraints*)_obj)->height);
}
	
EWXWEXPORT(void*, wxLayoutConstraints_centreX)(void* _obj)
{
	return (void*)(&((wxLayoutConstraints*)_obj)->centreX);
}
	
EWXWEXPORT(void*, wxLayoutConstraints_centreY)(void* _obj)
{
	return (void*)(&((wxLayoutConstraints*)_obj)->centreY);
}
	
EWXWEXPORT(void*, wxLayoutConstraints_Create)()
{
	return (void*) new wxLayoutConstraints();
}

EWXWEXPORT(void, wxIndividualLayoutConstraint_Set)(void* _obj, int rel, void* otherW, int otherE, int val, int marg)
{
	((wxIndividualLayoutConstraint*)_obj)->Set((wxRelationship)rel, (wxWindowBase*) otherW, (wxEdge)otherE, val, marg);
}
	
EWXWEXPORT(void, wxIndividualLayoutConstraint_LeftOf)(void* _obj, void* sibling, int marg)
{
	((wxIndividualLayoutConstraint*)_obj)->LeftOf((wxWindowBase*) sibling, (wxEdge)marg);
}
	
EWXWEXPORT(void, wxIndividualLayoutConstraint_RightOf)(void* _obj, void* sibling, int marg)
{
	((wxIndividualLayoutConstraint*)_obj)->RightOf((wxWindowBase*)sibling, (wxEdge)marg);
}
	
EWXWEXPORT(void, wxIndividualLayoutConstraint_Above)(void* _obj, void* sibling, int marg)
{
	((wxIndividualLayoutConstraint*)_obj)->Above((wxWindowBase*)sibling, marg);
}
	
EWXWEXPORT(void, wxIndividualLayoutConstraint_Below)(void* _obj, void* sibling, int marg)
{
	((wxIndividualLayoutConstraint*)_obj)->Below((wxWindowBase*)sibling, marg);
}
	
EWXWEXPORT(void, wxIndividualLayoutConstraint_SameAs)(void* _obj, void* otherW, int edge, int marg)
{
	((wxIndividualLayoutConstraint*)_obj)->SameAs((wxWindowBase*)otherW, (wxEdge)edge, (wxEdge)marg);
}
	
EWXWEXPORT(void, wxIndividualLayoutConstraint_PercentOf)(void* _obj, void* otherW, int wh, int per)
{
	((wxIndividualLayoutConstraint*)_obj)->PercentOf((wxWindowBase*)otherW, (wxEdge)wh, per);
}
	
EWXWEXPORT(void, wxIndividualLayoutConstraint_Absolute)(void* _obj, int val)
{
	((wxIndividualLayoutConstraint*)_obj)->Absolute(val);
}
	
EWXWEXPORT(void, wxIndividualLayoutConstraint_Unconstrained)(void* _obj)
{
	((wxIndividualLayoutConstraint*)_obj)->Unconstrained();
}
	
EWXWEXPORT(void, wxIndividualLayoutConstraint_AsIs)(void* _obj)
{
	((wxIndividualLayoutConstraint*)_obj)->AsIs();
}
	
EWXWEXPORT(void*, wxIndividualLayoutConstraint_GetOtherWindow)(void* _obj)
{
	return (void*)((wxIndividualLayoutConstraint*)_obj)->GetOtherWindow();
}
	
EWXWEXPORT(int, wxIndividualLayoutConstraint_GetMyEdge)(void* _obj)
{
	return (int)((wxIndividualLayoutConstraint*)_obj)->GetMyEdge();
}
	
EWXWEXPORT(void, wxIndividualLayoutConstraint_SetEdge)(void* _obj, int which)
{
	((wxIndividualLayoutConstraint*)_obj)->SetEdge((wxEdge)which);
}
	
EWXWEXPORT(void, wxIndividualLayoutConstraint_SetValue)(void* _obj, int v)
{
	((wxIndividualLayoutConstraint*)_obj)->SetValue(v);
}
	
EWXWEXPORT(int, wxIndividualLayoutConstraint_GetMargin)(void* _obj)
{
	return ((wxIndividualLayoutConstraint*)_obj)->GetMargin();
}
	
EWXWEXPORT(void, wxIndividualLayoutConstraint_SetMargin)(void* _obj, int m)
{
	((wxIndividualLayoutConstraint*)_obj)->SetMargin(m);
}
	
EWXWEXPORT(int, wxIndividualLayoutConstraint_GetValue)(void* _obj)
{
	return ((wxIndividualLayoutConstraint*)_obj)->GetValue();
}
	
EWXWEXPORT(int, wxIndividualLayoutConstraint_GetPercent)(void* _obj)
{
	return ((wxIndividualLayoutConstraint*)_obj)->GetPercent();
}
	
EWXWEXPORT(int, wxIndividualLayoutConstraint_GetOtherEdge)(void* _obj)
{
	return ((wxIndividualLayoutConstraint*)_obj)->GetOtherEdge();
}
	
EWXWEXPORT(int, wxIndividualLayoutConstraint_GetDone)(void* _obj)
{
	return (int)((wxIndividualLayoutConstraint*)_obj)->GetDone();
}
	
EWXWEXPORT(void, wxIndividualLayoutConstraint_SetDone)(void* _obj, int d)
{
	((wxIndividualLayoutConstraint*)_obj)->SetDone(d != 0);
}
	
EWXWEXPORT(int, wxIndividualLayoutConstraint_GetRelationship)(void* _obj)
{
	return ((wxIndividualLayoutConstraint*)_obj)->GetRelationship();
}
	
EWXWEXPORT(void, wxIndividualLayoutConstraint_SetRelationship)(void* _obj, int r)
{
	((wxIndividualLayoutConstraint*)_obj)->SetRelationship((wxRelationship)r);
}
	
EWXWEXPORT(int, wxIndividualLayoutConstraint_ResetIfWin)(void* _obj, void* otherW)
{
	return (int)((wxIndividualLayoutConstraint*)_obj)->ResetIfWin((wxWindowBase*)otherW);
}
	
EWXWEXPORT(int, wxIndividualLayoutConstraint_SatisfyConstraint)(void* _obj, void* constraints, void* win)
{
	return (int)((wxIndividualLayoutConstraint*)_obj)->SatisfyConstraint((wxLayoutConstraints*)constraints, (wxWindowBase*)win);
}
	
EWXWEXPORT(int, wxIndividualLayoutConstraint_GetEdge)(void* _obj, int which, void* thisWin, void* other)
{
	return ((wxIndividualLayoutConstraint*)_obj)->GetEdge((wxEdge)which, (wxWindowBase*) thisWin, (wxWindowBase*) other);
}
	
}
