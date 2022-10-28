#pragma once
#include "json11.hpp"
#include "any_type.h"

class DebuggerHostMessage
{
protected:
	void set_value(any_type& a, json11::Json v);
};

