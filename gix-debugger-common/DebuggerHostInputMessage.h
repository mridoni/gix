#pragma once

#include <string>

#include "any_type.h"
#include "json11.hpp"
#include "DebuggerHostMessage.h"

class DebuggerHostInputMessage : public DebuggerHostMessage
{
public:

	DebuggerHostInputMessage();
	DebuggerHostInputMessage(const std::string& _cmd);

	std::string cmd;	// for "simple" requests this is enough
	std::string status;	// when replying to a host debugger request

	any_type payload1;
	any_type payload2;
	any_type payload3;
	any_type payload4;
	any_type payload5;

	virtual std::string serialize() const;
	virtual bool deserialize(json11::Json& obj);
	virtual bool deserialize(const std::string& s);
};

