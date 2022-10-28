#pragma once

#include <string>

#include "any_type.h"
#include "json11.hpp"
#include "DebuggerHostMessage.h"

class DebuggerHostOutputMessage : public DebuggerHostMessage
{

public:
	
	DebuggerHostOutputMessage() {}
	DebuggerHostOutputMessage(std::string t, std::string s, std::string m);

	// For "simple" communications
	std::string type;
	std::string status;
	std::string message;

	any_type payload1;
	any_type payload2;
	any_type payload3;
	any_type payload4;
	any_type payload5;

	virtual std::string serialize() const;
	virtual bool deserialize(json11::Json& obj);
	virtual bool deserialize(const std::string& s);
};

