#include "DebuggerHostInputMessage.h"

DebuggerHostInputMessage::DebuggerHostInputMessage()
{
}

DebuggerHostInputMessage::DebuggerHostInputMessage(const std::string& _cmd)
{
	cmd = _cmd;
}

std::string DebuggerHostInputMessage::serialize() const
{
	json11::Json obj = json11::Json::object{
		{ "cmd", cmd },
		{ "status", status },
		{ "payload1", payload1 },
		{ "payload2", payload2 },
		{ "payload3", payload3 },
		{ "payload4", payload4 },
		{ "payload5", payload5 }
	};
	return obj.dump();
}

bool DebuggerHostInputMessage::deserialize(json11::Json& obj)
{
	if (!obj.is_null()) {
		cmd = obj["cmd"].string_value();
		status = obj["status"].string_value();
		set_value(payload1, obj["payload1"]);
		set_value(payload2, obj["payload2"]);
		set_value(payload3, obj["payload3"]);
		set_value(payload4, obj["payload4"]);
		set_value(payload5, obj["payload5"]);
		return true;
	}
	return false;
}

bool DebuggerHostInputMessage::deserialize(const std::string& s)
{
	std::string parse_err;
	json11::Json obj = json11::Json::parse(s, parse_err);
	return deserialize(obj);
}
