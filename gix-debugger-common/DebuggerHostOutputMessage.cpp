#include "DebuggerHostOutputMessage.h"
#include "json11.hpp"


DebuggerHostOutputMessage::DebuggerHostOutputMessage(std::string t, std::string s, std::string m)
{
	type = t;
    status = s;
	message = m;
}

std::string DebuggerHostOutputMessage::serialize() const
{
    json11::Json obj = json11::Json::object{
        { "type", type },
        { "status", status },
        { "message", message },
        { "payload1", payload1 },
        { "payload2", payload2 },
        { "payload3", payload3 },
        { "payload4", payload4 },
        { "payload5", payload5 }
    };
    return obj.dump();
}

bool DebuggerHostOutputMessage::deserialize(json11::Json& obj)
{
    if (!obj.is_null()) {
        type = obj["type"].string_value();
        status = obj["status"].string_value();
        message = obj["message"].string_value();
        set_value(payload1, obj["payload1"]);
        set_value(payload2, obj["payload2"]);
        set_value(payload3, obj["payload3"]);
        set_value(payload4, obj["payload4"]);
        set_value(payload5, obj["payload5"]);
        return true;
    }
    return false;
}

bool DebuggerHostOutputMessage::deserialize(const std::string& s)
{
    std::string parse_err;
    json11::Json obj = json11::Json::parse(s, parse_err);
    return deserialize(obj);
}
