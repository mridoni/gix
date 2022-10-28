#pragma once

#include <string>
#include <json11.hpp>

#include "NetworkManager.h"
#include "GixDebugger.h"

class DbgrCommandHandler
{

public:
	bool handleCommand(std::shared_ptr<NetworkManager>, const DebuggerHostInputMessage& msg_rcvd, bool* keep_running);

private:
	DebuggerHostDriver* debug_driver = nullptr;
};

