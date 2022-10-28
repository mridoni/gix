#pragma once

#include <string>


class GixDbgrClientUtils
{
public:
	static bool startDebuggerService(const std::string& debugger_path = std::string());
};


