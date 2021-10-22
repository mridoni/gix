#pragma once

#include <string>
#include <vector>

struct ErrorData
{
	int err_code;
	std::vector<std::string> err_messages;
};