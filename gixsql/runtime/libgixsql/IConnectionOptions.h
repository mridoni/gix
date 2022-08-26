#pragma once

#include <string>

struct IConnectionOptions
{
	bool autocommit = false;
	bool fixup_parameters = false;
	std::string client_encoding;
};

