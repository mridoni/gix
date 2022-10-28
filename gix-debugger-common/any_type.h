#pragma once

#include <string>
#include <map>
#include <variant>

#include "json11.hpp"
#include "GixDebuggerSessionConfig.h"

class GixDebuggerSessionConfig;

using var = std::variant<
							std::string, 
							int, 
							uint64_t, 
							bool, 
							std::vector<std::string>, 
							std::map<std::string, std::string>, 
							GixDebuggerSessionConfig,
							VariableDisplayData,
							std::map<std::string, VariableDisplayData>
>;

struct any_type : public var {

	using var::var;
	using var::operator=;

	json11::Json to_json() const;

	std::string toString() const; 
	int toInt() const;
	uint64_t toUInt64() const;
	bool toBool() const;
	std::vector<std::string> toStringVector() const;
	std::map<std::string, std::string> toStringMap() const;
	GixDebuggerSessionConfig toGixDebuggerSessionConfig() const;
	VariableDisplayData toVariableDisplayData() const;
	std::map<std::string, VariableDisplayData> toVariableDisplayDataMap() const;
};
