#include "any_type.h"

// 0: std::string,
// 1: int,
// 2: uint64_t,
// 3: bool,
// 4: std::vector<std::string>,
// 5: std::map<std::string, std::string>,
// 6: GixDebuggerSessionConfig
// 7: VariableDisplayData
// 8: std::map<std::string, VariableDisplayData>

json11::Json any_type::to_json() const
{
    switch (this->index()) {
        case 0:
            return json11::Json(std::get<0>(*this));

        case 1:
            return json11::Json(std::get<1>(*this));

        case 2:
            return json11::Json((double)std::get<2>(*this));
        
        case 3:
            return json11::Json(std::get<3>(*this));

        case 4:
            return json11::Json(std::get<4>(*this));

        case 5:
            return json11::Json(std::get<5>(*this));

        case 6:
            return json11::Json(std::get<6>(*this));

        case 7:
            return json11::Json(std::get<7>(*this));

        case 8:
            return json11::Json(std::get<8>(*this));

    }
    return json11::Json::object{};
}

std::string any_type::toString() const
{
    return std::get<0>(*this);
}

int any_type::toInt() const
{
    return std::get<1>(*this);
}

uint64_t any_type::toUInt64() const
{
    return std::get<2>(*this);
}

bool any_type::toBool() const
{
    return std::get<3>(*this);
}

std::vector<std::string> any_type::toStringVector() const
{
    return std::get<4>(*this);
}

std::map<std::string, std::string> any_type::toStringMap() const
{
    return std::get<5>(*this);
}

GixDebuggerSessionConfig any_type::toGixDebuggerSessionConfig() const
{
    return std::get<6>(*this);
}

VariableDisplayData any_type::toVariableDisplayData() const
{
    return std::get<7>(*this);
}

std::map<std::string, VariableDisplayData> any_type::toVariableDisplayDataMap() const
{
    if (this->index() != 8)
        return std::map<std::string, VariableDisplayData>();

    return std::get<8>(*this);
}


