#pragma once

#include "fmt/format.h"
#include "gixcommon_global.h"

#include <QString>

template <> struct fmt::formatter<QString> {

    constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) {
        return ctx.end();
    }

    template <typename FormatContext>
    auto format(const QString& p, FormatContext& ctx) const -> decltype(ctx.out()) {
        return fmt::format_to(ctx.out(), "{}", p.toStdString());
    }

};
