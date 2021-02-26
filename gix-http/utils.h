#pragma once

#include "QLogger.h"
#include <QString>

QLogger::LogLevel decode_log_level(QString l);
bool is_valid_log_file(QString f);
