#pragma once

#include <string>
#include <queue>
#include <utility>
#include <vector>
#include <iostream>

#include "spdlog/sinks/base_sink.h"
#include "NetworkManager.h"
#include "debugger-msg-defs.h"
#include "GixGlobals.h"
#include "OutputWindowLogger.h"

template<typename Mutex>
class ide_sink : public spdlog::sinks::base_sink <Mutex>
{
public:
	ide_sink();

	void setWindowPane(OutputWindowLogger* p) { pane = p; }

protected:
	void sink_it_(const spdlog::details::log_msg& msg) override
	{

		// log_msg is a struct containing the log entry info like level, timestamp, thread id etc.
		// msg.raw contains pre formatted log

		// If needed (very likely but not mandatory), the sink formats the message before sending it to its final destination:
		//spdlog::memory_buf_t formatted;
		//spdlog::sinks::base_sink<Mutex>::formatter_->format(msg, formatted);

		//#LOG
		// std::string s(msg.payload.data(), msg.payload.size());
		// GixGlobals::getLogManager()->logMessage(GIX_CONSOLE_LOG, QString::fromStdString(s), QLogger::LogLevel::Debug);

		std::string s(msg.payload.data(), msg.payload.size());
		pane->getWindowPane()->append(QString::fromStdString(s));
	}

	void flush_() override
	{
		std::cout << std::flush;
	}

private:
	OutputWindowLogger* pane = nullptr;
};

#include "spdlog/details/null_mutex.h"
#include <mutex>
using ide_sink_mt = ide_sink<std::mutex>;
using ide_sink_st = ide_sink<spdlog::details::null_mutex>;

template<typename Mutex>
inline ide_sink<Mutex>::ide_sink()
{
}
