#pragma once

#include <string>
#include <queue>
#include <utility>
#include <vector>
#include <iostream>

#include "spdlog/sinks/base_sink.h"
#include "NetworkManager.h"
#include "debugger-msg-defs.h"

template<typename Mutex>
class debugger_host_sink : public spdlog::sinks::base_sink <Mutex>
{
public:
	debugger_host_sink();

	void setNetworkManager(std::shared_ptr<NetworkManager> nm);

protected:
	void sink_it_(const spdlog::details::log_msg& msg) override
	{

		// log_msg is a struct containing the log entry info like level, timestamp, thread id etc.
		// msg.raw contains pre formatted log

		// If needed (very likely but not mandatory), the sink formats the message before sending it to its final destination:
		//spdlog::memory_buf_t formatted;
		//spdlog::sinks::base_sink<Mutex>::formatter_->format(msg, formatted);

		std::string s(msg.payload.data(), msg.payload.size());

		if (!network_manager || !network_manager->is_initialized()) {
			//msg_queue.push(std::pair(fmt::to_string(formatted), (int)msg.level));
			msg_queue.push(std::pair(s, (int)msg.level));
			return;
		}

		// First we check for queued messages
		if (msg_queue.size()) {
			while (!msg_queue.empty()) {
				std::pair<std::string, int> p = msg_queue.front();
				dbgr_client_debuggerMessage(p.first, p.second);
				msg_queue.pop();
			}
		}

		dbgr_client_debuggerMessage(s, (int)msg.level);
	}

	void flush_() override
	{
		std::cout << std::flush;
	}

private:
	std::shared_ptr<NetworkManager> network_manager;
	std::queue<std::pair<std::string, int>> msg_queue;

	bool dbgr_client_debuggerMessage(std::string msg, int level);
};

#include "spdlog/details/null_mutex.h"
#include <mutex>
using debugger_host_sink_mt = debugger_host_sink<std::mutex>;
using debugger_host_sink_st = debugger_host_sink<spdlog::details::null_mutex>;

template<typename Mutex>
inline debugger_host_sink<Mutex>::debugger_host_sink()
{
}

template<typename Mutex>
inline void debugger_host_sink<Mutex>::setNetworkManager(std::shared_ptr<NetworkManager> nm)
{
	network_manager = nm;
}

template<typename Mutex>
inline bool debugger_host_sink<Mutex>::dbgr_client_debuggerMessage(std::string msg, int level)
{
	if (!network_manager || !network_manager->is_initialized()) {
		return false;
	}

	DebuggerHostOutputMessage req;
	req.type = DBGR_MSG_OUT_TYPE_MESSAGE;
	req.payload1 = msg;
	req.payload2 = level;
	DebuggerHostInputMessage client_resp;
	std::string resp_string;
	network_manager->send_rcv(req.serialize(), resp_string);
	client_resp.deserialize(resp_string);
	return client_resp.status == DBGR_STATUS_OK;
}
