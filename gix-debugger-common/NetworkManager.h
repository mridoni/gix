#pragma once

#include <string>
#include <thread>
#include <mutex>
#include <functional>
#include <stdint.h>

#include <nng/nng.h>
#include <nng/protocol/pair0/pair.h>

#include "DebuggerHostInputMessage.h"
#include "DebuggerHostOutputMessage.h"

class GixDebuggerClient;

enum class NetworkManagerMode {
	NotSet,
	Active,
	Passive
};

class NetworkManager
{
	friend void* NetworkManagerThreadRunnerStub(void* lpParam);

public:

	NetworkManager(NetworkManagerMode m);
	~NetworkManager();

	bool init();
	NetworkManagerMode getMode();

	bool setLocalAddr(const std::string addr);
	bool setLocalPort(uint16_t port);
	bool setLocalUrl(const std::string& url);
	bool setRemoteAddr(const std::string addr);
	bool setRemotePort(uint16_t port);
	bool setRemoteUrl(const std::string& url);

	void setEncryptionEnabled(bool b);

	bool is_initialized();
	bool is_network_server_running();

	// To be used ONLY when sending a complete message (request+reply) to a remote host
	bool send_rcv(std::string msg, std::string& response);
	bool send_rcv(std::string msg, std::string& response, int ms_timeout);

	// To be used ONLY when replying to a HELLO request in passive mode
	bool setup_remote_connection();

	// To be used ONLY when replying to an incoming message (client context)
	bool reply(const DebuggerHostInputMessage& r);
	
	// To be used ONLY when replying to an incoming message (debugger host context)
	bool reply(const DebuggerHostOutputMessage& r);

	// To be used ONLY when replying to a remotely-originated message
	bool emit_response(bool b, const std::string& msg_ok, const std::string& msg_ko);
	bool emit_response_ko(std::string msg);
	bool emit_response_ok(std::string msg);

	std::function<bool(NetworkManager* nws, std::string, bool *response_sent)> onDataReceived;

	std::string last_error;

private:

	NetworkManagerMode mode = NetworkManagerMode::NotSet;

	bool _is_initialized = false;
	bool _is_network_server_running = false;
	bool _is_encrypted = false;

	bool keep_running_server_thread;

	std::string remote_host;
	uint16_t remote_port = 0;

	std::string local_host;
	uint16_t local_port = 0;

	GixDebuggerClient* dbgr_client;

	nng_socket nw_remote_socket;
	nng_socket nw_local_socket;
	std::thread rcv_thread;

	std::string private_rcv_buffer;
	bool is_waiting;

	bool connect_to_remote_host();
	bool disconnect();
	
	uint16_t select_local_port();
	bool _receive_data(const nng_socket& socket, std::string& msg);
	bool _send_data(const nng_socket& socket, const std::string& msg);

	void ReceiverThreadRunner(std::string url);

};

