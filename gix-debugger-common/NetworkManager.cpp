#include "NetworkManager.h"
#include "debugger-msg-defs.h"

#include "spdlog/spdlog.h"

#include <thread>
#include <sstream>

typedef struct thread_data {
	NetworkManager* network_server;
	std::string url;
} THREADDATA;

#ifdef _WIN32
    #define THREAD_ID GetCurrentThreadId
#else
    #define THREAD_ID pthread_self
#endif


void* NetworkManagerThreadRunnerStub(void* lpParam);

NetworkManager::NetworkManager(NetworkManagerMode m)
{
	mode = m;
}

NetworkManager::~NetworkManager()
{
	spdlog::debug("Network Manager is terminating, stopping server thread");
	keep_running_server_thread = false;

	spdlog::debug("Network Manager is terminating, closing all sockets");
	nng_closeall();
}

bool NetworkManager::init()
{
	if (mode == NetworkManagerMode::NotSet) {
		spdlog::error("Network manager mode not set");
		last_error = "network manager mode not set";
		return false;
	}

	spdlog::debug("Network Manager Mode is: {}", (mode == NetworkManagerMode::Active ? "active" : "passive"));

	if (mode == NetworkManagerMode::Active) {

		if (remote_host.empty() || !remote_port) {
			spdlog::error("Invalid host address [" + remote_host + ":" + std::to_string(remote_port) + "]");
			last_error = "invalid host address [" + remote_host + ":" + std::to_string(remote_port) + "]";
			return false;
		}

		if (!connect_to_remote_host()) {
			spdlog::error("Cannot connect to [" + remote_host + ":" + std::to_string(remote_port) + "]");
			last_error = "cannot connect to [" + remote_host + ":" + std::to_string(remote_port) + "]";
			return false;
		}
	}
	else {
		if (local_host.empty() || !local_port) {
			spdlog::error("Invalid host address [" + remote_host + ":" + std::to_string(remote_port) + "]");
			last_error = "invalid host address [" + remote_host + ":" + std::to_string(remote_port) + "]";
			return false;
		}
	}

	spdlog::trace("Starting server thread");

	THREADDATA* thread_data = (THREADDATA*)calloc(sizeof(THREADDATA), 1);
	if (!thread_data) {
		spdlog::error("Cannot start server thread");
		last_error = "cannot start server thread";
		return false;
	}

	if (mode == NetworkManagerMode::Active) {
		if (local_host.empty() || local_port == 0) {
			uint16_t lp = select_local_port();
			if (!lp) {
				nng_closeall();
				return false;
			}
			local_port = lp;
			local_host = DBGR_LOCAL_BINDING_DEFAULT_ADDR;
			spdlog::info("Binding address not set by client, using {}:{}", local_host, local_port);
		}
	}

	if (local_host == "0.0.0.0") {
		local_host = "127.0.0.1";
	}

	auto local_url = "tcp://" + local_host + ":" + std::to_string(local_port);

	thread_data->network_server = this;
	thread_data->url = local_url;

	std::thread nw_thread(NetworkManagerThreadRunnerStub, thread_data);
	nw_thread.detach();

	std::this_thread::sleep_for(std::chrono::milliseconds(1000));

	// Check if server was spun up successfully
	if (!_is_network_server_running) {
		spdlog::error("Server not initialized");
		last_error = "server not initialized";
		return false;
	}

	if (mode == NetworkManagerMode::Active) {
		std::string resp_content;
		DebuggerHostInputMessage m(DBGR_MSG_IN_CMD_HELLO);
		m.payload1 = local_url;
		if (!send_rcv(m.serialize(), resp_content)) {
			spdlog::error("Cannot send message");
			last_error = "cannot send message (" + m.serialize() + ")";
			return false;
		}
	}

	_is_initialized = true;
	last_error.clear();
	return true;
}

NetworkManagerMode NetworkManager::getMode()
{
	return mode;
}

bool NetworkManager::setLocalAddr(const std::string host)
{
	local_host = host;
	return true;
}

bool NetworkManager::setLocalPort(uint16_t port)
{
	local_port = port;
	return true;
}

bool NetworkManager::setRemoteAddr(const std::string host)
{
	if (mode == NetworkManagerMode::Passive) {
		return false;
	}
	remote_host = host;
	return true;
}

bool NetworkManager::setRemotePort(uint16_t port)
{
	if (mode == NetworkManagerMode::Passive) {
		return false;
	}
	remote_port = port;
	return true;
}


void NetworkManager::setEncryptionEnabled(bool b)
{
	this->_is_encrypted = b;
}

bool NetworkManager::connect_to_remote_host()
{
	int rv;
	if ((rv = nng_pair0_open(&this->nw_remote_socket)) != 0) {
		return false;
	}

	std::string url = "tcp://" + remote_host + ":" + std::to_string(remote_port);

	if ((rv = nng_dial(this->nw_remote_socket, url.c_str(), NULL, 0)) != 0) {
		spdlog::error("Cannot connect to {}: ({}) {}", url, rv, nng_strerror(rv));
		last_error = fmt::format("Cannot connect to {}: ({}) {}", url, rv, nng_strerror(rv));
		return false;
	}

	return true;
}

bool NetworkManager::disconnect()
{

	return true;
}

uint16_t NetworkManager::select_local_port()
{
	//TODO: make it random
	return DBGR_LOCAL_BINDING_DEFAULT_PORT;
}

bool NetworkManager::is_initialized()
{
	return _is_initialized;
}

bool NetworkManager::is_network_server_running()
{
	return _is_network_server_running;
}

bool NetworkManager::send_rcv(std::string msg, std::string& response)
{
	int rv;
	char* buf = NULL;
	size_t sz;

	spdlog::trace("Sending: {}", msg);
	if (!_send_data(this->nw_remote_socket, msg)) {
		spdlog::trace("Data sent");
		return false;
	}

	if (!_receive_data(this->nw_remote_socket, response)) {
		spdlog::trace("Invalid reply or no reply");
		return false;
	}
	spdlog::trace("Received reply: {}", response);

	return true;
}

// This must ONLY be used during the shutdown phase
// it fails at the first error it encounters
bool NetworkManager::send_rcv(std::string msg, std::string& response, int ms_timeout)
{
	int rv;
	char* buf = NULL;
	size_t sz;

	nng_duration cur_r_timeout;
	nng_duration cur_s_timeout;

	if (nng_getopt_ms(this->nw_remote_socket, NNG_OPT_RECVTIMEO, &cur_r_timeout)) {
		last_error = "nng_getopt_ms failed";
		return false;
	}

	if (nng_getopt_ms(this->nw_remote_socket, NNG_OPT_SENDTIMEO, &cur_s_timeout)) {
		last_error = "nng_getopt_ms failed";
		return false;
	}

	if (nng_setopt_ms(this->nw_remote_socket, NNG_OPT_RECVTIMEO, ms_timeout)) {
		last_error = "nng_setopt_ms failed";
		return false;
	}

	if (nng_setopt_ms(this->nw_remote_socket, NNG_OPT_SENDTIMEO, ms_timeout)) {
		last_error = "nng_setopt_ms failed ";
		return false;
	}

	spdlog::trace("Sending: {}", msg);
	if (!_send_data(this->nw_remote_socket, msg)) {
		spdlog::trace("Data sent");
		last_error = "data sent";
		return false;
	}

	if (!_receive_data(this->nw_remote_socket, response)) {
		spdlog::trace("Invalid reply or no reply");
		last_error = "invalid reply or no reply";
		return false;
	}

	if (nng_setopt_ms(this->nw_local_socket, NNG_OPT_RECVTIMEO, cur_r_timeout)) {
		last_error = "nng_setopt_ms failed";
		return false;
	}

	if (nng_setopt_ms(this->nw_local_socket, NNG_OPT_SENDTIMEO, cur_s_timeout)) {
		last_error = "nng_setopt_ms failed";
		return false;
	}

	spdlog::trace("Received reply: {}", response);

	return true;
}

bool NetworkManager::setup_remote_connection()
{
	return connect_to_remote_host();
}


// client context
bool NetworkManager::reply(const DebuggerHostInputMessage& r)
{
	return _send_data(this->nw_local_socket, r.serialize());
}

// debugger host context
bool NetworkManager::reply(const DebuggerHostOutputMessage& r)
{
	return _send_data(this->nw_local_socket, r.serialize());
}

bool NetworkManager::emit_response(bool b, const std::string& msg_ok, const std::string& msg_ko)
{
	return (b ? emit_response_ok(msg_ok) : emit_response_ok(msg_ko));
}

bool NetworkManager::emit_response_ko(std::string msg)
{
	int rv;
	DebuggerHostOutputMessage ko(DBGR_MSG_OUT_TYPE_STATUS, DBGR_STATUS_KO, msg);
	std::string data = ko.serialize();
	spdlog::trace("Replying: {}", data);
	return _send_data(this->nw_local_socket, data);
}

bool NetworkManager::emit_response_ok(std::string msg)
{
	int rv;
	DebuggerHostOutputMessage ok(DBGR_MSG_OUT_TYPE_STATUS, DBGR_STATUS_OK, msg);
	std::string data = ok.serialize();
	spdlog::trace("Replying: {}", data);
	return _send_data(this->nw_local_socket, data);
}

bool NetworkManager::_send_data(const nng_socket& socket, const std::string& msg)
{
	int rv;
	if ((rv = nng_send(socket, (void*)msg.c_str(), msg.size(), 0)) != 0) {
		spdlog::error("Error while sending data: ({}) {}", rv, nng_strerror(rv));
		last_error = fmt::format("error while sending data: ({}) {}", rv, nng_strerror(rv));
		return false;
	}
	return true;
}

bool NetworkManager::_receive_data(const nng_socket& socket, std::string& msg)
{
	char* buf = NULL;
	int rv;
	size_t sz;

#if 1
	if ((rv = nng_recv(socket, &buf, &sz, NNG_FLAG_ALLOC)) == 0) {
		msg = std::string(buf, sz);
		nng_free(buf, sz);
		return true;
	}
	else {
		msg = nng_strerror(rv);
		return false;
	}
#else
	while (true) {
		rv = nng_recv(socket, &buf, &sz, NNG_FLAG_NONBLOCK | NNG_FLAG_ALLOC);
		switch (rv) {
		case NNG_EAGAIN:
			break;

		case 0:
			msg = std::string(buf, sz);
			nng_free(buf, sz);
			return true;

		default:
			msg = nng_strerror(rv);
			return false;
		}
		std::this_thread::sleep_for(std::chrono::microseconds(3));
	}

	return false;
#endif
}


void* NetworkManagerThreadRunnerStub(void* lpParam)
{
	spdlog::trace("NetworkManagerThreadRunnerStub has taken control (thread id: {})", THREAD_ID());

	THREADDATA* td = (THREADDATA*)lpParam;
	td->network_server->ReceiverThreadRunner(td->url);

	return 0;
}

void NetworkManager::ReceiverThreadRunner(std::string url)
{
	spdlog::trace("Server thread is starting (thread id: {})", THREAD_ID());
	spdlog::trace("Server thread is preparing to listen at {}", url);

	int rv;
	if ((rv = nng_pair0_open(&this->nw_local_socket)) != 0) {
		spdlog::trace("an error occurred while opening the socket: ({}) {}", rv, nng_strerror(rv));
		last_error = fmt::format("an error occurred while opening the socket: ({}) {}", rv, nng_strerror(rv));
		return;
	}

	if ((rv = nng_listen(this->nw_local_socket, url.c_str(), NULL, 0)) != 0) {
		spdlog::trace("an error occurred while listening on the socket: ({}) {}", rv, nng_strerror(rv));
		last_error = fmt::format("an error occurred while listening on the socket: ({}) {}", rv, nng_strerror(rv));
		return;
	}

	if ((rv = nng_setopt_ms(this->nw_local_socket, NNG_OPT_RECVTIMEO, 3600 * 1000)) != 0) {
		spdlog::trace("socket configuration error");
		last_error = "";
		return;
	}

	//if ((rv = nng_pipe_notify(this->nw_local_socket, NNG_PIPE_EV_ADD_POST, nullptr, this) != 0)) {
	//	spdlog::trace("socket callbackconfiguration error");
	//	return;
	//}

	spdlog::trace("Listening on {}", url);
	_is_network_server_running = true;

	bool response_sent;
	keep_running_server_thread = true;
	while (keep_running_server_thread) {
		std::string msg;
		bool data_available = _receive_data(this->nw_local_socket, msg);
		if (!data_available) {
			spdlog::error("An error occurred while trying to receive data: {}", msg);
			break;
		}

		spdlog::trace("Received: [{}]", msg);

		if (onDataReceived != nullptr) {
			bool res = onDataReceived(this, msg, &response_sent);
			if (!response_sent) {

			}
		}

	}

	spdlog::trace("Network Manager server thread is exiting");

	_is_network_server_running = false;
}

bool NetworkManager::setLocalUrl(const std::string& url)
{
	return false;
}

bool NetworkManager::setRemoteUrl(const std::string& url)
{
	nng_url* url_data;
	if (nng_url_parse(&url_data, url.c_str()))
		return false;

	this->setEncryptionEnabled((strcmp(url_data->u_scheme, "tls+tcp") == 0));

	// the setter is for public use ONLY
	remote_host = url_data->u_hostname;
	remote_port = atoi(url_data->u_port);

	nng_url_free(url_data);
	return true;
}
