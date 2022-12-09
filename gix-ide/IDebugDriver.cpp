#include "IDebugDriver.h"
#include "GixDebuggerSessionConfig.h"

const QString IDebugDriver::CMD_DEBUGGER_STARTING = "DBGRSTARTING";
const QString IDebugDriver::CMD_GET_BREAKPOINTS = "GETBRKPS";
const QString IDebugDriver::CMD_DBGRBREAK = "DBGRBREAK";
const QString IDebugDriver::CMD_DBGR_MOD_CHANGED = "DBGRMODCHGD";
const QString IDebugDriver::CMD_DBGR_MOD_EXIT = "DBGRMODEXIT";
const QString IDebugDriver::CMD_DBGR_PRG_EXIT = "DBGRPRGEXIT";
const QString IDebugDriver::CMD_GET_CURPOS = "GETCURPOS";
const QString IDebugDriver::CMD_GET_VAR = "GETVAR:";
const QString IDebugDriver::CMD_GET_VARS = "GETVARS:";
const QString IDebugDriver::CMD_STEP = "STEP";
const QString IDebugDriver::CMD_STEP_INTO = "STEPINTO";
const QString IDebugDriver::CMD_CONTINUE = "CONTINUE";
const QString IDebugDriver::CMD_ABORT = "ABORT";
const QString IDebugDriver::RESP_OK = "OK";
const QString IDebugDriver::RESP_OK_WITH_RESULT = "OK:";
const QString IDebugDriver::RESP_KO = "KO";
const QString IDebugDriver::RESP_KO_WITH_RESULT = "KO:";


void IDebugDriver::acquireWaitLock()
{
	std::unique_lock<std::mutex> lk(cv_m);
	cv_on_break.wait(lk);
}

void IDebugDriver::releaseWaitLock()
{
    cv_on_break.notify_one();
}

void IDebugDriver::setSessionConfiguration(GixDebuggerSessionConfig* cfg)
{
	debug_session_config = cfg;
}

void IDebugDriver::setLogger(std::shared_ptr<spdlog::logger> l)
{
	logger = l;
}
