#include "ServiceManager.h"
#include "ExternalInterfaceData.h"
#include "ServiceModuleInfo.h"
#include "utils.h"
#include "global.h"

#include <QFileInfo>

bool ServiceManager::start(ServiceConfig* svc)
{
	if (!svc->enabled) {
		QLogger::QLog_Info(SERVER_LOG, QString("Service [%1] is not enabled, skipping").arg(svc->name));
		return true;
	}

	if (!is_valid_log_file(svc->log)) {
		QLogger::QLog_Error(SERVER_LOG, QString("Cannot write service log %1, service will not be started").arg(svc->log));
		return false;
	}

	svc->log_module = svc->name;
	log_manager->addDestination(svc->log, svc->log_module, decode_log_level(svc->log_level));

	ServiceModuleInfo *smi = ServiceModuleInfo::load(svc);

	if (!smi) {
		QLogger::QLog_Error(svc->log_module, QString("Cannot parse interface symbols, service will not be started"));
		return false;
	}

	svc->setPrivateData(smi);

	if (!svc->interface_in_field_name.isEmpty()) {
		if (smi->containsEntry(svc->interface_in_field_name) && smi->getEntry(svc->interface_in_field_name)->level == 1 && smi->getEntry(svc->interface_in_field_name)->path.startsWith("LS:")) {
			svc->itf_data_in = ExternalInterfaceData::build(smi, svc->interface_in_field_name);
			if (svc->itf_data_in)
				QLogger::QLog_Info(svc->log_module, QString("Validated and parsed input interface (%1)").arg(svc->interface_in_field_name));
			else {
				QLogger::QLog_Error(svc->log_module, QString("Cannot parse input interface definition (%1), service will not be started").arg(svc->interface_out_field_name));
				return false;
			}
		}
		else {
			QLogger::QLog_Error(svc->log_module, QString("Cannot validate input interface definition (%1), service will not be started").arg(svc->interface_in_field_name));
			return false;
		}
	}
	else {
		QLogger::QLog_Error(svc->log_module, QString("Invalid input interface definition, service will not be started"));
		return false;
	}

	if (!svc->interface_out_field_name.isEmpty()) {
		if (smi->containsEntry(svc->interface_out_field_name) && smi->getEntry(svc->interface_out_field_name)->level == 1 && smi->getEntry(svc->interface_out_field_name)->path.startsWith("LS:")) {
			svc->itf_data_out = ExternalInterfaceData::build(smi, svc->interface_out_field_name);
			if (svc->itf_data_out)
				QLogger::QLog_Info(svc->log_module, QString("Validated and parsed output interface (%1)").arg(svc->interface_out_field_name));
			else {
				QLogger::QLog_Error(svc->log_module, QString("Cannot parse output interface definition (%1), service will not be started").arg(svc->interface_out_field_name));
				return false;
			}
		}
		else {
			QLogger::QLog_Error(svc->log_module, QString("Cannot validate output interface definition (%1), service will not be started").arg(svc->interface_out_field_name));
			return false;
		}
	}
	else {
		QLogger::QLog_Error(svc->log_module, QString("Invalid output interface definition, service will not be started"));
		return false;
	}

	QLogger::QLog_Info(svc->log_module, QString("Started service %1").arg(svc->name));
	QLogger::QLog_Info(SERVER_LOG, QString("Started service %1").arg(svc->name));

	return true;
}
