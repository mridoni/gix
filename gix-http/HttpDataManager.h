#pragma once

#include "ExternalInterfaceData.h"
#include "httprequest.h"
#include "httpresponse.h"
#include "DataEntry.h"
#include "JsonManager.h"
#include "ServiceModuleInfo.h"

using namespace stefanfrings;

class CobolVar;
class DataEntry;

class HttpDataManager
{
public:
	HttpDataManager(ServiceConfig *svc, ExternalInterfaceData *id);
	~HttpDataManager();

	bool setupRequest(HttpRequest& req);
	
	bool setupResponse(HttpResponse& req);

	void *getDataBuffer();

private:
	ExternalInterfaceData *interface_data = nullptr;
	QMap<QString, DataEntry*> entry_name_map;
	QMap<QString, DataEntry*> entry_path_map;
	QList<DataEntry *> entry_list;

	void *data = nullptr;
	ServiceConfig *service = nullptr;
	void initialize_data();
	void init_cobol_var(CobolVar &v, DataEntry *e);
};

