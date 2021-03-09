/*
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/
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

