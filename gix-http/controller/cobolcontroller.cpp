/*
Copyright (C) 2010 Stefan Frings
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


/**
  @file
  @author Stefan Frings
*/

#include "cobolcontroller.h"
#include "ServerConfig.h"
#include "JsonManager.h"
#include "SysUtils.h"
#include "HttpDataManager.h"

#include <QVariant>
#include <QDateTime>
#include <QThread>

bool CobolController::runtime_initialized = false;

extern RuntimeHelper *runtime;

CobolController::CobolController()
{
	//if (!runtime_initialized) {
	//runtime->cob_init(0, NULL);
	//	runtime_initialized = true;
	//}

}

CobolController::~CobolController()
{
	//runtime->cob_tidy();
}

void CobolController::service(ServiceConfig *svc, HttpRequest &request, HttpResponse &response)
{
	int rc = 0;
	char bfr[256];

	QString s = svc->getServerConfig()->getBasePath();

	QScopedPointer<HttpDataManager> interface_in_field(new HttpDataManager(svc, svc->getInterfaceIn()));
	QScopedPointer<HttpDataManager> interface_out_field(new HttpDataManager(svc, svc->getInterfaceOut()));

	if (svc->getInterfaceIn() == NULL || svc->getInterfaceOut() == NULL) { // just in case
		response.setStatus(400);
		response.write("Bad request", true);
		return;
	}

	if (!validateRequest(request, svc->getInterfaceIn())) {
		response.setStatus(400);
		response.write("Bad request", true);
		return;
	}

#if _DEBUG
	void *cobol_data_in = interface_in_field->getDataBuffer();
	void *cobol_data_out = interface_out_field->getDataBuffer();

	HttpDataManager *iin = interface_in_field.get();
	HttpDataManager *iout = interface_out_field.get();
#endif
	QString prg = svc->getProgram();

	void *entry_point = (void *)runtime->cob_resolve((char *)prg.toLocal8Bit().constData());
	if (entry_point == NULL) {
		response.setStatus(500);
		response.write("Internal server error", true);
		return;
	}

	// Request -> cobol_data_in
	if (interface_in_field.get())
		interface_in_field->setupRequest(request);

	int(*func)(char *, char *) = (int(*)(char *, char *))entry_point;
	rc = func((char *)interface_in_field->getDataBuffer(), (char *)interface_out_field->getDataBuffer());


	if (rc) {
		response.setStatus(500);
		response.write("Internal error", true);
	}
	else {
		interface_out_field->setupResponse(response);
	}
}

bool CobolController::validateRequest(HttpRequest &request, ExternalInterfaceData *itf)
{
	return true;
}
