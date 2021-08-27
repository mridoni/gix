/*
Copyright (C) 2010-2021 Stefan Frings
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

    _DBG_OUT("Received request for COBOL controller\n");

	QString s = svc->getServerConfig()->getBasePath();

	QScopedPointer<HttpDataManager> interface_in_field(new HttpDataManager(svc, svc->getInterfaceIn()));
	QScopedPointer<HttpDataManager> interface_out_field(new HttpDataManager(svc, svc->getInterfaceOut()));

	if (svc->getInterfaceIn() == NULL || svc->getInterfaceOut() == NULL) { // just in case
		response.setStatus(400);
		response.write("Bad request", true);
        _DBG_OUT("Request validation: failed\n");
		return;
	}

	if (!validateRequest(request, svc->getInterfaceIn())) {
		response.setStatus(400);
		response.write("Bad request", true);
        _DBG_OUT("Request validation: failed\n");
		return;
	}

    _DBG_OUT("Request validation: ok\n");

	uint8_t *cobol_data_in = (uint8_t *)interface_in_field->getDataBuffer();
	uint8_t *cobol_data_out = (uint8_t *)interface_out_field->getDataBuffer();

	QString prg = svc->getProgram();

	// Request -> cobol_data_in
    _DBG_OUT("Setting up request data\n");
	if (interface_in_field.get())
		interface_in_field->setupRequest(request);

    _DBG_OUT("Calling GnuCOBOL module\n");

#if 1

    _DBG_OUT("Resolving entry point for module \"%s\"\n", prg.toLocal8Bit().data());

    void *entry_point = runtime->cob_resolve(prg.toLocal8Bit().constData());
    if (entry_point == NULL) {
        response.setStatus(500);
        response.write("Internal server error", true);
        _DBG_OUT("Cannot resolve module entry point\n");
        return;
    }
    else {
        _DBG_OUT("Module entry point resolved\n");
    }

	int(*func)(uint8_t *, uint8_t *) = (int(*)(uint8_t *, uint8_t *))entry_point;
	rc = func(cobol_data_in, cobol_data_out);
#else
	void *args[2] = { cobol_data_in, &cobol_data_out };
	rc = runtime->cob_call((char *)prg.toLocal8Bit().constData(), 2, (void **)&args);
#endif


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
