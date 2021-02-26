#pragma once

#include "httprequest.h"
#include "httpresponse.h"
#include "httprequesthandler.h"
#include "ExternalInterfaceData.h"

#include "RuntimeHelper.h"

using namespace stefanfrings;

/**
  This controller dumps the received HTTP request in the response.
*/

class CobolController : public HttpRequestHandler
{
    Q_OBJECT
        Q_DISABLE_COPY(CobolController)
public:

    /** Constructor */
    CobolController();
    ~CobolController();

    /** Generates the response */
    void service(ServiceConfig* svc, HttpRequest& request, HttpResponse& response);


private:
    static bool runtime_initialized;

    bool validateRequest(HttpRequest& request, ExternalInterfaceData * itf);

	/*RuntimeHelper runtime;*/
};
