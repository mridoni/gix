#pragma once

#include "httprequest.h"
#include "httpresponse.h"
#include "httprequesthandler.h"

using namespace stefanfrings;

/**
  This controller dumps the received HTTP request in the response.
*/

class SchemaController : public HttpRequestHandler
{
    Q_OBJECT
        Q_DISABLE_COPY(SchemaController)
public:

    /** Constructor */
    SchemaController();

    /** Generates the response */
    void service(ServiceConfig* svc, HttpRequest& request, HttpResponse& response);
};
