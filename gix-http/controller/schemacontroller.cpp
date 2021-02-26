/**
  @file
  @author Stefan Frings
*/

#include "schemacontroller.h"
#include "ServerConfig.h"
#include "JsonManager.h"
#include "ExternalInterfaceData.h"

#include <QVariant>
#include <QDateTime>
#include <QThread>

SchemaController::SchemaController()
{}

void SchemaController::service(ServiceConfig* svc, HttpRequest& request, HttpResponse& response)
{

    response.setHeader("Content-Type", "text/html; charset=ISO-8859-1");
    response.setCookie(HttpCookie("firstCookie", "hello", 600, QByteArray(), QByteArray(), QByteArray(), false, true));
    response.setCookie(HttpCookie("secondCookie", "world", 600));

    //ExternalInterfaceData * dcp = svc->getInterfaceData();
	JsonManager* jmgr = new JsonManager(svc);
	
    response.write(jmgr->getSchema(SchemaType::In).toUtf8(), true);
}
