/**
  @file
  @author Stefan Frings
*/

#include "global.h"


/** Cache for template files */
TemplateCache* templateCache;

/** Storage for session cookies */
HttpSessionStore* sessionStore;

/** Controller for static files */
StaticFileController* staticFileController;

/** Redirects log messages to a file */
QLogger::QLoggerManager* log_manager;

QString SERVER_LOG;