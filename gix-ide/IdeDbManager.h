#pragma once

#include <QMap>
#include <QObject>
#include <QString>

#include "IConnectionString.h"
#include "DbConnection.h"

class DbConnection;

class IdeDbManager : public QObject
{
	Q_OBJECT

public:
	IdeDbManager();
	~IdeDbManager();

	bool init();
	bool cleanup();
	bool test(IConnectionString*);
	void newConnection(IConnectionString *, bool save_password);
	void saveConnection(DbConnection *, bool save_password);
	void loadDbConnection(QString conn_pfx);

	static DbConnection* createConnection(IConnectionString*);
private:
	QMap<QString, DbConnection *> connections;



signals:
	void connectionAdded(QString, DbConnection*);

};

