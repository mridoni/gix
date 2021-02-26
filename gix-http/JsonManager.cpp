#include "JsonManager.h"
#include "ExternalInterfaceData.h"
#include "ServiceModuleInfo.h"
#include "DataEntry.h"
#include "linq/linq.hpp"

#include <QList>

JsonManager::JsonManager(ServiceConfig *svc)
{
	service = svc;
}

QString JsonManager::getSchema(SchemaType schema_type)
{

	QJsonObject  jobj;
	QJsonObject  jprops;

	QString field_id = schema_type == SchemaType::In ? service->getInterfaceInFieldName() : service->getInterfaceOutFieldName();

	ServiceModuleInfo *smi = (ServiceModuleInfo *)service->getPrivateData();
	DataEntry *e = smi->buildInterfaceEntryTree(field_id);

	QString suffix = (schema_type == SchemaType::In) ? "-in" : "-out";

	jobj["$schema"] = QJsonValue("http://json-schema.org/draft-07/schema#");
	jobj["$id"] = QJsonValue("http://mediumgray.info/gix-http/" + service->name + suffix + "#");
	jobj["title"] = QJsonValue(service->name);
	jobj["description"] = QJsonValue(service->description);
	jobj["type"] = QJsonValue("object");

	QList<DataEntry *> entries;
	entries << e;

	add_entry(jprops, entries);
	jobj["properties"] = jprops;

	QJsonDocument jdoc(jobj);

	return QString::fromUtf8(jdoc.toJson());
}

void JsonManager::add_entry(QJsonObject &jprops, QList<DataEntry *> entries)
{
	QList<DataEntry *>::iterator it;

	for (it = entries.begin(); it != entries.end(); ++it) {
		DataEntry *e = *it;

		QString jpropname = e->name.replace("-", "_").toLower();
		QString jproptype;
		if (e->type == WsEntryType::Numeric) {
			jproptype = (e->decimals > 0) ? "number" : "integer";
		}
		else
			jproptype = "string";

		QJsonObject jprop;
		jprop["description"] = e->name;
		jprop["type"] = jproptype;
		jprop["gix_format"] = e->format;
		jprop["is_signed"] = e->is_signed;
		jprop["has_decimals"] = e->decimals;
		jprop["display_size"] = e->display_size;
		jprop["storage_size"] = e->storage_size;
		jprop["offset"] = e->offset_local;
		jprop["parent"] = e->parent == nullptr ? "" : e->parent->path.mid(3);

		QStringList child_list = QStringList::fromStdList(cpplinq::from(e->children).select([](DataEntry *e) { return e->name;  }).to_list());
		jprop["children"] = child_list.join(',');

		jprops[jpropname] = jprop;

		if (e->type == WsEntryType::Group) {
			add_entry(jprops, e->children);
		}

	}
}

void JsonManager::add_data_entry(QJsonObject &jprops, QHash<QString, QString> &entries)
{
	QHash<QString, QString>::iterator it;

	for (it = entries.begin(); it != entries.end(); ++it) {

		QString jpropname = it.key();
		QString jpropval = it.value();

		jprops[jpropname] = jpropval;
	}
}

QString JsonManager::serializeData(QHash<QString, QString>&entries)
{

	QJsonObject  jobj;
	QJsonObject  jprops;

	add_data_entry(jprops, entries);
	jobj["data"] = jprops;

	QJsonDocument jdoc(jobj);

	return QString::fromUtf8(jdoc.toJson());
}
