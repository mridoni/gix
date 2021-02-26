#include "HttpDataManager.h"
#include "CobolVar.h"
#include "DataEntry.h"

HttpDataManager::HttpDataManager(ServiceConfig *svc, ExternalInterfaceData *id)
{
	interface_data = id;
	service = svc;
	if (interface_data) {
		data = (void *)calloc(interface_data->getStorageSize(), 1);
		initialize_data();
	}
}

HttpDataManager::~HttpDataManager()
{
	if (data)
		free(data);
}

bool HttpDataManager::setupRequest(HttpRequest& req)
{
	QMap<QString, QByteArray> req_data = service->getRequestParameters(req.getBody(), req.getParameterMap());
	
	//memset(data, 0, interface_data->getStorageSize());

	QMap<QString, QByteArray>::iterator it;
	for (it = req_data.begin(); it != req_data.end(); ++it) {
		QString param_name = it.key();
		QByteArray param_value = it.value();

		DataEntry *e = nullptr;
		// TODO: handle parameter validation (required fields, length, etc.)
		if (entry_name_map.contains(param_name)) {
			e = entry_name_map.value(param_name);
		}
		else {
			if (entry_path_map.contains("LS:" + param_name)) {
				e = entry_path_map.value("LS:" + param_name);
			}
		}

		if (e) {
			CobolVar v;

			init_cobol_var(v, e);
			
			v.setAddr((uint8_t*)data + e->offset_local);
			v.createCobolData(param_value.data());
		}
		else 
			return false;
	}
	return true;
}

void HttpDataManager::init_cobol_var(CobolVar &v, DataEntry *e)
{
	v.setLength(e->display_size);
	if (e->storage_type == WsEntryStorageType::Comp || e->storage_type == WsEntryStorageType::Comp3) {
		v.setType(e->is_signed ? COBOL_TYPE_SIGNED_NUMBER_PD : COBOL_TYPE_UNSIGNED_NUMBER_PD);
	}
	else {
		switch (e->type) {
			case WsEntryType::Alphanumeric:
				v.setType(COBOL_TYPE_ALPHANUMERIC);
				break;

			case WsEntryType::Numeric:
				v.setType(e->is_signed ? COBOL_TYPE_SIGNED_NUMBER_LS : COBOL_TYPE_UNSIGNED_NUMBER);
				break;

			default:
				v.setType(COBOL_TYPE_ALPHANUMERIC);
		}
	}
}

bool HttpDataManager::setupResponse(HttpResponse& resp)
{
	resp.setStatus(200);
	QHash<QString, QString> resp_data;

	if (service->getType().toLower() == "rest") {
		resp.setHeader("Content-Type", "text/json; charset=ISO-8859-1");

		for (DataEntry *e : entry_list) {
			CobolVar v;

			init_cobol_var(v, e);

			v.setAddr((uint8_t *)data + e->offset_local);
			v.createRealData();
			QString rv = v.getRealData();
			resp_data[e->name] = rv;
		}
		JsonManager jmgr(service);
		QString json_data = jmgr.serializeData(resp_data);
		resp.write(json_data.toUtf8().data());

		return true;
	}


	// SOAP goes here
	return false;
}

void *HttpDataManager::getDataBuffer()
{
	return data;
}
//
//QMap<QString, DataEntry*> HttpDataManager::getEntryMap()
//{
//	QList<DataEntry*>::iterator it;
//	QList<DataEntry*> t_res;
//	QMap<QString, DataEntry*> res;
//
//	//this->add_entries(this->getEntryList(), t_res);
//
//	for (it = t_res.begin(); it != t_res.end(); ++it)
//		res[(*it)->name] = (*it);
//
//	return res;
//}

//void HttpDataManager::add_entries(QList<DataEntry*> list, QList<DataEntry*>& res)
//{
//	QList<DataEntry*>::iterator it;
//
//	for (it = list.begin(); it != list.end(); ++it) {
//		DataEntry* e = (*it);
//		if (!e->isGroup())
//			res.append(e);
//		else
//			add_entries(e->children, res);
//	}
//}

void flatten_entry_tree(QList<DataEntry *> &f_entries, const QList<DataEntry *> &entries)
{
	for (DataEntry *e : entries) {
		f_entries.append(e);
		if (!e->children.isEmpty()) {
			flatten_entry_tree(f_entries, e->children);
		}
	}
}

void HttpDataManager::initialize_data()
{
	if (!data || !interface_data || !interface_data->getStorageSize())
		return;

	int sz = interface_data->getStorageSize();
	
	flatten_entry_tree(entry_list, QList<DataEntry *>({ interface_data->getEntryTree() }));
	for (DataEntry *e : entry_list) {
		entry_name_map.insert(e->name, e);
		entry_path_map.insert(e->path, e);
	}


	for (DataEntry *e : entry_list) {
		uint64_t ptr = (uint64_t)data;
		
		switch (e->type) {
			case WsEntryType::Alphabetic:
			case WsEntryType::Alphanumeric:
				memset((void *)(ptr + e->offset_local), ' ', e->storage_size);
				break;

			case WsEntryType::Numeric:
				switch (e->storage_type) {
					case WsEntryStorageType::Literal:
						memset((void *)(ptr + e->offset_local), '0', e->storage_size);
						break;

					case WsEntryStorageType::Comp3:
					case WsEntryStorageType::Comp5:
					case WsEntryStorageType::CompX:
						memset((void *)(ptr + e->offset_local), 0, e->storage_size);
						break;
				}
				break;
		}
	}
}
