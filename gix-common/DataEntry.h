#pragma once

#include <QString>
#include <QList>

#include "gixcommon_global.h"
#include "gix_esql_driver.hh"

class CobolModuleMetadata;

class GIXCOMMON_EXPORT WsReference {
public:

	WsReference();

	int line;
	bool is_write_reference;

};

enum class GIXCOMMON_EXPORT WsEntryType {
	Unknown = 0,	// only for initialization	
	Alphabetic = 1,
	Numeric = 2,
	Alphanumeric = 3,
	Group = 99,
	Filler = 98
};

enum class GIXCOMMON_EXPORT WsEntryStorageType {
	Unknown = 0,	// only for initialization
	Literal = 1,
	Comp3 = 2,
	Comp5 = 3,
	Comp = 4,
	CompX = 5
};

class ListingFileParser;

class GIXCOMMON_EXPORT Paragraph
{
public:
	QString file;
	QString type;
	QString name;
	int line;
	QList<int> referenced_at;
};

class GIXCOMMON_EXPORT DataEntry {

	friend class ListingFileParser;
	friend class CobolModuleMetadata;
	friend class ExternalInterfaceData;

public:

	DataEntry();
	DataEntry(const QString& name, const QString& path);
	virtual ~DataEntry();

	DataEntry *getTopMostParent();

	int computeTotalStorageSize();

	CobolModuleMetadata* owner;

	QString name;
	QString path;
	WsEntryType type;
	int level;
	int storage_size;
	int display_size;
	int offset_data_section;
	int offset_local;
	bool is_signed;
	bool is_placeholder = false;
	int decimals;
	bool is_required;
	QString format;
	WsEntryStorageType storage_type;
	QString storage; // COMP, COMP-3...
	int occurs;
	QString redefines;
	DataEntry* parent;
	QString base_var_name;	// only used by gix-http, debugger uses the corresponding field in VariableResolverData

	QString filename;
	int fileid;
	int line;
	bool included;

	bool not_ref;
	bool ref_by_child;
	bool ref_by_parent;

	QList<DataEntry*> children;
	QList<WsReference*> references;

	bool isFiller();
	bool isGroup();

	static DataEntry *fromCobolRawField(cb_field_ptr p);

private:
	int lst_line;

//#ifdef GIX_HTTP
//	bool is_required;
//#endif

};

