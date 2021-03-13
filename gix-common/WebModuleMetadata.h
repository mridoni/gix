/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
*/

#pragma once

#include "gixcommon_global.h"
#include "CobolModuleMetadata.h"

#define WEBM_FILE_FMT_VER_1_0 ((uint16_t) 0x0100)

class WebModuleMetadata
{
public:
	GIXCOMMON_EXPORT WebModuleMetadata();
	GIXCOMMON_EXPORT ~WebModuleMetadata();

	GIXCOMMON_EXPORT static WebModuleMetadata *execute(CobolModuleMetadata *cmm);
	GIXCOMMON_EXPORT static WebModuleMetadata *loadFromFile(const QString &filename);

	GIXCOMMON_EXPORT bool dumpToFile(const QString &filename);

private:
	int format_version;
	uint32_t flags;

	bool is_esql_preprocessed;

	QString original_source_file;
	QString running_source_file;

	QString sym_file;
	QString module_name;

	QList<DataEntry *> ws_entries;
	QList<DataEntry *> ls_entries;
	QList<DataEntry *> fs_entries;

	QList<SymbolMappingEntry *> syms_to_dbg_syms;

	void dump_data_entries(const QList<DataEntry *> entries, QDataStream &s);
	void dump_data_entry(const DataEntry *e, QDataStream &s);
	void flattenEntryTree(QList<DataEntry *> &f_entries, const QList<DataEntry *> &entries);
	void dump_symbol_mapping_entries(QDataStream &s);

	static void load_data_entries(const WebModuleMetadata *cmm, QList<DataEntry *> &entries, QDataStream &s);
	static void load_symbol_mapping_entries(WebModuleMetadata *cmm, QDataStream &s);
	static void load_data_entry(DataEntry *e, QMap<QString, QStringList> &rmap, QDataStream &s);
};

