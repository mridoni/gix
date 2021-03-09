/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
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

#include "WebModuleMetadata.h"
#include "GixGlobals.h"
#include "IGixLogManager.h"
#include "SymbolMappingEntry.h"
#include <linq/linq.hpp>

#include <QFile>
#include <QDataStream>

#define FLAG_M_BASE					0
#define FLAG_M_PREPROCD_ESQL		1
#define FLAG_M_PREPROCED_CONS_SRC	2

WebModuleMetadata::WebModuleMetadata()
{}

WebModuleMetadata::~WebModuleMetadata()
{}

GIXCOMMON_EXPORT WebModuleMetadata *WebModuleMetadata::execute(CobolModuleMetadata * cmm)
{
	int map_process_type = 0;
	IGixLogManager *logger = GixGlobals::getLogManager();

	WebModuleMetadata *wmm = new WebModuleMetadata();
	wmm->flags = FLAG_M_BASE;
	wmm->module_name = cmm->getModuleName();
	wmm->format_version = WEBM_FILE_FMT_VER_1_0;

	wmm->ws_entries = cmm->getWorkingStorageDataEntries();
	wmm->ls_entries = cmm->getLinkageDataEntries();
	wmm->fs_entries = cmm->getFileDataEntries();

	bool is_esql = cmm->isPreprocessedESQL();

	wmm->syms_to_dbg_syms = cmm->getSymbolMappingTable();

	if (is_esql)
		wmm->flags |= FLAG_M_PREPROCD_ESQL;

	return wmm;
}

GIXCOMMON_EXPORT WebModuleMetadata *WebModuleMetadata::loadFromFile(const QString &filename)
{
	QString file_full_path;

	QFile f(filename);
	if (!f.open(QIODevice::OpenModeFlag::ReadOnly))
		return NULL;

	QDataStream s(&f);
	WebModuleMetadata *wmm = new WebModuleMetadata();

	s >> wmm->format_version;
	s >> wmm->flags;
	s >> wmm->module_name;


	load_data_entries(wmm, wmm->ws_entries, s);
	load_data_entries(wmm, wmm->ls_entries, s);
	load_data_entries(wmm, wmm->fs_entries, s);

	load_symbol_mapping_entries(wmm, s);

	f.close();

	return wmm;
}

GIXCOMMON_EXPORT bool WebModuleMetadata::dumpToFile(const QString &filename)
{
	QFile f(filename);
	if (!f.open(QIODevice::OpenModeFlag::WriteOnly))
		return false;

	QDataStream s(&f);

	s << format_version;
	s << flags;
	s << module_name;

	dump_data_entries(ws_entries, s);
	dump_data_entries(ls_entries, s);
	dump_data_entries(fs_entries, s);

	dump_symbol_mapping_entries(s);

	f.close();

	return true;
}


void WebModuleMetadata::dump_data_entries(const QList<DataEntry *> entries, QDataStream &s)
{
	QList<DataEntry *> f_entries;
	flattenEntryTree(f_entries, entries);

	s << f_entries.size();
	for (int i = 0; i < f_entries.size(); i++) {
		dump_data_entry(f_entries.at(i), s);
	}
}

void WebModuleMetadata::dump_data_entry(const DataEntry *e, QDataStream &s)
{
	s << e->name;
	s << e->type;
	s << e->level;
	s << e->storage_size;
	s << e->display_size;
	s << e->offset_data_section;
	s << e->offset_local;
	s << e->is_signed;
	s << e->decimals;
	s << e->is_required;
	s << e->format;
	s << e->storage_type;
	s << e->storage; // COMP, COMP-3...
	s << e->occurs;
	s << e->redefines;
	s << ((e->parent != nullptr) ? e->parent->path : ".");
	s << e->path;

	s << e->fileid;
	s << e->line;
	s << e->included;

	s << e->not_ref;
	s << e->ref_by_child;
	s << e->ref_by_parent;

	s << (e->children.size());
	for (DataEntry *c : e->children)
		s << c->path;

	s << (e->references.size());
	for (WsReference *r : e->references) {
		s << r->line;
		s << r->is_write_reference;
	}
}

void WebModuleMetadata::flattenEntryTree(QList<DataEntry *> &f_entries, const QList<DataEntry *> &entries)
{
	for (DataEntry *e : entries) {
		f_entries.append(e);
		if (!e->children.isEmpty()) {
			flattenEntryTree(f_entries, e->children);
		}
	}
}

void WebModuleMetadata::dump_symbol_mapping_entries(QDataStream &s)
{
	s << syms_to_dbg_syms.size();
	for (SymbolMappingEntry *sme : syms_to_dbg_syms) {
		s << sme->id;
		s << sme->cbl_var;
		s << sme->storage_len;
	}
}

void WebModuleMetadata::load_symbol_mapping_entries(WebModuleMetadata *cmm, QDataStream &s)
{
	int nitems;
	s >> nitems;
	for (int i = 0; i < nitems; i++) {
		SymbolMappingEntry *sme = new SymbolMappingEntry();
		s >> sme->id;
		s >> sme->cbl_var;
		s >> sme->storage_len;
		cmm->syms_to_dbg_syms.append(sme);
	}
}


void WebModuleMetadata::load_data_entries(const WebModuleMetadata *cmm, QList<DataEntry *> &entries, QDataStream &s)
{
	int nentries;
	QList<DataEntry *> f_entries;
	QMap<QString, DataEntry *> entry_name_map;
	QMap<QString, QStringList> relation_map;	// Entry #0 : parent, Entry #1+ : children
	IGixLogManager *logger = GixGlobals::getLogManager();

	s >> nentries;
	for (int i = 0; i < nentries; i++) {
		DataEntry *e = new DataEntry();
		load_data_entry(e, relation_map, s);
		//e->owner = (WebModuleMetadata *)cmm;
		e->owner = nullptr;
		f_entries.append(e);
		entry_name_map[e->path] = e;
	}

	//assign_file_names(f_entries, cmm->filemap);

	for (DataEntry *e : f_entries) {
		if (!e || e->path.isEmpty() || !relation_map.contains(e->path) || relation_map.size() == 0) {
			logger->logMessage(GIX_CONSOLE_LOG, QCoreApplication::translate("gix", "Warning: bad entry in symbol file"), QLogger::LogLevel::Warning);
			continue;
		}

		e->parent = entry_name_map[relation_map[e->path].at(0)];

		for (int i = 1; i < relation_map[e->path].size(); i++) {
			e->children.append(entry_name_map[relation_map[e->path].at(i)]);
		}
	}

	auto l = QList<DataEntry *>::fromStdList(cpplinq::from(f_entries).where([](DataEntry *e) { return e->parent == nullptr; }).to_list());
	entries.append(l);
}

void WebModuleMetadata::load_data_entry(DataEntry *e, QMap<QString, QStringList> &rmap, QDataStream &s)
{
	int count;
	QString parent_path, rel;

	e->parent = nullptr;

	s >> e->name;
	s >> e->type;
	s >> e->level;
	s >> e->storage_size;
	s >> e->display_size;
	s >> e->offset_data_section;
	s >> e->offset_local;
	s >> e->is_signed;
	s >> e->decimals;
	s >> e->is_required;
	s >> e->format;
	s >> e->storage_type;
	s >> e->storage; // COMP, COMP-3...
	s >> e->occurs;
	s >> e->redefines;

	s >> parent_path;
	s >> e->path;

	s >> e->fileid;
	s >> e->line;
	s >> e->included;

	s >> e->not_ref;
	s >> e->ref_by_child;
	s >> e->ref_by_parent;

	QStringList relations;
	relations.append(parent_path);

	s >> count;
	for (int i = 0; i < count; i++) {
		s >> rel;
		relations.append(rel);
	}
	rmap[e->path] = relations;

	s >> count;
	for (int i = 0; i < count; i++) {
		WsReference *r = new WsReference();
		s >> r->line;
		s >> r->is_write_reference;
		e->references.append(r);
	}
}
