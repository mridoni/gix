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

#include "CobolModuleMetadata.h"
#include "DataEntry.h"
#include "Project.h"
#include "PathUtils.h"
#include "GixGlobals.h"
#include "CompilerConfiguration.h"
#include "SysUtils.h"
#include "IGixLogManager.h"
#include "SymbolMappingEntry.h"
#include "TPESQLProcessing.h"
#include "gix_esql_driver.hh"
#include "linq/linq.hpp"

#include <QFile>
#include <QList>
#include <QRegularExpression>
#include <linq/linq.hpp>

#if defined(_WIN32) && defined(_DEBUG)
#include <Windows.h>
#endif
#include <ESQLConfiguration.h>

CobolModuleMetadata::CobolModuleMetadata()
{
	module_name = "";
	flags = FLAG_M_BASE;
	is_preprocessed = false;
	original_module_file_id = 0;
	running_module_file_id = 0;

	working_storage = new DataEntry("WS", "WS");
	working_storage->is_placeholder = true;
	linkage_section = new DataEntry("LS", "LS");
	linkage_section->is_placeholder = true;
	file_section = new DataEntry("FS", "FS");
	file_section->is_placeholder = true;
}

CobolModuleMetadata::~CobolModuleMetadata()
{
	clear();

	delete working_storage;
	delete linkage_section;
	delete file_section;
}

bool CobolModuleMetadata::isPreprocessedESQL()
{
	return flags & FLAG_M_PREPROCD_ESQL;
}

bool CobolModuleMetadata::isUpToDate()
{
	return false;
}

void CobolModuleMetadata::clear()
{
	delete_data_entry_tree(working_storage->children);
	delete_data_entry_tree(linkage_section->children);
	delete_data_entry_tree(file_section->children);

	for (Paragraph *p : paragraphs) {
		if (p)
			delete p;
	}

	paragraphs.clear();


	for (SymbolMappingEntry *p : syms_to_dbg_syms) {
		if (p)
			delete p;
	}
	syms_to_dbg_syms.clear();
}

DataEntry *CobolModuleMetadata::findDefinition(QString srch, bool use_path)
{
	QString search_obj;
	DataEntry *e = nullptr;

	search_obj = srch.startsWith("*:") ? "WS:" + srch.mid(2) : srch;
	e = findEntry(working_storage->children, search_obj, use_path);
	if (e)
		return e;

	search_obj = srch.startsWith("*:") ? "LS:" + srch.mid(2) : srch;
	e = findEntry(linkage_section->children, search_obj, use_path);
	if (e)
		return e;

	search_obj = srch.startsWith("*:") ? "FS:" + srch.mid(2) : srch;
	e = findEntry(file_section->children, search_obj, use_path);
	if (e)
		return e;

	return nullptr;
}

QString CobolModuleMetadata::getDebugLocalSymbolName(QString n)
{
	for (auto sme : syms_to_dbg_syms) {
		if (sme->cbl_var == n)
			return sme->id;
	}
	return QString();
}

GIXCOMMON_EXPORT QList<SymbolMappingEntry *> &CobolModuleMetadata::getSymbolMappingTable() const
{
	return const_cast<QList<SymbolMappingEntry *> &>(syms_to_dbg_syms);
}

void CobolModuleMetadata::delete_data_entry_tree(QList<DataEntry *> l)
{
	for (int i = 0; i < l.size(); i++) {
		DataEntry *e = l.at(i);
		delete e;
	}
}


DataEntry *CobolModuleMetadata::findEntry(QList<DataEntry *> entries, QString srch, bool use_path)
{
	for (DataEntry *e : entries) {
		if (use_path) {
			if (e->path == srch)
				return e;
		}
		else {
			if (srch.contains(":")) {
				if (e->name == srch.mid(srch.indexOf(":") + 1))
					return e;
			}
			else {
				if (e->name == srch)
					return e;
			}
		}

		DataEntry *d = findEntry(e->children, srch, use_path);
		if (d)
			return d;

	}
	return nullptr;
}

//QStringList CobolModuleMetadata::collect_files()
//{
//	return QStringList();
//}


const QList<DataEntry *> &CobolModuleMetadata::getWorkingStorageDataEntries()
{
	return working_storage->children;
}

const QList<DataEntry *> &CobolModuleMetadata::getLinkageDataEntries()
{
	return linkage_section->children;
}

const QList<DataEntry *> &CobolModuleMetadata::getFileDataEntries()
{
	return file_section->children;
}

const QList<DataEntry *> CobolModuleMetadata::getDataEntries()
{
	QList<DataEntry *> entries;
	entries.append(working_storage->children);
	entries.append(linkage_section->children);
	entries.append(file_section->children);
	return entries;
}

const QMap<QString, Paragraph *> &CobolModuleMetadata::getParagraphs()
{
	return paragraphs;
}

bool CobolModuleMetadata::runningToOriginal(int running_module_file_id, int running_line, int *orig_file_id, int *orig_line)
{
	//if (running_line == 0) {
	//	Paragraph *entry_point = cpplinq::from(paragraphs).first_or_default([](Paragraph *p) { return p->type == "E"; });
	//	if (!entry_point)
	//		return false;

	//	running_line = entry_point->line;
	//}

	uint64_t k = ((uint64_t)running_module_file_id << 32) + running_line;

	if (running_to_orig_linemap.contains(k)) {
		uint64_t v = running_to_orig_linemap[k];
		*orig_file_id = (v >> 32) & 0xffffffff;
		*orig_line = v & 0xffffffff;
		return true;
	}

	return false;
}

bool CobolModuleMetadata::originalToRunning(int orig_file_id, int orig_line, int *running_module_file_id, int *running_line)
{
	uint64_t k = ((uint64_t)orig_file_id << 32) + orig_line;

	if (orig_to_running_linemap.contains(k)) {
		uint64_t v = orig_to_running_linemap[k];
		*running_module_file_id = (v >> 32) & 0xffffffff;
		*running_line = v & 0xffffffff;
		return true;
	}

	return false;
}

bool CobolModuleMetadata::getFileById(int id, QString &filename)
{
	if (!filemap.contains(id))
		return false;

	filename = filemap[id];
	return true;
}

bool CobolModuleMetadata::getFileByName(const QString &filename, int *id)
{
	if (!reverse_filemap.contains(filename))
		return false;

	*id = reverse_filemap[filename];
	return true;
}


int CobolModuleMetadata::runningFileId()
{
	return running_module_file_id;
}

int CobolModuleMetadata::originalFileId()
{
	return original_module_file_id;
}

QString CobolModuleMetadata::runningFile()
{
	return filemap[running_module_file_id];
}

QString CobolModuleMetadata::originalFile()
{
	return filemap[original_module_file_id];
}

QString CobolModuleMetadata::getLinkageSection()
{
	return linkage_section_text;
}

QString CobolModuleMetadata::getModuleName()
{
	return module_name;
}

ModuleDebugInfo *CobolModuleMetadata::getDebugInfo()
{
	//return debug_info;
	return nullptr;
}

void CobolModuleMetadata::flattenEntryTree(QList<DataEntry *> &f_entries, const QList<DataEntry *> &entries)
{
	for (DataEntry *e : entries) {
		f_entries.append(e);
		if (!e->children.isEmpty()) {
			flattenEntryTree(f_entries, e->children);
		}
	}
}

void CobolModuleMetadata::assign_file_ids(QList<DataEntry *> &entries)
{
	QMap<QString, int> reverse_filemap;
	for (int k : filemap.keys())
		reverse_filemap[filemap[k]] = k;

	for (DataEntry *e : entries) {
		if (e->isFiller())
			continue;

		if (reverse_filemap.contains(e->filename))
			e->fileid = reverse_filemap[e->filename];
		else {
			e->fileid = 0;
			//Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, QCoreApplication::translate("gix", "%1: cannot locate file %2 in filemap").arg(e->path).arg(e->filename.isEmpty() ? "(N/A)" : e->filename), QLogger::LogLevel::Warning);
		}
	}
}

void CobolModuleMetadata::assign_file_names(QList<DataEntry *> &entries, const QMap<int, QString> &filemap)
{
	for (DataEntry *e : entries) {
		if (e->isFiller())
			continue;

		if (filemap.contains(e->fileid))
			e->filename = filemap[e->fileid];
		else {
			e->filename = QString();
			//Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, QCoreApplication::translate("gix", "%1: cannot locate file id %2 in filemap").arg(e->path).arg(e->fileid), QLogger::LogLevel::Warning);
		}
	}
}

void CobolModuleMetadata::dump_data_entries(const QList<DataEntry *> entries, QDataStream &s)
{
	QList<DataEntry *> f_entries;
	flattenEntryTree(f_entries, entries);
	assign_file_ids(f_entries);

	s << f_entries.size();
	for (int i = 0; i < f_entries.size(); i++) {
		dump_data_entry(f_entries.at(i), s);
	}
}

void CobolModuleMetadata::dump_data_entry(const DataEntry *e, QDataStream &s)
{
	s << e->name;
	s << (int)e->type;
	s << e->level;
	s << e->storage_size;
	s << e->display_size;
	s << e->offset_data_section;
	s << e->offset_local;
	s << e->is_signed;
	s << e->decimals;
	s << e->is_required;
	s << e->format;
	s << (int)e->storage_type;
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

void CobolModuleMetadata::dump_paragraphs(QMap<QString, Paragraph *> paragraphs, QDataStream &s)
{
	s << paragraphs.size();

	QMap<QString, Paragraph *>::iterator it;

	for (it = paragraphs.begin(); it != paragraphs.end(); ++it) {
		s << it.key();
		Paragraph *p = it.value();
		s << p->name;
		s << p->file;
		//s << p->line;
		//s << p->type;
		//s << p->referenced_at.size();
		//for (int r : p->referenced_at) {
		//	s << r;
		//}
	}
}

void CobolModuleMetadata::dump_symbol_mapping_entries(QDataStream &s)
{
	s << syms_to_dbg_syms.size();
	for (SymbolMappingEntry *sme : syms_to_dbg_syms) {
		s << sme->id;
		s << sme->cbl_var;
		s << sme->storage_len;
	}
}

void CobolModuleMetadata::dump_data_entries_as_text(const QList<DataEntry *> entries, QTextStream &s)
{
	for (int i = 0; i < entries.size(); i++) {
		s << QString("\n- DataEntry %1/%2 (%3)\n").arg(i + 1).arg(entries.size()).arg(entries.at(i)->path);
		dump_data_entry_as_text(entries.at(i), s);
	}
}

void CobolModuleMetadata::dump_data_entry_as_text(const DataEntry *e, QTextStream &s)
{
	s << "Name: " << e->name << "\n";
	s << "Path: " << e->path << "\n";
	s << "Type: " << QString::number((int)e->type) << "\n";
	s << "Base var: " << e->base_var_name << "\n";
	s << "Display size: " << e->display_size << "\n";
	s << "Storage size: " << e->storage_size << "\n";
	s << "Storage type: " << QString::number((int)e->storage_type) << "\n";
	s << "Decimals: " << e->decimals << "\n";
	s << "Has sign: " << e->is_signed << "\n";
	s << "Level: " << e->level << "\n";
	s << "Filename: " << e->filename << "\n";
	s << "File ID: " << e->fileid << "\n";
	s << "Line: " << e->line << "\n";
}

void CobolModuleMetadata::dump_paragraphs_as_text(const QMap<QString, Paragraph *> paragraphs, QTextStream &s)
{
	for (int i = 0; i < paragraphs.size(); i++) {
		s << QString("\n- Paragraph %1/%2\n").arg(i + 1).arg(paragraphs.size());
		s << "Name: " << paragraphs.values().at(i)->name << "\n";
		s << "Filename: " << paragraphs.values().at(i)->file << "\n";
		s << "Line: " << paragraphs.values().at(i)->line << "\n";
	}
}

void CobolModuleMetadata::dump_filemap_as_text(const QMap<int, QString> &filemap, QTextStream &s)
{
	s << QString("\n- File map (%1 entries)\n").arg(filemap.size());
	for (int i = 0; i < filemap.size(); i++) {
		int k = filemap.keys().at(i);
		QString v = filemap.value(k);
		s << QString("%1 :  %2\n").arg(k).arg(v);
	}
}

void CobolModuleMetadata::dump_linemap_as_text(const QMap<uint64_t, uint64_t> &linemap, const QString &lm_desc, QTextStream &s)
{
	s << QString("\n- Line map %1 (%2 entries)\n").arg(lm_desc).arg(linemap.size());
	for (int i = 0; i < linemap.size(); i++) {
		uint64_t k = linemap.keys().at(i);
		uint64_t v = linemap.value(k);

		uint32_t in_file_id = k >> 32;
		uint32_t in_lineno = k & 0xffffffff;
		uint32_t out_file_id = v >> 32;
		uint32_t out_lineno = v & 0xffffffff;

		s << QString("%1@%2 => %3@%4\n").arg(in_lineno).arg(filemap.value(in_file_id)).arg(out_lineno).arg(filemap.value(out_file_id));
	}
}

void CobolModuleMetadata::dump_symbol_mapping_entries_as_text(QTextStream &s)
{
	int n = 1;
	for (SymbolMappingEntry *sme : syms_to_dbg_syms) {
		s << QString("\n- Symbol mapping (%1/%2)\n").arg(n++).arg(syms_to_dbg_syms.size());
		s << "Id: " << sme->id << "\n";
		s << "COBOL variable: " << sme->cbl_var << "\n";
		s << "Storage length: " << sme->storage_len << "\n";
	}
}

void CobolModuleMetadata::load_symbol_mapping_entries(CobolModuleMetadata *cmm, QDataStream &s)
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

void CobolModuleMetadata::dump_filemap(const QMap<int, QString> &filemap, QDataStream &s)
{
	s << filemap.size();

	QMap<int, QString>::const_iterator it;
	for (it = filemap.begin(); it != filemap.end(); ++it) {
		s << it.key();
		s << it.value();
	}
}

void CobolModuleMetadata::dump_linemap(const QMap<uint64_t, uint64_t> &orig_to_running_linemap, QDataStream &s)
{
	s << orig_to_running_linemap.size();

	QMap<uint64_t, uint64_t>::const_iterator it;
	for (it = orig_to_running_linemap.begin(); it != orig_to_running_linemap.end(); ++it) {
		s << (quint64)it.key();
		s << (quint64)it.value();
	}
}

void CobolModuleMetadata::load_data_entries(const CobolModuleMetadata *cmm, QList<DataEntry *> &entries, QDataStream &s)
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
		e->owner = (CobolModuleMetadata *)cmm;
		f_entries.append(e);
		entry_name_map[e->path] = e;
	}

	assign_file_names(f_entries, cmm->filemap);

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

void CobolModuleMetadata::load_data_entry(DataEntry *e, QMap<QString, QStringList> &rmap, QDataStream &s)
{
	int count;
	int i;
	QString parent_path, rel;

	e->parent = nullptr;

	s >> e->name;
	s >> i; e->type = (WsEntryType)i;
	s >> e->level;
	s >> e->storage_size;
	s >> e->display_size;
	s >> e->offset_data_section;
	s >> e->offset_local;
	s >> e->is_signed;
	s >> e->decimals;
	s >> e->is_required;
	s >> e->format;
	s >> i; e->storage_type = (WsEntryStorageType)i;
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

void CobolModuleMetadata::load_paragraphs(QMap<QString, Paragraph *> &p_paragraphs, QDataStream &s)
{
	QString p_key;
	int count, refcount, ref;

	s >> count;
	for (int i = 0; i < count; i++) {
		s >> p_key;
		Paragraph *p = new Paragraph();
		s >> p->name;
		s >> p->file;
		//s >> p->line;
		//s >> p->type;

		s >> refcount;
		for (int j = 0; j < refcount; j++) {
			s >> ref;
			//p->referenced_at.append(ref);
		}
		p_paragraphs[p_key] = p;
	}
}

void CobolModuleMetadata::load_filemap(QMap<int, QString> &p_filemap, QDataStream &s)
{
	int count, key;
	QString value;

	s >> count;

	for (int i = 0; i < count; i++) {
		s >> key;
		s >> value;

		p_filemap[key] = value;
	}
}

void CobolModuleMetadata::load_linemap(QMap<uint64_t, uint64_t> &p_linemap, QMap<uint64_t, uint64_t> &p_linemap_rev, QDataStream &s)
{
	int count;
	quint64 k, v;

	s >> count;

	for (int i = 0; i < count; i++) {
		s >> k;
		s >> v;

		p_linemap[k] = v;
	}

	s >> count;

	for (int i = 0; i < count; i++) {
		s >> k;
		s >> v;

		p_linemap_rev[k] = v;
	}
}

//void CobolModuleMetadata::build_linemaps(const QList<QPair<QPair<int, int>, QPair<int, int>>> &orig_to_running_linemap, QMap<uint64_t, uint64_t> &p_orig_to_running_linemap, QMap<uint64_t, uint64_t> &p_running_to_orig_linemap)
//{
//	for (auto e : orig_to_running_linemap) {
//		uint64_t k = ((uint64_t)e.first.first << 32) + e.first.second;
//		uint64_t v = ((uint64_t)e.second.first << 32) + e.second.second;
//
//		p_orig_to_running_linemap[k] = v;
//		p_running_to_orig_linemap[v] = k;
//	}
//}

void CobolModuleMetadata::process_entry_definitions(const QList<DataEntry *> &entries)
{
	for (DataEntry *e : entries) {
		int l = e->lst_line;
		process_entry_definitions(e->children);
	}
}

bool CobolModuleMetadata::dumpToFile(const QString &filename, bool as_text)
{
	if (as_text) {
		return dumpToTextFile(filename);
	}

	QFile f(filename);
	if (!f.open(QIODevice::OpenModeFlag::WriteOnly))
		return false;

	QDataStream s(&f);

	s << format_version;
	s << flags;

	s << module_name;
	s << last_parsed;

	s << original_module_file_id;
	s << running_module_file_id;

	s << copy_deps;

	s << linkage_section_text;

	dump_filemap(filemap, s);
	dump_linemap(orig_to_running_linemap, s);
	dump_linemap(running_to_orig_linemap, s);

	dump_data_entries(working_storage->children, s);
	dump_data_entries(linkage_section->children, s);
	dump_data_entries(file_section->children, s);

	dump_paragraphs(paragraphs, s);

	dump_symbol_mapping_entries(s);

	f.close();

	return true;
}

bool CobolModuleMetadata::dumpToTextFile(const QString &filename)
{
	QFile f(filename);
	if (!f.open(QIODevice::OpenModeFlag::WriteOnly))
		return false;

	QTextStream s(&f);

	s << "Format: " << SysUtils::toHexString((uint32_t)format_version) << '\n';
	s << "Flags: " << SysUtils::toHexString(flags) << '\n';

	s << "Module: " << module_name << '\n';

	QString lp = last_parsed.toString();
	s << "Last parsed: " << (lp.isEmpty() ? "(empty)" : lp) << '\n';

	s << "Original module source ID: " << original_module_file_id << '\n';
	s << "Running module source ID: " << running_module_file_id << '\n';

	QString cdl = copy_deps.join("; ");
	s << "Copy deps: " << (cdl.isEmpty() ? "(empty)" : cdl) << '\n';

	s << "Linkage section: " << (linkage_section_text.isEmpty() ? "(empty)" : linkage_section_text) << '\n';

	dump_filemap_as_text(filemap, s);
	dump_linemap_as_text(orig_to_running_linemap, "Original => Running", s);
	dump_linemap_as_text(running_to_orig_linemap, "Running => Original", s);

	dump_data_entries_as_text(working_storage->children, s);
	dump_data_entries_as_text(linkage_section->children, s);
	dump_data_entries_as_text(file_section->children, s);

	dump_paragraphs_as_text(paragraphs, s);

	dump_symbol_mapping_entries_as_text(s);

	f.close();

	return true;
}

GIXCOMMON_EXPORT QMap<QString, QStringList> CobolModuleMetadata::getFileDependencies()
{
	return file_dependencies;
}

#if defined(_WIN32) && defined(_DEBUG)

void dump_field_tree(DataEntry *e, int *l)
{
	QString pfx;
	pfx.fill(' ', (*l) * 4);

	OutputDebugStringA(pfx.toLocal8Bit().constData());
	OutputDebugStringA(e->name.toLocal8Bit().constData());
	OutputDebugStringA("\n");

	(*l)++;
	for (DataEntry *c : e->children) {
		dump_field_tree(c, l);
	}
	(*l)--;
}

#endif

void CobolModuleMetadata::fill_field_tree(const QList<cb_field_ptr> &flist, CobolModuleMetadata *cmm, DataEntry *parent)
{
	for (cb_field_ptr f : flist) {

		// top-level item
		DataEntry *e = DataEntry::fromCobolRawField(f);
		e->owner = cmm;
		e->parent = parent;
		e->fileid = cmm->reverse_filemap[e->filename];
		parent->children.append(e);

		if (f->children) {
			QList<cb_field_ptr> fchildren;
			fchildren.append(f->children);
			cb_field_ptr s = f->children->sister;
			while (s) {
				fchildren.append(s);
				s = s->sister;
			}
			fill_field_tree(fchildren, cmm, e);
		}
	}
}

GIXCOMMON_EXPORT CobolModuleMetadata *CobolModuleMetadata::build(ProjectFile *pf, ErrorData *err_data)
{
	GixPreProcessor gp;
	CopyResolver copy_resolver;

	if (!err_data) {
		return nullptr;
	}

	if (!pf) {
		err_data->err_code = -1;
		err_data->err_messages.append(QCoreApplication::translate("gix", "Bad parameters"));
		return nullptr;
	}

	QString configuration = GixGlobals::getCurrentConfiguration();
	QString platform = GixGlobals::getCurrentPlatform();

	Project *prj = pf->getParentProject();

	if (!prj) {
		err_data->err_code = -1;
		err_data->err_messages.append(QCoreApplication::translate("gix", "Bad parameters"));
		return nullptr;
	}

	QStringList copy_dirs = prj->getCopyDirList();

	if (prj->isEsql()) {
		QSettings settings;
		QScopedPointer<CompilerConfiguration> ccfg(CompilerConfiguration::get(configuration, platform, QVariantMap()));

		QString esql_cfg_id = settings.value("esql_preprocessor_id", ESQLConfigurationType::GixInternal).toString();
		CompilerEnvironment esql_cfg_env = ccfg.get()->getCompilerEnvironment();
		QScopedPointer<ESQLConfiguration> esql_cfg(ESQLConfiguration::get(esql_cfg_id, esql_cfg_env, configuration, platform));
		if (!esql_cfg.isNull()) {
			QStringList esql_copy_dirs = esql_cfg->getCopyPathList();
			if (!esql_copy_dirs.isEmpty())
				copy_dirs.append(esql_copy_dirs);
		}
	}

	copy_resolver.setCopyDirs(copy_dirs);
	copy_resolver.setExtensions(prj->getCopyExtList());

	gp.setCopyResolver(&copy_resolver);

	gp.setOpt("no_output", true);
	gp.setOpt("preprocess_copy_files", true);

	gp.setOpt("emit_static_calls", prj->PropertyGetValue("esql_static_calls", true));
	gp.setOpt("anonymous_params", prj->PropertyGetValue("esql_anon_params", true));

	TPESQLProcessing *pp = new TPESQLProcessing(&gp);
	gp.addStep(pp);

	gp.setOpt("emit_debug_info", true);
	gp.verbose = false;
	gp.verbose_debug = false;

	gp.setInputFile(pf->GetFileFullPath());
	gp.setOutputFile("");

	bool b = gp.process();
	if (!b) {
		err_data->err_code = gp.err_data.err_code;
		err_data->err_messages = gp.err_data.err_messages;
		return nullptr;
	}

	CobolModuleMetadata *cmm = new CobolModuleMetadata();

	cmm->module_name = pp->getModuleName();

	// *********************
	bool prj_opt_preprocess_copy_files = prj->PropertyGetValue("esql_preprocess_copy_files", false).toBool();
	if (prj_opt_preprocess_copy_files) {
		cmm->orig_to_running_linemap = pp->getBinarySrcLineMap();
		cmm->running_to_orig_linemap = pp->getBinarySrcLineMapReverse();
		cmm->reverse_filemap = pp->getFileMap();
		cmm->filemap = pp->getReverseFileMap();
		cmm->original_module_file_id = cmm->reverse_filemap.value(pp->getInput());
		cmm->running_module_file_id = cmm->reverse_filemap.value(pp->getOutput());
		cmm->flags |= FLAG_M_PREPROCD_COPY;
	}
	else {
		// 2nd pass
		GixPreProcessor gp2;
		gp2.setCopyResolver(&copy_resolver);

		gp2.setOpt("no_output", true);
		gp2.setOpt("preprocess_copy_files", false);

		gp2.setOpt("emit_static_calls", prj->PropertyGetValue("esql_static_calls", true));
		gp2.setOpt("anonymous_params", prj->PropertyGetValue("esql_anon_params", true));

		TPESQLProcessing *pp2 = new TPESQLProcessing(&gp2);
		gp2.addStep(pp2);

		gp2.setOpt("emit_debug_info", true);
		gp2.verbose = false;
		gp2.verbose_debug = false;

		gp2.setInputFile(pf->GetFileFullPath());
		gp2.setOutputFile("");

		bool b = gp2.process();
		if (!b) {
			err_data->err_code = gp.err_data.err_code;
			err_data->err_messages = gp.err_data.err_messages;
			if (cmm)
				delete cmm;

			return nullptr;
		}

		cmm->orig_to_running_linemap = pp2->getBinarySrcLineMap();
		cmm->running_to_orig_linemap = pp2->getBinarySrcLineMapReverse();
		cmm->reverse_filemap = pp2->getFileMap();
		cmm->filemap = pp2->getReverseFileMap();
		cmm->original_module_file_id = cmm->reverse_filemap.value(pp2->getInput());
		cmm->running_module_file_id = cmm->reverse_filemap.value(pp2->getOutput());
	}
	// *********************

	QMap<QString, cb_field_ptr> fmap = pp->getVariableDeclarationInfoMap();

	QList<cb_field_ptr> fvals = fmap.values();
	QList<cb_field_ptr> ftree = QList<cb_field_ptr>::fromStdList(cpplinq::from(fvals).where([](cb_field_ptr a) { return a->parent == nullptr;  }).to_list());
	QList<cb_field_ptr> ws_tree = QList<cb_field_ptr>::fromStdList(cpplinq::from(ftree).where([](cb_field_ptr a) { return a->data_section == DataSectionType::WorkingStorage; }).to_list());
	QList<cb_field_ptr> ls_tree = QList<cb_field_ptr>::fromStdList(cpplinq::from(ftree).where([](cb_field_ptr a) { return a->data_section == DataSectionType::LinkageSection; }).to_list());
	QList<cb_field_ptr> fs_tree = QList<cb_field_ptr>::fromStdList(cpplinq::from(ftree).where([](cb_field_ptr a) { return a->data_section == DataSectionType::FileSection; }).to_list());

	fill_field_tree(ws_tree, cmm, cmm->working_storage);
	fill_field_tree(ls_tree, cmm, cmm->linkage_section);
	fill_field_tree(fs_tree, cmm, cmm->file_section);

	int l = 0;
#if defined(_WIN32) && defined(_DEBUG)
	dump_field_tree(cmm->working_storage, &l);
#endif

	QMap<QString, srcLocation> ps = pp->getParagraphs();
	for (auto it = ps.begin(); it != ps.end(); ++it) {
		Paragraph *p = new Paragraph();
		QString name = it.key();
		srcLocation loc = it.value();
		p->name = name;
		p->file = loc.filename;
		p->line = loc.line;
		cmm->paragraphs[name] = p;
	}

	cmm->file_dependencies = pp->getFileDependencies();


	int i = 0;

	process_data_entry_offsets(cmm->working_storage->children, &i); i = 0;
	process_data_entry_local_offsets(cmm->working_storage->children, &i);
	process_data_entry_sizes(cmm->working_storage->children);
	process_data_entry_paths("", cmm->working_storage->children);

	process_data_entry_offsets(cmm->linkage_section->children, &i); i = 0;
	process_data_entry_local_offsets(cmm->linkage_section->children, &i);
	process_data_entry_sizes(cmm->linkage_section->children);
	process_data_entry_paths("", cmm->linkage_section->children);

	process_data_entry_offsets(cmm->file_section->children, &i); i = 0;
	process_data_entry_local_offsets(cmm->file_section->children, &i);
	process_data_entry_sizes(cmm->file_section->children);
	process_data_entry_paths("", cmm->file_section->children);

	// Debug symbols

	QString build_dir = pf->getParentProject()->getBuildDirectory(GixGlobals::getCurrentConfiguration(), GixGlobals::getCurrentPlatform());
	if (!build_dir.isEmpty() && QDir(build_dir).exists()) {
		QString ls_file = PathUtils::changeExtension(pf->GetFilename(), ".c.l.h");
		ls_file = PathUtils::combine(build_dir, ls_file);
		if (!QFile::exists(ls_file)) {
			//logger->logMessage(GIX_CONSOLE_LOG, QCoreApplication::translate("gix", "Warning: cannot locate C header file for module %1, variable inspection during debug will not be available").arg(cmm->module_name), QLogger::LogLevel::Warning);
		}
		else {
			QStringList lines = SysUtils::FileReadAllLines(ls_file);
			//QRegularExpression rxSymbol(R"(\sb_([0-9]+)\[([0-9]+)\];\s\/\*\s([A-Z\-a-z0-9]+)\s\*\/$)");
			QRegularExpression rxSymbol(R"(\sb_([0-9]+)\[([0-9]+)\];?(?:\ __attribute__\(\(aligned\)\))?;\s\/\*\s([A-Z\-a-z0-9]+)\s\*\/$)");

			for (QString ln : lines) {
				ln = ln.trimmed();
				if (!ln.length() || ln.startsWith("/*"))
					continue;

				QRegularExpressionMatch m = rxSymbol.match(ln);
				if (!m.hasMatch())
					continue;

				SymbolMappingEntry *sme = new SymbolMappingEntry();

				sme->cbl_var = m.captured(3);
				sme->id = "b_" + m.captured(1);
				sme->storage_len = m.captured(2).toInt();

				cmm->syms_to_dbg_syms.append(sme);
			}
		}
	}

	if (pf->getParentProject()->isEsql()) {
		cmm->is_preprocessed = true;
		cmm->flags |= FLAG_M_PREPROCD_ESQL;
	}
	else {
		cmm->is_preprocessed = false;
		cmm->flags = FLAG_M_BASE;
	}

	cmm->last_parsed = QDateTime::currentDateTime();

	return cmm;
}

uint32_t CobolModuleMetadata::getFlags()
{
	return flags;
}

CobolModuleMetadata *CobolModuleMetadata::loadFromFile(const QString &filename)
{
	QString file_full_path;

	QFile f(filename);
	if (!f.open(QIODevice::OpenModeFlag::ReadOnly))
		return NULL;

	QDataStream s(&f);
	CobolModuleMetadata *cmm = new CobolModuleMetadata();

	s >> cmm->format_version;
	s >> cmm->flags;
	s >> cmm->module_name;
	s >> cmm->last_parsed;

	s >> cmm->original_module_file_id;
	s >> cmm->running_module_file_id;

	s >> cmm->copy_deps;
	s >> cmm->linkage_section_text;

	load_filemap(cmm->filemap, s);
	for (int k : cmm->filemap.keys())
		cmm->reverse_filemap[cmm->filemap[k]] = k;

	load_linemap(cmm->orig_to_running_linemap, cmm->running_to_orig_linemap, s);

	load_paragraphs(cmm->paragraphs, s);

	load_symbol_mapping_entries(cmm, s);

	f.close();

	return cmm;

}


void CobolModuleMetadata::process_data_entry_offsets(QList<DataEntry *> &entries, int *cur_offset)
{
	for (DataEntry *e : entries) {
		e->offset_data_section = *cur_offset;

		if (!e->isGroup())
			*cur_offset += e->storage_size;

		process_data_entry_offsets(e->children, cur_offset);
	}
}

void CobolModuleMetadata::process_data_entry_sizes(QList<DataEntry *> &entries)
{
	for (DataEntry *e : entries) {
		if (e->type == WsEntryType::Group)
			e->storage_size = e->computeTotalStorageSize();
	}
}

void CobolModuleMetadata::process_data_entry_local_offsets(QList<DataEntry *> &entries, int *cur_offset)
{
	for (DataEntry *e : entries) {
		if (!e->parent || e->parent->is_placeholder)
			*cur_offset = 0;

		e->offset_local = *cur_offset;

		if (e->parent && !e->parent->is_placeholder && !e->isGroup())
			*cur_offset += e->storage_size;


		process_data_entry_local_offsets(e->children, cur_offset);
	}
}

void CobolModuleMetadata::process_data_entry_paths(QString path_prefix, QList<DataEntry *> &entries)
{
	for (DataEntry *e : entries) {
		QString npath = "";
		DataEntry *cur = e;
		do {
			npath = cur->name + ":" + npath;
			cur = cur->parent;
		} while (cur);

		e->path = (path_prefix != "") ? path_prefix + ":" + npath : npath;
		//e->path = path_prefix + ":" + npath;
		if (e->path.endsWith(":"))
			e->path = e->path.left(e->path.size() - 1);

		process_data_entry_paths(path_prefix, e->children);
	}
}

#if _DEBUG_LOG_ON
void CobolModuleMetadata::dumpOriginalToRunning()
{
	char bfr[1024];

	for (auto it = orig_to_running_linemap.begin(); it != orig_to_running_linemap.end(); ++it) {
		uint64_t orig = it.key();
		uint64_t running = it.value();

		uint32_t orig_file_id = (orig >> 32) & 0xffffffff;
		uint32_t orig_line = orig & 0xffffffff;

		uint32_t running_file_id = (running >> 32) & 0xffffffff;
		uint32_t running_line = running & 0xffffffff;

		QString ln = QString("O->R - %1@%2 => %3:%4\n").arg(orig_line).arg(filemap[orig_file_id]).arg(running_line).arg(filemap[running_file_id]);

#if defined(_WIN32)
		OutputDebugStringA(ln.toLocal8Bit().data());
#else
		fprintf(stderr, ln.toLocal8Bit().data());
#endif
	}
}
#endif