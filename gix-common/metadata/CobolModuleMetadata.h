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

/*
	This maps to a .sym file specific to each module
*/

#include <QString>
#include <QMap>
#include <QDateTime>
#include <QDataStream>
#include <QTextStream>

#include "gixcommon_global.h"

#include "ErrorData.h"
#include "ESQLDefinitions.h"

#define SYM_FILE_FMT_VER_1_0 ((uint16_t) 0x0100)

#define FLAG_M_BASE					0
#define FLAG_M_PREPROCD_ESQL		1
#define FLAG_M_PREPROCD_COPY		2

class Project;
class Paragraph;
class ProjectFile;
class DataEntry;
class ModuleDebugInfo;
class SymbolMappingEntry;
class TPESQLProcessing;
class CobolModuleMetadata;

class GIXCOMMON_EXPORT CobolModuleMetadata
{
	friend class MetadataWorker;

public:
	CobolModuleMetadata();
	~CobolModuleMetadata();

	
	static CobolModuleMetadata *loadFromFile(const QString &filename);
	
	const QList<DataEntry *> &getWorkingStorageDataEntries();
	const QList<DataEntry *> &getLinkageDataEntries();
	const QList<DataEntry *> &getFileDataEntries();
	const QList<DataEntry *> getDataEntries();
	const QMap<QString, Paragraph *> &getParagraphs();
	
	bool runningToOriginal(int running_module_file_id, int running_line, int *orig_file_id, int *orig_line);
	bool originalToRunning(int orig_file_id, int orig_line, int *running_module_file_id, int *running_line);
	bool getFileById(int id, QString &filename);
	bool getFileByName(const QString &filename, int *id);
	
	int runningFileId();
	int originalFileId();
	
	QString runningFile();
	QString originalFile();
	
	QString getLinkageSection();
	QString getModuleName();
	
	ModuleDebugInfo *getDebugInfo();
	
	bool isPreprocessedESQL();
	
	bool isUpToDate();
	
	void clear();

	uint32_t getFlags();
	
	DataEntry *findDefinition(QString def_path, bool use_path = false);
	QString getDebugLocalSymbolName(QString n);
	QList<SymbolMappingEntry *>& getSymbolMappingTable() const;
	
	bool dumpToFile(const QString &filename, bool as_text = false);
	bool dumpToTextFile(const QString &filename);
	
	QMap<QString, QStringList> getFileDependencies();
	
	void flattenEntryTree(QList<DataEntry *> &f_entries, const QList<DataEntry *> &entries);

#if _DEBUG_LOG_ON
	void dumpOriginalToRunning();
#endif

private:
	int format_version = SYM_FILE_FMT_VER_1_0;
	uint32_t flags = 0;

	bool is_preprocessed;

	int running_module_file_id;
	int original_module_file_id;

	QString sym_file;
	QString module_name;
	QDateTime last_parsed;

	QStringList copy_deps;

	// From ListingFileParser
	QString linkage_section_text;

	DataEntry *working_storage;
	DataEntry *linkage_section;
	DataEntry *file_section;

	QMap<QString, QStringList> file_dependencies;

	QMap<QString, Paragraph *> paragraphs;

	// From MapFileReader
	QMap <int, QString> filemap;
	QMap <QString, int> reverse_filemap;
	QMap<uint64_t, uint64_t> orig_to_running_linemap;
	QMap<uint64_t, uint64_t> running_to_orig_linemap;

	QList<SymbolMappingEntry *> syms_to_dbg_syms;

	static CobolModuleMetadata *build(ProjectFile *pf, ErrorData *err_data);

	DataEntry *findEntry(QList<DataEntry *> entries, QString def_path, bool use_path = false);

	void delete_data_entry_tree(QList<DataEntry *>);

	
	void assign_file_ids(QList<DataEntry *> &f_entries);
	static void assign_file_names(QList<DataEntry *> &f_entries, const QMap<int, QString> &filemap);

	void dump_data_entries(const QList<DataEntry *> entries, QDataStream &s);
	void dump_data_entry(const DataEntry *e, QDataStream &s);
	void dump_paragraphs(const QMap<QString, Paragraph *> paragraphs, QDataStream &s);
	void dump_filemap(const QMap <int, QString> &filemap, QDataStream &s);
	void dump_linemap(const QMap<uint64_t, uint64_t> &orig_to_running_linemap, QDataStream &s);
	void dump_symbol_mapping_entries(QDataStream &s);

	void dump_data_entries_as_text(const QList<DataEntry *> entries, QTextStream &s);
	void dump_data_entry_as_text(const DataEntry *e, QTextStream &s);
	void dump_paragraphs_as_text(const QMap<QString, Paragraph *> paragraphs, QTextStream &s);
	void dump_filemap_as_text(const QMap <int, QString> &filemap, QTextStream &s);
	void dump_linemap_as_text(const QMap<uint64_t, uint64_t> &linemap, const QString &lm_desc, QTextStream &s);
	void dump_symbol_mapping_entries_as_text(QTextStream &s);

	static void load_data_entries(const CobolModuleMetadata *cmm, QList<DataEntry *> &entries, QDataStream &s);
	static void load_data_entry(DataEntry *e, QMap<QString, QStringList> &rmap, QDataStream &s);
	static void load_paragraphs(QMap<QString, Paragraph *> &p_paragraphs, QDataStream &s);
	static void load_filemap(QMap <int, QString> &p_filemap, QDataStream &s);
	static void load_linemap(QMap<uint64_t, uint64_t> &p_linemap, QMap<uint64_t, uint64_t> &p_linemap_rev, QDataStream &s);
	static void load_symbol_mapping_entries(CobolModuleMetadata *cmm, QDataStream &s);

	//static void build_linemaps(const QList<QPair<QPair<int, int>, QPair<int, int>>> &orig_to_running_linemap, QMap<uint64_t, uint64_t> &out_linemap, QMap<uint64_t, uint64_t> &out_linemap_rev);
	static void process_entry_definitions(const QList<DataEntry *> &entries);

	static void fill_field_tree(const QList<cb_field_ptr> &flist, CobolModuleMetadata *cmm, DataEntry *parent);

	static void process_data_entry_offsets(QList<DataEntry *> &entries, int *cur_offset);
	static void process_data_entry_sizes(QList<DataEntry *> &entries);
	static void process_data_entry_local_offsets(QList<DataEntry *> &entries, int *cur_offset);
	static void process_data_entry_paths(QString path_prefix, QList<DataEntry *> &entries);

};

