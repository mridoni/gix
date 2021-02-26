#pragma once

#include <QString>
#include <QList>
#include <QDateTime>

#include "Project.h"
#include "ListingFileParserResult.h"


class ListingFileParser;
class DataEntry;
class CobolModuleMetadata;

class ListingFileParser
{
public:
	ListingFileParser(QString lstFile, QString srcFile);
	~ListingFileParser();

	bool parse();
	ListingFileParserResult *getResult();

private:
	QString listing_file;
	QString main_src_file;
	QString module_name;
	//Project *owner;
	ProjectFile *prj_file;

	int findLineLike(QString subj, const QStringList& lines, int start_at = 0);
	int findLineLike(QStringList subj_list, const QStringList& lines, int start_at = 0);
	QStringList findLineLikeWithResults(QString subj, const QStringList& lines, int *n, int start_at = 0);
	int findLineUnlike(QString subj, const QStringList& lines, int start_at = 0); 
	int findLineUnlike(QString subj1, QString subj2, const QStringList& lines, int start_at);
	int findEmptyLine(const QStringList& lines, int start_at);

	QString findAndResolveCopy(QStringList lns, int s, bool allow_commented_copy);
	
	DataEntry *parseWsEntry(QString);
	void process_data_entry_groups(QList<DataEntry *>, QList<DataEntry *>&);
	void process_data_entry_paths(QString path_prefix, QList<DataEntry *>&);
	void process_data_entry_offsets(QList<DataEntry *>&, int *cur_offset);
	void process_data_entry_local_offsets(QList<DataEntry *>&, int *cur_offset);
#if _DEBUG
	void dump_data_entry_offsets(QList<DataEntry *>& tes);
	void dump_data_entry_local_offsets(QList<DataEntry *>& tes);
#endif	
	void parse_data_section(QString path_prefix, const QStringList& lines, int s, QList<DataEntry *> &tmp_entries);
	void parse_linkage_section(const QStringList& lines);
	void parse_working_storage_section(const QStringList& lines);
	void parse_file_section(const QStringList& lines);
	void parse_paragraphs(const QStringList& lines);

	void process_references(const QStringList& lines, const QMap<QString, DataEntry *>& entries);

	void process_line_references(const QStringList&, QList<DataEntry *>);
	int find_def_in_copy_file(DataEntry *entry, QString copy_file);

	ListingFileParserResult* cfm;

};

