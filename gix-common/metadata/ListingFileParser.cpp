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

#if 0
#include "ListingFileParser.h"
#include "SysUtils.h"
#include "PathUtils.h"
#include "linq/linq.hpp"
#include "GixGlobals.h"
#include "DataEntry.h"
#include "CompilerConfiguration.h"
#include "BuildDriver.h"
#include "IGixLogManager.h"

#include <QRegularExpression>
#include <QFile>
#include <QVector>
#include <QStack>

#if defined(_DEBUG) && defined(_WIN32)
#include <windows.h>
#include <debugapi.h>
#endif

using namespace cpplinq;

ListingFileParser::ListingFileParser(QString lstFile, QString srcFile)
{
	listing_file = lstFile;
	main_src_file = srcFile;

	cfm = nullptr;
}


ListingFileParser::~ListingFileParser()
{
	//if (cfm)
	//	delete cfm;
}

bool ListingFileParser::parse()
{
	if (listing_file.isEmpty() || !QFile::exists(listing_file))
		return false;

	QStringList lines = SysUtils::FileReadAllLines(listing_file);
	if (lines.isEmpty())
		return false;

	lines = QStringList::fromStdList(from(lines).where([](QString s) {return !s.isEmpty(); }).to_list());
	if (lines.isEmpty())
		return false;

	cfm = new ListingFileParserResult();

	parse_working_storage_section(lines);
	parse_linkage_section(lines);
	parse_file_section(lines);
	parse_paragraphs(lines);

	cfm->last_parsed = QDateTime::currentDateTime();

	return true;
}

ListingFileParserResult *ListingFileParser::getResult()
{
	return cfm;
}

int ListingFileParser::findLineLike(QString subj, const QStringList &lines, int start_at)
{
	if (subj.isEmpty() || lines.isEmpty() || start_at < 0 || start_at >(lines.length() - 1))
		return -1;

	QRegularExpression rx(subj);
	for (int i = start_at; i < lines.length(); i++) {
		if (rx.match(lines.at(i)).hasMatch())
			return i;
	}

	return -1;
}

int ListingFileParser::findLineLike(QStringList subj_list, const QStringList &lines, int start_at)
{
	for (QString subj : subj_list) {
		int res = findLineLike(subj, lines, start_at);
		if (res != -1)
			return res;
	}
	return -1;
}

QStringList ListingFileParser::findLineLikeWithResults(QString subj, const QStringList &lines, int *n, int start_at)
{
	*n = -1;
	if (subj.isEmpty() || lines.isEmpty() || start_at < 0 || start_at >(lines.length() - 1)) {
		return QStringList();
	}

	QRegularExpression rx(subj);
	for (int i = start_at; i < lines.length(); i++) {
		QRegularExpressionMatch m = rx.match(lines.at(i));
		if (m.hasMatch()) {
			*n = i;
			return m.capturedTexts();
		}
	}
	return QStringList();
}

int ListingFileParser::findLineUnlike(QString subj, const QStringList &lines, int start_at)
{
	if (subj.isEmpty() || lines.isEmpty() || start_at < 0 || start_at >(lines.length() - 1))
		return -1;

	QRegularExpression rx(subj);
	for (int i = start_at; i < lines.length(); i++) {
		if (!rx.match(lines.at(i)).hasMatch())
			return i;
	}

	return -1;
}

int ListingFileParser::findLineUnlike(QString subj1, QString subj2, const QStringList &lines, int start_at)
{
	if (subj1.isEmpty() || subj2.isEmpty() || lines.isEmpty() || start_at < 0 || start_at >(lines.length() - 1))
		return -1;

	QRegularExpression rx1(subj1);
	QRegularExpression rx2(subj2);
	for (int i = start_at; i < lines.length(); i++) {
		if (!rx1.match(lines.at(i)).hasMatch() && !rx2.match(lines.at(i)).hasMatch())
			return i;
	}
	return -1;
}

int ListingFileParser::findEmptyLine(const QStringList &lines, int start_at)
{
	if (lines.isEmpty() || start_at < 0 || start_at >(lines.length() - 1))
		return -1;

	for (int i = start_at; i < lines.length(); i++) {
		QString ll = lines.at(i).trimmed();
		if (ll.isEmpty())
			return i;
	}
	return -1;
}

QString ListingFileParser::findAndResolveCopy(QStringList lns, int s, bool allow_commented_copy)
{
	//QString rxDef;
	//if (!allow_commented_copy)
	//	rxDef = "^([0-9]+C?)\\s+.{6}\\ \\s*COPY\\s+['\"]?([A-Za-z0-9_\\-]+)[^'\"]?\\s*\\.";
	//else
	//	rxDef = "^([0-9]+C?)\\s+.{6}[\\ \\*]\\s*COPY\\s+['\"]?([A-Za-z0-9_\\-]+)[^'\"]?\\s*\\.";

	//QRegularExpression rx(rxDef);
	//for (int i = s; i >= 0; i--) {
	//	QRegularExpressionMatch m = rx.match(lns.at(i));
	//	if (m.hasMatch()) {
	//		QString cpy_name = m.captured(2);
	//		for (QString copy_dir : owner->getCopyDirList()) {
	//			QString cpath = PathUtils::combine(copy_dir, cpy_name);
	//			for (QString ext : owner->getCopyExtList()) {
	//				if (ext == ".") {
	//					ext = "";
	//				}

	//				QString full_path = PathUtils::changeExtension(cpath, ext);
	//				if (QFile::exists(full_path))
	//					return full_path;
	//			}
	//		}
	//		return QString();
	//	}
	//}
	return QString();
}

DataEntry *ListingFileParser::parseWsEntry(QString s)
{
	auto parts = s.split(QRegularExpression("\\s+"), QString::SplitBehavior::SkipEmptyParts);
	if (parts.length() < 4)
		return nullptr;

	DataEntry *e = new DataEntry();
	//e->owner = cfm;
	e->name = parts[3];
	e->storage_size = parts[0].toInt();
	e->level = parts[2].toInt();


	if (parts[1] == "NUMERIC")
		e->type = WsEntryType::Numeric;
	else
		if (parts[1] == "ALPHANUMERIC")
			e->type = WsEntryType::Alphanumeric;
		else
			if (parts[1] == "GROUP")
				e->type = WsEntryType::Group;

	if (e->name.endsWith(","))
		e->name = e->name.left(e->name.length() - 1);

	if (e->type == WsEntryType::Numeric || e->type == WsEntryType::Alphanumeric) {
		e->format = parts[4];
		if (e->format.endsWith(","))
			e->format = e->format.left(e->format.length() - 1);

		if (parts.length() > 4 && parts[4].startsWith("COMP")) {
			e->storage = parts[4];
		}

		if (parts.length() > 4 && (parts.contains("OCCURS"))) {
			int p = parts.indexOf("OCCURS");
			if (p > 4 && parts.length() > (p + 1)) {
				e->occurs = parts.at(p + 1).toInt();
			}
		}

		if (parts.length() > 4 && (parts.contains("REDEFINES"))) {
			int p = parts.indexOf("REDEFINES");
			if (p > 4 && parts.length() > (p + 1)) {
				e->redefines = parts.at(p + 1);
			}
		}
	}

	return e;
}

void ListingFileParser::process_data_entry_groups(QList<DataEntry *> t_entries, QList<DataEntry *> &entries)
{
	QStack<DataEntry *> levels;

	for (int i = 0; i < t_entries.size(); i++) {
		DataEntry *e = t_entries.at(i);

		if (e->name == "GG1") {
			int n = 999;
		}

		if (e->level == 1 || e->level == 77) {
			e->parent = nullptr;
			entries.append(e);
			levels.clear();
			levels.push(e);
			continue;
		}

		if (levels.size() > 0 && levels.top() != nullptr) {


			if (e->level == levels.top()->level) {
				e->parent = levels.top()->parent;
				levels.top()->parent->children.append(e);
				levels.pop();
				levels.push(e);
				continue;
			}

			if (e->level > levels.top()->level) {
				e->parent = levels.top();
				levels.top()->children.append(e);
				levels.push(e);
				continue;
			}

			if (e->level < levels.top()->level) {
				levels.pop();
				e->parent = levels.top();
				levels.top()->children.append(e);
				continue;
			}
		}
	}
}

void ListingFileParser::process_data_entry_paths(QString path_prefix, QList<DataEntry *> &entries)
{
	for (DataEntry *e : entries) {
		QString npath = "";
		DataEntry *cur = e;
		do {
			npath = cur->name + ":" + npath;
			cur = cur->parent;
		} while (cur);

		e->path = path_prefix + ":" + npath;
		if (e->path.endsWith(":"))
			e->path = e->path.left(e->path.size() - 1);

		process_data_entry_paths(path_prefix, e->children);
	}
}

void ListingFileParser::process_data_entry_offsets(QList<DataEntry *> &entries, int *cur_offset)
{
	for (DataEntry *e : entries) {
		e->offset_data_section = *cur_offset;

		if (!e->isGroup())
			*cur_offset += e->storage_size;

		process_data_entry_offsets(e->children, cur_offset);
	}
}

void ListingFileParser::process_data_entry_local_offsets(QList<DataEntry *> &entries, int *cur_offset)
{
	for (DataEntry *e : entries) {
		if (!e->parent)
			*cur_offset = 0;

		e->offset_local = *cur_offset;

		if (e->parent && !e->isGroup())
			*cur_offset += e->storage_size;


		process_data_entry_local_offsets(e->children, cur_offset);
	}
}

#if _DEBUG
void ListingFileParser::dump_data_entry_offsets(QList<DataEntry *> &tes)
{
	char bfr[256];
	OutputDebugStringA("=== data section offsets ===\n");
	for (auto e : tes) {
		sprintf(bfr, "%4s : %d\n", e->name.toLocal8Bit().constData(), e->offset_data_section);
		OutputDebugStringA(bfr);
	}
}

void ListingFileParser::dump_data_entry_local_offsets(QList<DataEntry *> &tes)
{
	char bfr[256];
	OutputDebugStringA("=== local offsets ===\n");
	for (auto e : tes) {
		sprintf(bfr, "%4s : %d\n", e->name.toLocal8Bit().constData(), e->offset_local);
		OutputDebugStringA(bfr);
	}
}

#endif

void ListingFileParser::parse_working_storage_section(const QStringList &lines)
{
	int s = findLineLike("^\\s+WORKING\\-STORAGE\\ SECTION$", lines);
	if (s >= 0) {
		parse_data_section("WS", lines, s, cfm->ws_entries);
	}
}

void ListingFileParser::parse_file_section(const QStringList &lines)
{

}

void ListingFileParser::parse_paragraphs(const QStringList &lines)
{
	QRegularExpression rxParagraphDefinitionLine("^(P|E)\\s+([A-Za-z0-9_\\-]+)\\s+(\\d+)");
	QRegularExpression rxParagraphRefList("(?:(\\*?)(\\d+)\\s*)");

	int s = findLineLike("^LABEL\\s+DEFINED\\s+REFERENCES$", lines);

	if (s >= 0) {
		int e = findLineLike("^FUNCTION\\s+TYPE\\s+REFERENCES$", lines, s + 1);
		if (e < 0)
			e = findLineLike({ "^Error\\/Warning\\ summary\\:$",
								"^[0-9]+\\ warnings\\ in\\ compilation\\ group$" }, lines, s + 1);

		if (e > s) {
			Paragraph *cur_entry = nullptr;
			for (int n = s + 1; n < e; n++) {
				QString ln = lines.at(n);

				QRegularExpressionMatch m = rxParagraphDefinitionLine.match(ln);
				if (m.hasMatch()) {
					if (cur_entry != nullptr) {
						cfm->paragraphs[cur_entry->name] = cur_entry;
						if (cur_entry->type == "E")
							cfm->module_name = cur_entry->name;
					}
					cur_entry = new Paragraph();

					cur_entry->file = main_src_file;

					cur_entry->line = m.captured(3).toInt();
					cur_entry->name = m.captured(2);
					cur_entry->type = m.captured(1);

					ln = ln.mid(m.capturedEnd(3) + 1).trimmed();
				}
				else {
					ln = ln.trimmed();
				}

				if (ln.toLower() == "not referenced") {
					continue;
				}

				QRegularExpressionMatchIterator ri = rxParagraphRefList.globalMatch(ln);
				while (ri.hasNext()) {
					QRegularExpressionMatch r = ri.next();
					QString ref = r.captured(1);
					if (!ref.isEmpty()) {
						cur_entry->referenced_at.append(ref.toInt());
					}
				}
			}

			if (cur_entry != nullptr) {
				cfm->paragraphs[cur_entry->name] = cur_entry;
				if (cur_entry->type == "E")
					cfm->module_name = cur_entry->name;
			}
		}
	}
}

void ListingFileParser::parse_data_section(QString path_prefix, const QStringList &lines, int s, QList<DataEntry *> &entries)
{
	QList<DataEntry *> tmp_entries;
	QMap<QString, DataEntry *> entry_name_map;

	int e = findLineUnlike("^[0-9]+\\s+ALPHANUMERIC|^[0-9]+\\s+NUMERIC|^[0-9]+\\s+GROUP", lines, s + 1);
	if (e > s) {
		for (int n = s; n < e; n++) {
			DataEntry *wse = parseWsEntry(lines.at(n));
			if (wse != nullptr) {
				wse->lst_line = n;
				tmp_entries.append(wse);
				entry_name_map[wse->name] = wse;
			}
		}
	}

	

	process_references(lines, entry_name_map);
	process_data_entry_groups(tmp_entries, entries);
	process_data_entry_paths(path_prefix, entries);
	
	int i = 0;
	process_data_entry_offsets(entries, &i);

	i = 0;
	process_data_entry_local_offsets(entries, &i);
#if _DEBUG
	dump_data_entry_offsets(tmp_entries);
	dump_data_entry_local_offsets(tmp_entries);
#endif

	process_line_references(lines, entries);
}

void ListingFileParser::parse_linkage_section(const QStringList &lines)
{
	int s = findLineLike("^\\s+LINKAGE\\ SECTION$", lines);
	if (s >= 0) {
		parse_data_section("LS", lines, s, cfm->ls_entries);

		s = findLineLike("\\s+LINKAGE\\ SECTION\\.\\s*$", lines);
		if (s >= 0) {
			int e = findLineLike("\\s+FILE\\s+SECTION\\.\\s*$|\\s+WORKING\\-STORAGE\\s+SECTION\\.\\s*$|\\sPROCEDURE\\s+DIVISION\\.\\s*$", lines, s + 1);
			if (e > s) {
				QRegularExpression rxLineNumber("^[0-9]+\\s+");
				auto ls_lines = lines.mid(s + 1, (e - s) - 1);
				for (int i = 0; i < ls_lines.size(); i++) {
					ls_lines[i] = ls_lines[i].replace(rxLineNumber, "");
				}
				cfm->linkage_section_text = ls_lines.join("\n");
			}
		}
	}
}

void ListingFileParser::process_references(const QStringList &lines, const QMap<QString, DataEntry *> &entries)
{
	QRegularExpression rxFieldDefinitionLine("^([A-Za-z0-9_\\-]+)\\s+(\\d+)");
	QRegularExpression rxFieldRefList("(?:(\\*?)(\\d+)\\s*)");

	int s = findLineLike("^NAME\\s+DEFINED\\s+REFERENCES$", lines);
	if (s >= 0) {
		int e = findLineUnlike("^[A-Z_0-9\\-]+\\s+[0-9]+\\s+", "(?:(\\*?)(\\d+)\\s*)", lines, s + 1);
		if (e > s) {
			DataEntry *cur_entry = nullptr;
			QStack<QString> rlines;

			for (int n = e - 1; n > s; n--)
				rlines.push(lines[n]);

			while (!rlines.isEmpty()) {
				QString ln = rlines.pop();

				QRegularExpressionMatch m = rxFieldDefinitionLine.match(ln);
				if (m.hasMatch()) {
					QString var = m.captured(1);
					cur_entry = entries.contains(var) ? entries[var] : nullptr;
					if (!cur_entry)
						continue;

					cur_entry->lst_line = m.captured(2).toInt();
				}
				else {
					if (!cur_entry)
						continue;

					if (ln.toLower() == "not referenced") {
						cur_entry->not_ref = true;
						continue;
					}

					if (ln.toLower() == "referenced by parent") {
						cur_entry->ref_by_parent = true;
						continue;
					}

					if (ln.toLower() == "referenced by child") {
						cur_entry->ref_by_child = true;
						continue;
					}

					if (ln.toLower() == "referenced by parent/child") {
						cur_entry->ref_by_parent = true;
						cur_entry->ref_by_child = true;
						continue;
					}

					QRegularExpressionMatchIterator ri = rxFieldRefList.globalMatch(ln);
					while (ri.hasNext()) {
						QRegularExpressionMatch r = ri.next();
						QString ref = r.captured(2);
						QString t = r.captured(1);
						if (!ref.isEmpty()) {
							WsReference *rr = new WsReference();
							if (ref.startsWith("*")) {
								rr->is_write_reference = true;
								ref = ref.mid(1);
							}
							rr->line = ref.toInt();
							cur_entry->references.append(rr);
						}
					}

				}
			}
		}
	}

}

void ListingFileParser::process_line_references(const QStringList& lines, QList<DataEntry*> entries)
{
	IGixLogManager *logger = GixGlobals::getLogManager();
	int s = findLineLike("^SIZE\\s+TYPE\\s+LVL\\s+NAME\\s+PICTURE$", lines);
	if (s <= 0)
		return;

	auto a = lines.toStdList();
	auto end = std::next(a.begin(), s);
	std::list<QString> b(a.begin(), end);
	QStringList lns = QStringList::fromStdList(b);


	for (int i = 0; i < entries.size(); i++) {
		DataEntry* entry = entries.at(i);
		if (entry->isFiller())
			continue;

		//"^([0-9]+C?)\\s+.{6}[^\\*]\\s*%1\\s+%2\\s*%3\\."
		QString rx = QString("^([0-9]+C?)\\s+(.{6})[^\\*]\\s*%1\\s+%2\\s*.*\\.\\s*$")
			.arg(entry->level, 2, 10, QChar('0'))
			.arg(entry->name);

		QStringList res = findLineLikeWithResults(rx, lns, &s);
		if (res.isEmpty()) {
#ifdef _DEBUG
			logger->logMessage(GIX_CONSOLE_LOG, QString("NOT FOUND: %1 [%2]").arg(entry->name).arg(rx), QLogger::LogLevel::Debug);
#endif
			continue;
		}

		QString src_ln = res[1];

		if (!src_ln.endsWith("C") && res[2] != "GIXSQL") {
			entry->line = src_ln.toInt();
			entry->filename = main_src_file;
			entry->included = false;
#ifdef _DEBUG
			//Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, QString("FOUND (M) %1: %2@%3").arg(entry->name).arg(entry->line).arg(entry->file), QLogger::LogLevel::Debug);
#endif
			continue;
		}

		bool is_esql_preprocessed = (res[2] == "GIXSQL");
		QString copy_file = findAndResolveCopy(lns, s, is_esql_preprocessed);
		if (copy_file.isEmpty()) {
			entry->line = 0;
			entry->filename = QString();
			entry->included = false;
#ifdef _DEBUG
			//Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, QString("NOT FOUND (C) %1").arg(entry->name), QLogger::LogLevel::Debug);
#endif
		}
		else {
			if (!is_esql_preprocessed)
				entry->line = src_ln.mid(0, src_ln.length() - 1).toInt();
			else {
				entry->line = find_def_in_copy_file(entry, copy_file);
			}
			entry->filename = copy_file;
			entry->included = true;
			cfm->copy_deps.append(copy_file);
#ifdef _DEBUG
			//Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, QString("FOUND (C) %1: %2@%3").arg(entry->name).arg(entry->line).arg(entry->file), QLogger::LogLevel::Debug);
#endif
		}
	}

}


int ListingFileParser::find_def_in_copy_file(DataEntry *e, QString copy_file)
{
	if (listing_file.isEmpty() || !QFile::exists(copy_file))
		return 0;

	QStringList lines = SysUtils::FileReadAllLines(copy_file);
	if (lines.isEmpty())
		return 0;

	QRegularExpression rx("^.{6}[^\\*]\\s*(\\d+)\\s+([A-Za-z0-9_\\-]+)\\s+.*\\.*");

	for (int i = 0; i < lines.size(); i++) {
		QRegularExpressionMatch m = rx.match(lines.at(i));
		if (m.hasMatch()) {
			QStringList caps = m.capturedTexts();
			if (caps[2] == e->name && caps[1].toInt() == e->level)
				return i + 1;
		}
	}
	return 0;
}

#endif