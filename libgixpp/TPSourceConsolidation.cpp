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

#include "TPSourceConsolidation.h"
#include "PathUtils.h"
#include "SysUtils.h"
#include "GixPreProcessor.h"
#include <QDir>

TPSourceConsolidation::TPSourceConsolidation(GixPreProcessor *gpp) : ITransformationStep(gpp)
{
	nlines = 0;
	cur_output_line = 0;
	owner = gpp;
	map_only = false;
	current_input_line = 0;
	output_line = 0;
}

bool TPSourceConsolidation::run(ITransformationStep *prev_step)
{
	if (input_file.isEmpty() && !prev_step)
		return false;

	if (input_file.isEmpty())
		input_file = prev_step->getOutput();
	
	map_only = owner->getOpt("no_output").toBool();

	if (!map_only) {
		if (output_file.isEmpty()) {
			QString f = PathUtils::changeExtension(input_file, ".cblpp");
			f = QDir::tempPath() + QDir::separator() + PathUtils::getFilename(f);
			output_file = f;
		}

		if (!SysUtils::isWritableFile(output_file))
			return false;
	}

	input_file_stack.push(input_file);
	if (!processNextFile())
		return false;

	if (!map_only) {
		QFile f(output_file);
		if (!f.open(QIODevice::OpenModeFlag::WriteOnly | QIODevice::OpenModeFlag::Text))
			return false;

		if (!f.write(all_lines.join('\n').toUtf8().constData()))
			return false;

		f.close();
	}

	return true;
}

QString TPSourceConsolidation::getOutput(ITransformationStep *me)
{
	return output_file;
}

QMap<int, QString> &TPSourceConsolidation::getFileMap() const
{
	return const_cast<QMap<int, QString>&>(filemap);
}

QMap<QString, QString> &TPSourceConsolidation::getSrcLineMap() const
{
	return const_cast<QMap<QString, QString>&>(in_to_out);
}

bool TPSourceConsolidation::processNextFile()
{
	QString the_file = input_file_stack.top();
	QStringList input_lines = SysUtils::FileReadAllLines(the_file);
	QString copy_name, copy_file;

	if (!input_lines.size()) {
		input_file_stack.pop();
		return true;
	}

	for (int input_line = 1; input_line <= input_lines.size(); input_line++) {
		current_input_line = input_line;
		//fprintf(stderr, "Processing line %d of file %s\n", input_line, the_file.toUtf8().constData());

		QString cur_line = input_lines.at(input_line - 1);

		if (is_copy_statement(cur_line, copy_name)) {
			if (!owner->getCopyResolver()->resolveCopyFile(copy_name, copy_file)) {
				SET_ERR(5, QString("Cannot resolve copy %1").arg(copy_name).toUtf8().data());
				return false;
			}

			input_file_stack.push(copy_file);
			if (!processNextFile()) {
				SET_ERR(6, QString("Cannot process file %1").arg(copy_file).toUtf8().data());
				return false;
			}
		}
		else {
			put_output_line(cur_line);
		}

		current_input_line = input_line;
	}

	input_file_stack.pop();

	return true;
}

void TPSourceConsolidation::put_output_line(const QString &line)
{
	output_line++;

	QString output_id = QString("%1@%2").arg(output_line).arg(map_only ? input_file : output_file);	// No precompilation, input file is the same as output file
	QString input_id = QString("%1@%2").arg(current_input_line).arg(input_file_stack.top());
	in_to_out[input_id] = output_id;

	if (!map_only)
		all_lines.append(line);

}


bool TPSourceConsolidation::is_copy_statement(const QString line, QString &copy_name)
{
	QString ln = line;

	if (ln.length() < 7)
		return false;;

	if (ln[6] == '*')
		return false;

	ln = ln.mid(7).trimmed();

	if (ln.startsWith("COPY ")) {
		QString cname = ln.mid(5);
		if (cname[cname.length() - 1] == '.')
			cname = cname.mid(0, cname.length() - 1);

		cname = cname.trimmed();

		copy_name = cname;
		return true;
	}

	// TODO: handle multi-line EXEC SQL INCLUDE statements
	if (ln.startsWith("EXEC SQL INCLUDE ") && ln.contains("END-EXEC")) {
		QString cname = ln.mid(16).trimmed();
		cname = cname.mid(0, cname.indexOf(" ")).trimmed();
		copy_name = cname;
		return true;
	}

	return false;
}
