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

#include "DbCodeGenerator.h"
#include "CobolUtils.h"
#include "linq/linq.hpp"

using namespace cpplinq;

void DbCodeGenerator::setUseUpperCaseForCopyFields(bool b)
{
	uc_copy_fields = b;
}

bool DbCodeGenerator::generateCopyFile(IDataSourceInfo *conn_info, QString schema, QString table, QString& res)
{
	QStringList lines;
	QString line;

	res.clear();

	DbConnection* dbc = IdeDbManager::createConnection(conn_info);
	if (!dbc|| !dbc->connect())
		return false;

	std::vector<ColumnInfo *> columns;
	if (dbc->getColumns(schema, table, columns)) {

		auto lengths = cpplinq::from(columns).select([](ColumnInfo* a) { return a->name.size(); }).to_vector();
		int max_name_len = *std::max_element(lengths.begin(), lengths.end());

		line.fill(' ', 7);

		QString gname = (uc_copy_fields ? table.toUpper() : table).replace("_", "-");
		if (CobolUtils::isReservedWord(gname))
			gname = "T_" + gname;

		line += QString("01 %1.").arg(gname);
		lines.append(line);

		for (ColumnInfo* ci : columns) {

			QString name = QString::fromStdString(ci->name);
			QString native_type = QString::fromStdString(ci->native_type);
			ColumnType type = ci->type;
			QString pic, mask, decmask;

			if (ci->isNumeric()) {
				if (ci->base == 10) {
					pic = QString("PIC 9(%1)").arg(ci->length);
					if (ci->decimal_digits > 0)
						pic += QString("V(%1)").arg(ci->decimal_digits);
				}
				else {
					switch (ci->type) {
						case ColumnType::Real:
							pic = "COMP-1";
							break;						
						case ColumnType::Double :
							pic = "COMP-2";
							break;
					}
				}
			}
			else {
				pic = QString("PIC X(%1)").arg(ci->length);
			}
			
			QString varname = QString::fromStdString(ci->name);
			if (uc_copy_fields)
				varname = varname.toUpper();

			varname = varname.replace("_", "-");

			if (CobolUtils::isReservedWord(varname))
				varname = gname + "-" + varname;

			line.fill(' ', 11);
			line += "03 " + varname.leftJustified(max_name_len + 2);
			line += pic + ".";

			lines.append(line);
		}

		res = lines.join('\n');
	};

	dbc->disconnect();

	return true;
}
