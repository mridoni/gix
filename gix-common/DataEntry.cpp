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

#include "DataEntry.h"
#include "CobolModuleMetadata.h"

#define PIC_ALPHABETIC 		0x01
#define PIC_NUMERIC 		0x02
#define PIC_NATIONAL		0x04
#define PIC_ALPHANUMERIC	(PIC_ALPHABETIC | PIC_NUMERIC)

DataEntry::DataEntry()
{
	occurs = 0;
	level = 0;
	storage_size = 0;
	storage_type = WsEntryStorageType::Unknown;
	decimals = 0;
	line = 0;
	fileid = 0;
	lst_line = 0;
	parent = nullptr;
	not_ref = false;
	ref_by_child = false;
	ref_by_parent = false;
	included = false;
	type = WsEntryType::Unknown;
	offset_data_section = 0;
	offset_local = 0;
	display_size = 0;
#ifndef GIX_HTTP
	owner = nullptr;
#endif
#ifdef GIX_HTTP
	is_required = false;
#endif
}

DataEntry::DataEntry(const QString &name, const QString &path) : DataEntry()
{
	this->name = name;
	this->path = path;
}

DataEntry::~DataEntry()
{
	for (WsReference* ref : references) {
		if (ref)
			delete ref;
	}

	//for (DataEntry * e: children) {
	//	if (e)
	//		delete e;
	//}
}

DataEntry *DataEntry::getTopMostParent()
{
	DataEntry *p = this;
	while (p) {
		if (!p->parent || p->parent->is_placeholder)
			break;

		p = p->parent;
	}
	return p;
}

bool DataEntry::isFiller()
{
	return this->name.toUpper() == "FILLER" || this->type == WsEntryType::Filler;
}

bool DataEntry::isGroup()
{
	return this->children.size() > 0;
}

DataEntry *DataEntry::fromCobolRawField(cb_field_ptr p)
{
	DataEntry *e = new DataEntry();
	e->name = QString::fromStdString(p->sname);
	e->line = p->defined_at_source_line;
	e->filename = QString::fromStdString(p->defined_at_source_file);
	e->level = p->level;
	e->decimals = p->scale;
	e->is_signed = p->have_sign;
	e->occurs = p->occurs;
	
	QString format;

	if (!p->children) {
		switch (p->pictype) {
			case PIC_ALPHABETIC:
				e->type = WsEntryType::Alphabetic;
				e->storage_type = WsEntryStorageType::Literal;
				e->storage_size = p->picnsize;
				e->display_size = p->picnsize;
				format = "PIC A(" + QString::number(p->picnsize) + ")";
				break;

			case PIC_ALPHANUMERIC:
				e->type = WsEntryType::Alphanumeric;
				e->storage_type = WsEntryStorageType::Literal;
				e->storage_size = p->picnsize;
				e->display_size = p->picnsize;
				format = "PIC X(" + QString::number(p->picnsize) + ")";
				break;

			case PIC_NUMERIC:
				e->type = WsEntryType::Numeric;
				e->display_size = p->picnsize + (p->have_sign ? 1 : 0) + (p->scale > 0 ? 1 : 0);	// TODO: this is most likely wrong
				QString sign = p->have_sign ? "S" : "";
				format = "PIC " + sign + "9(" + QString::number(p->picnsize) + ")";

				if (p->scale)
					format += "V(" + QString::number(p->scale) + ")";

				//int bsize = p->picnsize + p->scale;	// TODO: verify this
				int bsize = p->picnsize;	// TODO: verify this

				if (p->usage != Usage::None) {
					switch (p->usage) {
						case Usage::Binary:
						case Usage::NativeBinary:
							format += p->usage == (Usage::Binary) ? " USAGE BINARY" : " USAGE COMP-5";
							e->storage_type = p->usage == (Usage::Binary) ? WsEntryStorageType::Comp : WsEntryStorageType::Comp5;
							if (bsize == 1) {	// 1 byte
								e->storage_size = 1;
							}
							else {
								if (bsize == 2) {	// 1 byte
									e->storage_size = 2;
								}
								else {
									if (bsize == 3 || bsize == 4) {	// 2 bytes
										e->storage_size = 2;
									}
									else {
										if (bsize >= 5 && bsize <= 9) {	// 4 bytes
											e->storage_size = 4;
										}
										else {
											if (bsize >= 10 && bsize <= 18) {	// 8 bytes
												e->storage_size = 8;
											}
											else {
												// Should never happen
											}
										}
									}
								}
							}
							break;
						case Usage::Float:
							format += " USAGE COMP-1";
							e->storage_type = WsEntryStorageType::Comp5;
							break;
						case Usage::Double:
							format += " USAGE COMP-2";
							e->storage_type = WsEntryStorageType::Comp5;
							break;
						case Usage::Packed:
							format += " USAGE COMP-3";
							e->storage_type = WsEntryStorageType::Comp3;
							e->storage_size = (bsize / 2) + 1;
							break;
					}
				}
				else {
					e->storage_type = WsEntryStorageType::Literal;
					e->storage_size = p->picnsize + (p->have_sign ? 1 : 0);
				}
				break;
		}
		e->format = format;
	}
	else {
		e->type = WsEntryType::Group;
	}
	return e;
}

int DataEntry::computeTotalStorageSize()
{
	int sum_ch = this->storage_size;
	for (DataEntry *c : this->children) {
		sum_ch += c->computeTotalStorageSize();
	}

	return sum_ch;
}

WsReference::WsReference()
{
	line = 0;
	is_write_reference = false;
}
