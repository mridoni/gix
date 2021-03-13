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

#include "IdeSearchManager.h"
#include <Ide.h>
#include <UiUtils.h>

#include "SearchDialog.h"
#include "IdeTaskManager.h"

IdeSearchManager::IdeSearchManager()
{
	params = new SearchParams();
}

IdeSearchManager::~IdeSearchManager()
{
	delete params;
}

QStringList IdeSearchManager::getSearchHistory()
{
	return search_history;
}

QStringList IdeSearchManager::getReplaceHistory()
{
	return replace_history;
}

SearchType IdeSearchManager::getLastUsedType()
{
	return last_used_type;
}

SearchMode IdeSearchManager::getLastUsedMode()
{
	return last_used_mode;
}

SearchDirection IdeSearchManager::getLastUsedDirection()
{
	return last_used_direction;
}

SearchParams* IdeSearchManager::getCurrentSearchParams()
{
	return params;
}

void IdeSearchManager::findNext()
{
	bool retflag = internalFindNext();
}

bool IdeSearchManager::internalFindNext()
{
	if (params->getTargetList()->size() == 0 || params->current_target >= params->getTargetList()->size())
		return false;

	SearchTarget* st = params->getTargetList()->at(params->current_target);
	int flags = SCFIND_NONE;

	if (st->last_direction != params->direction && st->last_start != 0 && st->last_end != 0) {
		// Invalidate current position
		st->current_position = (params->direction == SearchDirection::Up) ? st->last_start : st->last_end;
	}

	int s_start = st->current_position;
	int s_end = (params->direction == SearchDirection::Up) ? 0 : st->document->length();

	if (params->match_case)
		flags |= SCFIND_MATCHCASE;

	if (params->whole_word)
		flags |= SCFIND_WHOLEWORD;

	if (params->mode == SearchMode::Regex)
		flags |= SCFIND_REGEXP;

	auto pp = st->document->findText(flags, params->search_spec.toStdString().c_str(), s_start, s_end);
	if (pp.first == -1) {
		if (params->wrap_search && params->direction == SearchDirection::Up && pp.second == 0) {
			s_start = st->document->length();
			s_end = 0;
			pp = st->document->findText(flags, params->search_spec.toStdString().c_str(), s_start, s_end);
			if (pp.first == -1) {
				UiUtils::InfoDialog(tr("Text not found or end of document reached"));
				return false;
			}
		}
		else {
			if (params->wrap_search && params->direction == SearchDirection::Down && pp.second == st->document->length()) {
				s_start = 0;
				s_end = st->document->length();
				pp = st->document->findText(flags, params->search_spec.toStdString().c_str(), s_start, s_end);
				if (pp.first == -1) {
					UiUtils::InfoDialog(tr("Text not found or end of document reached"));
					return false;
				}
			}
			else {
				UiUtils::InfoDialog(tr("Text not found or end of document reached"));
				return false;
			}
		}

	}

	st->document->gotoPos((params->direction == SearchDirection::Up) ? pp.first : pp.second);
	st->current_position = (params->direction == SearchDirection::Up) ? pp.first : pp.second;

	st->document->setCurrentPos((params->direction == SearchDirection::Up) ? pp.first : pp.second);
	st->document->setSelection((params->direction == SearchDirection::Up) ? pp.first : pp.second,
									(params->direction == SearchDirection::Up) ? pp.second : pp.first);
	

	Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, QString("Found %1 at %2-%3, curpos: %4").arg(params->search_spec).arg(pp.first).arg(pp.second).arg(st->current_position), QLogger::LogLevel::Debug);

	st->last_direction = params->direction;
	st->last_start = pp.first;
	st->last_end = pp.second;
	
	return true;
}

void IdeSearchManager::replace()
{
	SearchTarget* st = params->getTargetList()->at(params->current_target);
	QString current_selection = QString::fromUtf8(st->document->getSelText());
	if (current_selection == params->search_spec) {		// TODO: account for regex search
		st->document->replaceSel(params->replace_spec.toUtf8().constData());
		return;
	}

	bool retflag = internalFindNext();
	if (retflag) {

		st->document->replaceSel(params->replace_spec.toUtf8().constData());
	}
}

int IdeSearchManager::replaceAll()
{
	if (params->getTargetList()->size() == 0 || params->current_target >= params->getTargetList()->size())
		return 0;

	SearchTarget* st = params->getTargetList()->at(params->current_target);
	int flags = SCFIND_NONE;

	int s_start = 0;
	int s_end = st->document->length() - 1;
	int count = 0;

	if (params->match_case)
		flags |= SCFIND_MATCHCASE;

	if (params->whole_word)
		flags |= SCFIND_WHOLEWORD;

	if (params->mode == SearchMode::Regex)
		flags |= SCFIND_REGEXP;

	int i = s_start;

	while (true) {
		st->document->setTargetStart(i);
		st->document->setTargetEnd(s_end);
		st->document->setSearchFlags(flags);

		auto sbfr = params->search_spec.toUtf8();
		auto rbfr = params->replace_spec.toUtf8();
		char* sdata = sbfr.data();
		char* rdata = rbfr.data();
		int rc = st->document->searchInTarget(strlen(sdata), sdata);
		if (rc == -1)
			break;
		
		if (params->mode == SearchMode::Regex)
			st->document->replaceTargetRE(strlen(rdata), rdata);
		else
			st->document->replaceTarget(strlen(rdata), rdata);

		i = st->document->targetEnd();

		count++;
	}

	return count;
}

int IdeSearchManager::count()
{

	if (params->getTargetList()->size() == 0 || params->current_target >= params->getTargetList()->size())
		return 0;

	SearchTarget* st = params->getTargetList()->at(params->current_target);
	int flags = SCFIND_NONE;

	int s_start = 0;
	int s_end = st->document->length() - 1;
	int count = 0;

	if (params->match_case)
		flags |= SCFIND_MATCHCASE;

	if (params->whole_word)
		flags |= SCFIND_WHOLEWORD;

	if (params->mode == SearchMode::Regex)
		flags |= SCFIND_REGEXP;


	int i = s_start;
	while (i < s_end) {
		auto pp = st->document->findText(flags, params->search_spec.toStdString().c_str(), i, s_end);
		if (pp.first != -1) {
			count++;
			i = pp.second;
		}
		else
			break;
	}

	return count;

}

SearchTarget::SearchTarget(MdiChild* w, int curpos)
{
	document = w;
	current_position = curpos;

	last_direction = SearchDirection::Down;
	last_start = 0;
	last_end = 0;

	this->connect(w, &MdiChild::caretPositionChanged, this, [this]() {
		current_position = document->currentPos();
		qDebug("CP: %d", current_position);
	});
}

QList<SearchTarget*>* SearchParams::getTargetList()
{
	return target_list;
}

void SearchParams::clearTargetList()
{
	QList<SearchTarget*>::iterator it;
	for (it = target_list->begin(); it != target_list->end(); ++it) {
		delete (*it);
	}
	target_list->clear();
}
