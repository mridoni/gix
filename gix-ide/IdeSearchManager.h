#pragma once

#include <QCoreApplication>
#include <QStringList>
#include "MdiChild.h"

enum class SearchType {
	Find = 1,
	Replace = 2,
	FindinFiles = 3
};

enum class SearchDirection {
	Up = 1,
	Down = 2
};

enum class SearchMode {
	Normal = 1,
	Regex = 2
};

class SearchTarget : QObject {

	Q_OBJECT

public:

	SearchTarget(MdiChild *, int curpos = 0);
	MdiChild* document;
	int current_position;

	SearchDirection last_direction;
	int last_start;
	int last_end;
};

class SearchParams {

public:

	SearchParams() {
		search_spec = "";
		replace_spec = "";
		type = SearchType::Find;
		mode = SearchMode::Normal;
		direction = SearchDirection::Down;
		match_case = false;
		whole_word = false;
		wrap_search = false;
		current_target = 0;
		target_list = new QList<SearchTarget*>();
	};

	bool match_case;
	bool whole_word;
	bool wrap_search;

	int current_target;

	QString search_spec;
	QString replace_spec;

	SearchType type;
	SearchMode mode;
	SearchDirection direction;

	QList<SearchTarget*> *getTargetList();
	void clearTargetList();

private:
	QList<SearchTarget*> *target_list;
};

class IdeSearchManager
{
	Q_DECLARE_TR_FUNCTIONS(IdeSearchManager);


	friend class SearchDialog;

public:

	IdeSearchManager();
	~IdeSearchManager();

	QStringList getSearchHistory();
	QStringList getReplaceHistory();

	SearchType getLastUsedType();
	SearchMode getLastUsedMode();
	SearchDirection getLastUsedDirection();

	SearchParams *getCurrentSearchParams();

	void findNext();
	void replace();
	int replaceAll();
	int count();

private:
	QStringList search_history;
	QStringList replace_history;

	// These are ONLY for no-dialog search (directly from the GUI)
	SearchType last_used_type;
	SearchMode last_used_mode;
	SearchDirection last_used_direction;

	SearchParams *params;

	bool internalFindNext();
};

