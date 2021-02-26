#pragma once

#include "EolMode.h"
#include "GixGlobals.h"

class IdeTaskManager;
class IdeSearchManager;
class CompilerManager;
class IdeDbManager;
class IdeTargetManager;
class IdeMetadataManager;

class Ide
{
public:
	Ide();
	~Ide();

	static void init();

	static IdeTaskManager *TaskManager();
	static IdeSearchManager *SearchManager();
	static IdeDbManager *DbManager();

	static EolMode getEolModeFromSettings();

private:
	static GixGlobalsCallbacks ide_callbacks;
};