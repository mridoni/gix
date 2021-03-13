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

#include "MainWindow.h"
#include "OutputWindow.h"
#include "WatchWindow.h"
#include "ProjectCollection.h"
#include "Project.h"
#include "DebugManager.h"
#include "IdeStatus.h"
#include "ListingFileParser.h"
#include "QLogger.h"
#include "MetadataLoader.h"

#include <QQueue>

class DebugManager;

Q_DECLARE_METATYPE(IdeStatus)
Q_DECLARE_METATYPE(QLogger::LogLevel);

class LogBacklogEntry {
public:
	QString module;
	QString msg;
	QLogger::LogLevel level;
};

class IdeTaskManager : public QObject
{
	//friend bool MetadataLoader::updateFileMetadata(ProjectFile* pf);

	Q_OBJECT
	Q_ENUM(IdeStatus)

public:
	IdeTaskManager();
	~IdeTaskManager();

	DebugManager *getDebugManager();

	void setCurrentProjectCollection(ProjectCollection *);
	ProjectCollection *getCurrentProjectCollection();
	Project *getCurrentProject();

	void init(MainWindow *mw, OutputWindow *ow, ProjectCollectionWindow *pcw, ConsoleWindow *cw, WatchWindow* ww, NavigationWindow *nw);

	void buildAll(QString configuration, QString platform);
	void buildClean(QString configuration, QString platform);
	void buildStop();

	void debugProject(Project *, QString configuration, QString platform);
	void runProject(Project *, QString configuration, QString platform, bool run_detached = false);
	void debugStep();
	void debugStop();
	void debugContinue();
	void debugError();

	bool loadProjectCollection(QString filename);
	bool closeCurrentProjectCollection();
	void loadProjectCollectionState(ProjectCollection *ppj);

	void saveCurrentProjectCollectionState();
	void resetCurrentProjectCollectionState();

	void clearModuleMetadata();
	bool isFileOpen(QString filename);
	bool isFileModified(QString filename);

	IdeStatus getStatus();
	void setStatus(IdeStatus s, bool force_signal = false);

	void addBreakpoint(QString, int);
	void removeBreakpoint(QString, int);
	QStringList getBreakpoints();
	void setBreakpoints(QStringList);
	bool existsBreakpoint(QString, int);

	void setWatchedVars(QStringList);
	QStringList getWatchedVars();
	void refreshWatchWindow();

	void addBookmark(QString, int);
	void removeBookmark(QString, int);
	void setBookmarks(QStringList);
	QStringList getBookmarks();
	bool existsBookmark(QString, int);
	void clearBookmarks(QString module);
	bool isDebugOutputEnabled();
	QString getNextBookmark();
	QString getPrevBookmark();
	QStringList getBookmarks(QString filename);

	void statusShowMessage(QString);
	void statusSetRangeMin(int);
	void statusSetRangeMax(int);
	void statusSetRangeValue(int);
	void statusSetRangeEnable(bool);

	QString getCurrentConfiguration();
	QString getCurrentPlatform();

	void consoleWriteStdOut(QString msg);
	void consoleWriteStdErr(QString msg);
	void consoleClear();

	//ListingFileParser *getListingFileParser(ProjectFile *prj, QString, QString);

	void flushLog();

	//CobolModuleMetadata* getModuleMetadata(ProjectFile *pf);
	//CobolModuleMetadata *getModuleMetadata(const QString& module_name);
	//CobolModuleMetadata *getModuleMetadataBySourceFile(const QString &src_file);

	//void setModuleMetadata(ProjectFile *pf, CobolModuleMetadata *cmm);

	void setIdeElementInfo(QString k, QVariant v);
	QVariant getIdeElementInfo(QString k);

	void gotoDefinition(CodeEditor *ce, QString s, int ln);
	void gotoDefinition(DataEntry *e);
	void gotoDefinition(Paragraph* p);
	void gotoFileLine(QString filename, int ln);

	bool backgroundTasksEnabled();
	void setBackgroundTasksEnabled(bool b);

signals:
	void IdeReady();
	void IdeStatusChanged(IdeStatus);
	void IdeEditorChangedPosition(QString, int);
	void IdeDebuggerBreak();

	void SettingsChanged();

	void stopBuildInvoked();
	void buildFinished(int);

	void print(QString msg, QLogger::LogLevel log_level);

	void fileLoaded(ProjectFile *);
	void fileSaved(ProjectFile *);
	void fileClosed(ProjectFile *);
	void fileActivated(ProjectFile *);
	void fileAddedToProject(ProjectFile*);

	void projectAdded(Project *);

	void projectCollectionClosed();
	void projectCollectionLoaded();

	//void updatedModuleMetadata(const Project *prj, const QString &module_name, CobolModuleMetadata *cmm, bool receiver_is_owner);
	//void updatedModuleMetadataFile(const Project *prj, const QString &module_name);

public slots:
	void logMessage(QString module, QString message, QLogger::LogLevel);
	void debugStopped();
	void debugStarted();


private:
	IdeStatus ide_status;

	MainWindow *main_window;
	OutputWindow *output_window;
	ProjectCollectionWindow *prjcoll_window;
	ProjectCollection *current_project_collection;
	ConsoleWindow *console_window;
	WatchWindow * watch_window;
	NavigationWindow* navigation_window;

	DebugManager *debug_manager;
	QMap<QString, QVariant> current_project_collection_data;

	//ListingFileManager listing_file_manager;

	QQueue<LogBacklogEntry *> log_backlog;

	void startLoadingMetadata(ProjectItem *pi);
	MdiChild* openFileNoSignals(QString filename);
	bool rebuildAllMetadata();

	QMap<QString, CobolModuleMetadata*> module_metadata_filemap;
	QMap<QString, CobolModuleMetadata*> module_metadata_map;

	int current_bookmark;
	bool background_tasks_enabled;

	BuildTarget *current_debug_target = nullptr;

	QMap<QString, QVariant> ide_element_info_map;
};

