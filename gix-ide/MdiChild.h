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

#ifdef __MINGW32__
#include <cstddef>
#endif

#include "CodeEditor.h"

#define MRKR_BKP		4
#define MRKR_DBG_CURLINE	5
#define MRKR_BOOKMARK	6

class CobolUtils;
class MdiChild;
class DataEntry;
class ProjectFile;

class MdiChild : public CodeEditor
{
    Q_OBJECT

public:
    MdiChild();
    MdiChild(int init_type);

    void newFile();
    bool loadFile(const QString &fileName);
    bool save();
    bool saveAs();
    bool saveFile(const QString &fileName);
    QString userFriendlyCurrentFile();
    QString currentFile() { return curFile; }

    EolMode getFileConfiguredEolMode();
    EolMode getFileActualEolMode();
    void setFileEolMode(EolMode m);

    ProjectFile *getProjectFile();

signals:
    void caretPositionChanged();
    void windowClosed(MdiChild *);

protected:
    void closeEvent(QCloseEvent *event) override;

private slots:
    void documentWasModified();

private:
    bool isMixedEol();

    bool maybeSave();
    void setCurrentFile(const QString &fileName);
    QString strippedName(const QString &fullFileName);
    void add_fold_level(DataEntry* e, int fold_level);

    QString curFile;
    bool isUntitled;

	//CobolUtils *syntaxHighlighter;
};

