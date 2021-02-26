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

	CobolUtils *syntaxHighlighter;
};

