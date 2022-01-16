/****************************************************************************
**
** Copyright (C) 2016 The Qt Company Ltd.
** Contact: https://www.qt.io/licensing/
**
** This file is part of the examples of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:BSD$
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and The Qt Company. For licensing terms
** and conditions see https://www.qt.io/terms-conditions. For further
** information use the contact form at https://www.qt.io/contact-us.
**
** BSD License Usage
** Alternatively, you may use this file under the terms of the BSD license
** as follows:
**
** "Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are
** met:
**   * Redistributions of source code must retain the above copyright
**     notice, this list of conditions and the following disclaimer.
**   * Redistributions in binary form must reproduce the above copyright
**     notice, this list of conditions and the following disclaimer in
**     the documentation and/or other materials provided with the
**     distribution.
**   * Neither the name of The Qt Company Ltd nor the names of its
**     contributors may be used to endorse or promote products derived
**     from this software without specific prior written permission.
**
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
** "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
** LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
** A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
** OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
** SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
** LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
** DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
** THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
** (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
** OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
**
** $QT_END_LICENSE$
**
****************************************************************************/

#include "MdiChild.h"
#ifdef __MINGW32__
typedef unsigned char byte;
#endif
#include <QtWidgets>

#include "Ide.h"
#include "IdeTaskManager.h"
#include "UiUtils.h"
#include "PathUtils.h"
#include "DataEntry.h"
#include "ProjectFile.h"

#include "MainWindow.h"
#include "PropertyWindow.h"

static const int FOLD_LEVEL_BASE = 0x400;

#define MAX_DOC_LEN 134217728   // 128 MB

MdiChild::MdiChild()
{
    setAttribute(Qt::WA_DeleteOnClose);
    isUntitled = true;

#if 0
    connect(GixGlobals::getMetadataManager(), &MetadataManager::updatedModuleMetadata, this, [this](CobolModuleMetadata *cmm) {
        ProjectCollection* ppj = Ide::TaskManager()->getCurrentProjectCollection();

        if (ppj && !curFile.isEmpty() && QFile(curFile).exists()) {
            ProjectFile* pf = ppj->locateProjectFileByPath(curFile);
            if (!pf)
                return;

            CobolModuleMetadata *cfm = GixGlobals::getMetadataManager()->getModuleMetadataBySource(pf->GetFileFullPath());
            if (!cfm)
                return;

            int fold_level = 0;
            for (int i = 0; i < this->lineCount(); i++)
                this->setFoldLevel(i, fold_level + FOLD_LEVEL_BASE);

            for (auto e : cfm->getWorkingStorageDataEntries()) {
                add_fold_level(e, fold_level);
            }
        }
    });
#endif


}

void MdiChild::add_fold_level(DataEntry* e, int fold_level)
{
    int level = fold_level + FOLD_LEVEL_BASE;
    if (e->children.size())
        level += SC_FOLDLEVELHEADERFLAG;

    this->setFoldLevel(e->line - 1, level);

    for (auto c : e->children) {
        add_fold_level(c, level + 1);
    }
}

MdiChild::MdiChild(int default_initialization) : CodeEditor(0, default_initialization)
{
    setAttribute(Qt::WA_DeleteOnClose);
    isUntitled = true;
}

void MdiChild::newFile()
{
    static int sequenceNumber = 1;

    isUntitled = true;
    curFile = tr("document%1.txt").arg(sequenceNumber++);
    setWindowTitle(curFile + "[*]");

    connect(this, &ScintillaEdit::notifyChange,
            this, &MdiChild::documentWasModified);

    this->setSavePoint();
    documentWasModified();
}

QObject *getNestedParent(QObject *o, int level)
{
    QObject *res = o;

    for (int i = 0; i < level; i++) {

        if (!res)
            return nullptr;

        res = res->parent();
    }
    return res;
}

bool MdiChild::loadFile(const QString &fileName)
{
    QSettings settings;

    QFile file(fileName);
    if (!file.open(QFile::ReadOnly)) {		// QFile::Text ?
        QMessageBox::warning(this, tr("MDI"),
                             tr("Cannot read file %1:\n%2.")
                             .arg(fileName)
                             .arg(file.errorString()));
        return false;
    }

    QDataStream in(&file);
    QApplication::setOverrideCursor(Qt::WaitCursor);

    QByteArray qba;
    qba.resize(file.size());
    in.readRawData(qba.data(), file.size());

    setText(qba.constData());
    
    QApplication::restoreOverrideCursor();

    setCurrentFile(fileName);

	connect(this, &ScintillaEdit::notifyChange, this, &MdiChild::documentWasModified);

	markerDeleteAll(MRKR_BKP);
	QStringList breakpoints = Ide::TaskManager()->getBreakpoints();
	for (int i = 0; i < breakpoints.size(); i++) {
		QString bkp = breakpoints[i];
        if (!bkp.contains("@"))
            continue;

		QString src_file = bkp.mid(bkp.indexOf("@") + 1);
        int ln = bkp.left(bkp.indexOf("@")).toInt();
        if (src_file == fileName)
            markerAdd(ln - 1, MRKR_BKP);
        else
            GixGlobals::getLogManager()->logMessage(GIX_CONSOLE_LOG, QString("Failed to map file %1 against %2").arg(src_file).arg(fileName), QLogger::LogLevel::Debug);

	}

    markerDeleteAll(MRKR_BOOKMARK);
    QStringList bookmarks = Ide::TaskManager()->getBookmarks(this->curFile);
    for (int i = 0; i < bookmarks.size(); i++) {
        QString bm = bookmarks[i];
        if (!bm.contains(":"))
            continue;

        if (bm.isEmpty())
        	continue;

        int lpos = bm.lastIndexOf(":");
        if (lpos == -1 || lpos >= (bm.size() - 1))
        	continue;

        int ln = bm.mid(lpos + 1).toInt();
        if (!ln)
        	continue;

        markerAdd(ln - 1, MRKR_BOOKMARK);
    }

    this->setSavePoint();
    documentWasModified();

    EolMode cfg_eol_mode = Ide::getEolModeFromSettings();
	EolMode actual_eol_mode = this->getFileActualEolMode();

	setFileEolMode(cfg_eol_mode);
    
	if (actual_eol_mode == EolMode::Mixed) {
        if (UiUtils::YesNoDialog(tr("This file (%1) has mixed EOL characters, do you want to normalize it?").arg(this->curFile))) {
			setFileEolMode(cfg_eol_mode);
        }
    }
	else {
		bool convert_on_open = settings.value("convert_eols_on_open", false).toBool();
		if (convert_on_open && actual_eol_mode != cfg_eol_mode) {
			if (UiUtils::YesNoDialog(tr("This file (%1) has EOL character that are not consistent with the default, do you want to normalize it?").arg(this->curFile))) {
				convertEOLs((int)cfg_eol_mode);
			}
		}
	}

    this->emptyUndoBuffer();

	ProjectCollection* ppj = Ide::TaskManager()->getCurrentProjectCollection();
	if (ppj != nullptr) {
		ProjectFile* pf = this->getProjectFile();
        if (pf) {
            QString fmt = pf->PropertyGetValue("cobc_source_format", settings.value("cobc_default_source_format")).toString();
            this->setSourceFormat(fmt == "fixed" ? SourceFileFormat::Fixed : SourceFileFormat::Free);

            emit Ide::TaskManager()->fileLoaded(pf);
        }
	}

    MainWindow *mw = (MainWindow *)getNestedParent(this, 4);
    QObject *o = (MainWindow *)getNestedParent(this, 4);
    QString c = o->metaObject()->className();

    if (mw) {
        PropertyWindow *pw = mw->getPropertyWindow();
        connect(pw, &PropertyWindow::notifyPropertyValueChanged, this, [this](PropertyDefinition *pd, QVariant v, ProjectItem *pi) {
            if (pd->Name == "cobc_source_format") {
                SourceFileFormat sfmt = (v == "fixed") ? SourceFileFormat::Fixed : SourceFileFormat::Free;
                setSourceFormat(sfmt);

                this->repaint();
            }
        });
    }

    return true;
}

bool MdiChild::save()
{
    if (isUntitled) {
        return saveAs();
    } else {
        return saveFile(curFile);
    }
}

bool MdiChild::saveAs()
{
    QString fileName = QFileDialog::getSaveFileName(this, tr("Save As"),
                                                    curFile);
    if (fileName.isEmpty())
        return false;

    return saveFile(fileName);
}

bool MdiChild::saveFile(const QString &fileName)
{
    QFile file(fileName);
    if (!file.open(QFile::WriteOnly)) { //  | QFile::Text?
        QMessageBox::warning(this, tr("MDI"),
                             tr("Cannot write file %1:\n%2.")
                             .arg(QDir::toNativeSeparators(fileName), file.errorString()));
        return false;
    }

    QDataStream out(&file);
    QApplication::setOverrideCursor(Qt::WaitCursor);

    //
    //QString txt = this->toPlainText();
    //out << txt;
    //out.flush();

    QByteArray qba = this->getText(MAX_DOC_LEN);
    out.writeRawData(qba.constData(), qba.length());

    file.close();

    QApplication::restoreOverrideCursor();

    setCurrentFile(fileName);

    this->setSavePoint();
    documentWasModified();

	ProjectCollection* ppj = Ide::TaskManager()->getCurrentProjectCollection();
	if (ppj != nullptr) {
		ProjectFile* pf = ppj->locateProjectFileByPath(fileName);
		emit Ide::TaskManager()->fileSaved(pf);
	}

    return true;
}

QString MdiChild::userFriendlyCurrentFile()
{
    return strippedName(curFile);
}

void MdiChild::closeEvent(QCloseEvent *event)
{
    if (maybeSave()) {
        event->accept();
    } else {
        event->ignore();
    }

    emit windowClosed(this);
}

void MdiChild::documentWasModified()
{
    setWindowModified(this->modify());
}

bool MdiChild::isMixedEol()
{
    QString t = this->toPlainText();
    int n_cr = t.count('\r');
    int n_lf = t.count('\n');

    if (n_cr == n_lf)
        return false;

    if (n_cr == 0 || n_lf == 0)
        return false;

    return true;
}

EolMode MdiChild::getFileActualEolMode()
{
	QByteArray bfr = this->getText(MAX_DOC_LEN);
	int n_cr = bfr.count('\r');
	int n_lf = bfr.count('\n');

    if (!n_cr && !n_lf)
        return Ide::getEolModeFromSettings();

	if (n_cr == n_lf)
		return EolMode::Windows;

	if (n_cr == 0 && n_lf > 0)
		return EolMode::Unix;

	if (n_cr > 0 && n_lf == 0)
		return EolMode::ClassicMacOS;

	return EolMode::Mixed;
}

EolMode MdiChild::getFileConfiguredEolMode()
{
	return (EolMode)((int)eOLMode());
}

void MdiChild::setFileEolMode(EolMode m)
{
	setEOLMode((int)m);
}

ProjectFile *MdiChild::getProjectFile()
{
    ProjectFile *pf = nullptr;

    if (!curFile.isEmpty()) {
        ProjectCollection *ppj = Ide::TaskManager()->getCurrentProjectCollection();
        if (ppj != nullptr) {
            pf = ppj->locateProjectFileByPath(curFile, true);
        }
    }
    return pf;
}

bool MdiChild::maybeSave()
{
    if (!this->modify())
        return true;

    const QMessageBox::StandardButton ret
            = QMessageBox::warning(this, tr("MDI"),
                                   tr("'%1' has been modified.\n"
                                      "Do you want to save your changes?")
                                   .arg(userFriendlyCurrentFile()),
                                   QMessageBox::Save | QMessageBox::Discard
                                   | QMessageBox::Cancel);
    switch (ret) {
    case QMessageBox::Save:
        return save();
    case QMessageBox::Cancel:
        return false;
    default:
        break;
    }
    return true;
}

void MdiChild::setCurrentFile(const QString &fileName)
{
    curFile = QFileInfo(fileName).canonicalFilePath();
    isUntitled = false;
    //document()->setModified(false);
    setWindowModified(false);
    setWindowTitle(userFriendlyCurrentFile() + "[*]");
}

QString MdiChild::strippedName(const QString &fullFileName)
{
    return QFileInfo(fullFileName).fileName();
}
