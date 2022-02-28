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

#include <QMainWindow>
#include <QPushButton>
#include <QTreeWidget>

#include "IdeStatus.h"
//#include "IDataSourceInfo.h"
//#include "DbConnection.h"
#include "MainWindow.h"
//#include "DbCodeGenerator.h"

class DbConnection;
class TableInfo;
class SchemaInfo;

enum class CodeGenerationType;
enum class CodeGenerationDest;

class DbManagerWindow : public QMainWindow
{
    Q_OBJECT

        friend class MainWindow;

public:
    DbManagerWindow(QWidget* parent, MainWindow* mw);
    ~DbManagerWindow();

    //public slots:
    //      void IdeStatusChanged(IdeStatus);

private slots:
    void addConnection();
    void connectionAdded(QString, DbConnection*);
    void prepareItemMenu(const QPoint& pos);

private:
    MainWindow* mainWindow;
    QPushButton* bAddConnection;

    QTreeWidget* dbTree;

    bool askForPassword(QString conn_name, QString& pwd);

    DbConnection* getItemConnection(QTreeWidgetItem* item);
    TableInfo* getItemTable(QTreeWidgetItem* item);
    SchemaInfo* getItemSchema(QTreeWidgetItem* item);

    void generateCode(DbConnection* conn, TableInfo* ti, CodeGenerationType ctype, CodeGenerationDest cdest);

};
