/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
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

#include "DbManagerWindow.h"
#include "DatabaseConnectionDialog.h"
#include "UiUtils.h"
#include "Ide.h"
#include "ISchemaManager.h"
#include "DbCodeGenerator.h"
#include "PathUtils.h"
#include "IdeTaskManager.h"

#include <QToolBar>
#include <QPushButton>
#include <QMenu>
#include <QInputDialog>
#include <QClipboard>
#include <QApplication>
#include <QFileDialog>

#define NODE_ROOT		0
#define NODE_DB			10
#define NODE_SCHEMAS	50
#define NODE_SCHEMA		51
#define NODE_TABLES		100
#define NODE_TABLE		101
#define NODE_COLUMNS	200
#define NODE_COLUMN		201

#define ROLE_NODE_TYPE Qt::UserRole
#define ROLE_NODE_DATA Qt::UserRole + 1

DbManagerWindow::DbManagerWindow(QWidget* parent, MainWindow* mw)
{
	mainWindow = mw;

	this->setWindowTitle("DB Manager");
	this->setMinimumWidth(150);
	this->setWindowFlags(Qt::Widget);
	QToolBar* toolBar = new QToolBar(this);
	this->addToolBar(toolBar);
	this->mainWindow = mw;

	bAddConnection = new QPushButton(this);
	bAddConnection->setFixedSize(16, 16);
	const QIcon newConnIcon = QIcon(":/icons/database_add.png");
	bAddConnection->setIcon(newConnIcon);
	toolBar->addWidget(bAddConnection);

	connect(bAddConnection, &QPushButton::clicked, this, &DbManagerWindow::addConnection);

	this->dbTree = new QTreeWidget(this);
	dbTree->setSelectionBehavior(QAbstractItemView::SelectRows);
	dbTree->setColumnCount(1);
	dbTree->setUniformRowHeights(true);
	dbTree->setHeaderHidden(true);

	QTreeWidgetItem* depItem = new QTreeWidgetItem((QTreeWidget*)0, QStringList(tr("Connections")));
	depItem->setIcon(0, QIcon(":/icons/database_table.png"));
	depItem->setData(0, ROLE_NODE_TYPE, NODE_ROOT);
	depItem->setData(0, ROLE_NODE_DATA, 0);
	dbTree->addTopLevelItem(depItem);

	this->setCentralWidget(dbTree);

	dbTree->setContextMenuPolicy(Qt::CustomContextMenu);
	connect(dbTree, &QTreeWidget::customContextMenuRequested, this, &DbManagerWindow::prepareItemMenu);

	connect(Ide::DbManager(), &IdeDbManager::connectionAdded, this, &DbManagerWindow::connectionAdded);

	connect(dbTree, &QTreeWidget::itemExpanded, this, [this](QTreeWidgetItem* item) {

		if (item->childCount() > 0)
			return;

		int node_type = item->data(0, ROLE_NODE_TYPE).toInt();

		switch (node_type) {

			case NODE_DB:
			{
				DbConnection* conn = getItemConnection(item);
				if (!conn)
					return;

				IDbInterface* dbi = conn->dbi;
				bool canceled = false;
				if (!conn->internal_conn->isOpen()) {
					if (!conn->save_password) {
						QString pwd;
						canceled = !askForPassword(item->text(0), pwd);
						if (!canceled) {
							conn->conn_info->setPassword(pwd.toStdString());
						}
						else {
							item->setExpanded(false);
						}
					}

					if (canceled)
						return;

					int rc = dbi->connect(conn->conn_info, 0, "UTF-8");
					if (rc) {
						UiUtils::ErrorDialog(tr("Cannot connect to ") + item->text(0));
						item->setExpanded(false);
						return;
					}

				}

				item->setIcon(0, QIcon(":/icons/database_connect.png"));

				QTreeWidgetItem* schemaListItem = new QTreeWidgetItem((QTreeWidget*)0, QStringList(tr("Schemas")));
				schemaListItem->setIcon(0, QIcon(":/icons/folder.png"));
				schemaListItem->setData(0, ROLE_NODE_TYPE, NODE_SCHEMAS);
				schemaListItem->setData(0, ROLE_NODE_DATA, 0);
				schemaListItem->setChildIndicatorPolicy(QTreeWidgetItem::ChildIndicatorPolicy::ShowIndicator);
				item->addChild(schemaListItem);
			}
			break;

			case NODE_SCHEMAS:
			{
				DbConnection* conn = getItemConnection(item);
				if (!conn)
					return;

				std::vector<SchemaInfo*> items;
				if (conn->dbi->manager()->getSchemas(items)) {

					for (SchemaInfo* schema : items) {
						QTreeWidgetItem* sItem = new QTreeWidgetItem((QTreeWidget*)0, QStringList(QString::fromStdString(schema->name)));
						sItem->setIcon(0, QIcon(":/icons/project.png"));
						sItem->setData(0, ROLE_NODE_TYPE, NODE_SCHEMA);
						sItem->setData(0, ROLE_NODE_DATA, QVariant::fromValue(static_cast<void*>(schema)));
						sItem->setChildIndicatorPolicy(QTreeWidgetItem::ChildIndicatorPolicy::ShowIndicator);
						item->addChild(sItem);
					}
				}
			}
			break;

			case NODE_SCHEMA:
			{
				SchemaInfo* si = getItemSchema(item);
				QString schema_name = QString::fromStdString(si->name);
				DbConnection* conn = getItemConnection(item);
				if (!conn)
					return;

				// Tables
				QTreeWidgetItem* tablesItem = new QTreeWidgetItem((QTreeWidget*)0, QStringList(tr("Tables")));
				tablesItem->setIcon(0, QIcon(":/icons/table_multiple.png"));
				tablesItem->setData(0, ROLE_NODE_TYPE, NODE_TABLES);
				tablesItem->setData(0, ROLE_NODE_DATA, 0);
				item->addChild(tablesItem);

				std::vector<TableInfo*> items;
				if (conn->dbi->manager()->getTables(schema_name.toStdString(), items)) {

					for (TableInfo* table : items) {
						QTreeWidgetItem* tblItem = new QTreeWidgetItem((QTreeWidget*)0, QStringList(QString::fromStdString(table->name)));
						tblItem->setIcon(0, QIcon(":/icons/database_table.png"));
						tblItem->setData(0, ROLE_NODE_TYPE, NODE_TABLE);
						tblItem->setData(0, ROLE_NODE_DATA, QVariant::fromValue(static_cast<void*>(table)));
						tblItem->setChildIndicatorPolicy(QTreeWidgetItem::ChildIndicatorPolicy::ShowIndicator);
						tablesItem->addChild(tblItem);
					}
				}

				// Other objects (views, etc.)
				// TODO...
			}
			break;

			case NODE_TABLE:
			{
				DbConnection* conn = getItemConnection(item);
				if (!conn)
					return;

				if (!conn->internal_conn->isOpen())
					return;

				TableInfo* ti = getItemTable(item);
				QString table_name = QString::fromStdString(ti->name);

				QTreeWidgetItem* columnsItem = new QTreeWidgetItem((QTreeWidget*)0, QStringList(tr("Columns")));
				columnsItem->setIcon(0, QIcon(":/icons/table_multiple.png"));
				columnsItem->setData(0, ROLE_NODE_TYPE, NODE_COLUMNS);
				columnsItem->setData(0, ROLE_NODE_DATA, table_name);
				columnsItem->setChildIndicatorPolicy(QTreeWidgetItem::ChildIndicatorPolicy::ShowIndicator);
				item->addChild(columnsItem);

				// Other table features
				// TODO...
			}
			break;

			case NODE_COLUMNS:
			{
				DbConnection* conn = getItemConnection(item);
				if (!conn)
					return;

				if (!conn->internal_conn->isOpen())
					return;

				TableInfo* ti = getItemTable(item);
				SchemaInfo* si = getItemSchema(item);
				QString table_name = QString::fromStdString(ti->name);

				std::vector<ColumnInfo*> items;
				if (conn->dbi->manager()->getColumns(si->name, ti->name, items)) {

					for (ColumnInfo* col : items) {
						QTreeWidgetItem* sItem = new QTreeWidgetItem((QTreeWidget*)0, QStringList(QString::fromStdString(col->name)));
						if (col->is_pk_column)
							sItem->setIcon(0, QIcon(":/icons/key.png"));
						else
							if (col->isNumeric())
								sItem->setIcon(0, QIcon(":/icons/bullet_yellow.png"));
							else
								sItem->setIcon(0, QIcon(":/icons/bullet_green.png"));

						sItem->setData(0, ROLE_NODE_TYPE, NODE_COLUMN);
						sItem->setData(0, ROLE_NODE_DATA, QVariant::fromValue(static_cast<void*>(col)));
						item->addChild(sItem);
					}
				}
			}
			break;
		}

	});

}

DbManagerWindow::~DbManagerWindow()
{}

void DbManagerWindow::connectionAdded(QString name, DbConnection* conn)
{
	QTreeWidgetItem* dbItem = new QTreeWidgetItem((QTreeWidget*)0, QStringList(name));
	dbItem->setIcon(0, QIcon(":/icons/database.png"));
	dbItem->setData(0, ROLE_NODE_TYPE, NODE_DB);
	dbItem->setData(0, ROLE_NODE_DATA, QVariant::fromValue(static_cast<void*>(conn)));
	dbItem->setChildIndicatorPolicy(QTreeWidgetItem::ChildIndicatorPolicy::ShowIndicator);
	dbTree->topLevelItem(0)->addChild(dbItem);
	dbTree->topLevelItem(0)->setExpanded(true);
	//dbItem->setExpanded(true);
}

bool DbManagerWindow::askForPassword(QString conn_name, QString& pwd)
{
	bool ok;
	pwd = "";

	QString text = QInputDialog::getText(this, tr("Enter the password"),
		tr("Password:"), QLineEdit::Password,
		"", &ok);

	if (ok)
		pwd = text;

	return ok;
}

DbConnection* DbManagerWindow::getItemConnection(QTreeWidgetItem* item)
{
	if (!item)
		return nullptr;

	while (item->data(0, ROLE_NODE_TYPE).toInt() != NODE_DB) {
		item = item->parent();
		if (!item)
			return nullptr;
	}

	void* data = item->data(0, ROLE_NODE_DATA).value<void*>();
	DbConnection* conn = (DbConnection*)data;
	return conn;
}

TableInfo* DbManagerWindow::getItemTable(QTreeWidgetItem* item)
{
	if (!item)
		return nullptr;

	while (item->data(0, ROLE_NODE_TYPE).toInt() != NODE_TABLE) {
		item = item->parent();
		if (!item)
			return nullptr;
	}

	void* data = item->data(0, ROLE_NODE_DATA).value<void*>();
	TableInfo* t = (TableInfo*)data;
	return t;
}

SchemaInfo* DbManagerWindow::getItemSchema(QTreeWidgetItem* item)
{
	if (!item)
		return nullptr;

	while (item->data(0, ROLE_NODE_TYPE).toInt() != NODE_SCHEMA) {
		item = item->parent();
		if (!item)
			return nullptr;
	}

	void* data = item->data(0, ROLE_NODE_DATA).value<void*>();
	SchemaInfo* t = (SchemaInfo*)data;
	return t;
}

void DbManagerWindow::generateCode(DbConnection* conn, TableInfo* ti, CodeGenerationType ctype, CodeGenerationDest cdest)
{
	bool rc = false;
	QString res;
	DbCodeGenerator cg;
	QString table_name = QString::fromStdString(ti->name);
	QString schema_name = QString::fromStdString(ti->schema_name);
	QClipboard* clipboard = QApplication::clipboard();
	Project* prj;

	switch (ctype) {
		case CodeGenerationType::CobolCopy:
			cg.setUseUpperCaseForCopyFields(true);
			rc = cg.generateCopyFile(conn->conn_info, schema_name, table_name, res);
			break;
	}

	if (rc && (!res.isEmpty())) {
		switch (cdest) {
			case CodeGenerationDest::Clipboard:
				clipboard->setText(res);
				break;

			case CodeGenerationDest::File:

				QFileDialog dialog;
				dialog.setAcceptMode(QFileDialog::AcceptSave);
				dialog.setOption(QFileDialog::Option::DontConfirmOverwrite, false);
				dialog.setFileMode(QFileDialog::AnyFile);
				dialog.setNameFilter(tr("COBOL COPY File (*.cpy)"));
				dialog.setDefaultSuffix("cpy");
				dialog.selectFile(QString::fromStdString(ti->name).toUpper() + ".cpy");

				prj = Ide::TaskManager()->getCurrentProject();
				if (prj) {
					connect(&dialog, &QFileDialog::directoryEntered, this, [&dialog, prj](QString dir) {
						if (!dir.startsWith(prj->GetBaseDir())) {
							UiUtils::ErrorDialog(QString(tr("You can only add a file under the project's directory")));
							dialog.setDirectory(prj->GetBaseDir());
						}
					});
					dialog.setDirectory(prj->GetBaseDir());
				}

				if (dialog.exec()) {
					QString path = dialog.selectedFiles()[0];
					QFile f(path);
					if (f.open(QIODevice::WriteOnly | QIODevice::Text )) {
						f.write(res.toUtf8());
						f.close();
					}
					else
						UiUtils::ErrorDialog(QString(tr("Cannot save file %1")).arg(path));

					if (prj) {
						ProjectFile* file = new ProjectFile();
						file->PropertySetValue("build_action", "copy");
						file->load(prj, PathUtils::getFilename(path));
						prj->GetChildren()->append(file);

						emit Ide::TaskManager()->fileAddedToProject(file);
						mainWindow->openFile(path);
					}

				}
				break;
		}
	}


}

void DbManagerWindow::addConnection()
{
	DatabaseConnectionDialog* dialog = new DatabaseConnectionDialog(this);

	// is_optional: set the data that will be presented to the user as auto-filled form
	dialog->setDatabaseName("");
	dialog->setDatabasePortNumber(0);
	dialog->setDatabaseHostName("");
	dialog->setDatabaseUsername("");
	dialog->setDatabaseDriverName("ODBC");
	dialog->setDatabasePassword("");

	// enable the connect button if all the data is correct
	dialog->checkFormData();
	// connect the dialog signal to a slot where I will use the connection
	connect(dialog,
		SIGNAL(databaseConnect(QSqlDatabase&)),
		this,
		SLOT(slotHandleNewDatabaseConnection(QSqlDatabase&)));

	// show the dialog (without auto-connection)
	dialog->run(false);

	//if (dialog->getConnectionInfo()) {
	//	UiUtils::InfoDialog(QString::fromStdString(dialog->getConnectionInfo()->get()));
	//}
}

void DbManagerWindow::prepareItemMenu(const QPoint& pos)
{
	QMenu* main_menu = nullptr;
	QTreeWidget* tree = dbTree;
	QTreeWidgetItem* item = tree->itemAt(pos);
	int node_type = item->data(0, ROLE_NODE_TYPE).toInt();
	void* data = item->data(0, ROLE_NODE_DATA).value<void*>();

	switch (node_type) {
		case NODE_TABLE:
		{
			main_menu = new QMenu(this);

			QMenu* menu_sendToClipboard = new QMenu(tr("Send to clipboard"), this);
			//menu_sendToClipboard->setStyleSheet("QMenu::item { padding-left: 8px; }");
			menu_sendToClipboard->setStatusTip(tr("Send to clipboard"));
			QAction* aCopyC = new QAction(tr("COPY file definition"), this);
			QAction* aCreateC = new QAction(tr("CREATE statement"), this);
			QAction* aSelectC = new QAction(tr("SELECT statement"), this);
			QAction* aInsertC = new QAction(tr("INSERT statement"), this);
			QAction* aUpdateC = new QAction(tr("INSERT statement"), this);
			QAction* aDeleteC = new QAction(tr("DELETE statement"), this);
			menu_sendToClipboard->addAction(aCopyC);
			menu_sendToClipboard->addSeparator();
			menu_sendToClipboard->addAction(aCreateC);
			menu_sendToClipboard->addAction(aSelectC);
			menu_sendToClipboard->addAction(aInsertC);
			menu_sendToClipboard->addAction(aDeleteC);
			main_menu->addMenu(menu_sendToClipboard);

			QMenu* menu_SendToFile = new QMenu(tr("Send to new file"), this);
			//menu_SendToFile->setStyleSheet("QMenu::item { padding: 2px 5px 2px 2px; }");
			menu_SendToFile->setStatusTip(tr("Send to new file"));
			QAction* aCopyF = new QAction(tr("COPY file definition"), this);
			QAction* aCreateF = new QAction(tr("CREATE statement"), this);
			QAction* aSelectF = new QAction(tr("SELECT statement"), this);
			QAction* aInsertF = new QAction(tr("INSERT statement"), this);
			QAction* aUpdateF = new QAction(tr("INSERT statement"), this);
			QAction* aDeleteF = new QAction(tr("DELETE statement"), this);
			menu_SendToFile->addAction(aCopyF);
			menu_SendToFile->addSeparator();
			menu_SendToFile->addAction(aCreateF);
			menu_SendToFile->addAction(aSelectF);
			menu_SendToFile->addAction(aInsertF);
			menu_SendToFile->addAction(aDeleteF);
			main_menu->addMenu(menu_SendToFile);

			DbConnection* conn = getItemConnection(item);

			connect(aCopyC, &QAction::triggered, this, [this, data, conn] { generateCode(conn, (TableInfo*)data, CodeGenerationType::CobolCopy, CodeGenerationDest::Clipboard); });
			connect(aCopyF, &QAction::triggered, this, [this, data, conn] { generateCode(conn, (TableInfo*)data, CodeGenerationType::CobolCopy, CodeGenerationDest::File); });
			connect(aCreateC, &QAction::triggered, this, [this, data, conn] { generateCode(conn, (TableInfo*)data, CodeGenerationType::CobolCopy, CodeGenerationDest::Clipboard); });
			connect(aCreateF, &QAction::triggered, this, [this, data, conn] { generateCode(conn, (TableInfo*)data, CodeGenerationType::CobolCopy, CodeGenerationDest::File); });
			connect(aSelectC, &QAction::triggered, this, [this, data, conn] { generateCode(conn, (TableInfo*)data, CodeGenerationType::CobolCopy, CodeGenerationDest::Clipboard); });
			connect(aSelectF, &QAction::triggered, this, [this, data, conn] { generateCode(conn, (TableInfo*)data, CodeGenerationType::CobolCopy, CodeGenerationDest::File); });
			connect(aInsertC, &QAction::triggered, this, [this, data, conn] { generateCode(conn, (TableInfo*)data, CodeGenerationType::CobolCopy, CodeGenerationDest::Clipboard); });
			connect(aInsertF, &QAction::triggered, this, [this, data, conn] { generateCode(conn, (TableInfo*)data, CodeGenerationType::CobolCopy, CodeGenerationDest::File); });
			connect(aUpdateC, &QAction::triggered, this, [this, data, conn] { generateCode(conn, (TableInfo*)data, CodeGenerationType::CobolCopy, CodeGenerationDest::Clipboard); });
			connect(aUpdateF, &QAction::triggered, this, [this, data, conn] { generateCode(conn, (TableInfo*)data, CodeGenerationType::CobolCopy, CodeGenerationDest::File); });
			connect(aDeleteC, &QAction::triggered, this, [this, data, conn] { generateCode(conn, (TableInfo*)data, CodeGenerationType::CobolCopy, CodeGenerationDest::Clipboard); });
			connect(aDeleteF, &QAction::triggered, this, [this, data, conn] { generateCode(conn, (TableInfo*)data, CodeGenerationType::CobolCopy, CodeGenerationDest::File); });
		}
		break;

		//case ProjectItemType::TFolder:
		//	menu = PrepareProjectFolderMenu(item, (ProjectFolder*)p);
		//	break;

		//case ProjectItemType::TFile:
		//	menu = PrepareProjectFileMenu(item, (ProjectFile*)p);
		//	break;

		default:
			main_menu = nullptr;
			break;
	}

	if (main_menu != nullptr) {
		QPoint pt(pos);
		main_menu->exec(tree->mapToGlobal(pos));
	}
}