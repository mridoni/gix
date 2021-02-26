#pragma once

#include <QMainWindow>
#include <QPushButton>
#include <QTreeWidget>

#include "IdeStatus.h"
//#include "IConnectionString.h"
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
