#include "WatchWindow.h"
#include "UiUtils.h"
#include "Ide.h"
#include "IdeTaskManager.h"

#include <QToolBar>
#include <QLabel>
#include <QLineEdit>
#include <QComboBox>
#include <QHeaderView>
#include <QBoxLayout>
#include <QPushButton>
#include <QInputDialog>
#include <QSettings>


WatchWindow::WatchWindow(QWidget *parent, MainWindow *mw) : QMainWindow(parent)
{
	this->setWindowTitle("Watch");
	this->setMinimumWidth(300);
	this->setWindowFlags(Qt::Widget); // <---------
	QToolBar* toolBar = new QToolBar(this);
	this->addToolBar(toolBar);
	this->mainWindow = mw;

	bAdd = new QPushButton(this);
	bAdd->setFixedSize(16, 16);
	const QIcon addIcon = QIcon(":/icons/bullet_add.png");
	bAdd->setIcon(addIcon);
	toolBar->addWidget(bAdd);

	bRemove = new QPushButton(this);
	bRemove->setFixedSize(16, 16);
	const QIcon removeIcon = QIcon(":/icons/bullet_delete.png");
	bRemove->setIcon(removeIcon);
	toolBar->addWidget(bRemove);

	bRefresh = new QPushButton(this);
	bRefresh->setFixedSize(16, 16);
	const QIcon refreshIcon = QIcon(":/icons/bullet_refresh.png");
	bRefresh->setIcon(refreshIcon);
	toolBar->addWidget(bRefresh);

	this->varTable = new QTableWidget(0, 2, this);
	this->setCentralWidget(varTable);

	varTable->setRowCount(0);
	QHeaderView* header = varTable->horizontalHeader();
	header->setSectionResizeMode(QHeaderView::Stretch);
	header->hide();

	connect(bAdd, &QPushButton::clicked, this, &WatchWindow::addButtonClicked);
	connect(bAdd, &QPushButton::clicked, this, &WatchWindow::removeButtonClicked);
}


WatchWindow::~WatchWindow()
{
}

void WatchWindow::IdeStatusChanged(IdeStatus s)
{
	bAdd->setEnabled(s == IdeStatus::DebuggingOnBreak);
	bRefresh->setEnabled(s == IdeStatus::DebuggingOnBreak);
	bRemove->setEnabled(s == IdeStatus::DebuggingOnBreak);

	if (s == IdeStatus::DebuggingOnBreak)
		refreshContent();
}

void WatchWindow::onDebuggerBreak()
{
	refreshContent();
}

void WatchWindow::refreshContent()
{
	QSettings settings;

	varTable->clear();

	if (Ide::TaskManager()->getDebugManager() == nullptr)
		return;

	DebugManager *debug_manager = Ide::TaskManager()->getDebugManager();

	varTable->setRowCount(debug_manager->getWatchedVarCount());
	varTable->setColumnCount(2);

	QHeaderView* header = varTable->horizontalHeader();
	header->setSectionResizeMode(QHeaderView::Stretch);
	header->setSizeAdjustPolicy(QAbstractScrollArea::AdjustToContents);
	header->hide();

	QHeaderView* vheader = varTable->verticalHeader();
	vheader->hide();

	vheader->setDefaultSectionSize(20);
	int font_size = settings.value("console_font_size", 8).toInt();

	QMap<QString,QString> watched_var_contents = Ide::TaskManager()->getDebugManager()->getPrintableVarListContent(debug_manager->getWatchedVarList());

	for (int i = 0; i < watched_var_contents.size(); i++) {
		QString name = watched_var_contents.keys().at(i);
		QString content = watched_var_contents.value(name);

		QTableWidgetItem *tw = new QTableWidgetItem();
		tw->setText(name);
		tw->setFlags(tw->flags() &  ~Qt::ItemIsEditable);

		QFont originalFont = tw->font();
		originalFont.setPointSize(UiUtils::computeFontSize(this, font_size));
		tw->setFont(originalFont);

		QLabel *qle = new QLabel(varTable);
		qle->setFont(originalFont);
		qle->setText(content);
		varTable->setCellWidget(i, 1, qle);
		varTable->setItem(i, 0, tw);
	}
}

void WatchWindow::addButtonClicked()
{
	bool ok;
	QString text = QInputDialog::getText(this, tr("Enter a variable/file name"),
		tr("Variable/field name:"), QLineEdit::Normal,
		"", &ok);

	if (!ok)
		return;

	if (text.isEmpty()) {
		UiUtils::ErrorDialog(tr("Invalid variable/field name"));
		return;
	}

	Ide::TaskManager()->getDebugManager()->addWatchedVar(text);

	refreshContent();
}

void WatchWindow::removeButtonClicked()
{
	QList<QTableWidgetItem *> selected = varTable->selectedItems();
	if (selected.size() == 0)
		return;

	for (int i = 0; i < selected.size(); i++) {
		QTableWidgetItem *item = selected.at(i);
		QString text = item->text();
		if (!text.isEmpty())
			Ide::TaskManager()->getDebugManager()->removeWatchedVar(text);
	}

	refreshContent();
}
