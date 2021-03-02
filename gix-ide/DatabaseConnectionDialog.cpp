#include "DatabaseConnectionDialog.h"
#include "DbInterfaceFactory.h"
#include "ConnectionString.h"
#include "Connection.h"
#include "UiUtils.h"
#include "IdeDbManager.h"
#include "Ide.h"

DatabaseConnectionDialog::DatabaseConnectionDialog(QWidget* parent) :

	QDialog(parent)
{

	// this dialog is modal
	setModal(true);
	// the title of this dialog
	setWindowTitle(tr("Database connection"));
	// place each GUI component
	setUpGUI();
	// load available drivers
	findAvailableDrivers();

	updateFields();

	conn_info = nullptr;
}

IConnectionString *DatabaseConnectionDialog::getConnectionInfo()
{
	return conn_info;
}

void DatabaseConnectionDialog::updateFields()
{
	auto dbtype = comboDatabaseDriverName->currentText();

	if (dbtype.toLower() == "odbc") {
		labelDatabaseHostName->setText(tr("DSN"));
		
		labelDatabasePort->setEnabled(false);
		editDatabasePort->setEnabled(false);
		
		labelDatabaseName->setEnabled(false);
		editDatabaseName->setEnabled(false);

		labelDefaultSchema->setEnabled(false);
		editDefaultSchema->setEnabled(false);
	}

	if (dbtype.toLower() == "pgsql") {
		labelDatabaseHostName->setText(tr("Host Name"));
		
		labelDatabasePort->setEnabled(true);
		editDatabasePort->setEnabled(true);

		labelDatabaseName->setEnabled(true);
		editDatabaseName->setEnabled(true);

		labelDefaultSchema->setEnabled(true);
		editDefaultSchema->setEnabled(true);
	}

	if (dbtype.toLower() == "mysql") {
		labelDatabaseHostName->setText(tr("Host Name"));
		
		labelDatabasePort->setEnabled(true);
		editDatabasePort->setEnabled(true);

		labelDatabaseName->setEnabled(true);
		editDatabaseName->setEnabled(true);

		labelDefaultSchema->setEnabled(true);
		editDefaultSchema->setEnabled(true);
	}
}

void DatabaseConnectionDialog::setUpGUI() {


	// create all gui components
	labelDatabaseDriverName = new QLabel(tr("Database Type (driver name)"), this);
	labelDatabasePort = new QLabel(tr("TCP/IP Port Number"), this);
	labelDatabaseName = new QLabel(tr("Database Name"), this);
	labelDatabaseHostName = new QLabel(tr("Host Name"), this);
	labelDatabaseUsername = new QLabel(tr("Username"), this);
	labelDatabasePassword = new QLabel(tr("Password"), this);
	labelDefaultSchema = new QLabel(tr("Default Schema"), this);
	labelDatabaseURL = new QLabel(this);
	labelDatabaseURL->setAlignment(Qt::AlignCenter);
	labelSavePassword = new QLabel(tr("Save password"), this);

	comboDatabaseDriverName = new QComboBox(this);
	editDatabaseHostName = new QLineEdit(this);
	editDatabasePort = new QLineEdit(this);
	editDatabaseName = new QLineEdit(this);
	editDatabaseUsername = new QLineEdit(this);
	editDatabasePassword = new QLineEdit(this);
	editDefaultSchema = new QLineEdit(this);
	checkSavePassword = new QCheckBox(this);

	editDatabasePassword->setEchoMode(QLineEdit::Password);
	connect(editDatabaseName,
		SIGNAL(editingFinished()),
		this,
		SLOT(slotCheckFormData()));
	connect(editDatabaseHostName,
		SIGNAL(editingFinished()),
		this,
		SLOT(slotCheckFormData()));
	connect(editDatabaseUsername,
		SIGNAL(editingFinished()),
		this,
		SLOT(slotCheckFormData()));
	connect(editDatabasePassword,
		SIGNAL(editingFinished()),
		this,
		SLOT(slotCheckFormData()));
	connect(editDatabasePassword,
		SIGNAL(returnPressed()),
		this,
		SLOT(slotCheckFormData()));

	// create the button box
	buttons = new QDialogButtonBox(this);
	buttons->addButton(QDialogButtonBox::Ok);
	buttons->addButton(QDialogButtonBox::Cancel);
	QPushButton* okButton = buttons->button(QDialogButtonBox::Ok);
	okButton->setText(tr("Connect"));
	okButton->setEnabled(false);
	connect(buttons,
		SIGNAL(accepted()),
		this,
		SLOT(slotPerformConnection()));
	connect(buttons,
		SIGNAL(rejected()),
		this,
		SLOT(close()));
	// create a vertical layout to display components

	QVBoxLayout* verticalLayout = new QVBoxLayout(this);
	// create a grid layout to add all the components

	QGridLayout* formGridLayout = new QGridLayout(this);
	QGroupBox* gridGroupBox = new QGroupBox(this);
	gridGroupBox->setTitle(tr("Database connection properties"));
	formGridLayout->addWidget(labelDatabaseDriverName, 0, 0);
	formGridLayout->addWidget(comboDatabaseDriverName, 0, 1);
	labelDatabaseDriverName->setBuddy(comboDatabaseDriverName);
	formGridLayout->addWidget(labelDatabaseHostName, 1, 0);
	formGridLayout->addWidget(editDatabaseHostName, 1, 1);
	labelDatabaseHostName->setBuddy(editDatabaseHostName);
	formGridLayout->addWidget(labelDatabasePort, 2, 0);
	formGridLayout->addWidget(editDatabasePort, 2, 1);
	labelDatabasePort->setBuddy(editDatabasePort);
	formGridLayout->addWidget(labelDatabaseName, 3, 0);
	formGridLayout->addWidget(editDatabaseName, 3, 1);
	labelDatabaseName->setBuddy(editDatabaseName);
	formGridLayout->addWidget(labelDatabaseUsername, 4, 0);
	formGridLayout->addWidget(editDatabaseUsername, 4, 1);
	labelDatabaseUsername->setBuddy(editDatabaseUsername);
	formGridLayout->addWidget(labelDatabasePassword, 5, 0);
	formGridLayout->addWidget(editDatabasePassword, 5, 1);
	formGridLayout->addWidget(labelDefaultSchema, 6, 0);
	formGridLayout->addWidget(editDefaultSchema, 6, 1);
	formGridLayout->addWidget(labelSavePassword, 7, 0);
	formGridLayout->addWidget(checkSavePassword, 7, 1);
	labelDatabasePassword->setBuddy(editDatabasePassword);
	// add all the elements to groupbox
	gridGroupBox->setLayout(formGridLayout);
	// place a new groupbox to contain the database connection URL

	QGroupBox* urlGroupBox = new QGroupBox(this);
	urlGroupBox->setTitle(tr("Database URL"));
	QHBoxLayout* urlLayout = new QHBoxLayout(this);
	urlLayout->addWidget(labelDatabaseURL);
	urlGroupBox->setLayout(urlLayout);
	// nest all layouts together

	verticalLayout->addWidget(gridGroupBox);
	verticalLayout->addStretch();
	verticalLayout->addWidget(urlGroupBox);
	verticalLayout->addWidget(buttons);
	comboDatabaseDriverName->setFocus();

	connect(comboDatabaseDriverName, &QComboBox::currentTextChanged, this, [this] { updateFields(); });

}

void DatabaseConnectionDialog::findAvailableDrivers() {

	// remove all items
	comboDatabaseDriverName->clear();
	// populate the combo box with all available drivers

	foreach(std::string driverName, DbInterfaceFactory::getAvailableDrivers())
		comboDatabaseDriverName->addItem(QString::fromStdString(driverName).toUpper());
}

bool DatabaseConnectionDialog::slotCheckFormData() {

	return checkFormData();
}

bool DatabaseConnectionDialog::checkFormData() {

	QString dbtype = comboDatabaseDriverName->currentText().toLower();
	if ((dbtype != "odbc" && editDatabaseName->text().isEmpty())
		|| editDatabaseHostName->text().isEmpty()
		|| editDatabaseUsername->text().isEmpty()
		|| editDatabasePassword->text().isEmpty())
		buttons->button(QDialogButtonBox::Ok)->setEnabled(false);
	else {
		// enable the connect button and give focus
		buttons->button(QDialogButtonBox::Ok)->setEnabled(true);
		buttons->button(QDialogButtonBox::Ok)->setFocus();
	}


	// if the connection can be established (or at least tried)
	// display the URL
	if (buttons->button(QDialogButtonBox::Ok)->isEnabled()) {
		QString cs = buildConnectionString();
		labelDatabaseURL->setText(cs);
	}
	else
		labelDatabaseURL->setText("");
	return buttons->button(QDialogButtonBox::Ok)->isEnabled();
}

void DatabaseConnectionDialog::doDatabaseConnection() 
{
	QString cs = buildConnectionString();
	ConnectionString *conn = ConnectionString::parseEx(cs.toStdString());
	if (conn) {
		if (!Ide::DbManager()->test(conn)) {
			UiUtils::ErrorDialog(tr("Cannot connect, please check the connection parameters"));
			return;
		}
		conn_info = conn;
		Ide::DbManager()->newConnection(conn_info, checkSavePassword->isChecked());
		this->hide();
	}
	else
		UiUtils::ErrorDialog(tr("Invalid parameters"));
}

QString DatabaseConnectionDialog::buildConnectionString()
{
	QString cs = comboDatabaseDriverName->currentText().toLower() + "://" + editDatabaseUsername->text() + ":" + editDatabasePassword->text() + "@" + editDatabaseHostName->text();

	if (!editDatabasePort->text().isEmpty())
		cs += ":" + editDatabasePort->text();

	if (!editDatabaseName->text().isEmpty())
		cs += "/" + editDatabaseName->text();

	if (!editDefaultSchema->text().isEmpty())
		cs += ("?" + editDefaultSchema->text());

	return cs;
}

void DatabaseConnectionDialog::slotPerformConnection() {

	// perform another check against the user data
	if (slotCheckFormData())
		doDatabaseConnection();
}

void DatabaseConnectionDialog::setDatabaseName(const QString& dbName) {

	editDatabaseName->setText(dbName);
}

void DatabaseConnectionDialog::setDatabasePortNumber(int portNumber) {

	if (portNumber)
		editDatabasePort->setText(QString::number(portNumber));
	else
		editDatabasePort->setText("");
}

void DatabaseConnectionDialog::setDatabaseHostName(const QString& hostname) {

	editDatabaseHostName->setText(hostname);
}

void DatabaseConnectionDialog::setDatabaseUsername(const QString& username) {

	editDatabaseUsername->setText(username);
}

void DatabaseConnectionDialog::setDatabaseDriverName(const QString& drvName) {

	int index = comboDatabaseDriverName->findText(drvName);
	if (index >= 0)
		comboDatabaseDriverName->setCurrentIndex(index);
}

void DatabaseConnectionDialog::setDatabasePassword(const QString& pwd) {

	editDatabasePassword->setText(pwd);
}

void DatabaseConnectionDialog::run(bool testOnConfirm) {
	test_connection = testOnConfirm;
	exec();
}