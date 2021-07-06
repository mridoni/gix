#include "AddCompilerWizard.h"
#include "UiUtils.h"
#include "CompilerDefinition.h"
#include "GixGlobals.h"
#include "PathUtils.h"
#include "linq/linq.hpp"

#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QFileDialog>
#include <QHBoxLayout>
#include <QHBoxLayout>

class CompilerOptsWizardPage : public QWizardPage
{
private:
	QLabel *l_path = nullptr;
	AddCompilerWizard *wiz = nullptr;

public:
	CompilerOptsWizardPage(QWidget *parent) : QWizardPage(parent)
	{
		wiz = (AddCompilerWizard *)parent;

		setTitle(tr("Locate compiler install"));

		QLabel *label = new QLabel(tr("GnuCOBOL install path"), this);
		label->setWordWrap(false);

		l_path = new QLabel(this);
		l_path->setStyleSheet("border: 1px solid #c0c0c0");

		QPushButton *b_path = new QPushButton("...", this);
		b_path->setFixedSize(24, 24);

		connect(b_path, &QPushButton::clicked, this, [this] {
			QFileDialog dialog;
			dialog.setFileMode(QFileDialog::Directory);
			dialog.setOption(QFileDialog::ShowDirsOnly);

			if (dialog.exec()) {
				QString path = dialog.selectedFiles()[0];
				this->l_path->setText(path);
			}
		});

		QHBoxLayout *hl = new QHBoxLayout();
		hl->addWidget(l_path);
		hl->addWidget(b_path);
		hl->setContentsMargins(0, 0, 24, 0);

		QVBoxLayout *layout = new QVBoxLayout;
		layout->addWidget(label);
		layout->addLayout(hl);

		setLayout(layout);
	}

	bool validatePage()
	{
		if (l_path->text().trimmed() == "" || !QDir(l_path->text()).exists()) {
			UiUtils::ErrorDialog(tr("Please select a valid path for your GnuCOBOL install"));
			return false;
		}

		QStringList info;
		
		if (!CompilerDefinition::testConfiguration(l_path->text(), info, &wiz->cdef)) {
			QString msg = tr("<p>This does not seem to be a valid GnuCOBOL install. The following errors occurred:</p><ul>");
			for (QString err : info)
				msg += ("<li>" + err + "</li>");

			msg += "</ul>";
			UiUtils::ErrorDialog(msg);

			return false;
		}

		wiz->setInfo(info);

		return true;
	}


};

class CompilerSaveWizardPage : public QWizardPage
{
private:
	QLineEdit *t_desc = nullptr;
	AddCompilerWizard *wiz = nullptr;
	QLabel *l_version;
	QLabel *l_platforms;
	QLabel *l_c_version;
	QLabel *l_screen_io;
	QLabel *l_idxd_fh;
	QLabel *l_math_lib;
	QLabel *l_xml;
	QLabel *l_json;

public:
	CompilerSaveWizardPage(QWidget *parent) : QWizardPage(parent)
	{
		wiz = (AddCompilerWizard *)parent;

		setTitle(tr("Register compiler install"));

		QLabel *label = new QLabel(tr("GnuCOBOL install description"), this);

		t_desc = new QLineEdit(this);
		t_desc->setStyleSheet("border: 1px solid #c0c0c0;margin-right:24px");

		QVBoxLayout *layout = new QVBoxLayout;
		layout->addWidget(label);
		layout->addWidget(t_desc);

		QHBoxLayout *layout_h = new QHBoxLayout;
		QVBoxLayout *layout_v2 = new QVBoxLayout;
		QVBoxLayout *layout_v3 = new QVBoxLayout;
		layout_h->addLayout(layout_v2);
		layout_h->addLayout(layout_v3);

		layout_v3->addStretch();

		QPushButton *b_details = new QPushButton(tr("Details..."), this);
		b_details->setStyleSheet("margin-right:24px");
		b_details->setFixedWidth(110);
		b_details->setFixedHeight(24);
		layout_v3->addWidget(b_details);

		l_version = new QLabel(this);
		l_platforms = new QLabel(this);
		l_c_version = new QLabel(this);
		l_screen_io = new QLabel(this);
		l_idxd_fh = new QLabel(this);
		l_math_lib = new QLabel(this);
		l_xml = new QLabel(this);
		l_json = new QLabel(this);

		layout_v2->addWidget(l_version);
		layout_v2->addWidget(l_platforms);
		layout_v2->addWidget(l_c_version);
		layout_v2->addWidget(l_screen_io);
		layout_v2->addWidget(l_idxd_fh);
		layout_v2->addWidget(l_math_lib);
		layout_v2->addWidget(l_xml);
		layout_v2->addWidget(l_json);

		layout->addLayout(layout_h);

		setLayout(layout);

		connect(b_details, &QPushButton::clicked, this, [this] {
			if (wiz && wiz->getInfo().size()) {
				QString msg = "<pre>" + wiz->getInfo().join("\r\n") + "</pre>";
				UiUtils::InfoDialog(msg);
			}
		});
	}

	QString lineWith(QStringList lines, QString s)
	{
		auto ll = cpplinq::from(lines).where([s](QString a) { return a.toLower().startsWith(s.toLower());  }).to_vector();
		if (ll.size()) {
			QString l = ll.at(ll.size()-1);
			l = l.mid(l.indexOf(':') + 1).trimmed();
			return l;
		}
		return QString();
	}

	void initializePage()
	{
		QStringList info = wiz->getInfo();
		CompilerDefinition *cd = wiz->cdef;

		if (info.size()) {

			l_version->setText(tr("Version") + ": " + info.at(0).mid(info.at(0).lastIndexOf(" ")).trimmed());
			l_platforms->setText(tr("Platform(s)") + ": " + cd->getTargetPlatforms().keys().join(", "));
			l_c_version->setText(tr("C Version") + ": " + lineWith(info, "C version"));
			l_screen_io->setText(tr("Screen I/O") + ": " + lineWith(info, "extended screen I/O"));
			l_idxd_fh->setText(tr("Indexed File Handler") + ": " + lineWith(info, "indexed file handler"));
			l_math_lib->setText(tr("Math Library") + ": " + lineWith(info, "mathematical library"));

			QString has_xml = lineWith(info, "XML library");
			has_xml = (has_xml.isEmpty() || has_xml == "disabled") ? "No" : has_xml;
			l_xml->setText(tr("XML Support") + ": " + has_xml);

			QString has_json = lineWith(info, "JSON library");
			has_json = (has_json.isEmpty() || has_json == "disabled") ? "No" : has_json;
			l_json->setText(tr("JSON Support") + ": " + has_json);
		}
	}

	bool validatePage()
	{
		CompilerDefinition *cd = wiz->cdef;

		if (t_desc->text().trimmed() == "" || t_desc->text().trimmed().size() > 60) {
			UiUtils::ErrorDialog(tr("Please select a valid description for your GnuCOBOL install (max 60 characters)"));
			return false;
		}

		QString compiler_id = "comp_" + QString::number(qrand());
		cd->setName(t_desc->text().trimmed());
		cd->setId(compiler_id);
		cd->setDefinitionFile(PathUtils::combine(GixGlobals::getCompilerDefsDir(), compiler_id + ".def"));
		cd->setVersion(wiz->getInfo().at(0).mid(wiz->getInfo().at(0).lastIndexOf(" ")).trimmed());
		bool res = cd->save();
		if (res) {
			wiz->new_compiler_id = compiler_id;
		}

		return res;
	}


};


AddCompilerWizard::AddCompilerWizard(QWidget *parent): QWizard(parent)
{
	opts_page = new CompilerOptsWizardPage(this);
	save_page = new CompilerSaveWizardPage(this);

	this->addPage(createIntroPage());
	this->addPage(opts_page);
	this->addPage(save_page);
	this->addPage(createConclusionPage());

	this->setWindowTitle("Add compiler");
}

AddCompilerWizard::~AddCompilerWizard()
{
	if (cdef)
		delete cdef;
}

QWizardPage *AddCompilerWizard::createIntroPage()
{
	QWizardPage *page = new QWizardPage;
	page->setTitle("Introduction");

	QLabel *label = new QLabel(tr("This wizard will help you add a new GnuCOBOL install to Gix-IDE"));
	label->setWordWrap(true);

	QVBoxLayout *layout = new QVBoxLayout;
	layout->addWidget(label);
	page->setLayout(layout);

	return page;
}

QWizardPage *AddCompilerWizard::createConclusionPage()
{
	return nullptr;
}

void AddCompilerWizard::setInfo(QStringList il)
{
	info = il;
}

QStringList AddCompilerWizard::getInfo()
{
	return info;
}
