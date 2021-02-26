#include "SearchDialog.h"

#include <QtWidgets>

#include "SysUtils.h"
#include "UiUtils.h"
#include "ProjectCollection.h"
#include "IdeTaskManager.h"
#include "Ide.h"

static const char* DEFAULT_CONFIGURATION = "debug";
static const char* DEFAULT_PLATFORM = "x64";

SearchDialog::SearchDialog(MainWindow* mw) : QDialog(mw)
{
	main_window = mw;

	setupUi(this);

	this->setAttribute(Qt::WA_DeleteOnClose);
	this->tabFindContainer->removeTab(2);
	this->setWindowFlags(windowFlags() & ~Qt::WindowContextHelpButtonHint);

	
	connect(tabFindContainer, &QTabWidget::currentChanged, this, &SearchDialog::searchTypeTabChanged);
	connect(btnClose, &QPushButton::clicked, this, [this] { close(); });
	connect(btnFindNext, &QPushButton::clicked, this, [this] { findNext(); });
	connect(btnCount, &QPushButton::clicked, this, [this] { count(); });
	connect(btnReplace, &QPushButton::clicked, this, [this] { replace(); });
	connect(btnReplaceAll, &QPushButton::clicked, this, [this] { replaceAll(); });
	connect(btnReplaceInOpenDocs, &QPushButton::clicked, this, [this] { replaceInOpenDocs(); });

	//connect(this, &SearchDialog::focusOutEvent, this, [this] { this->setWindowOpacity(0.5); });
}

void SearchDialog::show()
{
	QWidget::show();
}

bool SearchDialog::event(QEvent* e)
{
	if (e->type() == QEvent::ActivationChange) {
		
			this->setWindowOpacity(this->isActiveWindow() ? 1.0 : 0.5);
	}
	/*char bfr[256];
	sprintf(bfr, "%d\n", e->type());
	qDebug(bfr);*/

	return QDialog::event(e);
}

SearchDialog::~SearchDialog()
{
	//delete(tabs);
}

void SearchDialog::setup(SearchType type, QString search_spec)
{
	this->tabFindInFiles->setVisible(false);

	setupUiForSearchType(type);

	cbSearchSpec->lineEdit()->setFocus();

	MdiChild* cur_win = main_window->activeMdiChild();
	if (cur_win) {
		search_spec = cur_win->selectedText();
		if (search_spec.size() > 0) {

			cbSearchSpec->lineEdit()->setText(search_spec);
			cbSearchSpec->lineEdit()->selectAll();
		}
	}

	SearchParams* sp = Ide::SearchManager()->getCurrentSearchParams();
	optModeNormal->setChecked(sp->mode == SearchMode::Normal);
	optModeRegex->setChecked(sp->mode == SearchMode::Regex);
	optDirUp->setChecked(sp->direction == SearchDirection::Up);
	optDirDown->setChecked(sp->direction == SearchDirection::Down);
	cbOptMatchCase->setChecked(sp->match_case);
	cbOptWholeWord->setChecked(sp->whole_word);
	cbOptWrapSearch->setChecked(sp->wrap_search);
}

void SearchDialog::setupUiForSearchType(SearchType mode)
{
	switch (mode) {
	case SearchType::Find:
		tabFindContainer->setCurrentIndex(0);
		cbReplaceSpec->setVisible(false);
		lblReplaceSpec->setVisible(false);
		btnReplace->setVisible(false);
		btnReplaceAll->setVisible(false);
		btnReplaceInOpenDocs->setVisible(false);
		break;
	case SearchType::Replace:
		tabFindContainer->setCurrentIndex(1);
		cbReplaceSpec->setVisible(true);
		lblReplaceSpec->setVisible(true);
		btnReplace->setVisible(true);
		btnReplaceAll->setVisible(true);
		btnReplaceInOpenDocs->setVisible(true);
		break;
	case SearchType::FindinFiles:
		tabFindContainer->setCurrentIndex(2);
		break;
	default:
		break;
	}
}

void SearchDialog::searchTypeTabChanged(int idx)
{
	SearchType t = tabIndexToSearchType(idx);
	setupUiForSearchType(t);
}

void SearchDialog::findNext()
{

	QString search_spec = cbSearchSpec->currentText();
	if (search_spec.size() == 0)
		return;

	Ide::SearchManager()->getSearchHistory().append(search_spec);

	SearchParams* sp = Ide::SearchManager()->getCurrentSearchParams();

	MdiChild* cur_win = main_window->activeMdiChild();
	if (cur_win) {
		SearchTarget* st = nullptr;
		if (sp->getTargetList()->size() == 0 || cur_win != sp->getTargetList()->at(0)->document) {
			st = new SearchTarget(cur_win, cur_win->currentPos());
			sp->clearTargetList();
			sp->getTargetList()->append(st);
			sp->current_target = 0;
			st->current_position = cur_win->currentPos();			
		}
		else {
			st = sp->getTargetList()->at(0);
		}

		sp->search_spec = search_spec;

		sp->match_case = cbOptMatchCase->isChecked();
		sp->whole_word = cbOptWholeWord->isChecked();
		sp->wrap_search = cbOptWrapSearch->isChecked();

		sp->mode = optModeNormal->isChecked() ? SearchMode::Normal : SearchMode::Regex;
		sp->direction = optDirUp->isChecked() ? SearchDirection::Up : SearchDirection::Down;

		//Ide::SearchManager()->last_used_direction = sp->direction;
		//Ide::SearchManager()->last_used_direction = sp->direction;

		Ide::SearchManager()->findNext();
	}


}

void SearchDialog::count()
{
	QString search_spec = cbSearchSpec->currentText();
	if (search_spec.size() == 0)
		return;

	Ide::SearchManager()->getSearchHistory().append(search_spec);

	SearchParams* sp = Ide::SearchManager()->getCurrentSearchParams();

	MdiChild* cur_win = main_window->activeMdiChild();
	if (cur_win) {
		SearchTarget* st = nullptr;
		if (sp->getTargetList()->size() == 0 || cur_win != sp->getTargetList()->at(0)->document) {
			st = new SearchTarget(cur_win, cur_win->currentPos());
			sp->clearTargetList();
			sp->getTargetList()->append(st);
			sp->current_target = 0;
		}
		else {
			st = sp->getTargetList()->at(0);
		}

		sp->search_spec = search_spec;
		sp->replace_spec = cbReplaceSpec->currentText();

		sp->match_case = cbOptMatchCase->isChecked();
		sp->whole_word = cbOptWholeWord->isChecked();
		sp->wrap_search = cbOptWrapSearch->isChecked();

		sp->mode = optModeNormal->isChecked() ? SearchMode::Normal : SearchMode::Regex;
		sp->direction = optDirUp->isChecked() ? SearchDirection::Up : SearchDirection::Down;

		int count = Ide::SearchManager()->count();

		if (count == 0)
			UiUtils::InfoDialog(tr("Text not found"));
		else
			UiUtils::InfoDialog(QString("Number of occurrences: %1").arg(count));

	}
}

void SearchDialog::replace()
{
	QString search_spec = cbSearchSpec->currentText();
	if (search_spec.size() == 0)
		return;

	Ide::SearchManager()->getSearchHistory().append(search_spec);

	SearchParams* sp = Ide::SearchManager()->getCurrentSearchParams();

	MdiChild* cur_win = main_window->activeMdiChild();
	if (cur_win) {
		SearchTarget* st = nullptr;
		if (sp->getTargetList()->size() == 0 || cur_win != sp->getTargetList()->at(0)->document) {
			st = new SearchTarget(cur_win, cur_win->currentPos());
			sp->clearTargetList();
			sp->getTargetList()->append(st);
			sp->current_target = 0;
		}
		else {
			st = sp->getTargetList()->at(0);
		}

		sp->search_spec = search_spec;
		sp->replace_spec = cbReplaceSpec->currentText();

		sp->match_case = cbOptMatchCase->isChecked();
		sp->whole_word = cbOptWholeWord->isChecked();
		sp->wrap_search = cbOptWrapSearch->isChecked();

		sp->mode = optModeNormal->isChecked() ? SearchMode::Normal : SearchMode::Regex;
		sp->direction = optDirUp->isChecked() ? SearchDirection::Up : SearchDirection::Down;

		Ide::SearchManager()->replace();
	}
}

void SearchDialog::replaceAll()
{
	QString search_spec = cbSearchSpec->currentText();
	if (search_spec.size() == 0)
		return;

	Ide::SearchManager()->getSearchHistory().append(search_spec);

	SearchParams* sp = Ide::SearchManager()->getCurrentSearchParams();

	MdiChild* cur_win = main_window->activeMdiChild();
	if (cur_win) {
		SearchTarget* st = nullptr;
		if (sp->getTargetList()->size() == 0 || cur_win != sp->getTargetList()->at(0)->document) {
			st = new SearchTarget(cur_win, cur_win->currentPos());
			sp->clearTargetList();
			sp->getTargetList()->append(st);
			sp->current_target = 0;
		}
		else {
			st = sp->getTargetList()->at(0);
		}

		sp->search_spec = search_spec;
		sp->replace_spec = cbReplaceSpec->currentText();

		sp->match_case = cbOptMatchCase->isChecked();
		sp->whole_word = cbOptWholeWord->isChecked();
		sp->wrap_search = cbOptWrapSearch->isChecked();

		sp->mode = optModeNormal->isChecked() ? SearchMode::Normal : SearchMode::Regex;
		sp->direction = optDirUp->isChecked() ? SearchDirection::Up : SearchDirection::Down;

		int count = Ide::SearchManager()->replaceAll();

		if (count == 0)
			UiUtils::InfoDialog(tr("Text not found"));
		else
			UiUtils::InfoDialog(QString("Number of occurrences replaced: %1").arg(count));
	}
}

void SearchDialog::replaceInOpenDocs()
{
}

SearchType SearchDialog::tabIndexToSearchType(int idx)
{
	switch (idx) {
		case 0:
			return SearchType::Find;
		case 1:
			return SearchType::Replace;
		case 2:
			return SearchType::FindinFiles;
	}
	return SearchType::Find;
}

