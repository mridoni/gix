#pragma once
#include "PropertyOptionsDialog.h"

#include "ui_RestOptions.h"

class ProjectFile;

class RestOptionsDialog :
	public PropertyOptionsDialog, private Ui::Ui_RestOptions
{
public:
	RestOptionsDialog(QMainWindow* mw = 0);
	~RestOptionsDialog();

	// Inherited via PropertyOptionsDialog
	virtual bool show(ProjectItem* pi, PropertyDefinition* pd) override;
	virtual void accept() override;
	virtual void cancel() override;

private:
	QStringList get_itf_candidates_from_copy(ProjectFile *pf);
	void init_itf_fields(const QComboBox &cbcopy, int copyidx, QComboBox &cbfld);
};

