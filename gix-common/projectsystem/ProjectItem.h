#pragma once

#include <QString>
#include <QList>
#include <QVariant>
#include <QMap>
#include <QUuid>

#include "ProjectItemType.h"
#include "gixcommon_global.h"

class PropertyDefinition;

class GIXCOMMON_EXPORT ProjectItem
{

public:
	ProjectItem();
	virtual ~ProjectItem();

	QString GetFileRelativePath();
	QString GetFileFullPath();
	QString GetFilename();

	virtual QString GetBaseDir();

	bool HasChildren();

	bool isVirtual();
	void setVirtual(bool);

	QList<ProjectItem *> *GetChildren();
	ProjectItem *GetParent();
	void SetParent(ProjectItem *);

	int getIndex();
	int columnCount() const;

	QString getGuid();

	virtual bool isPropertyVisible(PropertyDefinition *);

	virtual ProjectItemType GetItemType() = 0;
	virtual QString GetDisplayName() = 0;
	

protected:
	ProjectItem *parent_item;
	QString filepath;
	QList<ProjectItem *> *children;
	bool is_virtual;

	QUuid guid;
};

