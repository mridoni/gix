#pragma once

#include <QString>
#include <QList>
#include <QVariant>

#include "gixcommon_global.h"

class ProjectItem;
class IBuildableItem;

class BuildTarget
{

public:

	GIXCOMMON_EXPORT BuildTarget(BuildTarget * parent);
	GIXCOMMON_EXPORT ~BuildTarget();
	 
	GIXCOMMON_EXPORT QStringList provides();
	GIXCOMMON_EXPORT bool provides(QString c);
	GIXCOMMON_EXPORT bool providesOneOf(QStringList clist);
	 
	GIXCOMMON_EXPORT QStringList uses();
	GIXCOMMON_EXPORT bool uses(QString c);
	GIXCOMMON_EXPORT bool usesOneOf(QStringList clist);
	 
	GIXCOMMON_EXPORT QString filename();
	 
	GIXCOMMON_EXPORT QString buildAction();
	GIXCOMMON_EXPORT QString buildSubAction();
	 
	GIXCOMMON_EXPORT void setProvides(QStringList s);
	GIXCOMMON_EXPORT void setProvides(QString s);
	 
	GIXCOMMON_EXPORT void setUses(QStringList s);
	GIXCOMMON_EXPORT void setUses(QString s);
	 
	GIXCOMMON_EXPORT void setFilename(QString s);
	GIXCOMMON_EXPORT void setBuildAction(QString s);
	GIXCOMMON_EXPORT void setBuildSubAction(QString s);
	 
	//void setLocation(QString s, QString stem_dir);
	 
	GIXCOMMON_EXPORT void setParent(BuildTarget *);
	GIXCOMMON_EXPORT BuildTarget * parent();
	 
	GIXCOMMON_EXPORT QList<BuildTarget *> *dependencies();
	GIXCOMMON_EXPORT bool isUpToDate();
	 
	GIXCOMMON_EXPORT QString toString();

	GIXCOMMON_EXPORT bool isOptional();
	GIXCOMMON_EXPORT void setOptional(bool b);
	 
	GIXCOMMON_EXPORT bool isVirtual();
	GIXCOMMON_EXPORT void setVirtual(bool b);	
	 
	GIXCOMMON_EXPORT IBuildableItem *getItem();
	GIXCOMMON_EXPORT void setItem(IBuildableItem *pi);
	 
	GIXCOMMON_EXPORT BuildTarget *getDependency(const QString &provides);
	 
	GIXCOMMON_EXPORT void setItemOwnership(bool b);

protected:

	QStringList target_provides;
	QStringList target_uses;

	QString target_filename;

	QString target_buildAction;
	QString target_buildSubAction;

	QList<BuildTarget *> *target_dependencies;

	BuildTarget *target_parent;

	bool is_optional;
	bool is_virtual;

	bool has_item_ownership;

	IBuildableItem *item;

	void dump_target(BuildTarget *, QString&, int *);

};

class CustomBuildTarget : public BuildTarget
{
public:
	CustomBuildTarget(BuildTarget *parent) : BuildTarget(parent) {}

private:
	QString cmd;
};

