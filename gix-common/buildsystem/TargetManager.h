#pragma once

#include <QMap>
#include <QString>
#include <QDomElement>

#include "TargetDefinition.h"
#include "MacroManager.h"
#include "IBuildableItem.h"
#include "ProjectItem.h"
#include "gixcommon_global.h"


class GIXCOMMON_EXPORT TargetManager
{
public:
	TargetManager();
	~TargetManager();
	void init();

	BuildTarget *composeBuildTarget(BuildTarget *parent, const QString &target_type, QVariantMap *props, IBuildableItem *bi);

private:
	QMap<QString, TargetDefinition *> target_defs;

	TargetDefinition *parseTargetDef(const QDomElement &xe);
	TargetDefinition *lookupTarget(const QString &tt, QVariantMap *props);

	BuildTarget *composeTargetUsingHandler(TargetDefinition *tdef, BuildTarget *parent, const QString &target_type, QVariantMap *props, IBuildableItem *bi);
};

