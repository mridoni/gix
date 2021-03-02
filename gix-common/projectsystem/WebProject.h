#pragma once

#include "Project.h"

class WebProject : public Project
{
public:
	WebProject(const QMap<QString, QVariant>& opts);
	WebProject(const Project& prj);

	virtual BuildTarget *getBuildTarget(QMap<QString, QVariant>, BuildTarget *parent) override;
	virtual QList<IBuildableItem *> getDependencies(const QString &use, QVariantMap *props, bool *yield_ownership) override;

private:
	QList<IBuildableItem *> splitProject();
};

