#pragma once

#include "Project.h"

class MultiBinaryProject : public Project
{
	friend class MultiArtifactBuilder;

public: 
	MultiBinaryProject(const QMap<QString, QVariant>& opts);
	MultiBinaryProject(const Project& prj);

	virtual BuildTarget *getBuildTarget(QMap<QString, QVariant>, BuildTarget *parent) override;
	virtual QList<IBuildableItem *> getDependencies(const QString &use, QVariantMap *props, bool *yield_ownership) override;

private:
	QList<IBuildableItem *> splitProject();

};

