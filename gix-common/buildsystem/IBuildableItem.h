#pragma once

#include <QString>
#include <QList>
#include <QVariant>
#include <QVariantMap>

class BuildTarget;
class IBuildableItem;
class ArtifactBuilder;

class IBuildableItem {

public:
	virtual BuildTarget *getBuildTarget(QMap<QString, QVariant>, BuildTarget *parent) = 0;
	virtual QList<IBuildableItem *> getDependencies(const QString& use, QVariantMap *props, bool *yield_ownership) = 0;

};