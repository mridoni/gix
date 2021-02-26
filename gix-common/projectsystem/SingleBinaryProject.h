#pragma once

#include "Project.h"

class SingleBinaryProject : public Project
{
public:
    SingleBinaryProject();
    SingleBinaryProject(const QMap<QString, QVariant>& opts);
    
    static SingleBinaryProject *fromProject(const Project *prj, bool copy_children = false);

    

    BuildTarget *generate_mksymbol_target(BuildTarget *parent, BuildTarget *bt_obj);

    virtual BuildTarget *getBuildTarget(QMap<QString, QVariant>, BuildTarget *parent) override;
    virtual QList<IBuildableItem *> getDependencies(const QString &use, QVariantMap *props, bool *yield_ownership) override;

};

