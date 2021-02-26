#pragma once

#include <QString>
#include <QDomElement>
#include "ProjectItem.h"
#include "Project.h"
#include "PropertySource.h"
#include "BuildTarget.h"
#include "IBuildableItem.h"
#include "IPersistableProjectItem.h"
#include "CobolModuleMetadata.h"

using namespace std;

class Project;
class QFile;

class GIXCOMMON_EXPORT ProjectFile : public ProjectItem, public PropertySource, public IPersistableProjectItem, public IBuildableItem
{
	friend class ProjectCollection;
	friend class Project;

public:
	ProjectFile();
	virtual ~ProjectFile();

	static ProjectFile *newProjectFile(ProjectItem* owner, QString filepath, bool type_from_ext = true);

	void extractCopyDeps(QList<ProjectFile *>& res, const QStringList& copy_dirs, const QStringList& copy_ext_list);
	Project *getParentProject();
	bool getOutputModuleAndFile(QString, QString, QString&, QString&);
	bool isStartupItem();
	bool isHttpService();
	bool isRestService();
	bool isSoapService();
	bool isCompilable();

	QString extractModuleName();
	QString getSymbolFilename(QString configuration = QString(), QString platform = QString());

	bool writeSourceTemplate(QFile& f, ProjectFileType t);

	// Inherited via ProjectItem
	virtual ProjectItemType GetItemType() override;
	virtual QString GetDisplayName() override;


	// Inherited via IPropertySource
	virtual PropertyDefinitionCollection& PropertyGetDefinitions() override;

	// Inherited via IPersistableProjectItem
	virtual bool load(ProjectItem * owner, QString filepath) override;
	virtual bool save(ProjectItem* owner = nullptr, QString filepath = QString::null) override;
	virtual bool revert() override;
	virtual bool isDirty() override;
	virtual void setDirty(bool b) override;

	// Inherited via IBuildableItem
	virtual BuildTarget * getBuildTarget(QMap<QString, QVariant>, BuildTarget *parent) override;
	virtual QList<IBuildableItem *> getDependencies(const QString &use, QVariantMap *props, bool *yield_ownership) override;

	ProjectFile *clone();


private:
	bool is_virtual;
	QList<BuildTarget *> getCobolCopyFiles();
	void add_copy_deps(const QStringList& copy_dirs, const QStringList& copy_ext_list, ProjectFile *pf, QList<ProjectFile *>& res);
	QString locate_copy_file(QString cpy_name, const QStringList& copy_dirs, const QStringList & copy_ext_list);
	
	QString resolve_module_reference(QString fp);

#ifdef USES_ENH_WS_BUILD
	BuildTarget * add_soap_ws_target(BuildTarget *obj_target, QString srcfile, QVariantMap local_env);
	BuildTarget * add_rest_ws_target(BuildTarget *obj_target, QString srcfile, QVariantMap local_env);
#endif
	static ProjectFile *fromCopyFile(QString, QString);


};

