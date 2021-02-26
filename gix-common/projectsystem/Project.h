#pragma once

#include <QString>
#include <QList>
#include <QMap>
#include <QVariant>

#include "ProjectType.h"
#include "ProjectItem.h"
#include "ProjectFile.h"
#include "ProjectFolder.h"
#include "PropertySource.h"
#include "IPersistableProjectItem.h"
#include "ProjectPropertyDefinitionCollection.h"
#include "BuildTarget.h"
#include "gixcommon_global.h"

using namespace std;

class ProjectFile;
class CompilerConfiguration;
class ProjectBuilder;

class GIXCOMMON_EXPORT Project : public ProjectItem, public PropertySource, public IPersistableProjectItem, public IBuildableItem
{
	friend class ProjectDependencyTracker;
	friend class ProjectCollection;
	friend class SingleBinaryProject;
	friend class SingleArtifactBuilder;

public:

	static Project* newProject(ProjectType type, ProjectFileType add_filetype, QString prj_filepath, const QMap<QString, QVariant>& opts);
	static Project* loadProject(ProjectItem* owner, QString filepath);
	
	Project();
	virtual ~Project();
	
	Project(const Project&);
	
	void addFile(ProjectFile *pf);
	
	void addToCopyPathList(QString);
	static int getLoadableItemCount(QString);
	int getLoadableItemCount();
	QList<ProjectFile*> getAllCompilableFiles();
	QList<ProjectFile*> getAllCopyFiles();
	QString getStartupItemName();
	ProjectFile *getStartupItem();
	ProjectType getType();
	
	QString locateSourceFileByModuleName(QString module_name, bool in_debugging_session = false);
	ProjectFile *locateProjectFileByPath(QString filepath, bool include_copy_files = false);
	QString getBuildDirectory(QString configuration, QString platform);
	QStringList getCopyDirList();
	QStringList getCopyExtList();
	
	QMap<QString, QString> getBuildFileMap();
	
	bool hasHttpService();
	bool isEsql();
	
	// Inherited via ProjectItem
	virtual ProjectItemType GetItemType() override;
	virtual QString GetDisplayName() override;
	
	// Inherited via PropertySource
	virtual PropertyDefinitionCollection& PropertyGetDefinitions() override;
	
	// Inherited via IPersistableProjectItem
	virtual bool load(ProjectItem *owner, QString filepath) override;
	virtual bool save(ProjectItem *owner = nullptr, QString filepath = QString::null) override;
	virtual bool revert() override;
	virtual bool isDirty() override;
	virtual void setDirty(bool b) override;
	 
	 
	//virtual ArtifactBuilder *getBuilder(ProjectBuilder* _main_builder) = 0;
	virtual BuildTarget *getPostBuildTarget(QMap<QString, QVariant> build_environment, BuildTarget *parent);

	static void deleteItem(IBuildableItem *item);

protected:
	QString description;
	ProjectType type;

	//void addDependencies(BuildTarget*, const QMap<QString, QVariant>& build_environment);

private:
	bool is_dirty;

	void dump_children(QDomNode&, ProjectItem *);
	void append_children(QDomNode&, ProjectItem *);
	void add_compilable_children(QList<ProjectFile *>&, ProjectItem *, bool include_copy_files = false);
	void add_copy_children(QList<ProjectFile*>& res, ProjectItem* pi);

};

