#pragma once

#include <QString>

class IPersistableProjectItem {
	
public:
	
	virtual bool load(ProjectItem *owner, QString filepath) = 0;
	virtual bool save(ProjectItem* owner = nullptr, QString filepath = QString()) = 0;
	virtual bool revert() = 0;

	virtual bool isDirty() = 0;
	virtual void setDirty(bool b) = 0;
};