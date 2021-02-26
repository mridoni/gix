#pragma once

#include <QString>
#include <QStringList>
#include <QMap>

/*
	<target type="singleartifact/dll">
		<filename>${prj.target.filename}</filename>
		<provides>dll</provides>
		<uses>obj</uses>
		<action>
			<action_id>link</action_id>
			<action_args>obj:*</action_args>
		</action>
	</target>
*/

class TargetDefinition
{
public:
	QString type;
	QString fulltype;
	QString filename;
	QStringList provides;
	QStringList uses;
	QString location;
	QString handler;
	bool is_optional;
	bool is_virtual;
	QString action_id;
	QString action_args;
	QStringList setdata;

	QMap<QString, bool> resolve_checks;
};

