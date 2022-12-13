/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
*/

#include "CompilerManager.h"
#include "PathUtils.h"
#include "IGixLogManager.h"
#include "GixGlobals.h"
#include "SysUtils.h"
#include <QDir>
#include <QFile>
#include <QRegularExpression>


CompilerManager::CompilerManager()
{
	init();
}

CompilerManager::~CompilerManager()
{
	cleanup();
}

void CompilerManager::init()
{
	IGixLogManager *logger = GixGlobals::getLogManager();

	logger->trace(LOG_CONFIG, "Initializing compiler manager");

	if (compiler_defs.size() > 0)
		cleanup();

	QString cdir = GixGlobals::getCompilerDefsDir();
	if (cdir.isEmpty()) {
		logger->error(LOG_CONFIG, "Compiler definitions directory is not set");
		return;
	}

	QDir compiler_defs_dir(QDir::fromNativeSeparators(cdir));
	if (!compiler_defs_dir.exists()) {
		logger->error(LOG_CONFIG, "Cannot locate compiler definitions directory: {}", cdir);
		return;
	}

	logger->trace(LOG_CONFIG, "Compiler definitions directory is {}", cdir);

	compiler_defs_dir.setNameFilters(QStringList("*.def"));

	for (QFileInfo def_file : compiler_defs_dir.entryInfoList(QDir::NoDotAndDotDot | QDir::Files)) {
		CompilerDefinition* cd = CompilerDefinition::load(def_file.absoluteFilePath());
		if (cd == nullptr) {
			logger->error(LOG_CONFIG, "Cannot parse compiler definition {}", def_file.absoluteFilePath());
			continue;
		}

		compiler_defs[cd->getId()] = cd;
	}

#if defined(__linux__)
	if (!compiler_defs.contains("system-default")) {

		QString compiler_info;
        QStringList compiler_errs;
		CompilerDefinition* cd_default = CompilerManager::tryGetDefault(compiler_info);

		if (cd_default != nullptr) {
			QStringList test_info;
			if (cd_default->testConfiguration(compiler_errs) && cd_default->save()) {
				logger->info(LOG_CONFIG, "Adding distribution-provided compiler ({})", compiler_info);
				compiler_defs[cd_default->getId()] = cd_default;
			}
			else {
				logger->warn(LOG_CONFIG, "Cannot add distribution-provided compiler");
			}
		}
	}
#endif	

}

QMap<QString, CompilerDefinition*> CompilerManager::getCompilers()
{
	return compiler_defs;
}

#ifdef __linux__

bool getDistributionInfo(QString d, QString v)
{
    return false;    
}

QString getInfoValue(QStringList rows, QString k)
{
    int idx = rows.indexOf(QRegularExpression("^" + k + " "));  
    if (idx < 0)
        return "";
    
    QString s = rows[idx];
    s = s.replace(k, "").trimmed();
    if (s.startsWith(":"))
        s = s.mid(1);
    
    return s;
}

#endif

GIXCOMMON_EXPORT CompilerDefinition* CompilerManager::tryGetDefault(QString& compiler_info)
{
#ifdef __linux__
	QProcess findProcess;
	QStringList arguments;
	arguments << "cobc";
	findProcess.start("which", arguments);
	findProcess.setReadChannel(QProcess::ProcessChannel::StandardOutput);

    if (findProcess.waitForFinished()) {
    
        QString retStr(findProcess.readAll());
    
        retStr = retStr.trimmed();
        if (!retStr.isEmpty()) {
            QFile file(retStr);
            QFileInfo check_file(file);
            if (check_file.exists() && check_file.isFile()) {
        
                arguments.clear();
                arguments << "-info";
                findProcess.start(retStr, arguments);
                findProcess.setReadChannel(QProcess::ProcessChannel::StandardOutput);
            
                if (findProcess.waitForFinished()) {
                
                    retStr = findProcess.readAll().trimmed();
                    if (!retStr.isEmpty()) {
                        QStringList rows = retStr.split('\n');
                        QString gc_version = rows.at(0).split(' ').last();
                        QString dist_name, dist_version;
                        if (!getDistributionInfo(dist_name, dist_version)) {
                            dist_name = "linux";
                            dist_version = "00";
                        }
                        CompilerDefinition *cd = new CompilerDefinition();
#if 0                        
						cd->setId("gnucobol-" + dist_name + "-" + dist_version);
#endif						
                        cd->setId("system-default");
                        cd->setHomedir("/usr");
                        cd->setHostPlatform(SysUtils::getGixBuildPlatform());
                        cd->setName("Default - GnuCOBOL " + gc_version);
                        cd->setIsVsBased(false);
                        cd->setVersion(gc_version);
                        
                        CompilerPlatformDefinition *p = new CompilerPlatformDefinition();
                        p->setBinDirPath(check_file.absoluteDir().absolutePath());
                        p->setConfigDirPath(getInfoValue(rows, "COB_CONFIG_DIR"));
                        p->setCopyDirPath(getInfoValue(rows, "COB_COPY_DIR"));
                        p->setIncludeDirPath("/usr/include");
                        p->setLibDirPath("/usr/lib");
                        
                        cd->addPlatform(SysUtils::getGixBuildPlatform(), p);
                        
                        return cd;
                    }
                }
        
            }
        }
    }
#endif
	return nullptr;
}

void CompilerManager::cleanup()
{
	for (CompilerDefinition *cd : compiler_defs) {
		delete cd;
	}

	compiler_defs.clear();
}
