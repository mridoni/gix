/*
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/


#include "ServiceModuleInfo.h"
#include "SymbolBufferReader.h"
#include "PathUtils.h"
#include "SysUtils.h"
#include "linq/linq.hpp"
#include "QLogger.h"

#include <QDir>
#include <QCoreApplication>

#if _WIN32
#include <Windows.h>
#include <imagehlp.h>
#define LIBHANDLE HINSTANCE
void ListDLLFunctions(QString sADllName, QList<QString> &slListOfDllFunctions);
#else
#include <dlfcn.h>
#define LIBHANDLE void *
#endif

#if defined(_WIN64)
#define GIX_SYM_PFX "__GIX_SYM_"
#else
#define GIX_SYM_PFX "_GIX_SYM_"
#endif



ServiceModuleInfo *ServiceModuleInfo::load(ServiceConfig *svc)
{
	ExternalInterfaceData *res = nullptr;

	//QString search_path = svc->getServerConfig()->getBasePath();
	//if (search_path.isEmpty() || !QDir(search_path).exists()) {
	QString search_path = svc->getServerConfig()->getSearchPath();
	if (search_path.isEmpty() || search_path == ".")
		search_path = QCoreApplication::applicationDirPath();

	if (search_path.isEmpty() || !QDir(search_path).exists()) {
		QLogger::QLog_Error(svc->getLogModule(), QString("No base path defined"));
		return nullptr;
	}
	//}

	QString shared_module = svc->getSharedModule();
	if (!shared_module.isEmpty()) {
		if (!QFileInfo(shared_module).isAbsolute()) {
			shared_module = PathUtils::combine(search_path, shared_module);
		}
	}
	else {
		shared_module = PathUtils::changeExtension(svc->getProgram(), (SysUtils::isWindows() ? ".dll" : ".so"));
		shared_module = PathUtils::combine(search_path, shared_module);
	}

	if (!QFile::exists(shared_module)) {
		QLogger::QLog_Error(svc->getLogModule(), QString("Cannot locate shared module for service %1: %2").arg(svc->getName()).arg(shared_module));
		return nullptr;
	}

	QLogger::QLog_Trace(svc->getLogModule(), QString("Extracting symbol info from %1").arg(shared_module));

	ServiceModuleInfo *ed = extractSharedModuleInfo(shared_module);
	if (!ed) {
		QLogger::QLog_Error(svc->getLogModule(), QString("Cannot extract module info for service %1 from module %2").arg(svc->getName()).arg(shared_module));
		return nullptr;
	}

	return ed;
}

bool ServiceModuleInfo::containsEntry(const QString &name)
{
	return entries.contains(name);
}

DataEntry *ServiceModuleInfo::getEntry(const QString &name)
{
	return entries.contains(name) ? entries[name] : nullptr;
}

QList<DataEntry *> ServiceModuleInfo::getEntryList() const
{
	return entries.values();
}


ServiceModuleInfo *ServiceModuleInfo::extractSharedModuleInfo(const QString &shared_module)
{
	char bfr[256];
	//IDbInterface *dbi = NULL;
	LIBHANDLE libHandle = NULL;

#if defined(_WIN32) || defined(_WIN64)
	QStringList cob_module_list;

	ListDLLFunctions(shared_module, cob_module_list);

	libHandle = LoadLibrary(QDir::toNativeSeparators(shared_module).toLocal8Bit().constData());

	if (!libHandle)
		return nullptr;

	for (QString module : cob_module_list) {
		void *p__GIX_SYM_MOD_EC = (void *)GetProcAddress(libHandle, (GIX_SYM_PFX + module + "_EC").toLocal8Bit().constData());
		void *p__GIX_SYM_MOD_ES = (void *)GetProcAddress(libHandle, (GIX_SYM_PFX + module + "_ES").toLocal8Bit().constData());

		void *p__GIX_SYM_MOD_MC = (void *)GetProcAddress(libHandle, (GIX_SYM_PFX + module + "_MC").toLocal8Bit().constData());
		void *p__GIX_SYM_MOD_MS = (void *)GetProcAddress(libHandle, (GIX_SYM_PFX + module + "_MS").toLocal8Bit().constData());

		uint8_t *__GIX_SYM_MOD_E = (uint8_t *)GetProcAddress(libHandle, (GIX_SYM_PFX + module + "_E").toLocal8Bit().constData());
		uint8_t *__GIX_SYM_MOD_M = (uint8_t *)GetProcAddress(libHandle, (GIX_SYM_PFX + module + "_M").toLocal8Bit().constData());

		if (!p__GIX_SYM_MOD_EC || !p__GIX_SYM_MOD_ES || !p__GIX_SYM_MOD_MC || !p__GIX_SYM_MOD_MS || !__GIX_SYM_MOD_E || !__GIX_SYM_MOD_M) {
			FreeLibrary(libHandle);
			return nullptr;
		}
		int __GIX_SYM_MOD_EC = *(int *)p__GIX_SYM_MOD_EC;
		int __GIX_SYM_MOD_ES = *(int *)p__GIX_SYM_MOD_ES;
		int __GIX_SYM_MOD_MC = *(int *)p__GIX_SYM_MOD_MC;
		int __GIX_SYM_MOD_MS = *(int *)p__GIX_SYM_MOD_MS;

		ServiceModuleInfo *smi = new ServiceModuleInfo();

		SymbolBufferReader sr(__GIX_SYM_MOD_E, __GIX_SYM_MOD_ES);
		for (int i = 0; i < __GIX_SYM_MOD_EC; i++) {
			DataEntry *e = new DataEntry();

			e->name = sr.readString();
			e->path = sr.readString();
			e->type = (WsEntryType)sr.readInt();
			e->level = sr.readInt();
			e->base_var_name = sr.readString();
			e->offset_local = sr.readInt();

			e->storage_size = sr.readInt();

			e->display_size = sr.readInt();
			e->is_signed = sr.readInt();
			e->decimals = sr.readInt();
			e->format = sr.readString();
			e->storage_type = (WsEntryStorageType)sr.readInt();
			e->storage = sr.readString();
			e->occurs = sr.readInt();
			e->redefines = sr.readString();

			if (!e->path.startsWith("LS:")) {
				delete e;
				continue;
			}

			smi->entries.insert(e->name, e);
		}

		// Not needed here
		//SymbolBufferReader sm(__GIX_SYM_MOD_M, __GIX_SYM_MOD_MS);
		//for (int i = 0; i < __GIX_SYM_MOD_MC; i++) {
		//	QString sym_name = sm.readString();
		//	QString var_name = sm.readString();
		//	int storage_size = sm.readInt();

		//	if (!cmi->locals.contains(var_name))
		//		continue;

		//	VariableResolverData *rd = cmi->locals[var_name];
		//	rd->local_sym_name = sym_name;
		//}
		// Not needed here (end)

		FreeLibrary(libHandle);

		return smi;
	}



#else
	// To be implemented
	/*
		strcat(bfr, ".so");

		libHandle = dlopen(bfr, RTLD_NOW);
		if (libHandle != NULL) {
			dblib_provider = (DBLIB_PROVIDER_FUNC)dlsym(libHandle, "get_dblib");
			// If the function address is valid, call the function.
			if (dblib_provider != NULL) {
				dbi = dblib_provider();
				lib_map[dbi] = libHandle;
			}
			else {
				return nullptr;
			}

			// Library not freed here
		}
		else {
			return nullptr;
		}
	*/
#endif


	return nullptr;
}


void ServiceModuleInfo::add_tree_children(DataEntry *e)
{
	QList<DataEntry *> entries = this->getEntryList();

	QString path = e->path;
	QList<DataEntry *> children = QList<DataEntry *>::fromStdList(cpplinq::from(entries).where([path](DataEntry *ee) {
		if (!ee->path.startsWith(path + ":"))
			return false;

		return !ee->path.mid(path.length() + 1).contains(":");	// Only direct children

	}).to_list());
	for (DataEntry *c : children) {
		e->children.append(c);
		c->parent = e;
		add_tree_children(c);
	}
	if (e->children.size())
		e->type = WsEntryType::Group;
}

DataEntry *ServiceModuleInfo::buildInterfaceEntryTree(const QString &root_field_id)
{
	if (root_field_id.isEmpty() || !this->containsEntry(root_field_id))
		return nullptr;

	DataEntry *e = this->getEntry(root_field_id);
	add_tree_children(e);

	return e;
}

#if defined(_WIN32) || defined(_WIN64)

void ListDLLFunctions(QString sADllName, QList<QString> &slListOfDllFunctions)
{
	DWORD *dNameRVAs(0);
	_IMAGE_EXPORT_DIRECTORY *ImageExportDirectory;
	unsigned long cDirSize;
	_LOADED_IMAGE LoadedImage;
	QString sName;
	slListOfDllFunctions.clear();
	if (MapAndLoad(sADllName.toLocal8Bit().constData(), NULL, &LoadedImage, TRUE, TRUE)) {
		ImageExportDirectory = (_IMAGE_EXPORT_DIRECTORY *)
			ImageDirectoryEntryToData(LoadedImage.MappedAddress,
				false, IMAGE_DIRECTORY_ENTRY_EXPORT, &cDirSize);
		if (ImageExportDirectory != NULL) {
			dNameRVAs = (DWORD *)ImageRvaToVa(LoadedImage.FileHeader,
				LoadedImage.MappedAddress,
				ImageExportDirectory->AddressOfNames, NULL);
			for (size_t i = 0; i < ImageExportDirectory->NumberOfNames; i++) {
				sName = (char *)ImageRvaToVa(LoadedImage.FileHeader,
					LoadedImage.MappedAddress,
					dNameRVAs[i], NULL);
				slListOfDllFunctions.append(sName);
			}
		}
		UnMapAndLoad(&LoadedImage);
	}
}
#endif
