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

#include <windows.h>
#include <imagehlp.h>

#include <QString>
#include <QList>

using namespace std;

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