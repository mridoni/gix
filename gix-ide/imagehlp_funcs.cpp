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