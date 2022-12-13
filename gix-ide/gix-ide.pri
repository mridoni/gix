
HEADERS += \
    $$PWD/ide_sink.h \
    ./boolinq.h \
	./Changeling.h \
	./CodeEditor.h \
	./ConsoleWidget.h \
	./ConsoleWindow.h \
	./CustomDialog.h \
	./DatabaseConnectionDialog.h \
	./DataWindow.h \
	./DbCodeGenerator.h \
	./DbConnection.h \
	./DbManagerWindow.h \
	./IDebugDriver.h \
	./ExperimentalDebugDriver.h \
	./StandardDebugDriver.h \
	./DebugDriverUtils.h \
	./DebugManager.h \
	./DependenciesWindow.h \
	./ElidedLabel.h \
	./EolMode.h \
	./GixVersion.h \
	./IdeDbManager.h \
	./Ide.h \
	./IdeLogManager.h \
	./IdeSearchManager.h \
	./IdeStatus.h \
	./IdeTaskManager.h \
	./IPersistableProjectItem.h \
	./ISymbolProvider.h \
	./MainWindow.h \
	./MdiChild.h \
	./NavigationWindow.h \
	./NewProjectDialog.h \
	./OutputWindow.h \
	./ProjectCollectionWindow.h \
	./ProjectDependencyTracker.h \
	./PropertyOptionsDialog.h \
	./PropertyWindow.h \
	./ProtectedLineEdit.h \
	./resource.h \
	./RestOptionsDialog.h \
	./SearchDialog.h \
	./SettingsDialog.h \
	./SoapOptionsDialog.h \
	./SqlMdiChild.h \
	./StdStreamRedirect.h \
	./TargetDefinition.h \
	./UiUtils.h \
	./WatchWindow.h \
	./AddCompilerDialog.h \
	./NoWheelComboBox.h \
	./AddCompilerWizard.h \
	./IdeStatusSyncSetter.h \ 
	./DragDropTreeWidget.h \
	./DragDropTableWidget.h \
	./DebugDriverFactory.h \
	./OutputWindowLogger.h \
	./ErrorWindow.h

SOURCES += \
        ./Changeling.cpp \
	./CodeEditor.cpp \
	./ConsoleWidget.cpp \
	./ConsoleWindow.cpp \
	./CustomDialog.cpp \
	./DatabaseConnectionDialog.cpp \
	./DataWindow.cpp \
	./DbCodeGenerator.cpp \
	./DbConnection.cpp \
	./DbManagerWindow.cpp \
	./IDebugDriver.cpp \
	./ExperimentalDebugDriver.cpp \
	./StandardDebugDriver.cpp \
	./DebugDriverUtils.cpp \
	./DebugManager.cpp \
	./DependenciesWindow.cpp \
	./ElidedLabel.cpp \
	./GixVersion.cpp \
	./Ide.cpp \
	./IdeDbManager.cpp \
	./IdeLogManager.cpp \
	./IdeSearchManager.cpp \
	./IdeTaskManager.cpp \
	./main.cpp \
	./MainWindow.cpp \
	./MdiChild.cpp \
	./NavigationWindow.cpp \
	./NewProjectDialog.cpp \
	./OutputWindow.cpp \
	./ProjectCollectionWindow.cpp \
	./ProjectDependencyTracker.cpp \
	./PropertyOptionsDialog.cpp \
	./PropertyWindow.cpp \
	./ProtectedLineEdit.cpp \
	./RestOptionsDialog.cpp \
	./SearchDialog.cpp \
	./SettingsDialog.cpp \
	./SoapOptionsDialog.cpp \
	./SqlMdiChild.cpp \
	./StdStreamRedirect.cpp \
	./TargetDefinition.cpp \
	./UiUtils.cpp \
	./WatchWindow.cpp \
	./AddCompilerDialog.cpp \
	./AddCompilerWizard.cpp \
	./IdeStatusSyncSetter.cpp \ 
	./DragDropTreeWidget.cpp \
	./DragDropTableWidget.cpp \
	./DebugDriverFactory.cpp \
	./OutputWindowLogger.cpp \
	./ErrorWindow.cpp

FORMS += ./NewProject.ui \
	./Settings.ui  \
	./SoapOptions.ui \
	./RestOptions.ui \
	./Search.ui \
	./AddCompilerDialog.ui
	
RESOURCES += icons.qrc \
    templates.qrc
