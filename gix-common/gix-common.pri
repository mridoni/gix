HEADERS +=  ./CobolVar.h \
			./CobolVarList.h \
			./DataEntry.h \
			./gixcommon_global.h \
			./GixGlobals.h \
			./IGixLogManager.h \
			./MacroManager.h \
			./PropertyConsts.h \
			./QLogger.h \
			./RsrcUtils.h \
			./ServerConfig.h \
			./SymbolBufferReader.h \
			./SymbolMappingEntry.h \
			./utils.h \
			./XPathParser.h \
			./ESQLConfiguration.h \
			./projectsystem/IPersistableProjectItem.h \
			./projectsystem/MultiBinaryProject.h \
			./projectsystem/PrjCollPropertyDefinitionCollection.h \
			./projectsystem/ProjectCollection.h \
			./projectsystem/ProjectFile.h \
			./projectsystem/ProjectFilePropertyDefinitionCollection.h \
			./projectsystem/ProjectFolder.h \
			./projectsystem/ProjectFolderPropertyDefinitionCollection.h \
			./projectsystem/Project.h \
			./projectsystem/ProjectItem.h \
			./projectsystem/ProjectItemType.h \
			./projectsystem/ProjectPropertyDefinitionCollection.h \
			./projectsystem/ProjectType.h \
			./projectsystem/PropertyDefinitionCollection.h \
			./projectsystem/PropertyDefinition.h \
			./projectsystem/PropertySource.h \
			./projectsystem/SingleBinaryProject.h \
			./projectsystem/WebProject.h \
			./buildsystem/BuildActionCompileHandler.h \
			./buildsystem/BuildActionHandler.h \
			./buildsystem/BuildActionLinkHandler.h \
			./buildsystem/BuildActionMakeListingHandler.h \
			./buildsystem/BuildActionNoOpHandler.h \
			./buildsystem/BuildActionPreprocessESQLHandler.h \
			./buildsystem/BuildActionRestModuleHandler.h \
			./buildsystem/BuildConsts.h \
			./buildsystem/BuildDriver.h \
			./buildsystem/BuildResult.h \
			./buildsystem/BuildTarget.h \
			./buildsystem/CompilerConfiguration.h \
			./buildsystem/CompilerDefinition.h \
			./buildsystem/CompilerEnvironment.h \
			./buildsystem/CompilerManager.h \
			./buildsystem/IBuildableItem.h \
			./buildsystem/LogOutputType.h \
			./buildsystem/TargetDefinition.h \
			./buildsystem/TargetManager.h \
			./metadata/CobolModuleMetadata.h \
			./metadata/ListingFileParser.h \
			./metadata/ListingFileParserResult.h \
			./metadata/MetadataManager.h \
			./metadata/MetadataWorker.h
	 
SOURCES +=  ./GixGlobals.cpp \
			./QLogger.cpp \
			./CobolVarList.cpp \
			./MacroManager.cpp \
			./SymbolBufferReader.cpp			 \
			./DataEntry.cpp \
			./ServerConfig.cpp \
			./CobolVar.cpp \
			./PropertyConsts.cpp \
			./RsrcUtils.cpp \
			./utils.cpp \
			./ESQLConfiguration.cpp \
			./projectsystem/Project.cpp \
			./projectsystem/WebProject.cpp \
			./projectsystem/SingleBinaryProject.cpp \
			./projectsystem/MultiBinaryProject.cpp \
			./projectsystem/ProjectFilePropertyDefinitionCollection.cpp \
			./projectsystem/PropertySource.cpp \
			./projectsystem/ProjectFile.cpp \
			./projectsystem/PropertyDefinitionCollection.cpp \
			./projectsystem/ProjectItem.cpp \
			./projectsystem/ProjectFolder.cpp \
			./projectsystem/PrjCollPropertyDefinitionCollection.cpp \
			./projectsystem/ProjectFolderPropertyDefinitionCollection.cpp \
			./projectsystem/PropertyDefinition.cpp \
			./projectsystem/ProjectCollection.cpp \
			./projectsystem/ProjectPropertyDefinitionCollection.cpp \
			./buildsystem/BuildActionNoOpHandler.cpp \
			./buildsystem/TargetManager.cpp \
			./buildsystem/CompilerConfiguration.cpp \
			./buildsystem/BuildResult.cpp \
			./buildsystem/BuildConsts.cpp \
			./buildsystem/BuildDriver.cpp \
			./buildsystem/BuildActionHandler.cpp \
			./buildsystem/CompilerDefinition.cpp \
			./buildsystem/BuildActionMakeListingHandler.cpp \
			./buildsystem/TargetDefinition.cpp \
			./buildsystem/BuildActionCompileHandler.cpp \
			./buildsystem/CompilerManager.cpp \
			./buildsystem/BuildTarget.cpp \
			./buildsystem/BuildActionPreprocessESQLHandler.cpp \
			./buildsystem/BuildActionLinkHandler.cpp \
			./metadata/MetadataWorker.cpp \
			./metadata/ListingFileParser.cpp \
			./metadata/CobolModuleMetadata.cpp \
			./metadata/MetadataManager.cpp \
			./metadata/ListingFileParserResult.cpp

RESOURCES += ./general.qrc
