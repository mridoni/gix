#pragma once

enum class ProjectFileType
{
	NoFile = 0,

	Source = 1,
	Copy = 2
};

enum class ProjectType
{
	NoProject = 0,

	SingleBinary = 1,
	MultipleBinaries = 2,
	Web = 3,
	CobJava = 4,
	CobCSharp = 5


};

