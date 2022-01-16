#pragma once

#ifdef _WIN32

#include "StackWalker.h"
#include <QStringList>

class BufferedStackWalker : public StackWalker
{
public:
	BufferedStackWalker() : StackWalker() {}
	BufferedStackWalker(DWORD dwProcessId, HANDLE hProcess) : StackWalker(dwProcessId, hProcess) {}
	QStringList lines;

protected:
	virtual void OnOutput(LPCSTR szText);
};

#endif