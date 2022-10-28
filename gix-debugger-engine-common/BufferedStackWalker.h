#pragma once

#ifdef _WIN32

#include "StackWalker.h"

#include <string>
#include <vector>

class BufferedStackWalker : public StackWalker
{
public:
	BufferedStackWalker() : StackWalker() {}
	BufferedStackWalker(DWORD dwProcessId, HANDLE hProcess) : StackWalker(dwProcessId, hProcess) {}
	std::vector<std::string> lines;

protected:
	virtual void OnOutput(LPCSTR szText);
};

#endif