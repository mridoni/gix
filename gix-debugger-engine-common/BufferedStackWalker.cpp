#include "BufferedStackWalker.h"

#ifdef _WIN32

void BufferedStackWalker::OnOutput(LPCSTR szText)
{
	lines.push_back(szText);
	StackWalker::OnOutput(szText);
}

#endif
