#include "BufferedStackWalker.h"

#ifdef _WIN32

void BufferedStackWalker::OnOutput(LPCSTR szText)
{
	lines.append(szText);
	StackWalker::OnOutput(szText);
}

#endif
