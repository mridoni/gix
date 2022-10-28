#include "DebugDriverFactory.h"

#include "StandardDebugDriver.h"
#include "ExperimentalDebugDriver.h"

IDebugDriver* DebugDriverFactory::get(DebugDriverType t, DebugManager *dm)
{
    switch (t) {
        case DebugDriverType::Standard:
            return new StandardDebugDriver(dm);

        case DebugDriverType::Experimental:
            return new ExperimentalDebugDriver(dm);
    }

    return nullptr;
}
