using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace gix_ide_tests
{
    public class IdeHostPlatformAttribute : Attribute
    {
        public string Platform;

        public IdeHostPlatformAttribute(string p)
        {
            Platform = p;
        }
    }
}
