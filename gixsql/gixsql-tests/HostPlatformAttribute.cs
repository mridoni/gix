using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace gixsql_tests
{
    public class HostPlatformAttribute : Attribute
    {
        public string Platform;

        public HostPlatformAttribute(string p)
        {
            Platform = p;
        }
    }
}
