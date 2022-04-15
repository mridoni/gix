using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace gix_ide_tests
{
    public class CobolSourceAttribute : Attribute
    {
        public string Module;
        public string[] Dependencies;

        public CobolSourceAttribute(string m, params string[] d)
        {
            Module = m;
            Dependencies = d;
        }
    }
}
