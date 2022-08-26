using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace gixsql_tests
{
    [AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
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
