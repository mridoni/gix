using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace gixsql_tests
{
    [AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
    public class GixSqlDataSourceAttribute : Attribute
    {
        public string type;
        public int index;

        public GixSqlDataSourceAttribute(string t, int i)
        {
            type = t;
            index = i;
        }
    }
}
