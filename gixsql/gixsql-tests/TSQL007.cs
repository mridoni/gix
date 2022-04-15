using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Data.Common;
using System.IO;
using System.Linq;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    [TestCategory("Misc SECTIONs")]
    public class TSQL007 : GixSqlTestBase
    {
        [TestInitialize]
        public new void Begin()
        {
            base.Begin();

            Environment.SetEnvironmentVariable("GIXSQL_DEBUG_LOG_ON", "1");
            Environment.SetEnvironmentVariable("GIXSQL_DEBUG_LOG", Path.Combine(TestTempDir, "gisql-debug.log"));
            Environment.SetEnvironmentVariable("GIXSQL_ERR_LOG", Path.Combine(TestTempDir, "gisql-error.log"));
        }


        [TestMethod]
        [CobolSource("TSQL007A.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL007A_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(false, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr() + "." + get_datasource_pwd());

            Console.WriteLine("Performed syntax check only: OK");
        }

    }
}
