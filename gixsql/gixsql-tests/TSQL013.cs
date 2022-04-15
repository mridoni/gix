using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    [TestCategory("FD parse error (#65)")]
    public class TSQL013 : GixSqlTestBase
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
        [CobolSource("TSQL013A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL013A_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            Console.WriteLine("Performed syntax check only: OK");
        }

 
    }
}
